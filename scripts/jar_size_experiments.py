#!/usr/bin/env python3
"""Temporary, read-only-to-release-branches JAR-size experiment runner.

Runs on a disposable GitHub Actions workspace. Candidate POM changes only exist
in shallow clones under RUNNER_TEMP. No releases, tags or branches are updated.
"""
from collections import defaultdict
from pathlib import Path
import os
import re
import subprocess
import time
import xml.etree.ElementTree as ET
from zipfile import ZipFile

ROOT = Path(__file__).resolve().parents[1]
WORK = Path(os.environ.get("RUNNER_TEMP", "/tmp")) / "votingplugin-size-experiments"
WORK.mkdir(parents=True, exist_ok=True)
(WORK / "logs").mkdir(exist_ok=True)
PROJECTS = {
    "SimpleAPI": (WORK / "SimpleAPI", "SimpleAPI"),
    "AdvancedCore": (WORK / "AdvancedCore", "AdvancedCore"),
    "VotingPlugin": (ROOT, "VotingPlugin"),
}
NS_URI = "http://maven.apache.org/POM/4.0.0"
NS = {"m": NS_URI}
ET.register_namespace("", NS_URI)
ET.register_namespace("xsi", "http://www.w3.org/2001/XMLSchema-instance")
results = []
checks = {}
log_number = 0


def command(label, argv, allow_failure=False):
    global log_number
    log_number += 1
    log_path = WORK / "logs" / ("%02d-%s.log" % (log_number, label))
    print("\n=== %s ===" % label, flush=True)
    started = time.monotonic()
    with log_path.open("w", encoding="utf-8") as output:
        process = subprocess.run(argv, stdout=output, stderr=subprocess.STDOUT, text=True)
    elapsed = time.monotonic() - started
    lines = log_path.read_text(encoding="utf-8", errors="replace").splitlines()
    highlights = [line for line in lines if
                  "downloadable artifact:" in line or
                  "AdvancedCore packaged artifact:" in line or
                  "Full artifact:" in line or
                  "BUILD SUCCESS" in line or "BUILD FAILURE" in line or
                  ("Tests run:" in line and "Failures:" in line) or
                  "[ERROR]" in line]
    print("command exited %d in %.1fs; %s" % (process.returncode, elapsed, log_path), flush=True)
    for line in highlights[-14:]:
        print(line[-330:], flush=True)
    if process.returncode != 0:
        for line in lines[-35:]:
            print(line[-350:], flush=True)
        if not allow_failure:
            raise RuntimeError("Failed: %s (details above)" % label)
    return process.returncode == 0


def clone(name, branch):
    folder = PROJECTS[name][0]
    command("clone-" + name, [
        "git", "clone", "--depth", "1", "--branch", branch,
        "https://github.com/BenCodez/" + name + ".git", str(folder)
    ])
    rev = subprocess.check_output(["git", "-C", str(folder), "rev-parse", "HEAD"], text=True).strip()
    print("%s source SHA: %s" % (name, rev), flush=True)


def pom_path(name):
    folder, directory = PROJECTS[name]
    return folder / directory / "pom.xml"


def jar_path(name):
    folder, directory = PROJECTS[name]
    return folder / directory / "target" / (name + ".jar")


def build(name, label, full=False, focused=False):
    goal = "package" if name == "VotingPlugin" else "install"
    argv = ["mvn", "-B", "-ntp", "-nsu", "-f", str(pom_path(name)), "clean", goal]
    if focused:
        argv += ["-Dtest=PackagedArtifactTest", "-Dsurefire.failIfNoSpecifiedTests=false"]
    elif not full:
        argv += ["-Dmaven.test.skip=true"]
    return command(label, argv, allow_failure=(full or focused))


def add_filter(name, artifact, patterns):
    path = pom_path(name)
    tree = ET.parse(path)
    root = tree.getroot()
    plugins = [p for p in root.findall(".//m:plugin", NS)
               if (p.findtext("m:artifactId", namespaces=NS) == "maven-shade-plugin")]
    if len(plugins) != 1:
        raise RuntimeError("Expected exactly one Shade plugin in " + name)
    config = plugins[0].find("m:executions/m:execution/m:configuration", NS)
    if config is None:
        raise RuntimeError("Could not find Shade execution configuration in " + name)
    filters = config.find("m:filters", NS)
    if filters is None:
        filters = ET.SubElement(config, "{%s}filters" % NS_URI)
    matches = [f for f in filters.findall("m:filter", NS)
               if f.findtext("m:artifact", namespaces=NS) == artifact]
    if matches:
        filt = matches[0]
    else:
        filt = ET.SubElement(filters, "{%s}filter" % NS_URI)
        ET.SubElement(filt, "{%s}artifact" % NS_URI).text = artifact
    excludes = filt.find("m:excludes", NS)
    if excludes is None:
        excludes = ET.SubElement(filt, "{%s}excludes" % NS_URI)
    existing = {el.text for el in excludes.findall("m:exclude", NS)}
    for pattern in patterns:
        if pattern not in existing:
            ET.SubElement(excludes, "{%s}exclude" % NS_URI).text = pattern
    tree.write(path, encoding="utf-8", xml_declaration=True)
    print("Added %s filter in %s: %s" % (artifact, name, ", ".join(patterns)), flush=True)


def analyze(name, stage, detailed=False):
    path = jar_path(name)
    if not path.is_file():
        raise RuntimeError("Missing JAR " + str(path))
    nbytes = path.stat().st_size
    groups = defaultdict(int)
    with ZipFile(path) as jar:
        entries = [entry for entry in jar.infolist() if not entry.is_dir()]
        manifest = jar.read("META-INF/MANIFEST.MF").decode("utf-8", "replace") if "META-INF/MANIFEST.MF" in jar.namelist() else ""
        is_multi_release = bool(re.search(r"(?im)^Multi-Release:\s*true\s*$", manifest))
        versioned = sum(entry.compress_size for entry in entries if entry.filename.startswith("META-INF/versions/"))
        for entry in entries:
            parts = entry.filename.split("/")
            group = "/".join(parts[:min(5, len(parts) - 1)]) if len(parts) > 1 else "(root)"
            groups[group] += entry.compress_size
        if detailed:
            print("Top compressed package groups in %s:" % name, flush=True)
            for group, size in sorted(groups.items(), key=lambda row: row[1], reverse=True)[:22]:
                print("  %9d bytes  %s" % (size, group), flush=True)
            print("  versioned resources: %d compressed bytes" % versioned, flush=True)
            for entry in sorted(entries, key=lambda item: item.compress_size, reverse=True)[:12]:
                print("  large entry: %9d  %s" % (entry.compress_size, entry.filename), flush=True)
    results.append((stage, name, nbytes, is_multi_release, versioned))
    print("[JAR_SIZE] %s | %s | %s bytes | %.3f MiB | multi-release=%s | versioned=%d" %
          (stage, name, f"{nbytes:,}", nbytes / (1024 * 1024), is_multi_release, versioned), flush=True)
    return nbytes, is_multi_release


def record_check(label, status):
    checks[label] = status
    print("[EXPERIMENT_RESULT] %s: %s" % (label, status), flush=True)


def test_if_smaller(label, project, before_bytes, after_bytes, run_check):
    if after_bytes == before_bytes:
        record_check(label, "NO SIZE CHANGE; further tests not needed")
        return
    ok = run_check()
    record_check(label, ("PASS" if ok else "FAIL") +
                 "; delta=%+d bytes (negative means smaller)" % (after_bytes - before_bytes))


def summary():
    print("\n===== COMPLETE JAR SIZE RESULTS =====", flush=True)
    for stage, name, count, multi, versioned in results:
        print("[SIZE_RESULT] %-35s %-15s %10d bytes  (%.3f MiB)" %
              (stage, name, count, count / (1024 * 1024)), flush=True)
    for label, status in checks.items():
        print("[TEST_RESULT] %-40s %s" % (label, status), flush=True)
    lines = ["## JAR size experiment results", "", "All size values are JAR file bytes from this same CI run.", "",
             "| Stage | Project | Bytes | MiB | Multi-release | Versioned bytes |",
             "|---|---|---:|---:|---|---:|"]
    for stage, name, count, multi, versioned in results:
        lines.append("| %s | %s | %s | %.3f | %s | %s |" %
                     (stage, name, f"{count:,}", count / (1024 * 1024),
                      "yes" if multi else "no", f"{versioned:,}"))
    lines.extend(["", "### Checks", ""])
    for label, status in checks.items():
        lines.append("- **%s:** %s" % (label, status))
    lines.extend(["", "Full upstream suites were run only for candidates that reduced bytes. "
                       "Cross-project builds were package/linkage checks; no live Minecraft server was launched.",
                       "The transport-trim experiment intentionally probes a compatibility-sensitive scenario and is not a release recommendation."])
    step_summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if step_summary:
        with open(step_summary, "a", encoding="utf-8") as output:
            output.write("\n".join(lines) + "\n")


def main():
    clone("SimpleAPI", "main")
    clone("AdvancedCore", "master")
    orig = {name: pom_path(name).read_bytes() for name in PROJECTS}

    # Install each exact upstream source into the runner's local Maven repository.
    for name in ("SimpleAPI", "AdvancedCore", "VotingPlugin"):
        build(name, "baseline-" + name)
    base = {name: analyze(name, "baseline", detailed=True)[0] for name in PROJECTS}
    record_check("baseline builds", "PASS; tests previously passed on official master CI")

    # Candidate A: all versioned dependency entries in VotingPlugin.
    add_filter("VotingPlugin", "*:*", ["META-INF/versions/**"])
    build("VotingPlugin", "vp-global-versions-size")
    new_vp = analyze("VotingPlugin", "vp-global-versions")[0]
    test_if_smaller("VP global versioned exclusion", "VotingPlugin", base["VotingPlugin"], new_vp,
                    lambda: build("VotingPlugin", "vp-global-versions-full-tests", full=True))
    pom_path("VotingPlugin").write_bytes(orig["VotingPlugin"])

    # Candidate B: versioned dependency entries embedded in AdvancedCore.
    add_filter("AdvancedCore", "*:*", ["META-INF/versions/**"])
    build("AdvancedCore", "ac-versions-size")
    new_ac = analyze("AdvancedCore", "ac-versioned-filter")[0]
    test_if_smaller("AC versioned dependency exclusion", "AdvancedCore", base["AdvancedCore"], new_ac,
                    lambda: build("AdvancedCore", "ac-versions-full-tests", full=True))
    build("VotingPlugin", "vp-with-ac-versions-size")
    analyze("VotingPlugin", "vp-with-ac-versioned-filter")
    pom_path("AdvancedCore").write_bytes(orig["AdvancedCore"])
    build("AdvancedCore", "ac-restored-after-version-test")

    # Candidate C: trim the unused upstream transport implementation from AC.
    # This intentionally may violate AC's public/published transitive contract.
    add_filter("AdvancedCore", "com.bencodez:simpleapi",
               ["com/bencodez/simpleapi/servercomm/http/**",
                "redis/clients/**", "org/eclipse/paho/**"])
    build("AdvancedCore", "ac-transport-trim-size")
    trimmed_ac = analyze("AdvancedCore", "ac-transport-trim")[0]
    if trimmed_ac < base["AdvancedCore"]:
        own_ok = build("AdvancedCore", "ac-transport-trim-full-tests", full=True)
        downstream_ok = build("VotingPlugin", "vp-transport-trim-package-checks", focused=True)
        record_check("AC transport trim and VP compatibility",
                     "AC tests=%s; VP packaged compatibility=%s; AC saved=%d bytes" %
                     ("PASS" if own_ok else "FAIL", "PASS" if downstream_ok else "FAIL",
                      base["AdvancedCore"] - trimmed_ac))
        if downstream_ok:
            analyze("VotingPlugin", "vp-with-ac-transport-trim")
    else:
        record_check("AC transport trim", "No reduction; downstream test skipped")
    pom_path("AdvancedCore").write_bytes(orig["AdvancedCore"])
    build("AdvancedCore", "ac-restored-after-transport-test")

    # Candidate D: remove checker and jspecify annotations only from shaded SimpleAPI,
    # then verify its full suite and downstream packaging if this saves bytes.
    add_filter("SimpleAPI", "*:*", ["org/checkerframework/**", "org/jspecify/**"])
    build("SimpleAPI", "simpleapi-annotations-size")
    reduced_sapi = analyze("SimpleAPI", "simpleapi-annotation-filter")[0]
    if reduced_sapi < base["SimpleAPI"]:
        api_ok = build("SimpleAPI", "simpleapi-annotations-full-tests", full=True)
        ac_ok = build("AdvancedCore", "ac-with-reduced-simpleapi")
        after_ac = analyze("AdvancedCore", "ac-with-reduced-simpleapi")[0]
        vp_ok = build("VotingPlugin", "vp-with-reduced-simpleapi-package-checks", focused=True)
        if vp_ok:
            analyze("VotingPlugin", "vp-with-reduced-simpleapi")
        record_check("SimpleAPI annotation stripping",
                     "SAPI tests=%s; AC package=%s; VP packaged checks=%s; SAPI saved=%d bytes; AC saved=%d bytes" %
                     ("PASS" if api_ok else "FAIL", "PASS" if ac_ok else "FAIL",
                      "PASS" if vp_ok else "FAIL",
                      base["SimpleAPI"] - reduced_sapi, base["AdvancedCore"] - after_ac))
    else:
        record_check("SimpleAPI annotation stripping", "No reduction; downstream test skipped")

    summary()


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print("\n[JAR_SIZE_ERROR] %s: %s" % (type(exc).__name__, exc), flush=True)
        summary()
        raise
