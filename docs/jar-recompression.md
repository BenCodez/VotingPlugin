# Lossless downloadable JAR recompression

After shading, the package phase repacks the downloadable JAR with standard
DEFLATE level 9. Entry names and uncompressed content remain identical,
including services, licenses, manifests and native libraries. ZIP attributes
and entry timestamps may be normalized. A package-phase comparison checks the complete
input/output inventories and bytes, and verifies that streaming JAR readers
still see the manifest first. The artifact name and runtime dependencies stay
the same; the added Ant task is build-only.

The repacker follows Shade and precedes the packaged-artifact tests. It keeps
ZIP64 support automatic, uses UTF-8 entry names, rejects duplicate names, and
includes the existing manifest before the remaining resources. It does not
rewrite the manifest. Deployment profiles use the existing `packaged.jar.name`
property, so the normal artifact path is preserved.

All profiles force Maven to recreate the project JAR before Shade, even when
inputs are unchanged. This prevents a previous shaded/recompressed artifact
from being reused as the next Shade input. The package test also rejects a
second Configurate namespace created by shading an already shaded JAR.

The shaded comparison input remains in `target/` without being attached as a
publication artifact. The staged archive is moved to the normal downloadable
path after repacking. The package test compares every input/output
entry and its bytes, then opens the final JAR through `JarInputStream` to check
streaming manifest discovery. Compression savings are reported without adding
a build-failing size threshold. The existing advisory size warning still applies.
