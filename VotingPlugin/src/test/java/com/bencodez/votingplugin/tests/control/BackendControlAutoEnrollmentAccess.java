package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import java.io.IOException;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.scheduler.BukkitTask;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.control.HostedControlManager;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;
import com.bencodez.votingplugin.util.ControlCredentialFile;
import com.bencodez.votingplugin.util.ControlCredentialFile.AutoEnrollmentInspection;
import com.bencodez.votingplugin.util.ControlCredentialFile.PendingAutoEnrollment;

/** Typed test-only access to package-private BackendControlAutoEnrollment seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendControlAutoEnrollmentAccess {
    private BackendControlAutoEnrollmentAccess() { }
    public static void send(BackendControlAutoEnrollment target) { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlAutoEnrollment", target, "send", new String[]{}, new Object[]{}); }
}
