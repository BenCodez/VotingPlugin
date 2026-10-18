package com.bencodez.votingplugin.tests.proxy.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.control.*;
import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService.*;
import java.io.IOException;
import java.util.Set;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;

/** Typed test-only access to package-private ProxyRoutingConfigurationService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ProxyRoutingConfigurationServiceAccess {
    private ProxyRoutingConfigurationServiceAccess() { }
    public static ProxyRoutingConfigurationService create(VotingPluginProxy proxy) { return (ProxyRoutingConfigurationService) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService", new String[]{"VotingPluginProxy"}, new Object[]{proxy}); }
    public static ProxyRoutingConfigurationService create(ProxyRoutingConfigurationServiceAccess.Platform platform) { return (ProxyRoutingConfigurationService) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService", new String[]{"Platform"}, new Object[]{platform}); }
        public interface Platform {
        ProxyRoutingConfiguration read();
        Set<String> configuredServers();
        void persist(ProxyRoutingConfiguration proposal, String expectedRevision) throws IOException;
        void rollback() throws IOException;
        void reload() throws Exception;
        default void verifyInstalled() throws IOException { ReflectiveAccess.defaultCall(this, "com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService$Platform", "verifyInstalled", new String[]{}, new Object[]{}); }
        default byte[] captureInstalledSnapshot() throws IOException { return (byte[]) ReflectiveAccess.defaultCall(this, "com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService$Platform", "captureInstalledSnapshot", new String[]{}, new Object[]{}); }
        default void verifyInstalledSnapshot(byte[] snapshot) throws IOException { ReflectiveAccess.defaultCall(this, "com.bencodez.votingplugin.proxy.control.ProxyRoutingConfigurationService$Platform", "verifyInstalledSnapshot", new String[]{"byte[]"}, new Object[]{snapshot}); }
    }

}
