package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.bukkit.Bukkit;
import org.bukkit.Server;
import org.bukkit.plugin.PluginDescriptionFile;
import org.bukkit.permissions.Permission;
import org.bukkit.permissions.PermissionDefault;
import org.bukkit.permissions.PermissibleBase;
import org.bukkit.permissions.ServerOperator;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;

class ExperimentalCommandPermissionTest {
    @Test void dedicatedPermissionsPassBaseGateWithoutGrantingOtherAdminCommands() throws Exception {
        PluginDescriptionFile descriptor;
        try (var reader = new InputStreamReader(getClass().getResourceAsStream("/plugin.yml"), StandardCharsets.UTF_8)) {
            descriptor = new PluginDescriptionFile(reader);
        }
        Map<String, Permission> permissions = new HashMap<>();
        for (Permission permission : descriptor.getPermissions()) {
            permissions.put(permission.getName().toLowerCase(java.util.Locale.ROOT), permission);
        }
        java.util.List<Permission> experimentalPermissions = new java.util.ArrayList<>();
        for (var type : ExperimentalGUIType.values()) {
            String name = "VotingPlugin.Commands.AdminVote."
                    + (type == ExperimentalGUIType.HOLOGRAM ? "TestHologram" : type.command());
            Permission permission = permissions.get(name.toLowerCase(java.util.Locale.ROOT));
            assertNotNull(permission, name);
            assertEquals(Boolean.TRUE, permission.getChildren().get("VotingPlugin.Commands.AdminVote"));
            assertEquals(PermissionDefault.OP, permission.getDefault());
            experimentalPermissions.add(permission);
        }
        Server server = mock(Server.class);
        PluginManager manager = mock(PluginManager.class);
        when(server.getPluginManager()).thenReturn(manager);
        when(manager.getPermission(anyString())).thenAnswer(inv ->
                permissions.get(inv.getArgument(0, String.class).toLowerCase(java.util.Locale.ROOT)));
        when(manager.getDefaultPermissions(anyBoolean())).thenReturn(Set.of());
        Plugin plugin = mock(Plugin.class);
        when(plugin.isEnabled()).thenReturn(true);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getServer).thenReturn(server);
            bukkit.when(Bukkit::getPluginManager).thenReturn(manager);
            for (Permission permission : experimentalPermissions) {
                PermissibleBase nonOp = new PermissibleBase(mock(ServerOperator.class));
                nonOp.addAttachment(plugin, permission.getName(), true);
                assertTrue(nonOp.hasPermission("VotingPlugin.Commands.AdminVote"));
                assertTrue(nonOp.hasPermission(permission.getName()));
                assertFalse(nonOp.hasPermission("VotingPlugin.Admin"));
                assertFalse(nonOp.hasPermission("VotingPlugin.Commands.AdminVote.Reload"));
            }
        }
    }
}
