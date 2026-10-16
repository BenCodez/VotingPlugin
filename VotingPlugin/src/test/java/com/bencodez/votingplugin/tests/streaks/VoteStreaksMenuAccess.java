package com.bencodez.votingplugin.tests.streaks;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.streaks.*;
import org.bukkit.inventory.InventoryHolder;
import com.bencodez.votingplugin.streaks.VoteStreaksMenu.*;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.BiFunction;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import com.bencodez.votingplugin.VotingPluginMain;

/** Typed test-only access to package-private VoteStreaksMenu seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteStreaksMenuAccess {
    private VoteStreaksMenuAccess() { }
    public static VoteStreaksMenu create(VotingPluginMain plugin) { return (VoteStreaksMenu) ReflectiveAccess.construct("com.bencodez.votingplugin.streaks.VoteStreaksMenu", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static VoteStreaksMenu create(VotingPluginMain plugin, Consumer<Runnable> worker, BiFunction<UUID, String, VoteStreaksSnapshot> snapshot) { return (VoteStreaksMenu) ReflectiveAccess.construct("com.bencodez.votingplugin.streaks.VoteStreaksMenu", new String[]{"VotingPluginMain", "Consumer", "BiFunction"}, new Object[]{plugin, worker, snapshot}); }
    public static boolean ownsForShutdown(Class<?> platform, Player player) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.streaks.VoteStreaksMenu", null, "ownsForShutdown", new String[]{"Class", "Player"}, new Object[]{platform, player}); }
    public static int sessionCount(VoteStreaksMenu target) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.streaks.VoteStreaksMenu", target, "sessionCount", new String[]{}, new Object[]{}); }
    public static final class Holder implements ReflectiveAccess.View, InventoryHolder {
        private final Object delegate;
        private Holder(Object delegate) { this.delegate = delegate; }
        public static Holder view(Object delegate) { return delegate == null ? null : new Holder(delegate); }
        @Override public Object delegate() { return delegate; }
        public int page() { return (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.streaks.VoteStreaksMenu$Holder", delegate, "page"); }
        @Override public boolean equals(Object other) { return other instanceof Holder value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public Inventory getInventory() { return (Inventory) ReflectiveAccess.call("com.bencodez.votingplugin.streaks.VoteStreaksMenu$Holder", delegate, "getInventory", new String[]{}, new Object[]{}); }
    }

}
