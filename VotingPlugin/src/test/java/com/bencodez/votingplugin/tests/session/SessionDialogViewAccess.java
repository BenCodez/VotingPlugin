package com.bencodez.votingplugin.tests.session;

import java.lang.reflect.Proxy;
import java.util.List;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

import org.bukkit.entity.Player;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.session.GuidedVotingSessions;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

/** Test-only view of the private dialog seam; production visibility is unchanged. */
public interface SessionDialogViewAccess {
    String OWNER = "com.bencodez.votingplugin.session.SessionDialogView";
    String ADAPTER = "com.bencodez.votingplugin.session.GuidedSessionDialogs";

    record Button(String label, String tooltip, String action, String url) { }

    Runnable show(Player player, String title, String body, List<Button> buttons,
            BiConsumer<UUID, String> action);

    static GuidedVotingSessions sessions(VotingPluginMain plugin, Supplier<SessionDialogViewAccess> views) {
        Supplier<Object> nativeViews = () -> {
            SessionDialogViewAccess view = views.get();
            if (view == null) return null;
            Class<?> type = ReflectiveAccess.type(OWNER);
            return Proxy.newProxyInstance(type.getClassLoader(), new Class<?>[] {type}, (proxy, method, args) -> {
                if (method.getDeclaringClass() == Object.class) {
                    return switch (method.getName()) {
                        case "equals" -> proxy == args[0];
                        case "hashCode" -> System.identityHashCode(proxy);
                        case "toString" -> "Test dialog callback";
                        default -> throw new AssertionError(method);
                    };
                }
                if (!method.getName().equals("show")) throw new AssertionError(method);
                List<?> nativeButtons = (List<?>) args[3];
                List<Button> buttons = nativeButtons.stream().map(button -> new Button(
                        property(button, "label"), property(button, "tooltip"),
                        property(button, "action"), property(button, "url"))).toList();
                @SuppressWarnings("unchecked")
                BiConsumer<UUID, String> callback = (BiConsumer<UUID, String>) args[4];
                return view.show((Player) args[0], (String) args[1], (String) args[2], buttons, callback);
            });
        };
        return (GuidedVotingSessions) ReflectiveAccess.construct(GuidedVotingSessions.class.getName(),
                new String[] {"VotingPluginMain", "Supplier"}, new Object[] {plugin, nativeViews});
    }

    static SessionDialogViewAccess nativeAdapter(VotingPluginMain plugin) {
        Object adapter = ReflectiveAccess.construct(ADAPTER, new String[] {"VotingPluginMain"}, new Object[] {plugin});
        return (player, title, body, buttons, callback) -> {
            List<Object> nativeButtons = buttons.stream().map(button -> ReflectiveAccess.construct(OWNER + "$Button",
                    new String[] {"String", "String", "String", "String"},
                    new Object[] {button.label(), button.tooltip(), button.action(), button.url()})).toList();
            return (Runnable) ReflectiveAccess.call(ADAPTER, adapter, "show",
                    new String[] {"Player", "String", "String", "List", "BiConsumer"},
                    new Object[] {player, title, body, nativeButtons, callback});
        };
    }

    private static String property(Object button, String name) {
        return (String) ReflectiveAccess.call(OWNER + "$Button", button, name, new String[0], new Object[0]);
    }
}
