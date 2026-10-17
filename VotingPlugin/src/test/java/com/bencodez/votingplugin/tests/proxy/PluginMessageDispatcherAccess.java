package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.*;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

/** Test-only forwarding access to the non-public production PluginMessageDispatcher. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class PluginMessageDispatcherAccess implements ReflectiveAccess.View {
    public static final long INTERVAL = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", "INTERVAL");
    public static final int MAX_LANE_CONTROLS = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", "MAX_LANE_CONTROLS");
    public static final int MAX_IN_FLIGHT = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", "MAX_IN_FLIGHT");
    private final Object delegate;
    private PluginMessageDispatcherAccess(Object delegate) { this.delegate = delegate; }
    public static PluginMessageDispatcherAccess view(Object delegate) { return delegate == null ? null : new PluginMessageDispatcherAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof PluginMessageDispatcherAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static PluginMessageDispatcherAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.proxy.PluginMessageDispatcher"))); }
    public PluginMessageDispatcherAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public PluginMessageDispatcherAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public PluginMessageDispatcherAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public PluginMessageDispatcherAccess(LongSupplier clock, Supplier<ScheduledExecutorService> executor, Consumer<String> replay) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", new String[]{"LongSupplier", "Supplier", "Consumer"}, new Object[]{clock, executor, replay})); }
    public boolean hasBudget(String server, String key, boolean receipt) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "hasBudget", new String[]{"String", "String", "boolean"}, new Object[]{server, key, receipt}); }
    public boolean trySend(String server, BooleanSupplier handoff) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "trySend", new String[]{"String", "BooleanSupplier"}, new Object[]{server, handoff}); }
    public boolean trySend(String server, String key, boolean receipt, BooleanSupplier handoff) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "trySend", new String[]{"String", "String", "boolean", "BooleanSupplier"}, new Object[]{server, key, receipt, handoff}); }
    public void acknowledge(String server, String key) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "acknowledge", new String[]{"String", "String"}, new Object[]{server, key}); }
    public Batch reserveBatch(Map<String, BooleanSupplier> handoffs, int size) { return PluginMessageDispatcherAccess.Batch.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "reserveBatch", new String[]{"Map", "int"}, new Object[]{handoffs, size})); }
    public boolean enqueue(String server, int size, long delay, BooleanSupplier handoff) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "enqueue", new String[]{"String", "int", "long", "BooleanSupplier"}, new Object[]{server, size, delay, handoff}); }
    public boolean enqueue(String server, int size, long delay, boolean blocksReplacement, BooleanSupplier handoff) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "enqueue", new String[]{"String", "int", "long", "boolean", "BooleanSupplier"}, new Object[]{server, size, delay, blocksReplacement, handoff}); }
    public int pendingControls() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "pendingControls", new String[]{}, new Object[]{}); }
    public boolean hasPendingEffects() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "hasPendingEffects", new String[]{}, new Object[]{}); }
    public void close() { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher", delegate, "close", new String[]{}, new Object[]{}); }
    public static final class Batch implements ReflectiveAccess.View {

        private final Object delegate;
        private Batch(Object delegate) { this.delegate = delegate; }
        public static Batch view(Object delegate) { return delegate == null ? null : new Batch(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Batch value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public void activate() { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher$Batch", delegate, "activate", new String[]{}, new Object[]{}); }
        public void cancel() { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.PluginMessageDispatcher$Batch", delegate, "cancel", new String[]{}, new Object[]{}); }
    }

}
