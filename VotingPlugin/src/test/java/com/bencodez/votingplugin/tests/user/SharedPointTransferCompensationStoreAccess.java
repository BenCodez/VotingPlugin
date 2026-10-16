package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/** Test-only forwarding access to the non-public production SharedPointTransferCompensationStore. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedPointTransferCompensationStoreAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private SharedPointTransferCompensationStoreAccess(Object delegate) { this.delegate = delegate; }
    public static SharedPointTransferCompensationStoreAccess view(Object delegate) { return delegate == null ? null : new SharedPointTransferCompensationStoreAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedPointTransferCompensationStoreAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedPointTransferCompensationStoreAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.user.SharedPointTransferCompensationStore"))); }
    public SharedPointTransferCompensationStoreAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedPointTransferCompensationStoreAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedPointTransferCompensationStoreAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedPointTransferCompensationStoreAccess(Path dataDirectory) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.user.SharedPointTransferCompensationStore", new String[]{"Path"}, new Object[]{dataDirectory})); }
    public void record(String transferId) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferCompensationStore", delegate, "record", new String[]{"String"}, new Object[]{transferId}); }
    public List<String> loadBatch() throws IOException { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferCompensationStore", delegate, "loadBatch", new String[]{}, new Object[]{}); }
    public void remove(String transferId) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferCompensationStore", delegate, "remove", new String[]{"String"}, new Object[]{transferId}); }
}
