package com.bencodez.votingplugin.tests.voteshop.service;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.voteshop.service.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/** Test-only forwarding access to the non-public production SharedMysqlCompensationStore. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedMysqlCompensationStoreAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private SharedMysqlCompensationStoreAccess(Object delegate) { this.delegate = delegate; }
    public static SharedMysqlCompensationStoreAccess view(Object delegate) { return delegate == null ? null : new SharedMysqlCompensationStoreAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedMysqlCompensationStoreAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedMysqlCompensationStoreAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.voteshop.service.SharedMysqlCompensationStore"))); }
    public SharedMysqlCompensationStoreAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedMysqlCompensationStoreAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedMysqlCompensationStoreAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedMysqlCompensationStoreAccess(Path dataDirectory) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.voteshop.service.SharedMysqlCompensationStore", new String[]{"Path"}, new Object[]{dataDirectory})); }
    public void record(String purchaseId) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlCompensationStore", delegate, "record", new String[]{"String"}, new Object[]{purchaseId}); }
    public List<String> loadBatch() throws IOException { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlCompensationStore", delegate, "loadBatch", new String[]{}, new Object[]{}); }
    public void remove(String purchaseId) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlCompensationStore", delegate, "remove", new String[]{"String"}, new Object[]{purchaseId}); }
}
