package com.bencodez.votingplugin.tests.proxy.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.control.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;

/** Test-only forwarding access to the non-public production ProxyConfigurationFileService. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ProxyConfigurationFileServiceAccess implements ReflectiveAccess.View {
    public static final String FILE_NAME = (String) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", "FILE_NAME");
    public static final String REDACTED = (String) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", "REDACTED");
    public static final int MAX_BYTES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", "MAX_BYTES");
    private final Object delegate;
    private ProxyConfigurationFileServiceAccess(Object delegate) { this.delegate = delegate; }
    public static ProxyConfigurationFileServiceAccess view(Object delegate) { return delegate == null ? null : new ProxyConfigurationFileServiceAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof ProxyConfigurationFileServiceAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static ProxyConfigurationFileServiceAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService"))); }
    public ProxyConfigurationFileServiceAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public ProxyConfigurationFileServiceAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public ProxyConfigurationFileServiceAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public ProxyConfigurationFileServiceAccess(VotingPluginProxy proxy) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", new String[]{"VotingPluginProxy"}, new Object[]{proxy})); }
    public ProxyConfigurationFileServiceAccess(Path target, MoveAction mover) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", new String[]{"Path", "MoveAction"}, new Object[]{target, mover})); }
    public ProxyConfigurationFileServiceAccess(Path target, MoveAction mover, TempFileAction tempFiles) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", new String[]{"Path", "MoveAction", "TempFileAction"}, new Object[]{target, mover, tempFiles})); }
    public Document read(String fileName) throws IOException { return ProxyConfigurationFileServiceAccess.Document.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", delegate, "read", new String[]{"String"}, new Object[]{fileName})); }
    public Preview preview(String fileName, String proposed) throws IOException { return ProxyConfigurationFileServiceAccess.Preview.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", delegate, "preview", new String[]{"String", "String"}, new Object[]{fileName, proposed})); }
    public Preview previewAgainstSnapshot(String proposed, String currentRaw) throws IOException { return ProxyConfigurationFileServiceAccess.Preview.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", delegate, "previewAgainstSnapshot", new String[]{"String", "String"}, new Object[]{proposed, currentRaw})); }
    public ApplyResult apply(String fileName, String proposed, String expectedRevision) throws IOException { return ProxyConfigurationFileServiceAccess.ApplyResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", delegate, "apply", new String[]{"String", "String", "String"}, new Object[]{fileName, proposed, expectedRevision})); }
    public PreparedApply prepareApply(String fileName, String proposed, String expectedRevision) throws IOException { return ProxyConfigurationFileServiceAccess.PreparedApply.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", delegate, "prepareApply", new String[]{"String", "String", "String"}, new Object[]{fileName, proposed, expectedRevision})); }
    public ApplyResult apply(PreparedApply prepared) throws IOException { return ProxyConfigurationFileServiceAccess.ApplyResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", delegate, "apply", new String[]{"PreparedApply"}, new Object[]{prepared})); }
    public static String revision(String value) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService", null, "revision", new String[]{"String"}, new Object[]{value}); }
    public static final class Document implements ReflectiveAccess.View {

        private final Object delegate;
        private Document(Object delegate) { this.delegate = delegate; }
        public static Document view(Object delegate) { return delegate == null ? null : new Document(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Document value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String fileName() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$Document", delegate, "fileName", new String[]{}, new Object[]{}); }
        public String content() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$Document", delegate, "content", new String[]{}, new Object[]{}); }
        public String revision() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$Document", delegate, "revision", new String[]{}, new Object[]{}); }
    }

    public static final class Preview implements ReflectiveAccess.View {

        private final Object delegate;
        private Preview(Object delegate) { this.delegate = delegate; }
        public static Preview view(Object delegate) { return delegate == null ? null : new Preview(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Preview value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String resolvedContent() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$Preview", delegate, "resolvedContent", new String[]{}, new Object[]{}); }
        public String revision() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$Preview", delegate, "revision", new String[]{}, new Object[]{}); }
        public List<String> changes() { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$Preview", delegate, "changes", new String[]{}, new Object[]{}); }
    }

    public static final class PreparedApply implements ReflectiveAccess.View {

        private final Object delegate;
        private PreparedApply(Object delegate) { this.delegate = delegate; }
        public static PreparedApply view(Object delegate) { return delegate == null ? null : new PreparedApply(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof PreparedApply value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String currentRaw() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$PreparedApply", delegate, "currentRaw", new String[]{}, new Object[]{}); }
        public String expectedRevision() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$PreparedApply", delegate, "expectedRevision", new String[]{}, new Object[]{}); }
        public Preview preview() { return ProxyConfigurationFileServiceAccess.Preview.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$PreparedApply", delegate, "preview", new String[]{}, new Object[]{})); }
    }

    public static final class ApplyResult implements ReflectiveAccess.View {

        private final Object delegate;
        private ApplyResult(Object delegate) { this.delegate = delegate; }
        public static ApplyResult view(Object delegate) { return delegate == null ? null : new ApplyResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof ApplyResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public Document document() { return ProxyConfigurationFileServiceAccess.Document.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$ApplyResult", delegate, "document", new String[]{}, new Object[]{})); }
        public List<String> changes() { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$ApplyResult", delegate, "changes", new String[]{}, new Object[]{}); }
        public boolean rolledBack() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyConfigurationFileService$ApplyResult", delegate, "rolledBack", new String[]{}, new Object[]{}); }
    }

    @FunctionalInterface
    public interface MoveAction {
        void move(Path source, Path destination) throws IOException;
    }

    @FunctionalInterface
    public interface TempFileAction {
        Path create(Path directory, String prefix, String suffix) throws IOException;
    }

}
