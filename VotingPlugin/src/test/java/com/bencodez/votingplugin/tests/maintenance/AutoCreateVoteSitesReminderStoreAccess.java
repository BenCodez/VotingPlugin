package com.bencodez.votingplugin.tests.maintenance;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.maintenance.*;
import com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderStore.*;
import java.io.IOException;
import java.nio.file.Path;

/** Typed test-only access to package-private AutoCreateVoteSitesReminderStore seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class AutoCreateVoteSitesReminderStoreAccess {
    private AutoCreateVoteSitesReminderStoreAccess() { }
    public static AutoCreateVoteSitesReminderStore create(Path dataDirectory) throws IOException { return (AutoCreateVoteSitesReminderStore) ReflectiveAccess.construct("com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderStore", new String[]{"Path"}, new Object[]{dataDirectory}); }
    public static AutoCreateVoteSitesReminderStore create(Path dataDirectory, AutoCreateVoteSitesReminderStoreAccess.Publisher publisher) throws IOException { return (AutoCreateVoteSitesReminderStore) ReflectiveAccess.construct("com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderStore", new String[]{"Path", "Publisher"}, new Object[]{dataDirectory, publisher}); }
    @FunctionalInterface
    public interface Publisher {
        void publish(Path staged, Path target) throws IOException;
    }

}
