package com.bencodez.votingplugin.tests.maintenance;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.maintenance.*;
import com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderService.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.LongSupplier;
import java.util.logging.Logger;

/** Typed test-only access to package-private AutoCreateVoteSitesReminderService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class AutoCreateVoteSitesReminderServiceAccess {
    private AutoCreateVoteSitesReminderServiceAccess() { }
    public static AutoCreateVoteSitesReminderService create(Path directory, ScheduledExecutorService executor, Logger logger, Settings settings) { return (AutoCreateVoteSitesReminderService) ReflectiveAccess.construct("com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderService", new String[]{"Path", "ScheduledExecutorService", "Logger", "Settings"}, new Object[]{directory, executor, logger, settings}); }
    public static AutoCreateVoteSitesReminderService create(Path directory, ScheduledExecutorService executor, Logger logger, Settings settings, LongSupplier clock, AutoCreateVoteSitesReminderServiceAccess.StoreFactory storeFactory) { return (AutoCreateVoteSitesReminderService) ReflectiveAccess.construct("com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderService", new String[]{"Path", "ScheduledExecutorService", "Logger", "Settings", "LongSupplier", "StoreFactory"}, new Object[]{directory, executor, logger, settings, clock, storeFactory}); }
    @FunctionalInterface
    public interface StoreFactory {
        AutoCreateVoteSitesReminderStore create(Path directory) throws IOException;
    }

}
