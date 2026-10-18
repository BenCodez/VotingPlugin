package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import com.bencodez.votingplugin.control.ControlInspectionService.*;
import com.bencodez.votingplugin.VotingPluginMain;
import com.google.gson.JsonObject;

/** Typed test-only access to package-private ControlInspectionService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ControlInspectionServiceAccess {
    public static final int MAX_ROWS = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.control.ControlInspectionService", "MAX_ROWS");
    private ControlInspectionServiceAccess() { }
    public static ControlInspectionService create(VotingPluginMain plugin) { return (ControlInspectionService) ReflectiveAccess.construct("com.bencodez.votingplugin.control.ControlInspectionService", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static ControlInspectionService create() { return (ControlInspectionService) ReflectiveAccess.construct("com.bencodez.votingplugin.control.ControlInspectionService", new String[]{}, new Object[]{}); }
    public static boolean rewardFileInventoryQuery(JsonObject query) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.ControlInspectionService", null, "rewardFileInventoryQuery", new String[]{"JsonObject"}, new Object[]{query}); }
}
