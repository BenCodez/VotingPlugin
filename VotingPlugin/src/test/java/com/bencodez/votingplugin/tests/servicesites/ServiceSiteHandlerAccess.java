package com.bencodez.votingplugin.tests.servicesites;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.servicesites.*;
import com.bencodez.votingplugin.servicesites.ServiceSiteHandler.*;
import java.io.IOException;
import java.io.InputStream;
import java.net.http.HttpResponse;
import java.util.concurrent.TimeUnit;

/** Typed test-only access to package-private ServiceSiteHandler seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ServiceSiteHandlerAccess {
    public static final int MAX_KEY_LENGTH = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", "MAX_KEY_LENGTH");
    public static final int MAX_VALUE_LENGTH = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", "MAX_VALUE_LENGTH");
    public static final String PRIMARY_URL = (String) ReflectiveAccess.field("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", "PRIMARY_URL");
    private ServiceSiteHandlerAccess() { }
    public static byte[] readSuccessfulResponse(HttpResponse<InputStream> response) throws IOException { return (byte[]) ReflectiveAccess.call("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", null, "readSuccessfulResponse", new String[]{"HttpResponse"}, new Object[]{response}); }
    public static byte[] readSuccessfulResponse(HttpResponse<InputStream> response, long timeout, TimeUnit unit) throws IOException { return (byte[]) ReflectiveAccess.call("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", null, "readSuccessfulResponse", new String[]{"HttpResponse", "long", "TimeUnit"}, new Object[]{response, timeout, unit}); }
    public static byte[] readBounded(InputStream input, int maximumBytes) throws IOException { return (byte[]) ReflectiveAccess.call("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", null, "readBounded", new String[]{"InputStream", "int"}, new Object[]{input, maximumBytes}); }
    public static boolean isSafeEntry(String key, String value) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.servicesites.ServiceSiteHandler", null, "isSafeEntry", new String[]{"String", "String"}, new Object[]{key, value}); }
}
