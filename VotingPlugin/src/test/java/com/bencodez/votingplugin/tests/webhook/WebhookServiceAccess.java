package com.bencodez.votingplugin.tests.webhook;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.webhook.*;
import com.bencodez.votingplugin.webhook.WebhookService.*;

/** Typed test-only access to package-private WebhookService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class WebhookServiceAccess {
    private WebhookServiceAccess() { }
    public static WebhookService create(WebhookLogger logger) { return (WebhookService) ReflectiveAccess.construct("com.bencodez.votingplugin.webhook.WebhookService", new String[]{"WebhookLogger"}, new Object[]{logger}); }
    public static WebhookService create(WebhookLogger logger, int queueCapacity, WebhookServiceAccess.Delay delay) { return (WebhookService) ReflectiveAccess.construct("com.bencodez.votingplugin.webhook.WebhookService", new String[]{"WebhookLogger", "int", "Delay"}, new Object[]{logger, queueCapacity, delay}); }
    @FunctionalInterface
    public interface Delay {
        void waitFor(long delayMs) throws InterruptedException;
    }

}
