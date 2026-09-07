package com.bencodez.votingplugin.servicesites;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.http.HttpResponse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

class ServiceSiteHandlerLimitsTest {
	@Test
	void acceptsNormalEntries() {
		assertTrue(ServiceSiteHandler.isSafeEntry("PlanetMinecraft", "planetminecraft.com"));
	}

	@Test
	void rejectsOversizedRemoteEntries() {
		assertFalse(ServiceSiteHandler.isSafeEntry("x".repeat(ServiceSiteHandler.MAX_KEY_LENGTH + 1), "example.com"));
		assertFalse(ServiceSiteHandler.isSafeEntry("Example", "x".repeat(ServiceSiteHandler.MAX_VALUE_LENGTH + 1)));
	}

	@Test
	void abortsStreamingBodyImmediatelyAfterLimit() throws Exception {
		byte[] allowed = new byte[32];
		assertTrue(ServiceSiteHandler.readBounded(new ByteArrayInputStream(allowed), 32).length == 32);
		assertThrows(IOException.class,
				() -> ServiceSiteHandler.readBounded(new ByteArrayInputStream(new byte[33]), 32));
	}

	@Test
	void closesErrorResponseStream() {
		class TrackingStream extends ByteArrayInputStream {
			boolean closed;
			TrackingStream() { super(new byte[] { 1 }); }
			@Override public void close() throws IOException { closed = true; super.close(); }
		}
		TrackingStream stream = new TrackingStream();
		@SuppressWarnings("unchecked")
		HttpResponse<InputStream> response = mock(HttpResponse.class);
		when(response.body()).thenReturn(stream);
		when(response.statusCode()).thenReturn(500);

		assertThrows(IOException.class, () -> ServiceSiteHandler.readSuccessfulResponse(response));
		assertTrue(stream.closed);
	}
}
