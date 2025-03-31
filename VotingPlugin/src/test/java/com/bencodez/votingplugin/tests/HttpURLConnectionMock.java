package com.bencodez.votingplugin.tests;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpURLConnectionMock {

	public static void setupMockHttpURLConnection(String playerName, int responseCode, String responseBody)
			throws Exception {
		// Mock URL and HttpURLConnection
		URL mockUrl = mock(URL.class);
		HttpURLConnection mockConnection = mock(HttpURLConnection.class);

		when(mockUrl.openConnection()).thenReturn(mockConnection);
		when(mockConnection.getResponseCode()).thenReturn(responseCode);
		InputStream mockInputStream = new ByteArrayInputStream(responseBody.getBytes());
		when(mockConnection.getInputStream()).thenReturn(mockInputStream);

		// When new URL is called, return the mocked URL
		// Note: This requires PowerMockito or a similar tool to mock the `new` keyword
		// Here, it's simplified and assumes the method uses a helper that can be mocked
	}
}
