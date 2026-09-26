package com.bencodez.votingplugin.backendproxy.transport;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;

class MqttBackendProxyTransportTest {
	@TempDir
	Path temporaryDirectory;

	@Test
	void authenticatedVoteIsAcceptedAndUnsignedVoteIsRejected() throws Exception {
		SharedTransportEnvelopeAuthenticator authenticator = authenticator();
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		setField(transport, "messageHandler", messages);
		setField(transport, "authenticator", authenticator);
		JsonEnvelope vote = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).put("player", "Alex").build();

		transport.acceptAuthenticatedEnvelope(authenticator.sign(vote, Domain.MQTT_PROXY_BACKEND, "proxy-a", "votingplugin/servers/backend-a"),
				"votingplugin/servers/backend-a");
		transport.acceptAuthenticatedEnvelope(vote, "votingplugin/servers/backend-a");

		verify(messages).onMessage(org.mockito.ArgumentMatchers.argThat(received ->
				VotingPluginWire.SUB_VOTE.equals(received.getSubChannel())
						&& vote.getFields().equals(received.getFields())));
		verifyNoMoreInteractions(messages);
	}

	@Test
	void copiedMqttVoteCannotAuthenticateOnAnotherBackendTopic() throws Exception {
		SharedTransportEnvelopeAuthenticator authenticator = authenticator();
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		setField(transport, "messageHandler", messages);
		setField(transport, "authenticator", authenticator);
		JsonEnvelope vote = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build();
		JsonEnvelope signed = authenticator.sign(vote, Domain.MQTT_PROXY_BACKEND, "proxy-a",
				"votingplugin/servers/backend-a");

		transport.acceptAuthenticatedEnvelope(signed, "votingplugin/servers/backend-b");
		org.mockito.Mockito.verifyNoInteractions(messages);
		transport.acceptAuthenticatedEnvelope(signed, "votingplugin/servers/backend-a");
		verify(messages).onMessage(org.mockito.ArgumentMatchers.any());
	}

	private SharedTransportEnvelopeAuthenticator authenticator() throws Exception {
		Path keyFile = temporaryDirectory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		return SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED);
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
