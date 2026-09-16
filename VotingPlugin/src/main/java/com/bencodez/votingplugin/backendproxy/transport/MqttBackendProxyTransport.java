package com.bencodez.votingplugin.backendproxy.transport;

import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class MqttBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	@Getter
	private MqttHandler mqttHandler;
	private GlobalMessageHandler messageHandler;
	private String publishTopic;
	private String subscriptionTopic;
	private String clientId;
	private String brokerUrl;
	private String username;
	private String password;

	public MqttBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		try {
			this.messageHandler = messageHandler;
			publishTopic = plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/proxy";
			subscriptionTopic = plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/"
					+ plugin.getOptions().getServer();
			clientId = plugin.getBungeeSettings().getMqttClientID();
			if (clientId.isEmpty()) {
				clientId = plugin.getOptions().getServer();
			}
			brokerUrl = plugin.getBungeeSettings().getMqttBrokerURL();
			username = plugin.getBungeeSettings().getMqttUsername();
			password = plugin.getBungeeSettings().getMqttPassword();
			startCapturedConnection();
		} catch (MqttException e) {
			throw new IllegalStateException("MQTT backend proxy transport initialization failed", e);
		} catch (Exception e) {
			throw new IllegalStateException("MQTT backend proxy transport initialization failed", e);
		}
	}

	protected MqttHandler createMqttHandler(MqttServerComm server) throws MqttException {
		return new MqttHandler(server, 2);
	}

	protected MqttServerComm createMqttServerComm() throws MqttException {
		return new MqttServerComm(clientId, brokerUrl, username, password);
	}

	private void startCapturedConnection() throws Exception {
		MqttHandler candidate = createMqttHandler(createMqttServerComm());
		try {
			candidate.subscribeEnvelopes(subscriptionTopic, (topic, envelope) -> messageHandler.onMessage(envelope));
			mqttHandler = candidate;
		} catch (Exception subscriptionFailure) {
			try {
				candidate.disconnect();
			} catch (Exception disconnectFailure) {
				subscriptionFailure.addSuppressed(disconnectFailure);
			}
			throw subscriptionFailure;
		}
	}

	@Override
	public void validate() {
		if (mqttHandler == null) throw new IllegalStateException("MQTT backend proxy transport initialization failed");
	}

	/** Returns whether a failed disconnect left the existing broker session usable. */
	public boolean isConnected() {
		return mqttHandler != null && mqttHandler.isConnected();
	}

	@Override
	public boolean send(JsonEnvelope envelope) {
		if (mqttHandler == null) {
			return false;
		}
		try {
			mqttHandler.publishEnvelope(publishTopic, envelope);
			return true;
		} catch (Exception e) {
			if (plugin != null && plugin.getLogger() != null) {
				plugin.getLogger().warning("MQTT backend proxy delivery failed");
				plugin.debug(e);
			}
			return false;
		}
	}

	@Override
	public void prepareForReplacement() {
		if (mqttHandler != null) {
			try {
				mqttHandler.disconnect();
			} catch (Exception e) {
				throw new IllegalStateException("Unable to disconnect the MQTT backend transport", e);
			}
			mqttHandler = null;
		}
	}

	@Override
	public void close() {
		if (mqttHandler != null) {
			try {
				mqttHandler.disconnect();
			} catch (Exception e) {
				plugin.getLogger().warning("Unable to disconnect the replaced MQTT backend transport");
			} finally {
				mqttHandler = null;
			}
		}
	}

	/** Reconnects a prepared predecessor with its original identity after rollback. */
	public void restoreAfterFailedReplacement() {
		if (messageHandler == null || clientId == null || brokerUrl == null || subscriptionTopic == null) {
			throw new IllegalStateException("MQTT backend proxy transport cannot be restored before startup");
		}
		try {
			startCapturedConnection();
		} catch (Exception e) {
			throw new IllegalStateException("MQTT backend proxy transport restoration failed", e);
		}
	}
}
