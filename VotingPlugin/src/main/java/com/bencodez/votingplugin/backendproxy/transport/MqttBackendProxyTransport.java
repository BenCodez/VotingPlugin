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

	public MqttBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		try {
			String id = plugin.getBungeeSettings().getMqttClientID();
			if (id.isEmpty()) {
				id = plugin.getOptions().getServer();
			}
			mqttHandler = new MqttHandler(new MqttServerComm(id, plugin.getBungeeSettings().getMqttBrokerURL(),
					plugin.getBungeeSettings().getMqttUsername(), plugin.getBungeeSettings().getMqttPassword()), 2);
			mqttHandler.subscribeEnvelopes(plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/"
					+ plugin.getOptions().getServer(), (topic, envelope) -> messageHandler.onMessage(envelope));
		} catch (MqttException e) {
			throw new IllegalStateException("MQTT backend proxy transport initialization failed", e);
		} catch (Exception e) {
			throw new IllegalStateException("MQTT backend proxy transport initialization failed", e);
		}
	}

	@Override
	public void validate() {
		if (mqttHandler == null) throw new IllegalStateException("MQTT backend proxy transport initialization failed");
	}

	@Override
	public void send(JsonEnvelope envelope) {
		if (mqttHandler == null) {
			return;
		}
		try {
			mqttHandler.publishEnvelope(plugin.getBungeeSettings().getMqttPrefix() + "votingplugin/servers/proxy",
					envelope);
		} catch (Exception e) {
			e.printStackTrace();
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
}
