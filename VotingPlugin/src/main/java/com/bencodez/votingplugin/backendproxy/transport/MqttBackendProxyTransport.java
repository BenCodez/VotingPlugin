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
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
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
	public void close() {
		// Preserve current MQTT lifecycle behavior.
	}
}
