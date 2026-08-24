package com.bencodez.votingplugin.backendproxy.transport;

import java.sql.SQLException;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class MysqlBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	@Getter
	private MySqlMessenger messenger;

	public MysqlBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		plugin.registerBungeeChannels(plugin.getBungeeSettings().getPluginMessagingChannel());
		try {
			messenger = new MySqlMessenger("VotingPlugin",
					plugin.getMysql().getMysql().getConnectionManager().getDataSource(),
					MySqlMessenger.Mode.BACKEND, plugin.getOptions().getServer(), msg -> {
						if (plugin.getBungeeSettings().isBungeeDebug()) {
							plugin.debug("Proxy sent envelope: " + msg.envelope.getSubChannel() + " "
									+ msg.envelope.getFields());
						}
						messageHandler.onMessage(msg.envelope);
					});
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void send(JsonEnvelope envelope) {
		if (messenger == null) {
			return;
		}
		try {
			messenger.sendToProxy(envelope);
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void close() {
		if (messenger != null) {
			messenger.shutdown();
		}
	}
}
