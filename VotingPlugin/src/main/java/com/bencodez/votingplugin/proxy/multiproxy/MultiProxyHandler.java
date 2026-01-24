// File: com/bencodez/votingplugin/proxy/multiproxy/MultiProxyHandler.java
package com.bencodez.votingplugin.proxy.multiproxy;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import lombok.Getter;

public abstract class MultiProxyHandler {
	private HashMap<String, ClientHandler> multiproxyClientHandles;
	private SocketHandler multiproxySocketHandler;

	@Getter
	private RedisHandler multiProxyRedis;

	public MultiProxyHandler() {
	}

	public abstract void addNonVotedPlayerCache(String uuid, String playerName);

	public abstract void clearVote(String uuid);

	public void close() {
		if (multiproxySocketHandler != null) {
			multiproxySocketHandler.closeConnection();
			multiproxySocketHandler = null;
		}
		if (multiProxyRedis != null && !getMultiProxyRedisUseExistingConnection()) {
			multiProxyRedis.close();
		}
	}

	public abstract boolean getDebug();

	public abstract EncryptionHandler getEncryptionHandler();

	public abstract MultiProxyMethod getMultiProxyMethod();

	public abstract String getMultiProxyPassword();

	public abstract String getMultiProxyRedisHost();

	public abstract int getMultiProxyRedisPort();

	public abstract int getMultiProxyRedisDbIndex();

	public abstract boolean getMultiProxyRedisUseExistingConnection();

	public abstract String getMultiProxyServerName();

	public abstract Collection<String> getMultiProxyServers();

	public abstract MultiProxyServerSocketConfiguration getMultiProxyServersConfiguration(String s);

	public abstract String getMultiProxySocketHostHost();

	public abstract int getMultiProxySocketHostPort();

	public abstract boolean getMultiProxySupportEnabled();

	public abstract String getMultiProxyUsername();

	public abstract File getPluginDataFolder();

	public abstract boolean getPrimaryServer();

	public abstract List<String> getProxyServers();

	public abstract RedisHandler getRedisHandler();

	public abstract String getVersion();

	public abstract void logInfo(String msg);

	public abstract void runAsnc(Runnable runnable);

	public abstract void setEncryptionHandler(EncryptionHandler encryptionHandler);

	public abstract void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid);

	public void loadMultiProxySupport() {
		if (!getMultiProxySupportEnabled()) {
			return;
		}

		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			if (getEncryptionHandler() == null) {
				setEncryptionHandler(
						new EncryptionHandler("VotingPlugin", new File(getPluginDataFolder(), "secretkey.key")));
			}

			if (multiproxySocketHandler != null) {
				multiproxySocketHandler.closeConnection();
				multiproxySocketHandler = null;
			}

			multiproxySocketHandler = new SocketHandler(getVersion(), getMultiProxySocketHostHost(),
					getMultiProxySocketHostPort(), getEncryptionHandler(), getDebug()) {

				@Override
				public void log(String str) {
					logInfo(str);
				}
			};

			multiproxySocketHandler.add(new SocketReceiver() {
				@Override
				public void onReceiveEnvelope(JsonEnvelope envelope) {
					handleEnvelope(envelope);
				}
			});

			multiproxyClientHandles = new HashMap<>();
			for (String s : getMultiProxyServers()) {
				MultiProxyServerSocketConfiguration d = getMultiProxyServersConfiguration(s);
				multiproxyClientHandles.put(s,
						new ClientHandler(d.getHost(), d.getPort(), getEncryptionHandler(), getDebug()));
			}

		} else {
			if (getMultiProxyRedisUseExistingConnection() && getRedisHandler() != null) {
				multiProxyRedis = getRedisHandler();
			} else {
				multiProxyRedis = new RedisHandler(getMultiProxyRedisHost(), getMultiProxyRedisPort(),
						getMultiProxyUsername(), getMultiProxyPassword(), getMultiProxyRedisDbIndex()) {
					@Override
					public void debug(String message) {
						if (getDebug()) {
							logInfo("MultiProxyRedis: " + message);
						}
					}
				};
			}

			runAsnc(() -> {
				RedisListener listener = multiProxyRedis.createEnvelopeListener(
						"VotingPluginProxy_" + getMultiProxyServerName(), (ch, env) -> handleEnvelope(env));
				multiProxyRedis.loadListener(listener);
			});
		}

		logInfo("Loaded multi-proxy support: " + getMultiProxyMethod().toString());
	}

	public void login(String uuid, String playerName) {
		if (!getMultiProxySupportEnabled() || getPrimaryServer()) {
			return;
		}
		sendMultiProxyEnvelope(VotingPluginWire.login(playerName, uuid, getMultiProxyServerName()));
	}

	public void sendClearVote(String uuid, String playerName) {
		if (!getMultiProxySupportEnabled()) {
			return;
		}
		sendMultiProxyEnvelope(VotingPluginWire.clearVote(uuid, playerName, getMultiProxyServerName()));
	}

	public void sendStatus() {
		if (!getMultiProxySupportEnabled()) {
			return;
		}
		sendMultiProxyEnvelope(VotingPluginWire.status(getMultiProxyServerName()));
	}

	public void sendMultiProxyEnvelope(JsonEnvelope envelope) {
		if (envelope == null) {
			return;
		}
		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			if (multiproxyClientHandles == null) {
				return;
			}
			for (ClientHandler h : multiproxyClientHandles.values()) {
				h.sendEnvelope(envelope);
			}
		} else if (getMultiProxyMethod().equals(MultiProxyMethod.REDIS)) {
			if (multiProxyRedis == null) {
				return;
			}
			for (String server : getProxyServers()) {
				multiProxyRedis.publishEnvelope("VotingPluginProxy_" + server, envelope);
			}
		}
	}

	private void handleEnvelope(JsonEnvelope envelope) {
		if (envelope == null) {
			return;
		}

		final String sub = envelope.getSubChannel() == null ? "" : envelope.getSubChannel();

		Map<String, String> f = envelope.getFields();
		if (f == null) {
			f = new HashMap<>();
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_STATUS)) {
			logInfo("Multi-proxy status message received");
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_CLEAR_VOTE)) {
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String server = f.getOrDefault(VotingPluginWire.K_SERVER, "");

			if (!uuid.isEmpty()) {
				clearVote(uuid);
				if (getPrimaryServer()) {
					sendMultiProxyEnvelope(VotingPluginWire.clearVotePrimary(uuid, player, server));
				}
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_CLEAR_VOTE_PRIMARY)) {
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			if (!uuid.isEmpty()) {
				clearVote(uuid);
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_LOGIN)) {
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			if (!player.isEmpty() && !uuid.isEmpty()) {
				addNonVotedPlayerCache(uuid, player);
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_VOTE) || sub.equalsIgnoreCase(VotingPluginWire.SUB_VOTE_ONLINE)) {
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			final String service = f.getOrDefault(VotingPluginWire.K_SERVICE, "");
			final String totals = f.getOrDefault(VotingPluginWire.K_TOTALS, "");
			final boolean realVote = Boolean.parseBoolean(f.getOrDefault(VotingPluginWire.K_REAL_VOTE, "false"));

			if (!player.isEmpty() && !uuid.isEmpty() && !service.isEmpty()) {
				triggerVote(player, service, realVote, true, 0L, VoteTotalsSnapshot.parseStorage(totals), uuid);
			}
			return;
		}

		if (getDebug()) {
			logInfo("MultiProxy ignored subchannel: " + sub + " fields=" + f);
		}
	}
}
