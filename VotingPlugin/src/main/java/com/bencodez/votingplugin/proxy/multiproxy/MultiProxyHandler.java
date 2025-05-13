package com.bencodez.votingplugin.proxy.multiproxy;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.bungeeapi.redis.RedisHandler;
import com.bencodez.advancedcore.bungeeapi.redis.RedisListener;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.votingplugin.proxy.BungeeMessageData;

import lombok.Getter;

public abstract class MultiProxyHandler {
	private HashMap<String, ClientHandler> multiproxyClientHandles;

	private SocketHandler multiproxySocketHandler;

	@Getter
	private RedisHandler multiProxyRedis;

	public MultiProxyHandler() {

	}

	public abstract void addNonVotedPlayerCache(String string, String string2);

	public abstract void clearVote(String string);

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

	public void loadMultiProxySupport() {
		if (getMultiProxySupportEnabled()) {
			if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
				if (getEncryptionHandler() == null) {
					setEncryptionHandler(new EncryptionHandler(new File(getPluginDataFolder(), "secretkey.key")));
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
					public void onReceive(String[] data) {
						if (data.length > 0) {
							if (data[0].equalsIgnoreCase("Status")) {
								logInfo("Multi-proxy status message received");
							} else if (data[0].equalsIgnoreCase("ClearVote")) {
								clearVote(data[2]);
								if (getPrimaryServer()) {
									sendMultiProxyServerMessage("ClearVotePrimary", data[1], data[2]);
								}
							} else if (data[0].equalsIgnoreCase("ClearVotePrimary")) {
								clearVote(data[2]);
							} else if (data[0].equalsIgnoreCase("login")) {
								addNonVotedPlayerCache(data[1], data[2]);
								// nonVotedPlayersCache.addPlayer(data[1], data[2]);
							}
						}
						if (data.length > 8) {
							if (data[0].equalsIgnoreCase("Vote")) {
								triggerVote(data[2], data[3], Boolean.valueOf(data[7]), true, 0,
										new BungeeMessageData(data[8]), data[1]);
							} else if (data[0].equalsIgnoreCase("VoteOnline")) {
								triggerVote(data[2], data[3], Boolean.valueOf(data[7]), true, 0,
										new BungeeMessageData(data[8]), data[1]);
							}
						}

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
							getMultiProxyUsername(), getMultiProxyPassword()) {

						@Override
						public void debug(String message) {
							if (getDebug()) {
								logInfo("MultiProxyRedis: " + message);
							}
						}

						@Override
						public void onMessage(String channel, String[] data) {
							if (data.length > 0) {
								if (data[0].equalsIgnoreCase("Status")) {
									logInfo("Multi-proxy status message received");
								} else if (data[0].equalsIgnoreCase("ClearVote")) {
									clearVote(data[2]);
									if (getPrimaryServer()) {
										sendMultiProxyServerMessage("ClearVotePrimary", data[1], data[2]);
									}
								} else if (data[0].equalsIgnoreCase("ClearVotePrimary")) {
									clearVote(data[2]);
								} else if (data[0].equalsIgnoreCase("login")) {
									addNonVotedPlayerCache(data[1], data[2]);
									// nonVotedPlayersCache.addPlayer(data[1], data[2]);
								}
							}
							if (data.length > 8) {
								if (data[0].equalsIgnoreCase("Vote")) {
									triggerVote(data[2], data[3], Boolean.valueOf(data[7]), true, 0,
											new BungeeMessageData(data[8]), data[1]);
								} else if (data[0].equalsIgnoreCase("VoteOnline")) {
									triggerVote(data[2], data[3], Boolean.valueOf(data[7]), true, 0,
											new BungeeMessageData(data[8]), data[1]);
								}
							}
						}
					};
				}

				runAsnc(new Runnable() {

					@Override
					public void run() {
						multiProxyRedis.loadListener(
								new RedisListener(multiProxyRedis, "VotingPluginProxy_" + getMultiProxyServerName()));
					}
				});

			}
			logInfo("Loaded multi-proxy support: " + getMultiProxyMethod().toString());
		}
	}

	public void login(String uuid, String playerName) {
		if (!getMultiProxySupportEnabled() || getPrimaryServer()) {
			return;
		}
		sendMultiProxyServerMessage("Login", uuid, playerName);
	}

	public abstract void logInfo(String msg);

	public abstract void runAsnc(Runnable runnable);

	public void sendMultiProxyServerMessage(String... messageData) {
		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			for (ClientHandler h : multiproxyClientHandles.values()) {
				h.sendMessage(messageData);
			}
		} else if (getMultiProxyMethod().equals(MultiProxyMethod.REDIS)) {
			for (String server : getProxyServers()) {
				multiProxyRedis.sendMessage("VotingPluginProxy_" + server, messageData);
			}
		}
	}

	public abstract void setEncryptionHandler(EncryptionHandler encryptionHandler);

	public abstract void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			BungeeMessageData text, String uuid);
}
