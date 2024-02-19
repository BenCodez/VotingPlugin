package com.bencodez.votingplugin.bungee.global.multiproxy;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;

import com.bencodez.advancedcore.api.misc.encryption.EncryptionHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.ClientHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketHandler;
import com.bencodez.advancedcore.bungeeapi.sockets.SocketReceiver;
import com.bencodez.votingplugin.bungee.BungeeMessageData;

public abstract class MultiProxyHandler {
	public abstract boolean getMultiProxySupportEnabled();

	public abstract EncryptionHandler getEncryptionHandler();

	public abstract File getPluginDataFolder();

	public abstract void setEncryptionHandler(EncryptionHandler encryptionHandler);

	public abstract void logInfo(String msg);

	public abstract boolean getDebug();

	public abstract String getVersion();

	private HashMap<String, ClientHandler> multiproxyClientHandles;

	private SocketHandler multiproxySocketHandler;

	public MultiProxyHandler() {

	}

	public void sendMultiProxyServerMessage(String... messageData) {
		for (ClientHandler h : multiproxyClientHandles.values()) {
			h.sendMessage(messageData);
		}
	}
	
	public void close() {
		if (multiproxySocketHandler != null) {
			multiproxySocketHandler.closeConnection();
			multiproxySocketHandler = null;
		}
	}

	public void loadMultiProxySupport() {
		if (getMultiProxySupportEnabled()) {
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
							// cachedOnlineVotes.remove(data[2]);
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

			multiproxyClientHandles = new HashMap<String, ClientHandler>();
			for (String s : getMultiProxyServers()) {
				MultiProxyServerSocketConfiguration d = getMultiProxyServersConfiguration(s);
				multiproxyClientHandles.put(s,
						new ClientHandler(d.getHost(), d.getPort(), getEncryptionHandler(), getDebug()));
			}

			logInfo("Loaded multi-proxy support");
		}
	}

	public abstract boolean getPrimaryServer();

	public void login(String uuid, String playerName) {
		if (!getMultiProxySupportEnabled()) {
			return;
		}
		if (getPrimaryServer()) {
			return;
		}
		sendMultiProxyServerMessage("Login", uuid, playerName);
	}

	protected abstract int getMultiProxySocketHostPort();

	protected abstract String getMultiProxySocketHostHost();

	protected abstract MultiProxyServerSocketConfiguration getMultiProxyServersConfiguration(String s);

	protected abstract Collection<String> getMultiProxyServers();

	protected abstract void triggerVote(String player, String service, boolean realVote, boolean timeQueue,
			long queueTime, BungeeMessageData text, String uuid);

	protected abstract void addNonVotedPlayerCache(String string, String string2);

	protected abstract void clearVote(String string);
}
