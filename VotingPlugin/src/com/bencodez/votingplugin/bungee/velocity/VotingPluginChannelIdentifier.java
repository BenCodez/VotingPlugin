package com.bencodez.votingplugin.bungee.velocity;

import com.velocitypowered.api.proxy.messages.ChannelIdentifier;

public class VotingPluginChannelIdentifier implements ChannelIdentifier {
	public VotingPluginChannelIdentifier() {
	}

	@Override
	public String getId() {
		return "vp:vp";
	}

}
