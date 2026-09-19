package com.bencodez.votingplugin.commands;

import org.bukkit.command.CommandSender;

/** Centralized authorization checks for destructive and configuration-changing admin actions. */
public final class AdminAuthorization {
	public static final String ADMIN_PERMISSION = "VotingPlugin.Admin";
	public static final String REMOVE_POINTS_ALL_PERMISSION = "VotingPlugin.Commands.AdminVote.RemovePoints.All";

	private AdminAuthorization() {
	}

	public static boolean canRemovePointsFromAll(CommandSender sender) {
		return sender.hasPermission(REMOVE_POINTS_ALL_PERMISSION) || sender.hasPermission(ADMIN_PERMISSION);
	}

	public static boolean canAddPointsToAll(CommandSender sender, int amount) {
		return amount >= 0 || canRemovePointsFromAll(sender);
	}

	public static boolean canAddPointsToAll(CommandSender sender, int amount, boolean allowAdminOverride) {
		return amount >= 0 || sender.hasPermission(REMOVE_POINTS_ALL_PERMISSION)
				|| allowAdminOverride && sender.hasPermission(ADMIN_PERMISSION);
	}

	public static boolean canEditConfig(CommandSender sender, String permission) {
		return sender.hasPermission(permission) || sender.hasPermission(ADMIN_PERMISSION);
	}

	public static boolean hasCommandOrAdmin(CommandSender sender, String permission) {
		return sender.hasPermission(permission) || sender.hasPermission(ADMIN_PERMISSION);
	}
}
