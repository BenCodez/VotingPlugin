package com.bencodez.votingplugin.user;

/** The externally observable outcome of a point transfer. */
public enum PointTransferResult {
	SUCCESS,
	INSUFFICIENT_POINTS,
	CANCELLED,
	PENDING_CONFIRMATION,
	UNAVAILABLE
}
