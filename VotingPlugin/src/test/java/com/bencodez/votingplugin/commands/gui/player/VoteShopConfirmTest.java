package com.bencodez.votingplugin.commands.gui.player;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class VoteShopConfirmTest {
	@Test
	void confirmationCanSubmitOnlyOnePurchase() {
		VoteShopConfirm confirmation = new VoteShopConfirm(null, null, null, null, null);

		assertTrue(confirmation.beginPurchase());
		assertFalse(confirmation.beginPurchase());
	}
}
