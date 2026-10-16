package com.bencodez.votingplugin.tests.commands.gui.player;

import com.bencodez.votingplugin.commands.gui.player.VoteShopConfirm;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class VoteShopConfirmTest {
	@Test
	void confirmationCanSubmitOnlyOnePurchase() {
		VoteShopConfirm confirmation = new VoteShopConfirm(null, null, null, null, null);

		assertTrue(VoteShopConfirmAccess.beginPurchase(confirmation));
		assertFalse(VoteShopConfirmAccess.beginPurchase(confirmation));
	}
}
