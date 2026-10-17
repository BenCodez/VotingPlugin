package com.bencodez.votingplugin.tests.commands.gui.player;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.commands.gui.player.*;
import com.bencodez.votingplugin.commands.gui.player.VoteShopConfirm.*;

/** Typed test-only access to package-private VoteShopConfirm seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteShopConfirmAccess {
    private VoteShopConfirmAccess() { }
    public static boolean beginPurchase(VoteShopConfirm target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteShopConfirm", target, "beginPurchase", new String[]{}, new Object[]{}); }
}
