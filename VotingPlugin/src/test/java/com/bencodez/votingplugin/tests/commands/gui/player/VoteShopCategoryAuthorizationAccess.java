package com.bencodez.votingplugin.tests.commands.gui.player;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.commands.gui.player.*;
import java.util.function.Predicate;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

/** Test-only forwarding access to the non-public production VoteShopCategoryAuthorization. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteShopCategoryAuthorizationAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private VoteShopCategoryAuthorizationAccess(Object delegate) { this.delegate = delegate; }
    public static VoteShopCategoryAuthorizationAccess view(Object delegate) { return delegate == null ? null : new VoteShopCategoryAuthorizationAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof VoteShopCategoryAuthorizationAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static VoteShopCategoryAuthorizationAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.commands.gui.player.VoteShopCategoryAuthorization"))); }
    public VoteShopCategoryAuthorizationAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public VoteShopCategoryAuthorizationAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public VoteShopCategoryAuthorizationAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public static boolean mayDisplay(VoteShopCategoryButton button, Predicate<String> permissionCheck) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteShopCategoryAuthorization", null, "mayDisplay", new String[]{"VoteShopCategoryButton", "Predicate"}, new Object[]{button, permissionCheck}); }
    public static boolean authorized(VoteShopCategoryButton button, Predicate<String> permissionCheck) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteShopCategoryAuthorization", null, "authorized", new String[]{"VoteShopCategoryButton", "Predicate"}, new Object[]{button, permissionCheck}); }
    public static boolean canOpen(VoteShopDefinition definition, String targetCategory, Predicate<String> permissionCheck) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteShopCategoryAuthorization", null, "canOpen", new String[]{"VoteShopDefinition", "String", "Predicate"}, new Object[]{definition, targetCategory, permissionCheck}); }
    public static boolean canPurchase(VoteShopDefinition definition, VoteShopCategory category, VoteShopItem item, Predicate<String> permissionCheck) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteShopCategoryAuthorization", null, "canPurchase", new String[]{"VoteShopDefinition", "VoteShopCategory", "VoteShopItem", "Predicate"}, new Object[]{definition, category, item, permissionCheck}); }
}
