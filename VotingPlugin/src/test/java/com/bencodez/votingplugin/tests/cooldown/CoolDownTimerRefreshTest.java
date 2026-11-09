package com.bencodez.votingplugin.tests.cooldown;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.*;
import org.bukkit.Server;
import org.bukkit.event.Event;
import org.bukkit.plugin.PluginManager;
import com.bencodez.advancedcore.api.user.*;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.*;
class CoolDownTimerRefreshTest {
  @org.junit.jupiter.api.Test void alreadyCompletedSiteIsNotDeliveredAgain() throws Exception { exercise(true, true, "OfflinePlayer", "Renamed", "Renamed"); }
  @org.junit.jupiter.api.Test void newerVoteIsConsultedBeforeDelivering() throws Exception { exercise(false, false, "OfflinePlayer", "", "OfflinePlayer"); }
  @org.junit.jupiter.api.Test void missingNameDoesNotInventIdentity() throws Exception { exercise(true, true, null, "", null); }
  private void exercise(boolean alreadyCompleted, boolean eligible, String scheduledName, String storedName, String expectedName) throws Exception {
    var id=UUID.randomUUID(); var plugin=mock(VotingPluginMain.class,RETURNS_DEEP_STUBS);
    var config=mock(Config.class);var sites=mock(VoteSiteManager.class);var site=mock(VoteSite.class);
    var server=mock(Server.class);var events=mock(PluginManager.class);
    when(plugin.isEnabled()).thenReturn(true);when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
    when(plugin.getConfigFile()).thenReturn(config);when(config.isPerSiteCoolDownEvents()).thenReturn(true);
    when(plugin.getVoteSiteManager()).thenReturn(sites);when(plugin.getServer()).thenReturn(server);when(server.getPluginManager()).thenReturn(events);
    when(site.getKey()).thenReturn("site");when(site.isVoteDelayDaily()).thenReturn(true);
    when(sites.getVoteSites()).thenReturn(new ArrayList<>(List.of(site)));
    when(sites.hasVoteSite("site")).thenReturn(true);when(sites.getVoteSite("site",true)).thenReturn(site);
    var user=mock(VotingPluginUser.class); Field p=VotingPluginUser.class.getDeclaredField("plugin");p.setAccessible(true);p.set(user,plugin);
    Field ap=AdvancedCoreUser.class.getDeclaredField("plugin");ap.setAccessible(true);ap.set(user,plugin);
    var data=new UserData(user);when(user.getData()).thenReturn(data);when(user.getUserData()).thenReturn(data);when(user.getPlugin()).thenReturn(plugin);
    doCallRealMethod().when(user).getCoolDownCheckSiteList();when(user.getCoolDownCheckSitePath()).thenReturn("CoolDownCheck_Sites");
    doCallRealMethod().when(user).getPlayerName();doCallRealMethod().when(user).setPlayerName(anyString());
    doCallRealMethod().when(user).userDataFetechMode(any());doCallRealMethod().when(user).getUserDataFetchMode();
    doCallRealMethod().when(user).updateTempCacheWithColumns(any());doCallRealMethod().when(user).clearTempCache();
    when(user.getCoolDownCheck()).thenReturn(true);when(user.canVoteAll()).thenReturn(false);when(user.canVoteSite(site)).thenReturn(true);
    when(user.getSitesVotedOn()).thenReturn(1);when(user.getJavaUUID()).thenReturn(id);when(user.getNextSiteAvailable()).thenReturn(new NextSite(site,60));
    when(plugin.getVotingPluginUserManager().getVotingPluginUser(id,false)).thenReturn(user);
    var storage=plugin.getUserManager(); doAnswer(call->{BiConsumer<UUID,ArrayList<Column>> each=call.getArgument(0);Consumer<Integer> done=call.getArgument(1);
      ArrayList<Column> columns=new ArrayList<>(); if(scheduledName != null) columns.add(new Column("PlayerName",new DataValueString(scheduledName))); columns.add(new Column("CoolDownCheck_Sites",new DataValueString("site//true"))); each.accept(id,columns);done.accept(1);return null;}).when(storage).forEachUserKeys(any(),any());
    var fresh=mock(VotingPluginUser.class);
    when(plugin.getVotingPluginUserManager().getVotingPluginUser(id,false)).thenReturn(user, fresh);
    var freshData=mock(UserData.class); when(fresh.getData()).thenReturn(freshData);
    when(freshData.getString(eq("PlayerName"),nullable(UserDataFetchMode.class))).thenReturn(storedName);
    var freshFlags=new HashMap<String,Boolean>();
    if (alreadyCompleted) freshFlags.put("site", true);
    when(fresh.getCoolDownCheckSiteList()).thenReturn(freshFlags);
    when(fresh.canVoteSite(site)).thenReturn(eligible);
    var check=new CoolDownCheck(plugin);Field f=CoolDownCheck.class.getDeclaredField("timer");f.setAccessible(true);((ScheduledExecutorService) f.get(check)).shutdownNow();var timer=mock(ScheduledExecutorService.class);var scheduled=new AtomicReference<Runnable>();
    when(timer.schedule(any(Runnable.class),anyLong(),any(TimeUnit.class))).thenAnswer(call->{scheduled.set(call.getArgument(0));return mock(ScheduledFuture.class);});
    f.set(check,timer);
    check.checkAllVoteSites();assertNull(data.getTempCache());assertNotNull(scheduled.get());
    verify(events,never()).callEvent(isA(PlayerVoteSiteCoolDownEndEvent.class));
    try (var bukkit=mockStatic(org.bukkit.Bukkit.class)) { scheduled.get().run(); bukkit.verifyNoInteractions(); }
    verify(events,never()).callEvent(isA(PlayerVoteSiteCoolDownEndEvent.class));
    verify(plugin.getVotingPluginUserManager(),times(2)).getVotingPluginUser(id,false);
    verify(plugin.getVotingPluginUserManager(),never()).getVotingPluginUser(id);
    if (expectedName == null) verify(fresh,never()).setPlayerName(anyString());
    else verify(fresh).setPlayerName(expectedName);
    verify(fresh).userDataFetechMode(UserDataFetchMode.NO_CACHE);
    verify(fresh,never()).setCoolDownCheckSite(any());
    check.shutdown();
  }
}
