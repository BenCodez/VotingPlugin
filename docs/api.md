# API Documentation

VotingPlugin provides a comprehensive API for developers to integrate voting functionality into their own plugins.

## Getting Started

### Maven Dependency

Add VotingPlugin to your project dependencies:

```xml
<repository>
    <id>BenCodez Repo</id>
    <url>https://nexus.bencodez.com/repository/maven-public/</url>
</repository>

<dependency>
    <groupId>com.bencodez</groupId>
    <artifactId>votingplugin</artifactId>
    <version>LATEST</version>
    <scope>provided</scope>
</dependency>
```

### Plugin.yml Dependency

```yaml
depend: [VotingPlugin]
# or
softdepend: [VotingPlugin]
```

### Getting the API Instance

```java
import com.bencodez.votingplugin.VotingPluginMain;

public class YourPlugin extends JavaPlugin {
    private VotingPluginMain votingPlugin;
    
    @Override
    public void onEnable() {
        // Get VotingPlugin instance
        votingPlugin = (VotingPluginMain) Bukkit.getPluginManager().getPlugin("VotingPlugin");
        
        if (votingPlugin == null) {
            getLogger().severe("VotingPlugin not found!");
            getServer().getPluginManager().disablePlugin(this);
            return;
        }
    }
}
```

## Core API Classes

### VotingPluginMain

Main plugin class providing access to all functionality:

```java
// Get user manager
VotingPluginUserManager userManager = votingPlugin.getUserManager();

// Get vote sites
ArrayList<VoteSite> voteSites = votingPlugin.getVoteSites();

// Get configuration
ConfigVoteSites voteConfig = votingPlugin.getVoteSites();

// Get vote party handler
VoteParty voteParty = votingPlugin.getVoteParty();

// Get top voter handler
TopVoterHandler topVoterHandler = votingPlugin.getTopVoterHandler();
```

### VotingPluginUser

Represents a player and their voting data:

```java
import com.bencodez.votingplugin.user.VotingPluginUser;

// Get user by name
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");

// Get user by UUID
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser(playerUUID);

// Get user by Player object
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser(player);
```

## Working with Users

### Getting Vote Data

```java
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");

// Get total votes
int totalVotes = user.getTotal();

// Get monthly votes
int monthlyVotes = user.getMonthTotal();

// Get weekly votes
int weeklyVotes = user.getWeekTotal();

// Get daily votes
int dailyVotes = user.getDayTotal();

// Get vote points
int votePoints = user.getPoints();

// Get last vote time for a site
VoteSite site = votingPlugin.getVoteSite("MinecraftServers");
long lastVote = user.getLastVote(site);

// Check if player can vote on a site
boolean canVote = user.canVoteAll(); // All sites
boolean canVoteOnSite = user.canVote(site); // Specific site
```

### Modifying Vote Data

```java
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");

// Add votes
user.addTotal(5); // Add 5 to total votes
user.addMonthTotal(3); // Add 3 to monthly total

// Add vote points
user.addPoints(10);

// Set vote points
user.setPoints(100);

// Set last vote time
user.setLastVote(site, System.currentTimeMillis());

// Give fake vote (triggers all vote processing)
user.addVote(site, false); // false = not a real vote
```

### User Rewards

```java
import com.bencodez.advancedcore.api.rewards.Reward;

// Give reward to user
Reward reward = new Reward();
reward.getCommands().add("give %player% diamond 5");
reward.getMessages().put("Player", "&aThanks for voting!");

user.giveReward(reward, false); // false = not offline reward
```

## Working with Vote Sites

### Getting Vote Sites

```java
import com.bencodez.votingplugin.objects.VoteSite;

// Get all vote sites
ArrayList<VoteSite> allSites = votingPlugin.getVoteSites();

// Get enabled vote sites only
ArrayList<VoteSite> enabledSites = votingPlugin.getVoteSitesEnabled();

// Get specific vote site
VoteSite site = votingPlugin.getVoteSite("MinecraftServers");

// Check if vote site exists
boolean exists = votingPlugin.hasVoteSite("MinecraftServers");
```

### Vote Site Information

```java
VoteSite site = votingPlugin.getVoteSite("MinecraftServers");

// Basic information
String displayName = site.getDisplayName();
String serviceSite = site.getServiceSite();
String voteURL = site.getVoteURL();
boolean enabled = site.isEnabled();
boolean hidden = site.isHidden();

// Vote timing
int voteDelay = site.getVoteDelay(); // Hours
boolean canVoteDaily = site.canVoteDaily();
boolean waitForDelay = site.getWaitUntilVoteDelay();

// Priority for sorting
int priority = site.getPriority();
```

## Event Handling

### Vote Events

Listen for voting events:

```java
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

public class VoteListener implements Listener {
    
    @EventHandler
    public void onPlayerVote(PlayerVoteEvent event) {
        VotingPluginUser user = event.getUser();
        VoteSite voteSite = event.getVoteSite();
        boolean realVote = event.isRealVote();
        
        // Your custom logic here
        String playerName = user.getPlayerName();
        String siteName = voteSite.getDisplayName();
        
        Bukkit.getLogger().info(playerName + " voted on " + siteName);
        
        // Cancel the event to prevent normal processing
        // event.setCancelled(true);
    }
}
```

### Vote Party Events

```java
import com.bencodez.votingplugin.events.VotePartyEvent;

@EventHandler
public void onVoteParty(VotePartyEvent event) {
    int votesReceived = event.getVotes();
    int votesRequired = event.getVotesRequired();
    
    // Custom vote party logic
    Bukkit.broadcastMessage("Vote party triggered with " + votesReceived + " votes!");
}
```

## Vote Party API

### Getting Vote Party Status

```java
import com.bencodez.votingplugin.voteparty.VoteParty;

VoteParty voteParty = votingPlugin.getVoteParty();

// Get current vote count
int currentVotes = voteParty.getTotalVotes();

// Get votes required
int votesRequired = voteParty.getVotesRequired();

// Get votes needed
int votesNeeded = voteParty.getNeededVotes();

// Check if vote party can happen
boolean canParty = currentVotes >= votesRequired;
```

### Manipulating Vote Party

```java
VoteParty voteParty = votingPlugin.getVoteParty();

// Add votes to party
voteParty.addTotal(5);

// Set total votes
voteParty.setTotalVotes(50);

// Reset vote party
voteParty.reset(true); // true = force reset

// Trigger vote party manually
voteParty.executeVoteParty();
```

## Top Voter API

### Getting Top Voters

```java
import com.bencodez.votingplugin.topvoter.TopVoterHandler;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;

TopVoterHandler topVoterHandler = votingPlugin.getTopVoterHandler();

// Get top voters for all time
ArrayList<TopVoterPlayer> topVoters = topVoterHandler.getTopVoters(TopVoter.AllTime);

// Get top voters for this month
ArrayList<TopVoterPlayer> monthlyTop = topVoterHandler.getTopVoters(TopVoter.Monthly);

// Get specific player's rank
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");
int rank = topVoterHandler.getRank(user, TopVoter.AllTime);
```

### Top Voter Information

```java
TopVoterPlayer topVoter = topVoters.get(0); // First place

String playerName = topVoter.getPlayer();
UUID playerUUID = topVoter.getUUID();
int votes = topVoter.getVotes();
int position = topVoter.getPosition();
```

## Custom Rewards

### Creating Rewards

```java
import com.bencodez.advancedcore.api.rewards.Reward;

// Create new reward
Reward reward = new Reward();

// Add commands
reward.getCommands().add("give %player% diamond 5");
reward.getCommands().add("eco give %player% 1000");

// Add messages
reward.getMessages().put("Player", "&aThanks for voting!");
reward.getMessages().put("Broadcast", "&6%player% voted and got rewards!");

// Add items
reward.getItems().add("DIAMOND:5");
reward.getItems().add("EMERALD:3");

// Give reward to player
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");
user.giveReward(reward, false);
```

### Conditional Rewards

```java
// Check conditions before giving rewards
if (user.getTotal() >= 50) {
    // Give milestone reward
    Reward milestoneReward = new Reward();
    milestoneReward.getCommands().add("give %player% nether_star 1");
    user.giveReward(milestoneReward, false);
}

// Check if player voted today
if (user.getDayTotal() > 0) {
    // Give daily bonus
    user.addPoints(5);
}
```

## Database API

### Direct Database Access

```java
import com.bencodez.votingplugin.user.UserManager;

UserManager userManager = votingPlugin.getUserManager();

// Get all users with vote data
Set<String> allUsers = userManager.getAllUUIDs();

// Check if user exists in database
boolean exists = userManager.doesUserExist("player-uuid");

// Get user data
VotingPluginUser user = userManager.getVotingPluginUser("PlayerName");
```

## Utility Methods

### Time Utilities

```java
import com.bencodez.advancedcore.api.misc.MiscUtils;

// Get current day number
int currentDay = MiscUtils.getInstance().getCurrentDay();

// Check if two times are same day
boolean sameDay = MiscUtils.getInstance().isSameDay(time1, time2);

// Get day from milliseconds
int day = MiscUtils.getInstance().getDayFromMili(timestamp);
```

### Player Utilities

```java
VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");

// Check if player is online
boolean online = user.isOnline();

// Get Bukkit player object (if online)
Player player = user.getPlayer();

// Check if player is vanished
boolean vanished = user.isVanished();

// Get player's UUID
UUID uuid = user.getUUID();
```

## Example Integration

### Complete Vote Listener Example

```java
package com.yourplugin.listeners;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

public class CustomVoteListener implements Listener {
    
    private VotingPluginMain votingPlugin;
    
    public CustomVoteListener(VotingPluginMain votingPlugin) {
        this.votingPlugin = votingPlugin;
    }
    
    @EventHandler
    public void onPlayerVote(PlayerVoteEvent event) {
        VotingPluginUser user = event.getUser();
        VoteSite voteSite = event.getVoteSite();
        
        // Get player if online
        Player player = user.getPlayer();
        
        // Custom milestone checking
        int totalVotes = user.getTotal();
        
        // Every 25 votes, give special reward
        if (totalVotes % 25 == 0 && totalVotes > 0) {
            if (player != null) {
                player.sendMessage("§6Congratulations! You've reached " + totalVotes + " total votes!");
                Bukkit.dispatchCommand(Bukkit.getConsoleSender(), "give " + user.getPlayerName() + " diamond_block 1");
            }
        }
        
        // Site-specific bonuses
        if (voteSite.getDisplayName().equals("Minecraft-Servers.net")) {
            // Extra bonus for this specific site
            user.addPoints(2);
            if (player != null) {
                player.sendMessage("§aBonus +2 vote points for voting on Minecraft-Servers.net!");
            }
        }
        
        // Weekend bonus
        if (isWeekend()) {
            user.addPoints(3);
            if (player != null) {
                player.sendMessage("§6Weekend bonus! +3 extra vote points!");
            }
        }
    }
    
    private boolean isWeekend() {
        int dayOfWeek = java.time.LocalDate.now().getDayOfWeek().getValue();
        return dayOfWeek == 6 || dayOfWeek == 7; // Saturday or Sunday
    }
}
```

### Custom Command Example

```java
package com.yourplugin.commands;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class VoteStatsCommand implements CommandExecutor {
    
    private VotingPluginMain votingPlugin;
    
    public VoteStatsCommand(VotingPluginMain votingPlugin) {
        this.votingPlugin = votingPlugin;
    }
    
    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (!(sender instanceof Player)) {
            sender.sendMessage("§cThis command can only be used by players!");
            return true;
        }
        
        Player player = (Player) sender;
        VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser(player);
        
        // Display comprehensive vote statistics
        player.sendMessage("§6§l=== Your Vote Statistics ===");
        player.sendMessage("§eTotal Votes: §a" + user.getTotal());
        player.sendMessage("§eMonthly Votes: §a" + user.getMonthTotal());
        player.sendMessage("§eWeekly Votes: §a" + user.getWeekTotal());
        player.sendMessage("§eDaily Votes: §a" + user.getDayTotal());
        player.sendMessage("§eVote Points: §a" + user.getPoints());
        
        // Calculate voting streak
        int streak = calculateVotingStreak(user);
        player.sendMessage("§eVoting Streak: §a" + streak + " days");
        
        // Show top voter rank
        int rank = votingPlugin.getTopVoterHandler().getRank(user, com.bencodez.votingplugin.topvoter.TopVoter.AllTime);
        player.sendMessage("§eAll-Time Rank: §a#" + rank);
        
        return true;
    }
    
    private int calculateVotingStreak(VotingPluginUser user) {
        // Custom streak calculation logic
        // This is a simplified example
        return user.getDayTotal() > 0 ? 1 : 0;
    }
}
```

## Best Practices

### Performance

1. **Cache user objects**: Don't create new VotingPluginUser objects repeatedly
2. **Async operations**: Use async tasks for database operations
3. **Batch operations**: Group multiple operations together
4. **Check online status**: Only perform expensive operations for online players

### Error Handling

```java
try {
    VotingPluginUser user = votingPlugin.getVotingPluginUserManager().getVotingPluginUser("PlayerName");
    if (user != null) {
        // Safe to use user object
        user.addPoints(10);
    }
} catch (Exception e) {
    // Handle errors gracefully
    getLogger().warning("Error processing vote for player: " + e.getMessage());
}
```

### Memory Management

1. **Don't store references**: Don't keep long-term references to user objects
2. **Clean up listeners**: Unregister event listeners when plugin disables
3. **Avoid memory leaks**: Don't store player data in static collections

For more examples and advanced usage, check the [VotingPlugin source code](https://github.com/BenCodez/VotingPlugin) on GitHub.