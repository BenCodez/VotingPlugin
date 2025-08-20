# Rewards System

VotingPlugin offers a comprehensive reward system that allows you to give players items, commands, permissions, and more when they vote.

## Reward Types

### Vote Points
Virtual currency that players earn for voting:

```yaml
# Give vote points
VotePoints: 10
```

### Commands
Execute commands when players vote:

```yaml
Commands:
- 'give %player% diamond 5'
- 'eco give %player% 1000'
- 'broadcast &6%player% just voted!'
- 'lp user %player% permission set vip.perks true 7d'
```

### Items
Give items directly to players:

```yaml
Items:
- 'DIAMOND:5'  # 5 diamonds
- 'EMERALD:3'  # 3 emeralds
- 'ENCHANTED_GOLDEN_APPLE:1'  # 1 golden apple
```

### Messages
Send messages to players or broadcast:

```yaml
Messages:
  Player: '&aThanks for voting! You received rewards!'
  Broadcast: '&6%player% voted and received awesome rewards!'
```

### Permissions
Grant temporary or permanent permissions:

```yaml
Permissions:
- 'group.vip:7d'  # VIP group for 7 days
- 'special.voter:permanent'  # Permanent permission
```

## Reward Categories

### First Vote Rewards

Rewards for a player's very first vote:

```yaml
FirstVote:
  Messages:
    Player: '&aWelcome! Thanks for your first vote!'
    Broadcast: '&6%player% voted for the first time!'
  Commands:
  - 'give %player% diamond_sword 1'
  - 'eco give %player% 5000'
  VotePoints: 25
  Items:
  - 'GOLDEN_APPLE:3'
```

### Daily Rewards

#### First Vote Today
Reward for the first vote of each day:

```yaml
FirstVoteToday:
  Messages:
    Player: '&aFirst vote today! Daily bonus!'
  VotePoints: 5
  Commands:
  - 'give %player% emerald 2'
```

#### All Sites Daily
Reward for voting on all sites in one day:

```yaml
AllSites:
  Messages:
    Player: '&aYou voted on all sites today!'
    Broadcast: '&6%player% voted on ALL sites today!'
  Commands:
  - 'give %player% nether_star 1'
  - 'eco give %player% 10000'
  VotePoints: 50
```

### Cumulative Rewards

Rewards given every X votes:

```yaml
Cumulative:
  # Every 10 votes
  '10':
    Enabled: true
    # AllTime, Monthly, Weekly, Daily
    RewardType: 'AllTime'
    Messages:
      Player: '&aEvery 10 votes bonus!'
    Commands:
    - 'give %player% diamond_block 1'
    VotePoints: 20
  
  # Every 25 votes
  '25':
    Enabled: true
    RewardType: 'Monthly'  # Resets monthly
    Commands:
    - 'crate give %player% vote 1'
    VotePoints: 75
```

### Milestone Rewards

One-time rewards at specific vote totals:

```yaml
Milestones:
  # 50 total votes
  '50':
    Messages:
      Player: '&aCongratulations on 50 votes!'
      Broadcast: '&6%player% reached 50 votes!'
    Commands:
    - 'give %player% diamond_chestplate 1'
    - 'enchant %player% protection 4'
    VotePoints: 100
  
  # 100 total votes
  '100':
    Messages:
      Player: '&aAmazing! 100 votes milestone!'
    Commands:
    - 'give %player% elytra 1'
    - 'give %player% firework_rocket 64'
    VotePoints: 250
  
  # 500 total votes
  '500':
    Commands:
    - 'lp user %player% permission set legendary.voter true'
    - 'give %player% beacon 1'
    VotePoints: 1000
```

### Vote Streak Rewards

Rewards for consecutive daily voting:

```yaml
VoteStreak:
  # 7 day streak
  '7':
    Messages:
      Player: '&aWeek-long voting streak!'
    Commands:
    - 'give %player% diamond_horse_armor 1'
    VotePoints: 35
  
  # 30 day streak
  '30':
    Messages:
      Player: '&aIncredible! 30 day voting streak!'
      Broadcast: '&6%player% has a 30-day voting streak!'
    Commands:
    - 'give %player% dragon_head 1'
    VotePoints: 150
```

## Advanced Reward Features

### Random Rewards

Give random rewards from a pool:

```yaml
RandomRewards:
  Enabled: true
  # Chance to get random reward (0-100)
  Chance: 25
  
  Rewards:
    Common:
      Weight: 70
      Commands:
      - 'give %player% iron_ingot 5'
    
    Rare:
      Weight: 25
      Commands:
      - 'give %player% diamond 2'
    
    Epic:
      Weight: 5
      Commands:
      - 'give %player% netherite_ingot 1'
```

### Conditional Rewards

Rewards based on conditions:

```yaml
ConditionalRewards:
  # Only on weekends
  Weekend:
    Condition: '%votingplugin_is_weekend%'
    Commands:
    - 'give %player% emerald 5'
    Messages:
      Player: '&aWeekend voting bonus!'
  
  # VIP players only
  VIPBonus:
    Condition: '%vault_rank% == VIP'
    VotePoints: 10
    Messages:
      Player: '&aVIP voting bonus!'
```

### Time-Based Rewards

Different rewards based on time:

```yaml
TimeBasedRewards:
  # Morning votes (6 AM - 12 PM)
  Morning:
    StartHour: 6
    EndHour: 12
    Commands:
    - 'give %player% bread 10'
    Messages:
      Player: '&aEarly bird bonus!'
  
  # Evening votes (6 PM - 11 PM)
  Evening:
    StartHour: 18
    EndHour: 23
    Commands:
    - 'give %player% cake 5'
    Messages:
      Player: '&aEvening voter bonus!'
```

## Vote Site Specific Rewards

Different rewards per vote site:

```yaml
# In VoteSites.yml
VoteSites:
  MinecraftServers:
    # Site-specific rewards override global rewards
    Rewards:
      VotePoints: 8
      Commands:
      - 'give %player% diamond 2'
      Messages:
        Player: '&aThanks for voting on Minecraft-Servers!'
  
  ServerList:
    Rewards:
      VotePoints: 6
      Commands:
      - 'give %player% emerald 3'
      Messages:
        Player: '&aThanks for voting on ServerList!'
```

## Reward Commands

### Item Commands
```yaml
Commands:
# Basic item giving
- 'give %player% diamond 5'
- 'give %player% enchanted_golden_apple 1'

# Items with enchantments
- 'give %player% diamond_sword 1'
- 'enchant %player% sharpness 5'
- 'enchant %player% unbreaking 3'

# Items with custom names/lore
- 'give %player% diamond_sword{display:{Name:"\"&6Voter Sword\"",Lore:["\"&7Thanks for voting!\""]}} 1'
```

### Economy Commands
```yaml
Commands:
# Vault economy
- 'eco give %player% 1000'
- 'eco take %player% 500'

# Other economy plugins
- 'money give %player% 1000'
- 'baltop add %player% 1000'
```

### Permission Commands
```yaml
Commands:
# LuckPerms
- 'lp user %player% permission set voter.perks true'
- 'lp user %player% parent add vip 7d'
- 'lp user %player% meta set suffix "&7[Voter]"'

# GroupManager
- 'manuadd %player% vip'
- 'manpromote %player%'
```

### Other Plugin Commands
```yaml
Commands:
# Crates
- 'crate give %player% vote 1'
- 'cr give %player% basic 3'

# Jobs
- 'jobs boost %player% all 1.5 60'

# Custom commands
- 'kit voter %player%'
- 'warp voter %player%'
```

## Placeholders in Rewards

### VotingPlugin Placeholders
- `%player%` - Player name
- `%uuid%` - Player UUID
- `%votes%` - Total votes
- `%votes_month%` - Monthly votes
- `%vote_points%` - Current vote points
- `%top_voter%` - Top voter rank
- `%vote_streak%` - Current streak

### Time Placeholders
- `%votingplugin_is_weekend%` - Weekend check
- `%votingplugin_hour%` - Current hour
- `%votingplugin_day%` - Current day

### External Placeholders
Integration with PlaceholderAPI:

```yaml
Commands:
- 'give %player% diamond %vault_eco_balance_fixed%'  # Give diamonds = balance
- 'broadcast &6%player% (%vault_rank%) voted!'       # Show player rank
```

## Reward Examples

### Starter Server Rewards

```yaml
# Simple rewards for new servers
FirstVote:
  Commands:
  - 'give %player% diamond 5'
  - 'give %player% golden_apple 3'
  Messages:
    Player: '&aWelcome! Thanks for voting!'

AllSites:
  Commands:
  - 'give %player% emerald_block 1'
  VotePoints: 10
  Messages:
    Player: '&aYou voted on all sites!'

Cumulative:
  '5':
    Commands:
    - 'give %player% iron_block 5'
```

### Economy-Focused Rewards

```yaml
# Heavy economy integration
FirstVote:
  Commands:
  - 'eco give %player% 5000'
  Messages:
    Player: '&a+$5,000 for your first vote!'

AllSites:
  Commands:
  - 'eco give %player% 25000'
  Messages:
    Player: '&a+$25,000 for voting on all sites!'

Cumulative:
  '10':
    Commands:
    - 'eco give %player% 10000'
```

### RPG Server Rewards

```yaml
# RPG/Adventure server rewards
FirstVote:
  Commands:
  - 'give %player% diamond_sword{Enchantments:[{id:sharpness,lvl:3}]} 1'
  - 'give %player% diamond_helmet{Enchantments:[{id:protection,lvl:2}]} 1'

Milestones:
  '100':
    Commands:
    - 'give %player% elytra{Enchantments:[{id:unbreaking,lvl:3}]} 1'
    - 'mcmmo addxp %player% all 1000'
```

### Survival Server Rewards

```yaml
# Balanced survival rewards
AllSites:
  Commands:
  - 'give %player% shulker_box 1'
  - 'give %player% totem_of_undying 1'

VoteStreak:
  '14':
    Commands:
    - 'give %player% beacon 1'
    - 'give %player% diamond_block 9'
```

## Best Practices

### Balancing

1. **Don't overpower**: Avoid giving overpowered items early
2. **Progressive rewards**: Increase value with commitment
3. **Server economy**: Consider your server's economy balance
4. **Rarity consideration**: Rare items should require dedication

### Engagement

1. **Immediate gratification**: Give instant rewards
2. **Long-term goals**: Use milestones for retention
3. **Variety**: Mix different reward types
4. **Special events**: Time-based bonuses for excitement

### Technical

1. **Test commands**: Verify all commands work correctly
2. **Permission checks**: Ensure commands have proper permissions
3. **Placeholder validation**: Test all placeholders work
4. **Error handling**: Monitor logs for command failures

## Troubleshooting Rewards

### Common Issues

1. **Commands not executing**: Check console for errors
2. **Items not received**: Verify inventory space
3. **Economy not working**: Check Vault integration
4. **Permissions failing**: Verify permission plugin integration

### Debug Commands

```bash
# Test specific reward
/adminvotetestreward <player> <reward>

# Give fake vote (triggers rewards)
/adminvotevote <player> <site> fake

# Check player reward status
/adminvoteuser <player>
```

For more reward configurations, see:
- [Vote Parties](vote-parties.md)
- [Configuration Reference](configuration.md)
- [API Documentation](api.md)