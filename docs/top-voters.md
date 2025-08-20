# Top Voters System

The Top Voters system tracks and displays the most active voters on your server, encouraging competition and recognizing dedicated players.

## Overview

VotingPlugin tracks voting statistics and creates leaderboards for different time periods:

- **All Time**: Total votes since joining
- **Monthly**: Votes in the current month
- **Weekly**: Votes in the current week
- **Daily**: Votes today

## Configuration

Top voter settings are configured in `Config.yml`:

### Basic Settings

```yaml
TopVoter:
  # Enable top voter system
  Enabled: true
  
  # Number of players to track in leaderboards
  TopVoterAmount: 10
  
  # Cache time for top voter data (minutes)
  CacheTime: 30
  
  # Include offline players in rankings
  IncludeOfflinePlayers: true
```

### Display Settings

```yaml
TopVoter:
  # Show position numbers in commands
  ShowPositions: true
  
  # Format for displaying vote counts
  VoteFormat: '%votes% votes'
  
  # Message when no top voters exist
  NoTopVotersMessage: '&cNo top voters found!'
```

### Calculation Settings

```yaml
TopVoter:
  # Which vote total to use for "All Time" rankings
  # Options: AllTime, Monthly, Weekly, Daily
  PrimaryTotal: 'AllTime'
  
  # Reset monthly totals automatically
  ResetMonthlyTotals: true
  
  # Store monthly totals with dates
  StoreMonthTotalsWithDate: true
```

## Top Voter Types

### All Time Top Voters

Tracks total votes since the player first voted:

```yaml
# Access via commands
/votetop
/votetop alltime
/vtop
```

### Monthly Top Voters

Tracks votes for the current month:

```yaml
# Monthly totals can reset automatically
TopVoter:
  MonthlyTotals:
    # Reset on first day of month
    AutoReset: true
    
    # Time of day to reset (24-hour format)
    ResetHour: 0
```

Commands:
```bash
/votetop monthly
/votetop month
```

### Weekly Top Voters

Tracks votes for the current week:

```yaml
# Weekly settings
TopVoter:
  WeeklyTotals:
    # Day week starts (1=Monday, 7=Sunday)
    WeekStartDay: 1
    
    # Auto reset weekly totals
    AutoReset: true
```

Commands:
```bash
/votetop weekly
/votetop week
```

### Daily Top Voters

Tracks votes for today:

```yaml
# Daily totals reset at midnight server time
TopVoter:
  DailyTotals:
    # Time zone for daily resets
    TimeZone: 'America/New_York'
    
    # Hour to reset daily totals
    ResetHour: 0
```

Commands:
```bash
/votetop daily
/votetop today
/votetoday
```

## GUI Integration

Top voter GUIs are configured in `GUI.yml`:

### Main Top Voter GUI

```yaml
TopVoterGUI:
  # GUI title with placeholder support
  Title: '&6Top Voters - %topvoter_type%'
  
  # GUI size
  Size: 54
  
  # Background item
  Background:
    Material: 'GRAY_STAINED_GLASS_PANE'
    Name: ' '
  
  # Player head configuration
  PlayerHead:
    # Display name format
    Name: '&6%position%. %player%'
    
    # Lore lines
    Lore:
    - '&7Votes: &a%votes%'
    - '&7Vote Points: &b%votepoints%'
    - '&7Last Vote: &e%last_vote%'
    - ''
    - '&eClick for more info!'
```

### Navigation Items

```yaml
TopVoterGUI:
  # Previous page button
  PreviousPage:
    Material: 'ARROW'
    Name: '&cPrevious Page'
    Slot: 45
  
  # Next page button
  NextPage:
    Material: 'ARROW'
    Name: '&aNext Page'
    Slot: 53
  
  # Period selector
  PeriodSelector:
    Material: 'CLOCK'
    Name: '&6Change Period'
    Slot: 49
    Lore:
    - '&7Current: &a%current_period%'
    - '&7Click to change!'
```

## Exclusion System

### Permission-Based Exclusion

Players with specific permissions can be excluded from rankings:

```yaml
# Permission to exclude from top voter
VotingPlugin.TopVoter.Ignore: true
```

```yaml
# In Config.yml
TopVoter:
  # Use permission-based exclusion
  UsePermissionExclusion: true
  
  # Permission node for exclusion
  ExclusionPermission: 'VotingPlugin.TopVoter.Ignore'
```

### Manual Exclusion

Exclude specific players by UUID or name:

```yaml
TopVoter:
  # Exclude specific players
  ExcludedPlayers:
  - 'StaffMember1'
  - 'TestAccount'
  
  # Exclude by UUID
  ExcludedUUIDs:
  - '123e4567-e89b-12d3-a456-426614174000'
```

### Automatic Exclusion

```yaml
TopVoter:
  # Exclude banned players
  ExcludeBannedPlayers: true
  
  # Exclude players who haven't voted recently
  ExcludeInactivePlayers: true
  
  # Days of inactivity before exclusion
  InactivityDays: 30
```

## Rewards for Top Voters

### Automatic Rewards

Give rewards to top voters automatically:

```yaml
TopVoterRewards:
  # Enable automatic rewards
  Enabled: true
  
  # When to give rewards
  Schedule: 'MONTHLY'  # DAILY, WEEKLY, MONTHLY
  
  # Rewards for different positions
  Positions:
    # First place
    '1':
      Commands:
      - 'give %player% diamond_block 5'
      - 'eco give %player% 10000'
      - 'broadcast &6%player% was the top voter this month!'
      Messages:
        Player: '&aCongratulations! You were #1 voter this month!'
    
    # Second place
    '2':
      Commands:
      - 'give %player% diamond_block 3'
      - 'eco give %player% 7500'
      Messages:
        Player: '&aGreat job! You were #2 voter this month!'
    
    # Third place
    '3':
      Commands:
      - 'give %player% diamond_block 2'
      - 'eco give %player% 5000'
      Messages:
        Player: '&aWell done! You were #3 voter this month!'
    
    # Top 10
    'TOP10':
      Commands:
      - 'give %player% diamond 10'
      - 'eco give %player% 2500'
      Messages:
        Player: '&aYou were in the top 10 voters this month!'
```

### Manual Rewards

```bash
# Give rewards to current top voters
/adminvotegiveall TopVoterReward monthly 1-10

# Give specific reward to top voter
/adminvotegivereward TopPlayer TopVoterFirst
```

## Commands

### Player Commands

| Command | Description |
|---------|-------------|
| `/votetop` | View all-time top voters |
| `/votetop alltime` | View all-time top voters |
| `/votetop monthly` | View monthly top voters |
| `/votetop weekly` | View weekly top voters |
| `/votetop daily` | View daily top voters |
| `/votetoday` | View today's voters |
| `/votebest` | View your best voting stats |

### Admin Commands

| Command | Description |
|---------|-------------|
| `/adminvotetop` | Admin top voter management |
| `/adminvoteresyncmilestones` | Recalculate top voter data |
| `/adminvotecleartotal` | Clear specific totals |

## Placeholders

### General Placeholders

| Placeholder | Description |
|-------------|-------------|
| `%votingplugin_votes%` | Player's total votes |
| `%votingplugin_votes_month%` | Monthly votes |
| `%votingplugin_votes_week%` | Weekly votes |
| `%votingplugin_votes_day%` | Daily votes |
| `%votingplugin_top_voter%` | All-time rank |
| `%votingplugin_top_voter_monthly%` | Monthly rank |
| `%votingplugin_top_voter_weekly%` | Weekly rank |
| `%votingplugin_top_voter_daily%` | Daily rank |

### Top Voter List Placeholders

| Placeholder | Description |
|-------------|-------------|
| `%votingplugin_top_voter_1%` | #1 voter name |
| `%votingplugin_top_voter_1_votes%` | #1 voter vote count |
| `%votingplugin_top_voter_monthly_1%` | #1 monthly voter |
| `%votingplugin_top_voter_weekly_1%` | #1 weekly voter |

## Integration Examples

### Scoreboard Integration

```yaml
# For scoreboard plugins
sidebar:
  - '&6&lTop Voters'
  - '&71. %votingplugin_top_voter_1% &f(%votingplugin_top_voter_1_votes%)'
  - '&72. %votingplugin_top_voter_2% &f(%votingplugin_top_voter_2_votes%)'
  - '&73. %votingplugin_top_voter_3% &f(%votingplugin_top_voter_3_votes%)'
  - ''
  - '&7Your rank: &a#%votingplugin_top_voter%'
```

### Chat Integration

```yaml
# Chat format showing voter rank
chat-format: '%vault_prefix%%player% &7[#%votingplugin_top_voter%] &f%message%'
```

### Webstore Integration

```yaml
# Show top voters on website
# Use placeholders in webstore descriptions
```

## Performance Optimization

### Caching

```yaml
TopVoter:
  # Cache settings for better performance
  CacheTime: 60        # Minutes to cache data
  CacheSize: 100       # Number of players to cache
  
  # Background updates
  BackgroundUpdate: true
  UpdateInterval: 300   # Seconds between updates
```

### Database Optimization

```yaml
TopVoter:
  # Optimize database queries
  BatchSize: 50         # Players per database query
  UseIndexes: true      # Use database indexes
  
  # Limit data retention
  MaxHistoryMonths: 12  # Keep 12 months of data
```

## Troubleshooting

### Top Voters Not Updating

**Check Configuration:**
```yaml
TopVoter:
  Enabled: true          # Must be enabled
  CacheTime: 30         # Try lower cache time
```

**Manual Update:**
```bash
/adminvotebackgroundupdate
/adminvoteresyncmilestones
```

### Wrong Vote Counts

**Verify Data Source:**
```yaml
TopVoter:
  PrimaryTotal: 'AllTime'  # Check correct source
```

**Resync Data:**
```bash
/adminvoteresyncmilestones
```

### Performance Issues

**Optimize Settings:**
```yaml
TopVoter:
  CacheTime: 120        # Increase cache time
  TopVoterAmount: 10    # Reduce tracked players
  BackgroundUpdate: true # Enable background updates
```

## Best Practices

### Engagement

1. **Regular recognition**: Announce top voters regularly
2. **Visible rankings**: Display in spawn or website
3. **Meaningful rewards**: Make top voter rewards worthwhile
4. **Fair competition**: Use appropriate time periods

### Performance

1. **Reasonable cache times**: Don't update too frequently
2. **Limit tracked players**: Only track what you need
3. **Background updates**: Use async updates when possible
4. **Database maintenance**: Clean old data periodically

### Balance

1. **Multiple time periods**: Give everyone a chance to be #1
2. **Exclusion rules**: Exclude staff if appropriate
3. **Reward scaling**: Higher ranks get better rewards
4. **Recognition variety**: Mix commands, items, and titles

For more information, see:
- [Commands Reference](commands.md)
- [Permissions Guide](permissions.md)
- [Configuration Reference](configuration.md)