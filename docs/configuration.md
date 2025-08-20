# Configuration Reference

VotingPlugin offers extensive configuration options through multiple YAML files. This guide covers all major configuration sections.

## Configuration Files

| File | Purpose |
|------|---------|
| `Config.yml` | Main plugin configuration (871 lines) |
| `VoteSites.yml` | Vote site definitions |
| `SpecialRewards.yml` | Reward configurations |
| `GUI.yml` | GUI customization |
| `Shop.yml` | Vote shop settings |
| `BungeeSettings.yml` | Proxy server settings |

## Config.yml - Main Configuration

### Data Storage

```yaml
# Storage backend selection
DataStorage: SQLITE  # or MYSQL

# MySQL configuration (if using MYSQL)
MySQL:
  Host: 'localhost'
  Port: 3306
  Database: 'votingplugin'
  Username: 'username'
  Password: 'password'
  MaxConnections: 5
  Prefix: ''  # Table prefix for multiple servers
  UseSSL: true
  PublicKeyRetrieval: false
```

### Vote Reminding

```yaml
VoteReminding:
  # Enable automatic vote reminders
  Enabled: true
  
  # Remind players when they join
  RemindOnJoin: true
  
  # Interval between reminders (minutes)
  ReminderInterval: 30
  
  # Only remind if player can vote on all sites
  RemindForAllSites: false
  
  # Delay before first reminder (seconds)
  DelayBeforeReminding: 5
```

### Vote Delay Settings

```yaml
# Global vote delay settings
VoteDelay:
  # Enable vote delay checking
  Enabled: true
  
  # Default delay between votes (hours)
  Default: 24
  
  # Allow bypass with permission
  AllowBypass: true
```

### Broadcasting

```yaml
Broadcasting:
  # Broadcast when players vote
  Enabled: true
  
  # Broadcast to all servers (proxy mode)
  BroadcastToAllServers: false
  
  # Custom broadcast message
  Message: '&a%player% voted and received rewards!'
  
  # Blocked servers (won't receive broadcasts)
  BlockedServers: []
```

### GUI Settings

```yaml
GUI:
  # Enable GUI interfaces
  Enabled: true
  
  # Default GUI size
  Size: 54
  
  # Use skull textures for players
  UsePlayerSkulls: true
  
  # Cache player skulls
  CachePlayerSkulls: true
```

## VoteSites.yml - Vote Site Configuration

### Basic Vote Site Setup

```yaml
VoteSites:
  # Site identifier (no spaces or special characters)
  MinecraftServers:
    # Enable this vote site
    Enabled: true
    
    # Display name
    Name: 'Minecraft-Servers.net'
    
    # Priority for sorting (higher = first)
    Priority: 10
    
    # Hide from some GUIs
    Hidden: false
    
    # Service site name (from Votifier)
    ServiceSite: 'Minecraft-Servers.net'
    
    # Vote URL for players
    VoteURL: 'https://minecraft-servers.net/server/12345/'
    
    # Hours between votes
    VoteDelay: 24
    
    # Wait until delay before accepting votes
    WaitUntilVoteDelay: false
    
    # Reset daily (for sites that reset at midnight)
    VoteDelayDaily: false
```

### Advanced Vote Site Settings

```yaml
  MinecraftServers:
    # Custom rewards for this site
    Rewards:
      VotePoints: 5
      Commands:
      - 'give %player% diamond 1'
      Messages:
        Player: '&aThanks for voting on Minecraft-Servers!'
    
    # Vote delay in minutes (overrides hours)
    VoteDelayMin: 30
    
    # Custom vote delay hour (for sites that reset at specific times)
    VoteDelayHour: 6  # Reset at 6 AM
    
    # Require authentication before voting
    RequireAuth: false
    
    # Custom service site configuration
    ServiceSiteConfig:
      CheckMethod: 'POST'
      CheckURL: 'https://api.site.com/check'
```

## SpecialRewards.yml - Reward Configuration

### First Vote Rewards

```yaml
# Reward for very first vote ever
FirstVote:
  Messages:
    Player: '&aWelcome! Thanks for your first vote!'
    Broadcast: '&6%player% voted for the first time!'
  Commands:
  - 'give %player% diamond 5'
  - 'eco give %player% 1000'
  VotePoints: 10
```

### Daily/Weekly Rewards

```yaml
# First vote of the day
FirstVoteToday:
  Messages:
    Player: '&aFirst vote today! Bonus rewards!'
  VotePoints: 5
  Commands:
  - 'give %player% emerald 1'

# Voting on all sites in one day
AllSites:
  Messages:
    Player: '&aYou voted on all sites today!'
    Broadcast: '&6%player% voted on all sites!'
  Commands:
  - 'give %player% nether_star 1'
  VotePoints: 25
```

### Cumulative Rewards

```yaml
Cumulative:
  # Every 20 votes
  '20':
    Enabled: true
    # Type: AllTime, Monthly, Weekly, Daily
    Type: 'AllTime'
    Messages:
      Player: '&aEvery 20 votes bonus!'
    Commands:
    - 'give %player% diamond_block 1'
    VotePoints: 50
```

### Milestone Rewards

```yaml
Milestones:
  # One-time rewards at specific vote counts
  '50':
    Messages:
      Player: '&aCongratulations on 50 votes!'
    Commands:
    - 'give %player% diamond_sword 1'
    - 'enchant %player% sharpness 5'
  
  '100':
    Messages:
      Player: '&aAmazing! 100 votes reached!'
    Commands:
    - 'give %player% elytra 1'
    VotePoints: 100
```

## GUI.yml - Interface Customization

### Main Vote GUI

```yaml
VoteGUI:
  # GUI title
  Title: '&6Vote Sites'
  
  # GUI size (9, 18, 27, 36, 45, 54)
  Size: 54
  
  # Background item
  Background:
    Material: 'GRAY_STAINED_GLASS_PANE'
    Name: ' '
  
  # Vote site item template
  VoteSite:
    # Available: Material from site config
    Material: 'PAPER'
    Name: '&6%name%'
    Lore:
    - '&7Click to vote!'
    - '&7Reward: &a%rewards%'
    - '&7Next vote: &e%next%'
```

### Top Voter GUI

```yaml
TopVoterGUI:
  Title: '&6Top Voters - %topvoter%'
  Size: 54
  
  # Player head configuration
  PlayerHead:
    Name: '&6%player%'
    Lore:
    - '&7Votes: &a%votes%'
    - '&7Rank: &e#%position%'
    - '&7Points: &b%points%'
```

## Shop.yml - Vote Shop Configuration

```yaml
VoteShop:
  # Enable vote shop
  Enabled: true
  
  # Shop GUI title
  Title: '&6Vote Point Shop'
  
  # Items for sale
  Items:
    DiamondSword:
      # Cost in vote points
      Cost: 50
      
      # Item configuration
      Item:
        Material: 'DIAMOND_SWORD'
        Name: '&bDiamond Sword'
        Lore:
        - '&7A sharp diamond sword'
      
      # Commands to run when purchased
      Commands:
      - 'give %player% diamond_sword 1'
      
      # Permission required to buy
      Permission: 'voteshop.sword'
      
      # Limit purchases per player
      Limit: 1
```

## Advanced Configuration

### Time Zone Settings

```yaml
# Server timezone configuration
TimeZone: 'America/New_York'
TimeHourOffset: 0

# Enable time change failsafe
TimeChangeFailSafeBypass: false
```

### Cache Settings

```yaml
Cache:
  # Cache player data (minutes)
  PlayerData: 30
  
  # Cache top voter data (minutes)
  TopVoter: 60
  
  # Cache vote site data (minutes)
  VoteSites: 15
```

### Database Optimization

```yaml
MySQL:
  # Connection pool settings
  MaxConnections: 10
  MinConnections: 2
  ConnectionTimeout: 30000
  IdleTimeout: 600000
  MaxLifetime: 1800000
  
  # Performance settings
  CachePrepStmts: true
  PrepStmtCacheSize: 250
  PrepStmtCacheSqlLimit: 2048
```

### Security Settings

```yaml
Security:
  # Enable vote validation
  ValidateVotes: true
  
  # Maximum votes per day per player
  MaxVotesPerDay: 10
  
  # Enable IP checking
  CheckIP: false
  
  # Blocked IPs
  BlockedIPs: []
```

## Configuration Examples

### Basic Server Setup

```yaml
# Minimal configuration for new servers
DataStorage: SQLITE
VoteReminding:
  Enabled: true
  RemindOnJoin: true
Broadcasting:
  Enabled: true
GUI:
  Enabled: true
```

### Large Network Setup

```yaml
# Configuration for large networks
DataStorage: MYSQL
MySQL:
  Host: 'mysql.server.com'
  Port: 3306
  Database: 'minecraft_votes'
  Username: 'vote_user'
  Password: 'secure_password'
  MaxConnections: 20
  Prefix: 'network_'

Broadcasting:
  BroadcastToAllServers: true
  
Cache:
  PlayerData: 60
  TopVoter: 120
```

### High-Performance Setup

```yaml
# Optimized for performance
MySQL:
  MaxConnections: 50
  CachePrepStmts: true
  PrepStmtCacheSize: 500

Cache:
  PlayerData: 120
  TopVoter: 240
  VoteSites: 60

GUI:
  CachePlayerSkulls: true
  
Security:
  ValidateVotes: false  # Disable for better performance
```

## Configuration Tips

### Performance

1. **Use MySQL** for large servers (100+ players)
2. **Increase cache times** for stable configurations
3. **Disable validation** if you trust your vote sources
4. **Use connection pooling** for database efficiency

### Reliability

1. **Regular backups** of database and config files
2. **Test configurations** on development servers first
3. **Monitor logs** for configuration errors
4. **Use validation** to catch configuration mistakes

### User Experience

1. **Clear reward descriptions** in GUIs
2. **Reasonable vote delays** (24 hours standard)
3. **Progressive rewards** to maintain engagement
4. **Informative messages** for player feedback

## Troubleshooting Configuration

### Common Issues

1. **YAML syntax errors**: Use online YAML validators
2. **Database connection issues**: Check credentials and network
3. **Rewards not working**: Verify command syntax and permissions
4. **GUI not displaying**: Check material names and GUI size

### Validation Commands

```bash
# Check configuration validity
/adminvoteconfig

# Test reward configurations
/adminvotetestreward

# Validate database connection
/adminvotestatus
```

For specific feature configuration, see:
- [Vote Parties](vote-parties.md)
- [Rewards Guide](rewards.md)
- [Proxy Setup](proxy-setup.md)