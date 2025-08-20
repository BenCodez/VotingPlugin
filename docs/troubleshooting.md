# Troubleshooting Guide

This guide helps you diagnose and fix common issues with VotingPlugin.

## Common Issues

### Votes Not Being Received

#### Symptoms
- Players vote but no rewards are given
- Console shows no vote messages
- `/votelast` shows no recent votes

#### Causes & Solutions

**1. Votifier Not Configured**
```bash
# Check if Votifier is installed and running
/plugins

# Verify Votifier configuration
# Check votifier/config.yml for correct host/port
# Ensure vote site has correct IP and port
```

**2. Service Site Mismatch**
```bash
# Check console when someone votes
# Look for "Unknown vote from service: SiteName"

# Fix in VoteSites.yml:
ServiceSite: 'Exact-Site-Name-From-Console'
```

**3. Vote Site Configuration**
```yaml
# In VoteSites.yml - ensure site is enabled
VoteSites:
  YourSite:
    Enabled: true  # Must be true
    ServiceSite: 'Correct-Name-From-Console'
```

**4. Database Connection Issues**
```bash
# Check database connectivity
/adminvotestatus

# If using MySQL, verify credentials in Config.yml
MySQL:
  Host: 'correct-host'
  Port: 3306
  Database: 'correct-database'
  Username: 'correct-username'
  Password: 'correct-password'
```

### Rewards Not Working

#### Symptoms
- Votes are received but no rewards given
- Console shows command errors
- Players complain about missing rewards

#### Causes & Solutions

**1. Command Execution Errors**
```bash
# Check console for error messages
# Common errors:
# - "Unknown command"
# - "Insufficient permissions"
# - "Player not found"

# Test commands manually:
/give PlayerName diamond 1
/eco give PlayerName 1000
```

**2. Inventory Full**
```yaml
# Players need inventory space for items
# Check if using:
Items:
- 'DIAMOND:64'  # Requires 64 free slots!

# Solution: Use smaller amounts or commands instead
Commands:
- 'give %player% diamond 5'  # Drops if inventory full
```

**3. Permission Issues**
```bash
# Console might need permission for reward commands
# Check console errors for "You don't have permission"

# Grant console permissions or use different commands
```

**4. Invalid Reward Configuration**
```yaml
# Check YAML syntax
Commands:
- give %player% diamond 1    # Missing quotes
- 'give %player% diamond 1'  # Correct

# Verify item names
Items:
- 'DIMOND:5'     # Wrong spelling
- 'DIAMOND:5'    # Correct
```

### Configuration Errors

#### YAML Syntax Errors

**Common Mistakes:**
```yaml
# Wrong indentation
Commands:
- 'give %player% diamond 1'
- 'eco give %player% 1000'    # Should align with above

# Missing quotes
VoteURL: https://site.com     # Should be quoted
VoteURL: 'https://site.com'   # Correct

# Wrong boolean values
Enabled: yes    # Should be true/false
Enabled: true   # Correct
```

**Validation:**
```bash
# Use online YAML validators
# Check plugin load errors in console
# Use /adminvotereload to test changes
```

### Database Issues

#### SQLite Problems

**File Permissions:**
```bash
# Ensure server has write access to plugin folder
chmod 755 plugins/VotingPlugin/
chmod 644 plugins/VotingPlugin/*.db
```

**Corruption:**
```bash
# Backup database first
cp plugins/VotingPlugin/Users.db plugins/VotingPlugin/Users.db.backup

# Check for corruption (if you have sqlite3 installed)
sqlite3 plugins/VotingPlugin/Users.db "PRAGMA integrity_check;"
```

#### MySQL Problems

**Connection Issues:**
```bash
# Test connection outside of plugin
mysql -h hostname -P port -u username -p database

# Common issues:
# - Firewall blocking connection
# - Wrong credentials
# - Database doesn't exist
# - Max connections exceeded
```

**Configuration:**
```yaml
MySQL:
  Host: 'localhost'        # Use IP if localhost fails
  Port: 3306              # Verify correct port
  Database: 'votingplugin' # Must exist
  Username: 'vote_user'    # Must have permissions
  Password: 'password'     # Correct password
  MaxConnections: 5       # Don't set too high
```

### Performance Issues

#### High CPU Usage

**Causes:**
- Too frequent database queries
- Large number of vote sites
- Complex reward commands

**Solutions:**
```yaml
# Increase cache times in Config.yml
Cache:
  PlayerData: 60     # Cache player data longer
  TopVoter: 120     # Cache top voter data longer

# Reduce database connections
MySQL:
  MaxConnections: 3  # Lower for small servers
```

#### Memory Issues

**Signs:**
- Server lag during votes
- OutOfMemoryError in console
- Slow response times

**Solutions:**
```yaml
# Optimize database settings
MySQL:
  MaxConnections: 5      # Don't overdo connections
  ConnectionTimeout: 30000

# Disable unnecessary features
GUI:
  CachePlayerSkulls: false  # Uses less memory
  
TopVoter:
  CacheTime: 1800          # Shorter cache time
```

### Proxy Server Issues

#### BungeeCord Problems

**Votes Not Syncing:**
```yaml
# Ensure consistent configuration across servers
# Check bungeeconfig.yml on proxy:
BungeeMethod: 'PLUGINMESSAGING'  # or 'SOCKETS'
BungeePort: 1297                # Must match backends

# On backend servers in Config.yml:
BungeeSettings:
  BungeeMethod: 'PLUGINMESSAGING'
  BungeeServer:
    Host: 'proxy-ip'
    Port: 1297
```

**Connection Issues:**
```bash
# Check if plugin messaging channel is working
# Look for connection errors in console
# Verify network connectivity between proxy and backends
```

#### Velocity Problems

**Plugin Loading:**
```bash
# Ensure VotingPlugin is in Velocity plugins folder
# Check Velocity console for loading errors
# Verify Java version compatibility
```

### Vote Party Issues

#### Not Triggering

**Check Configuration:**
```yaml
# In SpecialRewards.yml
VoteParty:
  Enabled: true                # Must be enabled
  VotesRequired: 50           # Reasonable target
  OnlyOncePerDay: false       # Check restrictions
  ResetEachDay: false         # Check reset settings
```

**Debug Vote Count:**
```bash
# Check current vote party progress
/voteparty

# Check vote party settings
/adminvotevoteparty

# Manually trigger for testing
/adminvotevoteparty trigger
```

#### Rewards Not Distributed

**Player Requirements:**
```yaml
VoteParty:
  GiveAllPlayers: false           # Check if restricting to online
  GiveOnlinePlayersOnly: true     # Affects who gets rewards
  PlayerVoteParticipation: true   # May require player to have voted
```

### GUI Issues

#### Not Opening

**Permission Check:**
```bash
# Ensure player has permission
/adminvotepermsplayer PlayerName

# Required permissions:
# VotingPlugin.Commands.VoteGUI
# VotingPlugin.Player
```

**Configuration Check:**
```yaml
# In GUI.yml
GUI:
  Enabled: true    # Must be enabled
  
VoteGUI:
  Size: 54        # Must be valid size (9,18,27,36,45,54)
```

#### Items Not Displaying

**Material Names:**
```yaml
# Check for invalid material names
VoteSite:
  Material: 'PAPER'     # Must be valid 1.13+ material name
  # Not: 'WOOL:14' (old format)
  # Use: 'RED_WOOL' (new format)
```

### Command Issues

#### Commands Not Working

**Permission Issues:**
```bash
# Check if player has required permissions
/adminvotepermsplayer PlayerName

# Common missing permissions:
# VotingPlugin.Player - Basic commands
# VotingPlugin.Commands.Vote - /vote command
```

**Plugin Conflicts:**
```bash
# Check for command conflicts
/plugins

# Other plugins might override VotingPlugin commands
# Use specific aliases: /vgui instead of /vote
```

### PlaceholderAPI Issues

#### Placeholders Not Working

**Installation Check:**
```bash
# Ensure PlaceholderAPI is installed
/plugins

# Install VotingPlugin expansion
/papi ecloud download votingplugin
/papi reload
```

**Placeholder Format:**
```yaml
# Use correct placeholder format
%votingplugin_votes%          # Correct
%VotingPlugin_votes%          # Wrong case
%votingplugin-votes%          # Wrong separator
```

## Debug Tools

### Built-in Debug Commands

```bash
# General status
/adminvotestatus

# Plugin version and info
/adminvoteversion

# Player-specific debug
/adminvoteuser PlayerName

# Permission check
/adminvotepermsplayer PlayerName

# Test rewards
/adminvotetestreward PlayerName RewardName

# Test fake vote
/adminvotevote PlayerName SiteName fake
```

### Console Debug

**Enable Debug Mode:**
```yaml
# In Config.yml
Debug: true

# This shows detailed information in console:
# - Vote processing steps
# - Database queries
# - Reward execution
# - Error details
```

### Log Files

**Check Server Logs:**
```bash
# Look for VotingPlugin messages
tail -f logs/latest.log | grep VotingPlugin

# Common error patterns:
# "SQLException" - Database issues
# "Unknown command" - Command execution problems
# "YAML" - Configuration syntax errors
```

## Getting Help

### Before Asking for Help

1. **Check this troubleshooting guide**
2. **Review your configuration files**
3. **Check console for error messages**
4. **Test with minimal configuration**
5. **Try debug commands**

### Information to Provide

When seeking help, include:

```bash
# Plugin version
/adminvoteversion

# Server software and version
/version

# Error messages from console

# Relevant configuration sections

# Steps to reproduce the issue
```

### Where to Get Help

1. **[SpigotMC Discussion](https://www.spigotmc.org/resources/votingplugin.15358/discuss)**
2. **[GitHub Issues](https://github.com/BenCodez/VotingPlugin/issues)**
3. **[Discord Communities](https://discord.gg/spigot)** (general Minecraft development)

### Common Solutions Quick Reference

| Issue | Quick Fix |
|-------|-----------|
| Votes not received | Check Votifier config, ServiceSite names |
| Rewards not working | Check console errors, test commands manually |
| GUI not opening | Check permissions, GUI.yml configuration |
| Database errors | Verify credentials, check file permissions |
| Vote party not working | Check enabled status, vote requirements |
| Commands not working | Check permissions, test command syntax |
| Placeholders not working | Install PlaceholderAPI expansion |
| Performance issues | Increase cache times, reduce connections |

Remember: Most issues are configuration-related. Double-check your YAML syntax and ensure all required plugins are installed and configured correctly.