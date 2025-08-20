# Proxy Setup Guide

This guide covers setting up VotingPlugin on BungeeCord and Velocity proxy networks for multi-server vote synchronization.

## Overview

VotingPlugin supports multi-server setups where:

1. **Proxy server** receives votes from voting sites
2. **Backend servers** receive vote data from proxy
3. **Vote data syncs** across the entire network
4. **Players receive rewards** on any connected server

## BungeeCord Setup

### Prerequisites

- BungeeCord proxy server
- Multiple Spigot/Paper backend servers
- VotingPlugin.jar file
- Votifier installed on proxy

### Step 1: Proxy Configuration

**Install on BungeeCord:**
1. Place `VotingPlugin.jar` in BungeeCord's `plugins/` folder
2. Restart BungeeCord
3. Configure `bungeeconfig.yml`:

```yaml
# Basic proxy settings
BungeeMethod: 'PLUGINMESSAGING'  # or 'SOCKETS'
BungeeManageTotals: true
Broadcast: true

# Vote caching
VoteCacheTime: 3600  # 1 hour cache

# Database settings (use MySQL for multi-server)
DataStorage: MYSQL
MySQL:
  Host: 'mysql.yourserver.com'
  Port: 3306
  Database: 'votingplugin'
  Username: 'vote_user'
  Password: 'secure_password'
  MaxConnections: 10

# Vote site configuration
VoteSites:
  MinecraftServers:
    Enabled: true
    Name: 'Minecraft-Servers.net'
    ServiceSite: 'Minecraft-Servers.net'
    VoteURL: 'https://minecraft-servers.net/server/12345/'
    VoteDelay: 24
```

### Step 2: Backend Server Configuration

**On each Spigot/Paper server:**

1. Place `VotingPlugin.jar` in `plugins/` folder
2. Configure `Config.yml`:

```yaml
# Enable BungeeCord mode
BungeeSettings:
  # Use same method as proxy
  BungeeMethod: 'PLUGINMESSAGING'
  
  # Proxy server details
  BungeeServer:
    Host: 'proxy.yourserver.com'  # Your BungeeCord IP
    Port: 1297                    # Plugin messaging port
  
  # Let proxy manage vote totals
  BungeeManageTotals: true

# Use same database as proxy
DataStorage: MYSQL
MySQL:
  Host: 'mysql.yourserver.com'
  Port: 3306
  Database: 'votingplugin'
  Username: 'vote_user'
  Password: 'secure_password'
  MaxConnections: 5
```

### Step 3: Vote Site Setup

**Configure Votifier on BungeeCord:**

```yaml
# In votifier/config.yml on proxy
host: '0.0.0.0'
port: 8192
debug: false
```

**Point vote sites to proxy:**
- Use your BungeeCord server IP
- Use Votifier port (usually 8192)
- Use the public key from BungeeCord's Votifier

### Communication Methods

#### Plugin Messaging (Recommended)

```yaml
# On BungeeCord - bungeeconfig.yml
BungeeMethod: 'PLUGINMESSAGING'
PluginMessageChannel: 'votingplugin:vote'

# On backend servers - Config.yml
BungeeSettings:
  BungeeMethod: 'PLUGINMESSAGING'
  PluginMessageChannel: 'votingplugin:vote'
```

#### Socket Communication

```yaml
# On BungeeCord - bungeeconfig.yml
BungeeMethod: 'SOCKETS'
BungeeServer:
  Host: '0.0.0.0'
  Port: 1297

# On backend servers - Config.yml
BungeeSettings:
  BungeeMethod: 'SOCKETS'
  BungeeServer:
    Host: 'proxy-ip'
    Port: 1297
```

## Velocity Setup

### Prerequisites

- Velocity proxy server
- Multiple Spigot/Paper backend servers
- VotingPlugin.jar file
- NuVotifier installed on proxy

### Step 1: Velocity Proxy Configuration

**Install on Velocity:**
1. Place `VotingPlugin.jar` in Velocity's `plugins/` folder
2. Restart Velocity
3. Configure the velocity configuration file:

```yaml
# Communication method
MultiProxyMethod: 'PLUGINMESSAGING'

# Global data settings
GlobalData:
  Enabled: true
  UseMainMySQL: true

# Database configuration
DataStorage: MYSQL
MySQL:
  Host: 'mysql.yourserver.com'
  Port: 3306
  Database: 'votingplugin'
  Username: 'vote_user'
  Password: 'secure_password'
  MaxConnections: 15

# Vote sites
VoteSites:
  MinecraftServers:
    Enabled: true
    Name: 'Minecraft-Servers.net'
    ServiceSite: 'Minecraft-Servers.net'
    VoteURL: 'https://minecraft-servers.net/server/12345/'
    VoteDelay: 24
```

### Step 2: Backend Configuration for Velocity

**On each backend server:**

```yaml
# In Config.yml
BungeeSettings:
  BungeeMethod: 'PLUGINMESSAGING'
  BungeeManageTotals: true
  
  # Velocity proxy details
  BungeeServer:
    Host: 'velocity-proxy-ip'
    Port: 25577  # Velocity's default port

# Same database as proxy
DataStorage: MYSQL
MySQL:
  Host: 'mysql.yourserver.com'
  Port: 3306
  Database: 'votingplugin'
  Username: 'vote_user'
  Password: 'secure_password'
  MaxConnections: 5
```

## Advanced Multi-Proxy Setup

### Redis Communication

For large networks with multiple proxies:

```yaml
# Enable Redis communication
MultiProxyMethod: 'REDIS'

# Redis settings
MultiProxyRedis:
  Host: 'redis.yourserver.com'
  Port: 6379
  Password: 'redis_password'
  UseExistingConnection: false
```

### MQTT Communication

Alternative for complex networks:

```yaml
# MQTT broker settings
MQTT:
  BrokerURL: 'tcp://mqtt.yourserver.com:1883'
  ClientID: 'VotingPlugin_Proxy1'
  Username: 'mqtt_user'
  Password: 'mqtt_password'
  Prefix: 'votingplugin'
```

## Vote Party Synchronization

### Cross-Server Vote Parties

```yaml
# On proxy server
VoteParty:
  # Send to all connected servers
  SendToAllServers: true
  
  # Or specific servers only
  ServersToSend:
  - 'survival'
  - 'creative'
  - 'skyblock'
  
  # Commands run on proxy
  BungeeCommands:
  - 'alert &6Network-wide vote party started!'
  
  # Broadcast message
  Broadcast: '&6Vote party! Check your server for rewards!'
```

### Server-Specific Vote Parties

```yaml
# Different vote parties per server
VoteParty:
  PerServer: true
  
  # Server-specific requirements
  ServerSettings:
    survival:
      VotesRequired: 50
      Enabled: true
    
    creative:
      VotesRequired: 30
      Enabled: true
    
    skyblock:
      VotesRequired: 25
      Enabled: false
```

## Database Considerations

### MySQL Setup for Multi-Server

**Create dedicated database:**
```sql
CREATE DATABASE votingplugin;
CREATE USER 'vote_user'@'%' IDENTIFIED BY 'secure_password';
GRANT ALL PRIVILEGES ON votingplugin.* TO 'vote_user'@'%';
FLUSH PRIVILEGES;
```

**Optimize for multiple connections:**
```yaml
MySQL:
  MaxConnections: 20        # Higher for proxy
  ConnectionTimeout: 30000
  IdleTimeout: 600000
  MaxLifetime: 1800000
  
  # Connection pooling
  CachePrepStmts: true
  PrepStmtCacheSize: 250
  PrepStmtCacheSqlLimit: 2048
```

### Table Prefixes

Use prefixes for multiple networks:

```yaml
MySQL:
  Prefix: 'network1_'  # Different prefix per network
```

## Troubleshooting

### Votes Not Syncing

**Check proxy configuration:**
```bash
# On proxy
/votingplugin status

# Verify vote reception
/votingplugin testvote PlayerName SiteName fake
```

**Check backend connection:**
```bash
# On backend server
/adminvotestatus

# Test communication
/adminvotereload
```

### Database Connection Issues

**Test connection:**
```bash
# Check if database is accessible
mysql -h hostname -P port -u username -p database

# Verify table structure
SHOW TABLES;
DESCRIBE votingplugin_users;
```

**Common fixes:**
```yaml
# Increase connection timeout
MySQL:
  ConnectionTimeout: 60000
  
# Reduce max connections if getting limits
MySQL:
  MaxConnections: 5
```

### Plugin Messaging Issues

**Verify channel registration:**
```yaml
# Ensure same channel on all servers
PluginMessageChannel: 'votingplugin:vote'

# Check for channel conflicts
# Other plugins might use same channel
```

### Performance Issues

**Optimize cache settings:**
```yaml
# On proxy (handle more load)
Cache:
  PlayerData: 120
  TopVoter: 240
  
# On backend (less caching needed)
Cache:
  PlayerData: 60
  TopVoter: 120
```

## Best Practices

### Security

1. **Firewall rules**: Restrict database access
2. **Strong passwords**: Use complex MySQL passwords
3. **SSL connections**: Enable MySQL SSL if possible
4. **Regular backups**: Backup database regularly

### Performance

1. **Dedicated database server**: Separate MySQL server
2. **Connection pooling**: Use appropriate connection limits
3. **Caching**: Optimize cache times for your network size
4. **Monitoring**: Monitor database performance

### Reliability

1. **Test configuration**: Test on staging environment first
2. **Gradual deployment**: Deploy to one server at a time
3. **Monitor logs**: Watch for errors during deployment
4. **Backup before changes**: Always backup before updates

### Network Design

1. **Consistent configuration**: Keep configs synchronized
2. **Clear naming**: Use descriptive server names
3. **Documentation**: Document your network setup
4. **Update procedures**: Plan for plugin updates

## Example Configurations

### Small Network (2-3 servers)

```yaml
# Simple setup with plugin messaging
BungeeMethod: 'PLUGINMESSAGING'
DataStorage: MYSQL
MySQL:
  MaxConnections: 5
Cache:
  PlayerData: 30
```

### Medium Network (5-10 servers)

```yaml
# More robust setup
BungeeMethod: 'PLUGINMESSAGING'
DataStorage: MYSQL
MySQL:
  MaxConnections: 15
  ConnectionTimeout: 30000
Cache:
  PlayerData: 60
  TopVoter: 120
```

### Large Network (10+ servers)

```yaml
# High-performance setup
MultiProxyMethod: 'REDIS'
DataStorage: MYSQL
MySQL:
  MaxConnections: 50
  CachePrepStmts: true
Cache:
  PlayerData: 120
  TopVoter: 240
Redis:
  Host: 'redis-cluster'
  Port: 6379
```

For more advanced configurations, see:
- [Configuration Reference](configuration.md)
- [Vote Parties Guide](vote-parties.md)
- [Troubleshooting Guide](troubleshooting.md)