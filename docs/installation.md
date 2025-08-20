# Installation Guide

This guide will help you install VotingPlugin on your Minecraft server.

## Requirements

- **Minecraft Server**: Spigot, Paper, or CraftBukkit 1.13+
- **Java**: Version 8 or higher
- **Optional**: BungeeCord or Velocity for multi-server setups

## Single Server Installation

### Step 1: Download the Plugin

1. Download the latest version from [SpigotMC](https://www.spigotmc.org/resources/votingplugin.15358/)
2. Or download from [GitHub Releases](https://github.com/BenCodez/VotingPlugin/releases)

### Step 2: Install the Plugin

1. Place the `VotingPlugin.jar` file in your server's `plugins/` folder
2. Restart your server (not reload)
3. The plugin will generate configuration files automatically

### Step 3: Initial Configuration

After the first start, you'll find these files in `plugins/VotingPlugin/`:

- `Config.yml` - Main configuration file
- `VoteSites.yml` - Vote site definitions
- `SpecialRewards.yml` - Reward configurations
- `GUI.yml` - GUI customization
- And more...

## Proxy Server Installation

### BungeeCord Setup

1. Install VotingPlugin on your **BungeeCord proxy**:
   - Place `VotingPlugin.jar` in `plugins/` folder of your BungeeCord server
   - Configure `bungeeconfig.yml` for your setup

2. Install VotingPlugin on **all backend servers**:
   - Place the same `VotingPlugin.jar` in each Spigot server's `plugins/` folder
   - Configure each server's `Config.yml` to connect to BungeeCord

3. Configure vote listeners:
   - Point your vote sites to your BungeeCord proxy IP/port
   - Configure Votifier on the proxy server

### Velocity Setup

1. Install VotingPlugin on your **Velocity proxy**:
   - Place `VotingPlugin.jar` in `plugins/` folder of your Velocity server
   - Configure the velocity configuration file

2. Install VotingPlugin on **all backend servers**:
   - Place the same `VotingPlugin.jar` in each server's `plugins/` folder
   - Configure connection settings

## Database Configuration

### SQLite (Default)

No additional setup required. SQLite files are created automatically in the plugin folder.

### MySQL

1. Create a MySQL database for VotingPlugin
2. Edit `Config.yml`:

```yaml
DataStorage: MYSQL

MySQL:
  Host: 'localhost'
  Port: 3306
  Database: 'votingplugin'
  Username: 'your_username'
  Password: 'your_password'
  MaxConnections: 5
  Prefix: ''
```

## Optional Dependencies

These plugins enhance VotingPlugin functionality:

### Essential
- **[Votifier](https://www.spigotmc.org/resources/nuvotifier.13449/)**: Required for receiving votes
- **[PlaceholderAPI](https://www.spigotmc.org/resources/placeholderapi.6245/)**: For placeholder support

### Recommended
- **[Vault](https://www.spigotmc.org/resources/vault.34315/)**: For economy integration
- **[LuckPerms](https://luckperms.net/)**: For permission-based rewards

### Optional
- **[DiscordSRV](https://www.spigotmc.org/resources/discordsrv.18494/)**: Discord integration
- **[AuthMe](https://www.spigotmc.org/resources/authmereloaded.6269/)**: Login integration
- **[SuperVanish](https://www.spigotmc.org/resources/supervanish-be-invisible.1331/)**: Vanish support

## Verification

After installation, verify everything works:

1. Run `/adminvote version` - Should show plugin version
2. Run `/vote` - Should show vote sites (initially empty)
3. Check console for any error messages
4. Test voting with `/adminvote vote <player> <site> test`

## Next Steps

1. **[Configure Vote Sites](configuration.md#vote-sites)** - Set up your voting sites
2. **[Configure Rewards](rewards.md)** - Create rewards for voters
3. **[Set up Vote Parties](vote-parties.md)** - Enable community voting events
4. **[Configure Permissions](permissions.md)** - Set up player permissions

## Troubleshooting

If you encounter issues:

1. Check the [Troubleshooting Guide](troubleshooting.md)
2. Verify all requirements are met
3. Check server logs for error messages
4. Test with minimal configuration first

For additional help, visit the [SpigotMC discussion page](https://www.spigotmc.org/resources/votingplugin.15358/discuss).