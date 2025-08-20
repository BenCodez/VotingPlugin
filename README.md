# VotingPlugin

[![SpigotMC Downloads](https://img.shields.io/badge/SpigotMC-15.3k%20downloads-orange)](https://www.spigotmc.org/resources/votingplugin.15358/)
[![Version](https://img.shields.io/github/v/release/BenCodez/VotingPlugin)](https://github.com/BenCodez/VotingPlugin/releases)
[![License](https://img.shields.io/github/license/BenCodez/VotingPlugin)](LICENSE.md)

A highly customizable Minecraft voting plugin that allows players to vote for your server and receive rewards. Supports multiple server platforms including Spigot, Paper, Bungee, and Velocity.

## 🚀 Features

- **Multi-Platform Support**: Works with Spigot, Paper, Bungee, and Velocity
- **Vote Parties**: Community-based voting events with group rewards
- **Flexible Rewards**: Commands, items, permissions, and more
- **Top Voter System**: Track and reward your most dedicated voters
- **Extensive Configuration**: 870+ configuration options for complete customization
- **Database Support**: SQLite and MySQL support with connection pooling
- **Reminder System**: Automated vote reminders for players
- **GUI Interfaces**: User-friendly GUIs for players and administrators
- **PlaceholderAPI Support**: Extensive placeholder support for other plugins
- **Developer API**: Rich API for developers to integrate with

## 📖 Documentation

| Topic | Description |
|-------|-------------|
| [🔧 Installation](docs/installation.md) | Server setup and installation guide |
| [⚙️ Configuration](docs/configuration.md) | Complete configuration reference |
| [💬 Commands](docs/commands.md) | All commands and their usage |
| [🔑 Permissions](docs/permissions.md) | Permission nodes reference |
| [🎉 Vote Parties](docs/vote-parties.md) | Setting up and configuring vote parties |
| [🏆 Rewards](docs/rewards.md) | Creating and managing vote rewards |
| [📊 Top Voters](docs/top-voters.md) | Top voter system configuration |
| [🌐 Proxy Setup](docs/proxy-setup.md) | BungeeCord and Velocity configuration |
| [🔗 API](docs/api.md) | Developer API documentation |
| [❓ Troubleshooting](docs/troubleshooting.md) | Common issues and solutions |

## ⚡ Quick Start

1. **Download** the latest version from [SpigotMC](https://www.spigotmc.org/resources/votingplugin.15358/)
2. **Install** the plugin in your `plugins/` folder
3. **Restart** your server
4. **Configure** vote sites in `plugins/VotingPlugin/VoteSites.yml`
5. **Set up** rewards in `plugins/VotingPlugin/SpecialRewards.yml`

## 📦 Maven Dependency

Add VotingPlugin as a dependency to your project:

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

**Available Versions:**
- `LATEST` - Latest stable release
- `6.18.8-SNAPSHOT` - Development snapshot

## 🤝 Support

- **SpigotMC**: [Plugin Page](https://www.spigotmc.org/resources/votingplugin.15358/)
- **Issues**: [GitHub Issues](https://github.com/BenCodez/VotingPlugin/issues)
- **Wiki**: [Documentation](https://github.com/BenCodez/VotingPlugin/wiki)

## 📋 Requirements

- **Minecraft**: 1.13+ (Folia supported)
- **Java**: 8+
- **Server**: Spigot, Paper, BungeeCord, Velocity

## 🔧 Build Instructions

```bash
git clone https://github.com/BenCodez/VotingPlugin.git
cd VotingPlugin/VotingPlugin
mvn clean package
```

## 📄 License

This project is licensed under a custom license. See [LICENSE.md](LICENSE.md) for details.

---

**Created by [BenCodez](https://github.com/BenCodez)** | **[Donate](https://github.com/sponsors/BenCodez)**
