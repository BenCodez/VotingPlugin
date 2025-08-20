# Permissions Reference

VotingPlugin uses a hierarchical permission system that allows fine-grained control over player access to features.

## Main Permission Groups

### Player Permissions

| Permission | Description | Default |
|------------|-------------|---------|
| `VotingPlugin.Player` | Allows basic player commands | `true` |
| `VotingPlugin.Mod` | Allows moderator commands | `op` |
| `VotingPlugin.Admin` | Allows administrator commands | `op` |

### Core Player Permissions

| Permission | Description | Commands Affected |
|------------|-------------|------------------|
| `VotingPlugin.Player` | Basic voting access | `/vote`, `/votegui`, `/votetotal`, etc. |
| `VotingPlugin.Commands.Vote` | Access to `/vote` command | `/vote` |
| `VotingPlugin.Commands.VoteGUI` | Access to voting GUI | `/votegui` |
| `VotingPlugin.Commands.VoteHelp` | Access to help command | `/votehelp` |
| `VotingPlugin.Commands.VoteTotal` | View vote totals | `/votetotal` |
| `VotingPlugin.Commands.VoteLast` | View last vote time | `/votelast` |
| `VotingPlugin.Commands.VoteNext` | View next vote time | `/votenext` |
| `VotingPlugin.Commands.VoteTop` | View top voters | `/votetop` |
| `VotingPlugin.Commands.VoteToday` | View today's voters | `/votetoday` |

### Special Feature Permissions

| Permission | Description | Default |
|------------|-------------|---------|
| `VotingPlugin.TopVoter.Ignore` | Exempt from top voter rankings | `false` |
| `VotingPlugin.NoRemind` | Disable vote reminders | `false` |
| `VotingPlugin.BypassWaitUntilVoteDelay` | Bypass vote delay restrictions | `false` |
| `VotingPlugin.Login.RemindVotes` | Enable login vote reminders | `true` |
| `VotingPlugin.Login.RemindVotes.All` | Remind only when can vote on ALL sites | `false` |

## Administrator Permissions

### Core Admin Permissions

| Permission | Description |
|------------|-------------|
| `VotingPlugin.Admin` | Full administrator access |
| `VotingPlugin.Admin.Debug` | View debug information in-game |
| `VotingPlugin.Commands.AdminVote` | Access to admin vote commands |

### Specific Admin Command Permissions

| Permission | Commands | Description |
|------------|----------|-------------|
| `VotingPlugin.Commands.AdminVote.Reload` | `/adminvotereload` | Reload plugin configuration |
| `VotingPlugin.Commands.AdminVote.Vote` | `/adminvotevote` | Give fake votes to players |
| `VotingPlugin.Commands.AdminVote.Report` | `/adminvotereport` | Generate system reports |
| `VotingPlugin.Commands.AdminVote.Version` | `/adminvoteversion` | View plugin version |
| `VotingPlugin.Commands.AdminVote.Sites` | `/adminvotesites` | Manage vote sites |
| `VotingPlugin.Commands.AdminVote.Rewards` | `/adminvoterewards` | Manage rewards |
| `VotingPlugin.Commands.AdminVote.UUID` | `/adminvoteuuid` | Look up player UUIDs |
| `VotingPlugin.Commands.AdminVote.Config` | `/adminvoteconfig` | Access configuration editor |
| `VotingPlugin.Commands.AdminVote.GUI` | `/adminvotegui` | Access admin GUI |

### Data Management Permissions

| Permission | Commands | Description |
|------------|----------|-------------|
| `VotingPlugin.Commands.AdminVote.ClearTotal` | `/adminvotecleartotal` | Clear player vote totals |
| `VotingPlugin.Commands.AdminVote.ClearCache` | `/adminvoteclearcache` | Clear vote cache |
| `VotingPlugin.Commands.AdminVote.Purge` | `/adminvotepurge` | Purge old player data |
| `VotingPlugin.Commands.AdminVote.Convert` | `/adminvoteconvert*` | Data conversion commands |
| `VotingPlugin.Commands.AdminVote.BackgroundUpdate` | `/adminvotebackgroundupdate` | Force background updates |

### Vote Party Permissions

| Permission | Commands | Description |
|------------|----------|-------------|
| `VotingPlugin.Commands.AdminVote.VoteParty` | `/adminvotevoteparty` | Manage vote parties |
| `VotingPlugin.Commands.AdminVote.Edit.VoteParty` | GUI editors | Edit vote party settings |

### Service Site Permissions

| Permission | Commands | Description |
|------------|----------|-------------|
| `VotingPlugin.Admin.GenerateServiceSite` | `/adminvoteservicesites` | Generate service site configurations |
| `VotingPlugin.Commands.AdminVote.ServiceSites` | `/adminvoteservicesites` | Manage service sites |

### Testing & Debugging Permissions

| Permission | Commands | Description |
|------------|----------|-------------|
| `VotingPlugin.Commands.AdminVote.Test` | `/adminvotetest` | Run plugin tests |
| `VotingPlugin.Commands.AdminVote.TestReward` | `/adminvotetestreward` | Test reward systems |
| `VotingPlugin.Commands.AdminVote.JavaScript` | `/adminvotejavascript` | Test JavaScript rewards |
| `VotingPlugin.Commands.AdminVote.Placeholders` | `/adminvoteplaceholders` | View available placeholders |

## Sign Permissions

| Permission | Description |
|------------|-------------|
| `VotingPlugin.Sign.Create` | Create VotingPlugin signs |
| `VotingPlugin.Sign.Use` | Use VotingPlugin signs |

## GUI Access Permissions

| Permission | Description |
|------------|-------------|
| `VotingPlugin.GUI.Voter` | Access voter GUI interfaces |
| `VotingPlugin.GUI.Admin` | Access admin GUI interfaces |
| `VotingPlugin.GUI.TopVoter` | Access top voter GUI |
| `VotingPlugin.GUI.VoteShop` | Access vote shop GUI |

## Permission Examples

### Basic Player Setup
```yaml
# Give basic voting permissions
groups:
  default:
    permissions:
    - VotingPlugin.Player
    - VotingPlugin.Commands.Vote
    - VotingPlugin.Commands.VoteGUI
    - VotingPlugin.Commands.VoteTotal
```

### VIP Player Setup
```yaml
# VIP players with additional features
groups:
  vip:
    permissions:
    - VotingPlugin.Player
    - VotingPlugin.Commands.*  # All player commands
    - VotingPlugin.BypassWaitUntilVoteDelay  # Skip vote delays
```

### Moderator Setup
```yaml
# Moderators with basic admin commands
groups:
  moderator:
    permissions:
    - VotingPlugin.Mod
    - VotingPlugin.Commands.AdminVote.Report
    - VotingPlugin.Commands.AdminVote.Version
    - VotingPlugin.Commands.AdminVote.Sites
```

### Administrator Setup
```yaml
# Full administrator access
groups:
  admin:
    permissions:
    - VotingPlugin.Admin
    - VotingPlugin.Admin.Debug
    - VotingPlugin.Commands.AdminVote.*
```

### Special Cases
```yaml
# Player who doesn't want reminders
users:
  PlayerName:
    permissions:
    - VotingPlugin.NoRemind

# Staff member excluded from top voter
users:
  StaffMember:
    permissions:
    - VotingPlugin.TopVoter.Ignore
```

## Permission Inheritance

VotingPlugin follows a logical permission hierarchy:

1. **VotingPlugin.Admin** includes all mod and player permissions
2. **VotingPlugin.Mod** includes basic player permissions
3. **VotingPlugin.Player** includes all basic voting functionality

## Wildcard Permissions

You can use wildcards for easier permission management:

| Wildcard | Effect |
|----------|--------|
| `VotingPlugin.*` | All VotingPlugin permissions |
| `VotingPlugin.Commands.*` | All command permissions |
| `VotingPlugin.Commands.AdminVote.*` | All admin command permissions |
| `VotingPlugin.GUI.*` | All GUI access permissions |

## Default Permissions

By default, VotingPlugin assigns these permissions:

- `VotingPlugin.Player` - Given to all players (default: true)
- `VotingPlugin.Admin` - Given to operators only (default: op)
- `VotingPlugin.Mod` - Given to operators only (default: op)

## LuckPerms Examples

### Basic Setup
```
# Give basic permissions to default group
/lp group default permission set VotingPlugin.Player true

# Give admin permissions to admin group
/lp group admin permission set VotingPlugin.Admin true

# Remove reminder permissions from specific player
/lp user PlayerName permission set VotingPlugin.NoRemind true
```

### Advanced Setup
```
# Create voting-specific groups
/lp creategroup voters
/lp group voters permission set VotingPlugin.Player true
/lp group voters permission set VotingPlugin.Commands.Vote* true

# VIP voters with bypass permissions
/lp creategroup vipvoters
/lp group vipvoters inherit voters
/lp group vipvoters permission set VotingPlugin.BypassWaitUntilVoteDelay true
```

## Permission Troubleshooting

### Common Issues

1. **Commands not working**: Check that player has `VotingPlugin.Player` permission
2. **Admin commands failing**: Verify `VotingPlugin.Admin` permission
3. **GUI not opening**: Check specific GUI permissions
4. **Reminders not working**: Verify `VotingPlugin.Login.RemindVotes` permission

### Debug Commands
```
# Check player permissions
/adminvotepermsplayer <player>

# Check general permissions
/adminvoteperms
```

For more help with permissions, see the [Troubleshooting Guide](troubleshooting.md).