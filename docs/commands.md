# Commands Reference

VotingPlugin provides extensive command functionality for both players and administrators.

## Player Commands

### Basic Voting Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/vote` | | Display available vote sites | `VotingPlugin.Player` |
| `/votegui` | `/vgui` | Open voting GUI | `VotingPlugin.Player` |
| `/voteurl` | `/vurl` | Get vote URLs | `VotingPlugin.Player` |
| `/votehelp` | `/vhelp` | Show command help | `VotingPlugin.Player` |

### Vote Information Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/votetotal` | `/vtotal` | View your total votes | `VotingPlugin.Player` |
| `/votelast` | `/vlast` | See when you last voted | `VotingPlugin.Player` |
| `/votenext` | `/vnext` | See when you can vote next | `VotingPlugin.Player` |
| `/votetoday` | `/vtoday` | See who voted today | `VotingPlugin.Player` |
| `/votetop` | `/vtop` | View top voters | `VotingPlugin.Player` |
| `/votelist` | `/vlist` | List all vote sites | `VotingPlugin.Player` |
| `/votestreak` | `/vstreak` | View your voting streak | `VotingPlugin.Player` |

### Vote Features Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/voteparty` | `/vparty` | View vote party status | `VotingPlugin.Player` |
| `/voteshop` | `/vshop` | Open vote point shop | `VotingPlugin.Player` |
| `/votepoints` | `/vpoints` | View your vote points | `VotingPlugin.Player` |
| `/votebest` | `/vbest` | View best voting statistics | `VotingPlugin.Player` |
| `/voteall` | `/vall` | Vote on all sites | `VotingPlugin.Player` |
| `/votechoices` | `/vchoices` | View voting choices | `VotingPlugin.Player` |

### Toggle Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/votetogglebroadcast` | `/vtogglebroadcast` | Toggle vote broadcasts | `VotingPlugin.Player` |
| `/votetogglereminders` | `/vtogglereminder`, `/vtoggle` | Toggle vote reminders | `VotingPlugin.Player` |

## Administrator Commands

### Basic Admin Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvote` | `/av` | Main admin command | `VotingPlugin.Admin` |
| `/adminvotehelp` | `/avhelp`, `/av?` | Admin command help | `VotingPlugin.Admin` |
| `/adminvotereload` | `/avreload` | Reload plugin configuration | `VotingPlugin.Admin` |
| `/adminvoteversion` | `/avversion` | Show plugin version | `VotingPlugin.Admin` |

### Player Management Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotevote` | `/avvote`, `/fakevote` | Give fake vote to player | `VotingPlugin.Admin` |
| `/adminvotegivereward` | `/avgivereward` | Give specific reward | `VotingPlugin.Admin` |
| `/adminvoteuuid` | `/avuuid` | Look up player UUID | `VotingPlugin.Admin` |
| `/adminvoteplayername` | `/avplayername` | Look up player name | `VotingPlugin.Admin` |
| `/adminvoteuser` | `/avuser` | View user data | `VotingPlugin.Admin` |

### Configuration Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvoteconfig` | `/avconfig` | Open config editor | `VotingPlugin.Admin` |
| `/adminvotegui` | `/avgui` | Open admin GUI | `VotingPlugin.Admin` |
| `/adminvotesites` | `/avsites` | Manage vote sites | `VotingPlugin.Admin` |
| `/adminvoterewards` | `/avrewards` | Manage rewards | `VotingPlugin.Admin` |
| `/adminvotevotesite` | `/avvotesite` | Edit specific vote site | `VotingPlugin.Admin` |

### System Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotereport` | `/avreport` | Generate system report | `VotingPlugin.Admin` |
| `/adminvotestatus` | `/avstatus` | View plugin status | `VotingPlugin.Admin` |
| `/adminvotetest` | `/avtest` | Run plugin tests | `VotingPlugin.Admin` |
| `/adminvoteupdatecheck` | `/avupdatecheck` | Check for updates | `VotingPlugin.Admin` |

### Data Management Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotecleartotal` | | Clear player vote totals | `VotingPlugin.Admin` |
| `/adminvoteclearcache` | `/avclearcache` | Clear vote cache | `VotingPlugin.Admin` |
| `/adminvotepurge` | `/avpurge` | Purge old data | `VotingPlugin.Admin` |
| `/adminvoteuserremove` | `/avuserremove` | Remove user data | `VotingPlugin.Admin` |
| `/adminvoteuseruuidremove` | `/avuseruuidremove` | Remove user by UUID | `VotingPlugin.Admin` |

### Vote Party Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotevoteparty` | `/avvoteparty` | Manage vote parties | `VotingPlugin.Admin` |

### Advanced Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotebackgroundupdate` | `/avbackgroundupdate` | Force background update | `VotingPlugin.Admin` |
| `/adminvoteconvertfromdata` | `/avconvertfromdata` | Convert from old data | `VotingPlugin.Admin` |
| `/adminvoteconverttodata` | `/avconverttodata` | Convert to new data format | `VotingPlugin.Admin` |
| `/adminvoteresyncmilestones` | `/avresyncmilestones` | Resync milestone rewards | `VotingPlugin.Admin` |
| `/adminvoteresyncmilestonesalreadygiven` | `/avresyncmilestonesalreadygiven` | Resync given milestones | `VotingPlugin.Admin` |
| `/adminvoteresetmilestonecount` | `/avresetmilestonecount` | Reset milestone counts | `VotingPlugin.Admin` |

### Utility Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvoteperms` | `/avperms` | Check permissions | `VotingPlugin.Admin` |
| `/adminvotepermsplayer` | `/avpermsplayer` | Check player permissions | `VotingPlugin.Admin` |
| `/adminvoteplaceholders` | `/avplaceholders` | View available placeholders | `VotingPlugin.Admin` |
| `/adminvotejavascript` | `/avjavascript` | Test JavaScript rewards | `VotingPlugin.Admin` |
| `/adminvoteruncmd` | `/avruncmd` | Run command as console | `VotingPlugin.Admin` |

### Service Site Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvoteservicesites` | `/avservicesites` | Manage service sites | `VotingPlugin.Admin.GenerateServiceSite` |
| `/adminvotesetrequestmethod` | `/avsetrequestmethod` | Set request method for sites | `VotingPlugin.Admin` |

### Maintenance Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotereloadall` | `/avreloadall` | Reload all configurations | `VotingPlugin.Admin` |
| `/adminvoteclearofflinevotes` | `/avclearofflinevotes` | Clear offline votes | `VotingPlugin.Admin` |
| `/adminvoteclearofflinerewards` | `/avclearofflinerewards` | Clear offline rewards | `VotingPlugin.Admin` |
| `/adminvoteforcetimechanged` | `/avforcetimechanged` | Force time change detection | `VotingPlugin.Admin` |
| `/adminvotecurrentplugintime` | `/avcurrentplugintime` | View current plugin time | `VotingPlugin.Admin` |

### Reward Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotetestreward` | `/avtestreward` | Test reward giving | `VotingPlugin.Admin` |
| `/adminvotegiveall` | `/avgiveall` | Give rewards to all players | `VotingPlugin.Admin` |
| `/adminvotegiveallonline` | `/avgiveallonline` | Give rewards to online players | `VotingPlugin.Admin` |
| `/adminvoteresetpoints` | `/avresetpoints` | Reset player vote points | `VotingPlugin.Admin` |

### Special Commands

| Command | Aliases | Description | Permission |
|---------|---------|-------------|------------|
| `/adminvotechoices` | `/avchoices` | Manage voting choices | `VotingPlugin.Admin` |
| `/adminvoteedit` | `/avedit` | Edit plugin data | `VotingPlugin.Admin` |
| `/adminvotedownloadjenkins` | `/avdownloadjenkins` | Download from Jenkins | `VotingPlugin.Admin` |

## Command Examples

### Player Examples
```
/vote                    # Show available vote sites
/votetotal              # Show your total votes
/votenext               # See when you can vote next
/voteparty              # Check vote party progress
```

### Admin Examples
```
/adminvote vote Steve MinecraftServers fake    # Give fake vote to Steve
/adminvote reload                              # Reload configuration
/adminvote user Steve                          # View Steve's voting data
/adminvote givereward Steve FirstVote          # Give FirstVote reward to Steve
```

## Notes

- Most commands support tab completion
- Admin commands require appropriate permissions
- Some commands may be disabled based on configuration
- Use `/adminvotehelp` for detailed command usage
- Command aliases allow for shorter typing

For permission details, see the [Permissions Reference](permissions.md).