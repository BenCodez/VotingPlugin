# Vote Parties

Vote parties are community events where the entire server works together to reach a voting goal, and everyone gets rewarded when the goal is met.

## How Vote Parties Work

1. **Goal Setting**: Set a target number of votes (e.g., 50 votes)
2. **Community Voting**: All players vote to contribute to the goal
3. **Goal Achievement**: When the target is reached, a vote party triggers
4. **Rewards**: All online players (or all players) receive rewards
5. **Reset**: The counter resets for the next vote party

## Basic Configuration

Vote parties are configured in `SpecialRewards.yml`:

```yaml
VoteParty:
  # Enable vote parties
  Enabled: true
  
  # Number of votes required to trigger a vote party
  VotesRequired: 50
  
  # Reset vote party count after each party
  ResetEachDay: false
  
  # Only allow one vote party per day
  OnlyOncePerDay: false
  
  # Only allow one vote party per week
  OnlyOncePerWeek: false
  
  # Broadcast message when vote party starts
  Broadcast: '&6Vote Party! Everyone gets rewards!'
```

## Advanced Settings

### Incremental Vote Requirements

```yaml
VoteParty:
  # Increase votes required after each party
  IncreaseVotesRequired: 10
  
  # Example: First party needs 50, second needs 60, third needs 70, etc.
```

### Player Participation Requirements

```yaml
VoteParty:
  # Give rewards to all players (online and offline)
  GiveAllPlayers: false
  
  # Only give rewards to online players
  GiveOnlinePlayersOnly: true
  
  # Require players to have voted to get rewards
  PlayerVoteParticipation: true
  
  # Minimum votes required for player to participate
  PlayerVotes: 1
```

### Time-Based Restrictions

```yaml
VoteParty:
  # Reset vote count daily
  ResetEachDay: true
  
  # Reset vote count weekly  
  ResetEachWeek: false
  
  # Reset vote count monthly
  ResetEachMonth: false
  
  # Only one party per day
  OnlyOncePerDay: true
  
  # Only one party per week
  OnlyOncePerWeek: false
```

## Vote Party Rewards

Configure rewards given during vote parties:

```yaml
VoteParty:
  # Rewards given to all participants
  Rewards:
    # Give vote points
    VotePoints: 10
    
    # Run commands for each player
    Commands:
    - 'give %player% diamond 5'
    - 'eco give %player% 1000'
    - 'broadcast &6%player% received vote party rewards!'
    
    # Give items directly
    Items:
    - 'DIAMOND:5'
    - 'EMERALD:3'
    
    # Send messages
    Messages:
      Player: '&aYou received vote party rewards!'
      Broadcast: '&6Vote party completed! Everyone got rewards!'
```

## Vote Reminders

Set up reminders when the vote party is close to completion:

```yaml
VoteParty:
  # Votes remaining to trigger reminders
  VoteReminderAtVotes: [5, 10, 15]
  
  # Reminder message
  VoteReminderBroadcast: '&6Only %votesrequired% more votes needed for a vote party!'
  
  # Player who just voted triggers reminder
  VoteReminderPlayerVote: true
```

## GUI Integration

Vote parties integrate with the voting GUI:

```yaml
GUI:
  VoteParty:
    # Show vote party progress in GUI
    Enabled: true
    
    # GUI item for vote party status
    Item: 'CAKE'
    
    # Display name
    Name: '&6Vote Party Progress'
    
    # Lore showing progress
    Lore:
    - '&7Votes: &a%voteparty_votes%&7/&a%voteparty_votes_required%'
    - '&7Progress: &a%voteparty_percentage%%'
    - '&7Time until reset: &a%voteparty_time_until_reset%'
```

## Commands

### Player Commands
- `/voteparty` - View vote party status and progress
- `/vparty` - Alias for `/voteparty`

### Admin Commands
- `/adminvotevoteparty` - Open vote party management GUI
- `/avvoteparty` - Alias for admin vote party command

## Placeholders

VotingPlugin provides placeholders for vote parties:

| Placeholder | Description |
|-------------|-------------|
| `%voteparty_votes%` | Current vote count |
| `%voteparty_votes_required%` | Votes needed for party |
| `%voteparty_votes_remaining%` | Votes remaining |
| `%voteparty_percentage%` | Progress percentage |
| `%voteparty_time_until_reset%` | Time until daily/weekly reset |

## Example Configurations

### Simple Daily Vote Party

```yaml
VoteParty:
  Enabled: true
  VotesRequired: 30
  ResetEachDay: true
  OnlyOncePerDay: true
  GiveOnlinePlayersOnly: true
  
  Rewards:
    VotePoints: 5
    Commands:
    - 'give %player% diamond 2'
    - 'eco give %player% 500'
    
    Messages:
      Player: '&aYou received daily vote party rewards!'
      Broadcast: '&6Daily vote party completed!'
```

### Progressive Vote Party

```yaml
VoteParty:
  Enabled: true
  VotesRequired: 25
  IncreaseVotesRequired: 5  # 25, 30, 35, 40, etc.
  ResetEachWeek: true
  GiveAllPlayers: true
  
  # More rewards for higher tiers
  Rewards:
    VotePoints: 
      Min: 3
      Max: 15  # Scales with party number
    
    Commands:
    - 'crate give %player% vote %voteparty_number%'
```

### Exclusive Vote Party

```yaml
VoteParty:
  Enabled: true
  VotesRequired: 100
  PlayerVoteParticipation: true
  PlayerVotes: 3  # Must vote at least 3 times
  GiveOnlinePlayersOnly: true
  
  # Special rewards for active voters
  Rewards:
    Commands:
    - 'lp user %player% permission set special.voter true 7d'
    - 'give %player% nether_star 1'
```

## Multi-Server Vote Parties

For BungeeCord/Velocity networks:

```yaml
# In bungeeconfig.yml or velocity config
VoteParty:
  # Send vote party to all servers
  SendToAllServers: true
  
  # Or specify specific servers
  ServersToSend:
  - 'survival'
  - 'creative'
  - 'skyblock'
  
  # Bungee commands (run on proxy)
  BungeeCommands:
  - 'alert &6Vote party started on all servers!'
  
  # Broadcast message
  Broadcast: '&6Network-wide vote party!'
```

## Troubleshooting

### Vote Party Not Triggering

1. **Check votes count**: Use `/voteparty` to see current progress
2. **Verify configuration**: Ensure `Enabled: true` in SpecialRewards.yml
3. **Check restrictions**: Daily/weekly limits might prevent triggering
4. **Test manually**: Use `/adminvotevoteparty` to test

### Rewards Not Given

1. **Player participation**: Check if `PlayerVoteParticipation` requirements are met
2. **Online requirement**: Verify `GiveOnlinePlayersOnly` setting
3. **Permission issues**: Ensure players can receive rewards
4. **Command errors**: Check console for command execution errors

### Vote Count Issues

1. **Reset timing**: Check if daily/weekly reset is working correctly
2. **Database sync**: Ensure vote counts are properly saved
3. **Multi-server**: Verify BungeeCord/Velocity sync is working

## Best Practices

### Balancing

- **Reasonable goals**: Don't set vote requirements too high
- **Fair rewards**: Balance rewards with effort required
- **Time limits**: Use daily/weekly resets to keep engagement

### Engagement

- **Clear progress**: Show vote party progress in GUIs and broadcasts
- **Reminders**: Use vote reminders to encourage participation
- **Community feel**: Use broadcasts to create excitement

### Technical

- **Test thoroughly**: Test vote parties before going live
- **Monitor performance**: Watch for lag during reward distribution
- **Backup data**: Regular backups prevent vote count loss

For more advanced setups, see the [Proxy Setup Guide](proxy-setup.md) and [Configuration Reference](configuration.md).