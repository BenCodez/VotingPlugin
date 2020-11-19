---
name: Bug report
about: Bug report for VotingPlugin
title: 'detected yml error please see console for details voting plugin'
labels: Possible Bug
assignees: ''

---

**Versions**
Plugin version and spigot version. 1.16.4, 5.24.1

**Describe the bug**
Alright so I did everything then when i started up the game it said detected yml error please see console for details voting plugin so i went to console and saw [14:47:46 ERROR]: [VotingPlugin] Failed to load VoteSites.yml. so then i thought i did something wrong but i couldnt undertsand what it is. Ill put the votesites.yml here and tell me if you see the problem. Thank you, HBHC1262 

**To Reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
I thought it was going to work

**Screenshots/Configs**
You dont need a screenshot the votingsite.yml is in additional context, Thank you :)

**Additional context** THIS IS THE VOTESITE.YML VoteSites:
  # VoteSite key
  # Used for saving data for votesites
  # Use a short name if you can
  topg:
    # Enable voteSite
    # If false, votesite will not be loaded by plugin
    # Default: false
    Enabled: true
    
    # Display name of voting site
    Name: 'topg.org'
          
    # Priority of this site
    # Used for sorted list
    # High prority, higher on the list
    Priority: 5
          
    # The serviceName from the vote, has to be correct to work correctly
    # Gotten from voting on the site (will be in console/log)
    # Usually is the main part of the url(e.g. PlanetMinecraft.com), but not always
    # See console when you vote (it will notify you if the plugin didn't find a votingsite matching the service site
    ServiceSite: 'https://topg.org/Minecraft/'

    # vote url for /vote
    # Format for this can be set in Format.yml
    # Do not include colors here
    # If you need to force a url set the url as this:
    # VoteURL: '[Text="ANY TEXT TO CLICK ON HERE",url="URLHERE"]'
    VoteURL: 'https://topg.org/Minecraft/in-618653'

    # Time between votes in hours (used for /vote next)
    # Most sites are 24 hours
    VoteDelay: 24
    
    # Use to have a minute votedelay
    #VoteDelayMin: 30
    
    # Require waiting until votedelay time has pasted in order to accept vote
    # Recommend: false
    WaitUntilVoteDelay: false
    
    # Reset vote delay each day (for certain sites that do this)
    # Recommend: false
    VoteDelayDaily: false
    
    # If true, rewards can be executed offline (requires ForceOffline to be true in the reward)
    # Setting this to true will execute the reward offline while the player is offline
    # This basiclly disables offline voting if set to true, don't misinterpret this option
    ForceOffline: false
    
    # VoteSite Material for some GUI's
    # Removing this may result in an error, required on each site
    DisplayItem:
      Material: 'DIAMOND'
      Amount: 1

    # Rewards to give
    # https://github.com/Ben12345rocks/AdvancedCore/wiki/Rewards
    Rewards:
      # Execute console commands
      Commands:
       Console:
       'eco give %player% 10'
      # Send player a message
      Messages:
        Player: '&bYou voted on &a&l&topg.org Thank you :)'
  # Site 2 example
  Site2:
    Enabled: false
    Name: 'Site2'
    ServiceSite: 'PlanetMinecraft.com'
    VoteURL: 'link to vote URL here, used in /vote'
    VoteDelay: 24
    DisplayItem:
      Material: 'DIAMOND'
      Amount: 1
    Rewards:
      Commands:
      - 'say hello'
      # Advanced example
      # 50% chance for reward1, if reward1 not given then 20% chance to get reward2, if reward2 not given then fallback
      AdvancedPriority:
        # Similar to priority, but no need to have to use reward files
        # Add requirements under each reward
        # Will go in order from list here and try to run each of the following rewards...
        # This name can be anything, but they need to be different
        Reward1:
          Chance: 50
          # Any other requirement here
          # If any requirement fails, the next reward will be attempted
          # Use permission requirement for rank based rewards
          #RequirePermission: true
          #Permission: 'permhere'
          Messages:
            Player: 'You got first reward'
        Reward2:
          Chance: 20
          Messages:
            Player: 'You got second reward'
        # Fallback, 100% chance, after other rewards fail chances
        Fallback:
          Messages:
            Player: 'You got unlucky'

      Messages:
        Player: 'You voted'
    
# Reward for any site
# Similar to AnySiteReward
# This requires the site to be configured in order to get the reward.
EverySiteReward: []

