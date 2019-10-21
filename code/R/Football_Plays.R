# ======================================
# Load and Transform Football Data
#
# This script will load games from the history file
# and from the data.csv download of 'current' games
# and then enhance with new columns.
# Then events will be rolled up to play sequences
# and finally to game/team/age level for age 
# statistics and clustering
#
# ======================================
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(zoo)
library(lubridate)
library(randomForest)

setwd("/GitDev/Footyapp/data/") 

FootyGames <- data.frame()
# Read in the full history of games that have been 
# converted to the new format so far
if(file.exists("FootyGamesFull.csv")){
  FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)
}

# Read in the output from the app.  This may contain games
# that have already been converted as it is a full history
footy <- read.csv("data.csv", sep = "|", stringsAsFactors = FALSE)

# Find the unique games that have not been converted so far
games <- unique(footy[! footy$gameID %in% FootyGames$gameID, ]$gameID)

print(paste("Number of games -", length(games)))

# Loop through the games to convert
for (j in 1:length(games)) {
  print(paste(j, games[j]))
  # Get just the game being converted on this iteration
  footy_match <- footy %>% filter(gameID == games[j])
  
  # Initialise fields
  kickoff = FALSE
  prevPlayNumber = 0
  half = 1
  footy_match$half = 0
  footy_match$attackIncursion = 0
  footy_match$adjEventName = ""
  footy_match$penaltyIncursion = 0
  footy_match$adjPass = 0
  footy_match$playNumber = 0
  footy_match$sequenceNumber = 0
  footy_match$isBackPass = 0
  footy_match$isFwdPass = 0
  footy_match$isSidePass = 0
  footy_match$isPass10m = 0
  footy_match$isPass10_20m = 0
  footy_match$isPass20m = 0
  footy_match$usPhase = ""
  footy_match$oppositionPhase = ""
  footy_match$pseudoEvent = FALSE
  # Adjust events to make dribbles into first touches
  # if they are too fast or too long
  footy_match$adjEventName = case_when(footy_match$eventName != "dribble" ~ footy_match$eventName,
                                       footy_match$distanceToNextEventMetres / footy_match$duration > 8 ~ "first touch",
                                       footy_match$distanceToNextEventMetres > 15 ~ "first touch",
                                       TRUE ~ footy_match$eventName)
  footy_match$player = 0
  footy_match$zone = ""
  footy_match$maxSeq = 0
  footy_match$isAttackIncursion = 0
  footy_match$isGoal = 0
  footy_match$firstEvent = ""
  footy_match$playByUs = ""
  footy_match$matchName = ""
  
  # For each event, figure out what it is
  for (i in 1:nrow(footy_match)) {
    
    footy_match[i, ]$half = half
    # There may be a change ends event before the game starts
    # so wait for the actual kickoff..
    if (!kickoff) {
      # kick off is the start of the game, set this as the adjusted event
      if (footy_match[i, ]$adjEventName == "first touch" ) {
        footy_match[i, ]$adjEventName = "kick off"
        kickoff = TRUE
        # 1st event of the game
        footy_match[i, ]$playNumber = 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        
        # Determine who has the ball so which team is in
        # possession and which team is opposing
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
      }
      # If after the kickoff then decide what has happened
    } else {
      # The end of the half or game, so increment half
      if (footy_match[i, ]$adjEventName %in% c("change ends", "finish")) {
        if (footy_match[i, ]$adjEventName == "change ends") {
          half = half + 1
        }
        # Previous event was a side out, so this event is a throw in
      } else if (footy_match[i-1, ]$adjEventName %in% c("sideline out", "sideline out defence") &
                 footy_match[i, ]$adjEventName == "first touch") {       
        # Start of a new play sequence
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "throw in"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        
        # if previous event was a goal kick which means the
        # attacking team put the ball over the base line
        # then this event is a goal kick
      } else if (footy_match[i-1, ]$adjEventName == "goalkick" &
                 footy_match[i, ]$adjEventName == "first touch") {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "goal kick"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        # if previous event was a corner which means the
        # defending team put the ball over the base line
        # then this event is a corner kick
      } else if (footy_match[i-1, ]$adjEventName == "corner" &
                 footy_match[i, ]$adjEventName == "first touch") {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "corner kick"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        # if previous event was a penalty
        # then this event to restart is a penalty kick
      } else if (footy_match[i-1, ]$adjEventName == "penalty" &
                 footy_match[i, ]$adjEventName == "first touch") {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "penalty kick"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        # If this event is keeper which means the keeper picks up the ball
        # then the next event is a keeper punt
      } else if (footy_match[i, ]$adjEventName == "keeper") {
        # first touch is outside the penalty area so punt has been missed
        # Insert a kick..
        if ((footy_match[i+1, ]$lengthMetres > 16.5 |
             footy_match[i+1, ]$widthMetres > (75 - 17.34) |
             footy_match[i+1, ]$widthMetres < 17.34) &
            !(footy_match[i+1, ]$adjEventName %in% c("change ends", "finish")) ) {
          
          # Create a keeper punt that was missed by the app
          footy_match[i, ]$playNumber = prevPlayNumber + 1
          footy_match[i, ]$sequenceNumber = 1
          footy_match[i, ]$player = 1
          footy_match[i, ]$adjEventName = "keeper punt"
          if (footy_match[i, ]$byUs == "true") {
            footy_match[i, ]$usPhase = "BP"
            footy_match[i, ]$oppositionPhase = "BPO"
          } else {
            footy_match[i, ]$usPhase = "BPO"
            footy_match[i, ]$oppositionPhase = "BP"
          }
          
        }
        # If the next event is a keeper picking up the ball
        # then make the previous event to be the keeper catch
        # assuming the keeper just picks up the ball without running 
        # with it.
      } else if (footy_match[i+1, ]$adjEventName == "keeper") {
        
        footy_match[i, ]$playNumber = prevPlayNumber 
        footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
        footy_match[i, ]$player = footy_match[i-1, ]$player + 1
        footy_match[i, ]$adjEventName = "keeper catch"
        footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
        footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
        
      } else if (footy_match[i-1, ]$adjEventName == "keeper" &
                 !(footy_match[i, ]$adjEventName %in% c("change ends", "finish")) &
                 !(footy_match[i, ]$lengthMetres > 16.5 |
                   footy_match[i, ]$widthMetres > (75 - 17.34) |
                   footy_match[i, ]$widthMetres < 17.34)) {
        
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "keeper punt"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        
        # If the event is a foul then update the sequence number
      } else if (footy_match[i, ]$adjEventName %in% c("offside", "direct freekick", "indirect freekick")) {
        footy_match[i, ]$playNumber = prevPlayNumber
        footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
        footy_match[i, ]$player = footy_match[i-1, ]$player
        
        # If the previous event is a foul 
        # then the restart is a foul restart
      } else if (footy_match[i-1, ]$adjEventName %in% c("offside", "direct freekick", "indirect freekick")) {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = case_when(footy_match[i-1, ]$adjEventName == "offside" ~ "offside restart",
                                                  footy_match[i-1, ]$adjEventName == "direct freekick" ~ "direct restart",
                                                  TRUE ~ "indirect restart")
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        
        # If the previous event is a change of ends
        # then this event is a kick off
      } else if (footy_match[i-1, ]$adjEventName == "change ends" &
                 footy_match[i, ]$adjEventName == "first touch") {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "kick off"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } 
        # If a goal was scored then this event is a kick off
      } else if (footy_match[i-1, ]$adjEventName == "goal" &
                 footy_match[i, ]$adjEventName == "first touch") {
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$player = 1
        footy_match[i, ]$adjEventName = "kick off"
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BP"
          footy_match[i, ]$oppositionPhase = "BPO"
        } else {
          footy_match[i, ]$usPhase = "BPO"
          footy_match[i, ]$oppositionPhase = "BP"
        } 
        # Include action before sideline out/corner/goal kick as part of same play
      } else if (footy_match[i+1, ]$adjEventName %in% c("sideline out", "corner", "goalkick") &
                 footy_match[i, ]$byUs != footy_match[i-1, ]$byUs) {
        footy_match[i, ]$playNumber = prevPlayNumber
        footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
        footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
        footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
        footy_match[i, ]$player = footy_match[i-1, ]$player + 1
        if (footy_match[i+1, ]$adjEventName == "sideline out") {
          footy_match[i+1, ]$adjEventName = "sideline out defence"
        }
        
        # else this is continuing the same play
      } else if (footy_match[i, ]$byUs == footy_match[i-1, ]$byUs) {
        footy_match[i, ]$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
        footy_match[i, ]$playNumber = prevPlayNumber
        footy_match[i, ]$usPhase = footy_match[i-1, ]$usPhase
        footy_match[i, ]$oppositionPhase = footy_match[i-1, ]$oppositionPhase
        footy_match[i, ]$player = case_when(footy_match[i-1, ]$adjEventName %in% c("first touch", "dribble", "clearance") &
                                              footy_match[i, ]$adjEventName %in% c("dribble") ~ footy_match[i-1, ]$player,
                                            footy_match[i, ]$adjEventName %in% c("sideline out", "corner", "goalkick", "shot", "rebound", "goal",
                                                                                 "sideline out defence") ~ 
                                              footy_match[i-1, ]$player,
                                            TRUE ~ footy_match[i-1, ]$player + 1)
        
        # turnover ball, so the same event is the end of play
        # and the start of the next play
      } else {
        # Put in an end marker for this play when there is a turnover
        footy_extra = footy_match[i, ]
        footy_extra$pseudoEvent = TRUE
        # For the opposition taking the ball, coordinates have changed
        # so reverse to make them the same as the attacking teams
        footy_extra$lengthFraction = 1 - footy_extra$lengthFraction
        footy_extra$lengthMetres = 110 - footy_extra$lengthMetres 
        footy_extra$widthFraction = 1 - footy_extra$widthFraction
        footy_extra$widthMetres = 75 - footy_extra$widthMetres         
        footy_extra$playNumber = prevPlayNumber
        footy_extra$sequenceNumber = footy_match[i-1, ]$sequenceNumber + 1
        footy_extra$player = footy_match[i-1, ]$player + 1
        footy_extra$usPhase = footy_match[i-1, ]$usPhase
        footy_extra$oppositionPhase = footy_match[i-1, ]$oppositionPhase
        footy_extra$adjEventName = "turnover"
        
        footy_match <- rbind(footy_match, footy_extra)
        
        # The start of the new play
        footy_match[i, ]$sequenceNumber = 1
        footy_match[i, ]$playNumber = prevPlayNumber + 1
        footy_match[i, ]$player = 1
        
        # Turnover and then straight back as turnover
        # but cleared at least 10 metres then count as a Clearance
        if (footy_match[i, ]$byUs != footy_match[i+1, ]$byUs &
            !is.na(footy_match[i, ]$distance) &
            footy_match[i, ]$distance >= 10 & footy_match[i, ]$lengthChangeMetres > 3 &
            footy_match[i, ]$lengthFraction <= 0.33) {
          footy_match[i, ]$adjEventName = "clearance"
        }
        
        if (footy_match[i, ]$byUs == "true") {
          footy_match[i, ]$usPhase = "BPO > BP"
          footy_match[i, ]$oppositionPhase = "BP > BPO"
        } else { 
          footy_match[i, ]$usPhase = "BP > BPO"
          footy_match[i, ]$oppositionPhase = "BPO > BP"
        }  
        
      }       
      
    }
    
    # sort out was it an attacking incursion, penalty area incursion
    # and track previous play number
    if (footy_match[i, ]$playNumber > 0) {
      prevPlayNumber = footy_match[i, ]$playNumber
      if (footy_match[i, ]$byUs == footy_match[i+1, ]$byUs) {
        # Mark events as a completed pass between attacking team members
        if (footy_match[i, ]$adjEventName %in% c("throw in", "dribble", "kick off", "first touch", 
                                                 "goal kick", "keeper punt", "corner kick",
                                                 "offside restart", "direct restart", "indirect restart") &
            footy_match[i+1, ]$adjEventName %in% c("first touch")) {
          footy_match[i, ]$adjPass = 1
        }
        
        # If the play takes place in the attacking third, mark as attack incursion
        if (footy_match[i, ]$lengthFraction >= 0.66666667) {
          footy_match[i, ]$attackIncursion = 1
        }
        # if the play takes place in the penalty area, mark as a penalty incursion
        if (footy_match[i, ]$lengthMetres >= (110 - 16.5) &
            footy_match[i, ]$widthMetres <= (75 - 17.34) &
            footy_match[i, ]$widthMetres >= 17.34) {
          footy_match[i, ]$penaltyIncursion = 1
        }
        
      }
    }
    
  }
  FootyGames <- rbind(FootyGames, footy_match)
  
}
print (i)
# If any events have NA direction or duration, then set to 0
FootyGames[is.na(FootyGames$direction), ]$direction <- 0
FootyGames[is.na(FootyGames$duration), ]$duration <- 0

# Set direction of passes
FootyGames$isBackPass <- ifelse(FootyGames$adjPass == 1 &
                                  FootyGames$lengthChangeMetres < 0 &
                                  (abs(FootyGames$direction) < 75 |  
                                     abs(FootyGames$direction) > 105), 1, 0)
FootyGames$isFwdPass <- ifelse(FootyGames$adjPass == 1 &
                                 FootyGames$lengthChangeMetres > 0 &
                                 (abs(FootyGames$direction) < 75 |  
                                    abs(FootyGames$direction) > 105), 1, 0)
FootyGames$isSidePass <- ifelse(FootyGames$adjPass == 1 &
                                  (abs(FootyGames$direction) >= 75 &  
                                     abs(FootyGames$direction) <= 105), 1, 0)
# For forward passes, what was their length
FootyGames$isPass10m <- ifelse(FootyGames$isFwdPass == 1 &
                                 FootyGames$distance <= 10, 1, 0)
FootyGames$isPass10_20m <- ifelse(FootyGames$isFwdPass == 1 &
                                    FootyGames$distance >10 &
                                    FootyGames$distance <= 20, 1, 0)
FootyGames$isPass20m <- ifelse(FootyGames$isFwdPass == 1 &
                                 FootyGames$distance > 20, 1, 0)

# For goals, make sure the length is at the end..
FootyGames[FootyGames$adjEventName == "goal",]$lengthMetres <- 110
FootyGames[FootyGames$adjEventName == "goal",]$lengthFraction <- 1

# Set the zone (1-18) the play happened in
FootyGames$zone <- (floor(ifelse(FootyGames$widthFraction== 1, 2, 
                                 FootyGames$widthFraction*3))+1)+(3*floor(ifelse(FootyGames$lengthFraction>= 1, 5, 
                                                                                 FootyGames$lengthFraction*6)))
# write out the full set of games
write.csv(FootyGames, "FootyGamesFull.csv", row.names = FALSE)


FootyGames <- read.csv("FootyGamesFull.csv", stringsAsFactors = FALSE)

# Make a league of their own for display purposes
df_new <- data.frame(gameID = c('-LlKBxEgkAda_vAyje2H',
                                '-Ln-tc1HqL2dAG6ymkkq',
                                '-Li7z1mG3w5e8FbqBu9o',
                                '-Le4Q6dgeZByZGRi9_Ip',
                                '-LflM0OQOmZ0CPgmmACm',
                                '-Lr1mYFITd1jCXjVNKKQ',
                                '-KkjKGc9-GRe3jM2lKSK',
                                '-KlWCZWYcst-2XOGqTPQ',
                                '-Kk8Ljt2sZnrjvQN_S4b',
                                '-LaOGTuP56oBG_c_FrYn',
                                '-LccnAreea7ZXK_tgM-k',
                                '-LYYjvAA97CbIEp4n-Y8',
                                '-KlBdnOEPB6QWyr8dYD7',
                                '-KdohFX_Pz7_526JvrEb',
                                '-L6Dp__w2g2LMHFtMtPa'),
                     ourName = rep('Bears', 15),
                     theirName = c('Sharks', 'Kangaroos', 'Panthers',
                                   'Wombats', 'Wallabies', 'Possums',
                                   'Bilbies', 'Swifts', 'Dingoes',
                                   'Rabbits', 'Devils', 'Eagles',
                                   'Crocodiles', 'Phoenix', 'Quokkas'),
                     match_date = c('2019-04-01', '2019-04-08', '2019-04-15', 
                              '2019-04-22', '2019-04-29', '2019-05-06',
                              '2019-05-13', '2019-05-20', '2019-05-27',
                              '2019-06-03', '2019-06-10', '2019-06-17',
                              '2019-06-24', '2019-07-01', '2019-07-08'),
                     stringsAsFactors = FALSE)
FootyGames <- FootyGames %>%
left_join(df_new, 
          c("gameID" = "gameID"), suffix = c("", ".y")) %>%
  mutate(competition = ifelse(is.na(ourName.y), competition, '2019 Regional Boys Under 14'),
  ourName = ifelse(is.na(ourName.y), ourName, ourName.y),
  theirName = ifelse(is.na(theirName.y), theirName, theirName.y),
  time = ifelse(is.na(theirName.y), time, paste(match_date, substring(time, 12))),
  epochTime = as.numeric(as.POSIXct(substr(time, 1, 23), format='%Y-%m-%d %H:%M:%OS')))

FootyGames$ourName.y <- NULL
FootyGames$theirName.y <- NULL
FootyGames$match_date <- NULL



# Now, taking the full set of games, add more enhancements
footy_games_final <- FootyGames %>% 
  group_by(gameID, competition, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, ourName, theirName, playNumber, sequenceNumber) %>%
  mutate(kickoff_date = as.POSIXct(substr(time, 1, 19), format='%Y-%m-%d %H:%M:%S'),
         age_group = ifelse(competition %in% c("2019 World Cup 2019 Mens Open",             
                                                  "2019 International Friendly Mens Open",        
                                                  "2019 Icc Football Mens Open",
                                                  "2019 Mens Open"), "Top-Flight",
                            word(competition,-1)),
         maxSequenceNumber = max(sequenceNumber),
         # Reverse width so that it will display correctly
         # Original coordinates has 0,0 as back left post
         widthMetres = 75 - widthMetres,
         widthFraction = 1 - widthFraction,
         isAttackIncursion = max(attackIncursion),
         isGoal = ifelse(last(adjEventName == "goal"), 1, 0),
         numberPasses = sum(adjPass),
         playByUs = first(byUs),
         firstEvent = first(adjEventName),
         lastEvent = last(adjEventName),
         matchName = paste(format(ymd(substr(time, 1, 10)), "%d/%m/%y"), "-", theirName),
         backPassTurnover = ifelse(isBackPass == 1 & sequenceNumber == 1 & adjEventName == "first touch", 1, 0),
         fwdPassTurnover = ifelse(isFwdPass == 1 & sequenceNumber == 1 & adjEventName == "first touch", 1, 0),
         sidePassTurnover = ifelse(isSidePass == 1 & sequenceNumber == 1 & adjEventName == "first touch", 1, 0),
         rightAttack = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone==10 & lead(zone) %in% c(13, 16) & byUs == lead(byUs), 1, 0)),
         middleAttack = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone==11 & lead(zone)%in% c(14, 17) & byUs == lead(byUs), 1, 0)),
         leftAttack = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone==12 & lead(zone)%in% c(15, 18) & byUs == lead(byUs), 1, 0)),
         isZone14Incursion = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=14 & lead(zone) == 14 & byUs == lead(byUs), 1, 0)),
         isZone17Incursion = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=17 & lead(zone) == 17 & byUs == lead(byUs), 1, 0)),
         isZone11PassIncursion = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=11 & lead(zone) == 11 & adjPass == 1 & byUs == lead(byUs), 1, 0)),
         isBackKeeperPass = ifelse(sequenceNumber == maxSequenceNumber, 0, ifelse(zone!=2 & lead(zone) == 2 & adjPass == 1 & byUs == lead(byUs), 1, 0)),
         playArea = case_when(lengthFraction <= 0.333333 ~ "Defence",
                              lengthFraction >= 0.666667 ~ "Attack",
                              TRUE ~ "Midfield"),
         Movement = case_when(playNumber == 0 ~ "",
                              sequenceNumber == maxSequenceNumber ~ "",
                              adjPass == 1 & playArea == 'Defence' &
                                lead(playArea) == 'Midfield' ~ "D-M",
                              adjPass == 1 & playArea == 'Midfield' &
                                lead(playArea) == 'Defence' ~ "M-D",
                              adjPass == 1 & playArea == 'Midfield' &
                                lead(playArea) == 'Attack' ~ "M-A",
                              adjPass == 1 & playArea == 'Attack' &
                                lead(playArea) == 'Midfield' ~ "A-M",
                              TRUE ~ "")
  )

# Now, turn events into 1 row per play sequence, summarising various features
footy_match_play <- footy_games_final %>% filter(playNumber != 0) %>%
  group_by(gameID, competition, age_group, ourName, theirName, playNumber) %>%
  arrange(gameID, competition, age_group, ourName, theirName, playNumber, sequenceNumber) %>%
  summarise(gameDate = first(kickoff_date),
            playByUs = first(playByUs),
            matchName = first(matchName),
            teamName = first(ifelse(playByUs == "true", ourName, theirName)),
            usPhase = first(usPhase),
            half = first(half),
            oppositionPhase = first(oppositionPhase),
            firstEvent = first(adjEventName),
            lastEvent = last(adjEventName),
            Area = first(playArea),
            lastArea = last(playArea),
            firstEventArea = paste(firstEvent, "-", Area),
            firstPenalyIncursion = first(penaltyIncursion),
            firstZone = first(zone),
            lastZone = last(zone),
            numSequences = max(sequenceNumber),
            numPlayers = max(player),
            distance = sum(ifelse(playByUs == byUs &
                                    !lead(adjEventName) %in% c("goal kick", "sideline out"), 
                                  distanceToNextEventMetres, 0)),
            attackDistance = sum(ifelse(playByUs == byUs &
                                          !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                          playArea == 'Attack', 
                                        distanceToNextEventMetres, 0)),
            midfieldDistance = sum(ifelse(playByUs == byUs &
                                            !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                            playArea == 'Midfield', 
                                          distanceToNextEventMetres, 0)),
            defenceDistance = sum(ifelse(playByUs == byUs &
                                           !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                           playArea == 'Defence', 
                                         distanceToNextEventMetres, 0)),
            isAttackIncursion = max(attackIncursion),
            isGoal = max(isGoal),
            isPenaltyIncursion = max(penaltyIncursion),
            isZone17Incursion = max(isZone17Incursion, na.rm=TRUE),
            isZone14Incursion = ifelse(isZone17Incursion == 1, 0, max(isZone14Incursion, na.rm=TRUE)),
            isZone11PassIncursion = max(isZone11PassIncursion, na.rm=TRUE),
            playPhase = first(ifelse(playByUs == "true", usPhase, oppositionPhase)),
            totalPasses = sum(adjPass),
            totalBackPasses = sum(isBackPass),
            totalFwdPasses = sum(isFwdPass),
            totalSidePasses = sum(isSidePass),
            total10mPasses = sum(isPass10m),
            total10_20mPasses = sum(isPass10_20m),
            total20mPasses = sum(isPass20m),
            possessionDuration = sum(ifelse(playByUs == byUs &
                                              !lead(adjEventName) %in% c("goal kick", "sideline out"), duration, 0)),
            AttackDuration = sum(ifelse(playByUs == byUs &
                                          !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                          playArea == 'Attack', duration, 0)),
            MidfieldDuration = sum(ifelse(playByUs == byUs &
                                            !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                            playArea == 'Midfield', duration, 0)),
            DefenceDuration = sum(ifelse(playByUs == byUs &
                                           !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                           playArea == 'Defence', duration, 0)),
            AttackHalfDuration = sum(ifelse(playByUs == byUs &
                                              !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                              lengthMetres >= 55, duration, 0)),
            DefenceHalfDuration = sum(ifelse(playByUs == byUs &
                                               !lead(adjEventName) %in% c("goal kick", "sideline out") &
                                               lengthMetres < 55, duration, 0)),
            playDuration = sum(duration),
            attackPasses = sum(ifelse(playArea == "Attack", adjPass, 0)),
            midfieldPasses = sum(ifelse(playArea == "Midfield", adjPass, 0)),
            defencePasses = sum(ifelse(playArea == "Defence", adjPass, 0)),
            crossfieldPlay = ifelse(max(widthMetres) - min(widthMetres) > 50 &
                                      Area == "Defence", 1, 0),
            goals = sum(ifelse(adjEventName == "goal", 1, 0)),
            shots = sum(ifelse(adjEventName == "shot", 1, 0)),
            backPassTurnover = max(backPassTurnover),
            fwdPassTurnover = max(fwdPassTurnover),
            sidePassTurnover = max(sidePassTurnover),
            rightAttack = max(rightAttack),
            middleAttack = max(middleAttack),
            leftAttack = max(leftAttack),
            defenceOut = ifelse(lastEvent == "sideline out defence", 1, 0),
            MAPlay = max(ifelse(Movement == "M-A", 1, 0)),
            AMPlay = max(ifelse(Movement == "A-M", 1, 0)),
            MDPlay = max(ifelse(Movement == "M-D", 1, 0)),
            DMPlay = max(ifelse(Movement == "D-M", 1, 0)),
            backKeeperPasses = sum(isBackKeeperPass),
            clearances = sum(ifelse(adjEventName == "clearance", 1, 0)),
            passZero = ifelse(totalPasses == 0, 1, 0),
            pass1 = ifelse(totalPasses > 0 & totalPasses < 2, 1, 0),
            pass2_3 = ifelse(totalPasses > 1 & totalPasses < 4, 1, 0),
            pass4_6 = ifelse(totalPasses > 3 & totalPasses < 7, 1, 0),
            pass7Plus = ifelse(totalPasses > 6, 1, 0)
  )

#footy_games_final <- read.csv("FootyGamesFinal.csv", stringsAsFactors = FALSE)
#footy_match_play <- read.csv("FootyMatchPlayFull.csv", stringsAsFactors = FALSE)
write.csv(footy_games_final %>%
            filter(competition %in% c("2019 World Cup 2019 Mens Open",             
                                      "2019 International Friendly Mens Open",        
                                      "2019 Icc Football Mens Open",
                                      "2019 Mens Open")), "FootyGamesTop.csv", row.names = FALSE)
write.csv(footy_match_play %>%
            filter(competition %in% c("2019 World Cup 2019 Mens Open",             
                                                  "2019 International Friendly Mens Open",        
                                                  "2019 Icc Football Mens Open",
                                                  "2019 Mens Open")), "FootyMatchPlayTop.csv", row.names = FALSE)


# Write out results
write.csv(footy_games_final, "FootyGamesFinalPro.csv", row.names = FALSE)
write.csv(footy_match_play, "FootyMatchPlayFullPro.csv", row.names = FALSE)
# Save cutdown versions for U13s and U16s for shiny App
write.csv(footy_games_final %>%
            filter(competition == "2019 Regional Boys Under 14"), "FootyGames1Pro.csv", row.names = FALSE)
write.csv(footy_match_play %>%
            filter(competition == "2019 Regional Boys Under 14"), "FootyMatchPlayPro.csv", row.names = FALSE)

# Then for shiny R app, summarise to 1 row per age group per us/opposition
footy_games_age <- footy_match_play %>% filter(playNumber > 0) %>%
  mutate(goalAttack = ifelse(isGoal == 1 & Area == "Attack", 1, 0),
         goalMidfield = ifelse(isGoal == 1 & Area == "Midfield", 1, 0),
         goalDefence = ifelse(isGoal == 1 & Area == "Defence", 1, 0),
         goalTurnover = ifelse(isGoal == 1 & firstEvent == "first touch", 1, 0),
         goalRestart = ifelse(isGoal == 1 & firstEvent != "first touch", 1, 0)
  ) %>%
  group_by(age_group, playByUs) %>%
  summarise_if(is.numeric, list(sum), na.rm = TRUE) %>%
  mutate(ppm = totalPasses/(possessionDuration/60),
         ppmAttack = attackPasses/(AttackDuration/60),
         ppmMidfield = midfieldPasses/(MidfieldDuration/60),
         ppmDefence = defencePasses/(DefenceDuration/60),
         aveVelocity = distance/(possessionDuration),
         attackVelocity = attackDistance/(AttackDuration),
         midfieldVelocity = midfieldDistance/(MidfieldDuration),
         defenceVelocity = defenceDistance/(DefenceDuration)
  )
write.csv(footy_games_age, "FootyMatchAgeFullPro.csv", row.names = FALSE)

# =========================
# Clustering Structures
# =========================
# Re-read full Plays per match, as a starting point..
footy_match_play <- read.csv("FootyMatchPlayFullPro.csv", stringsAsFactors = FALSE)

# Now summarise to 1 row per game per team, for clustering
footy_match_cluster <- footy_match_play %>%
  group_by(competition, gameID, 
           teamName, playByUs) %>%
  # Percentages are within each team, not across the game
  summarise(gameDate= min(gameDate),
            theirName = max(theirName),
            matchName = max(matchName),
            gameDuration = sum(possessionDuration),
            gameDistance = sum(distance, na.rm=TRUE),
            plays = n(),
            goals = sum(goals),
            shots = sum(shots),
            totalPasses = sum(totalPasses),
            ppm = totalPasses/(gameDuration/60),
            aveVelocity = gameDistance/(gameDuration),
            
            attackDuration_pcnt = sum(AttackDuration) / gameDuration,
            midfieldDuration_pcnt = sum(MidfieldDuration) / gameDuration,
            defenceDuration_pcnt = sum(DefenceDuration) / gameDuration,
            AttackHalfDuration_pcnt = sum(AttackHalfDuration) / gameDuration,
            DefenceHalfDuration_pcnt = sum(DefenceHalfDuration) / gameDuration,
            attackDistance_pcnt = sum(attackDistance, na.rm=TRUE) / gameDistance,
            midfieldDistance_pcnt = sum(midfieldDistance, na.rm=TRUE) / gameDistance,
            defenceDistance_pcnt = sum(defenceDistance, na.rm=TRUE) / gameDistance,
            isZone17Incursion_pcnt = sum(isZone17Incursion, na.rm=TRUE)/sum(isAttackIncursion),
            isZone14Incursion_pcnt = sum(isZone14Incursion, na.rm=TRUE)/sum(isAttackIncursion),
            isZone11PassIncursion_pcnt = sum(isZone11PassIncursion, na.rm=TRUE)/sum(isAttackIncursion),
            totalBackPasses_pcnt = sum(totalBackPasses)/totalPasses,
            totalFwdPasses_pcnt = sum(totalFwdPasses)/totalPasses,
            totalSidePasses_pcnt = sum(totalSidePasses)/totalPasses,
            total10mPasses_pcnt = sum(total10mPasses)/sum(total10mPasses +
                                                            total10_20mPasses + total20mPasses),
            total10_20mPasses_pcnt = sum(total10_20mPasses)/sum(total10mPasses +
                                                                  total10_20mPasses + total20mPasses),
            total20mPasses_pcnt = sum(total20mPasses)/sum(total10mPasses +
                                                            total10_20mPasses + total20mPasses),
            attackPasses_pcnt = sum(attackPasses)/totalPasses,
            midfieldPasses_pcnt = sum(midfieldPasses)/totalPasses,
            defencePasses_pcnt = sum(defencePasses)/totalPasses,
            crossfieldPlay_pcnt = sum(crossfieldPlay)/sum(defencePasses),
            backPassTurnover_pcnt = sum(backPassTurnover)/sum(backPassTurnover + 
                                                                sidePassTurnover + fwdPassTurnover),
            fwdPassTurnover_pcnt = sum(fwdPassTurnover)/sum(backPassTurnover + 
                                                              sidePassTurnover + fwdPassTurnover),
            sidePassTurnover_pcnt = sum(sidePassTurnover)/sum(backPassTurnover + 
                                                                sidePassTurnover + fwdPassTurnover),
            rightAttack_pcnt = sum(rightAttack)/sum(rightAttack + 
                                                      middleAttack + leftAttack),
            middleAttack_pcnt = sum(middleAttack)/sum(rightAttack + 
                                                        middleAttack + leftAttack),
            leftAttack_pcnt = sum(middleAttack)/sum(rightAttack + 
                                                      middleAttack + leftAttack),
            MAPlay_pcnt = sum(MAPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            AMPlay_pcnt = sum(AMPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            MDPlay_pcnt = sum(MDPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            DMPlay_pcnt = sum(DMPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            backKeeperPasses_pcnt = sum(backKeeperPasses)/sum(totalBackPasses),
            clearances_pcnt = sum(clearances)/plays,
            passZero_pcnt = sum(passZero)/plays,
            passPlay_pcnt = 1 - passZero_pcnt,
            pass1_pcnt = sum(pass1)/plays,
            pass2_3_pcnt = sum(pass2_3)/plays,
            pass4_6_pcnt = sum(pass4_6)/plays,
            pass7Plus_pcnt = sum(pass7Plus)/plays
  ) %>%
  ungroup () %>%
  group_by(competition, gameID) %>%
  mutate(result = case_when(sum(goals) == 0 ~ "Draw",
                            goals/sum(goals) == 0.5 ~ "Draw",
                            goals/sum(goals) < 0.5 ~ "Loss",
                            TRUE ~ "Win")
  )

write.csv(footy_match_cluster, "FootyMatchClusterPro.csv", row.names = FALSE)


# Then create 1 row per team for clustering
footy_team_cluster <- footy_match_play %>%
  group_by(competition, teamName, playByUs, age_group) %>%
  summarise(gameDate= min(gameDate),
            theirName = max(theirName),
            matchName = max(matchName),
            gameDuration = sum(possessionDuration),
            gameDistance = sum(distance, na.rm=TRUE),
            plays = n(),
            goals = sum(goals),
            shots = sum(shots),
            totalPasses = sum(totalPasses),
            ppm = totalPasses/(gameDuration/60),
            aveVelocity = gameDistance/(gameDuration),
            
            attackDuration_pcnt = sum(AttackDuration) / gameDuration,
            midfieldDuration_pcnt = sum(MidfieldDuration) / gameDuration,
            defenceDuration_pcnt = sum(DefenceDuration) / gameDuration,
            AttackHalfDuration_pcnt = sum(AttackHalfDuration) / gameDuration,
            DefenceHalfDuration_pcnt = sum(DefenceHalfDuration) / gameDuration,
            attackDistance_pcnt = sum(attackDistance, na.rm=TRUE) / gameDistance,
            midfieldDistance_pcnt = sum(midfieldDistance, na.rm=TRUE) / gameDistance,
            defenceDistance_pcnt = sum(defenceDistance, na.rm=TRUE) / gameDistance,
            isZone17Incursion_pcnt = sum(isZone17Incursion, na.rm=TRUE)/sum(isAttackIncursion),
            isZone14Incursion_pcnt = sum(isZone14Incursion, na.rm=TRUE)/sum(isAttackIncursion),
            isZone11PassIncursion_pcnt = sum(isZone11PassIncursion, na.rm=TRUE)/sum(isAttackIncursion),
            totalBackPasses_pcnt = sum(totalBackPasses)/totalPasses,
            totalFwdPasses_pcnt = sum(totalFwdPasses)/totalPasses,
            totalSidePasses_pcnt = sum(totalSidePasses)/totalPasses,
            total10mPasses_pcnt = sum(total10mPasses)/sum(total10mPasses +
                                                            total10_20mPasses + total20mPasses),
            total10_20mPasses_pcnt = sum(total10_20mPasses)/sum(total10mPasses +
                                                                  total10_20mPasses + total20mPasses),
            total20mPasses_pcnt = sum(total20mPasses)/sum(total10mPasses +
                                                            total10_20mPasses + total20mPasses),
            attackPasses_pcnt = sum(attackPasses)/totalPasses,
            midfieldPasses_pcnt = sum(midfieldPasses)/totalPasses,
            defencePasses_pcnt = sum(defencePasses)/totalPasses,
            crossfieldPlay_pcnt = sum(crossfieldPlay)/sum(defencePasses),
            backPassTurnover_pcnt = sum(backPassTurnover)/sum(backPassTurnover + 
                                                                sidePassTurnover + fwdPassTurnover),
            fwdPassTurnover_pcnt = sum(fwdPassTurnover)/sum(backPassTurnover + 
                                                              sidePassTurnover + fwdPassTurnover),
            sidePassTurnover_pcnt = sum(sidePassTurnover)/sum(backPassTurnover + 
                                                                sidePassTurnover + fwdPassTurnover),
            rightAttack_pcnt = sum(rightAttack)/sum(rightAttack + 
                                                      middleAttack + leftAttack),
            middleAttack_pcnt = sum(middleAttack)/sum(rightAttack + 
                                                        middleAttack + leftAttack),
            leftAttack_pcnt = sum(middleAttack)/sum(rightAttack + 
                                                      middleAttack + leftAttack),
            MAPlay_pcnt = sum(MAPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            AMPlay_pcnt = sum(AMPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            MDPlay_pcnt = sum(MDPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            DMPlay_pcnt = sum(DMPlay)/sum(MAPlay + AMPlay + MDPlay + DMPlay),
            backKeeperPasses_pcnt = sum(backKeeperPasses)/sum(totalBackPasses),
            clearances_pcnt = sum(clearances)/plays,
            passZero_pcnt = sum(passZero)/plays,
            passPlay_pcnt = 1 - passZero_pcnt,
            pass1_pcnt = sum(pass1)/plays,
            pass2_3_pcnt = sum(pass2_3)/plays,
            pass4_6_pcnt = sum(pass4_6)/plays,
            pass7Plus_pcnt = sum(pass7Plus)/plays
  ) %>%
  ungroup ()

write.csv(footy_team_cluster, "FootyTeamClusterPro.csv", row.names = FALSE)

## ==== Cleanup ====
rm(list = ls())
