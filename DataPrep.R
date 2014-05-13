#################### Research Questions ####################
# Research Question 1: Are soccer referees more likely to give red cards to dark skin toned players than light skin toned players?
# 
# Research Question 2: Are soccer referees from countries high in skin-tone prejudice more likely to award red cards to dark skin toned players?

#################### Package Party ####################
require(ggplot2)
require(plyr)
require(sandwich)
require(msm)
require(MASS)
require(pscl)
require(AER)
require(car)
require(irr)

##################### DATA ####################
data = read.csv("data.csv")

##################### Check interrater agreement #################
# 
# Question:do the two independent coders that rated the skin color of players generally agree?

icc(data[c('rater1','rater2')], model="oneway", type="agreement")
icc(data[c('rater1','rater2')], model="oneway", type="consistency")

# Strong agreement and consistency between raters

# compute average rating

data$meanrating = rowMeans(data[c('rater1','rater2')],na.rm=T)

##################### Summarize data by player #################

data.player = ddply(data,.(playerShort),summarize,
                    # total red cards
                    totalReds = sum(redCards),
                    # total yellow cards
                    totalYellows = sum(yellowCards),
                    # total yellow then red cards
                    totalyellowReds = sum(yellowReds),
                    
                    # player skin tone rating
                    skintone = meanrating[1],
                    # player height
                    height = height[1],
                    # player weight
                    weight = weight[1],
                    # player age (their birth year)
                    age = as.numeric(substr(as.character(birthday[1]),7,10)),
                    # Total player team wins  
                    wins = sum(victories),
                    # Total player team losses
                    losses = sum(defeats),
                    # Total player team ties
                    ties = sum(ties),
                    # player team win-loss difference
                    wins.loss = wins-losses,
                    # Total player goals
                    goals = sum(goals),
                    
                    # player position
                    position = position[1],
                    # player club
                    club = club[1],
                    # player's league country
                    country = leagueCountry[1],
                    # total games played by player
                    games = sum(games)
)
# remove NAs
data.player= na.omit(data.player)

#create a trichotomous variable 1-2.5 = light, 3.5-5 = dark, 3 = neither
data.player$tonesplit = ifelse(data.player$skintone>3.5,
                               "dark",ifelse(data.player$skintone<3,"light","neither"))

# Recode positions to be O for offensive, D for defensive, M for midfield, G for goalie
# this makes analyzing the output a bit easier, but doesn't imapact effect estimates much
data.player$pos = as.character(data.player$position)
data.player$pos[data.player$pos %in% 
                  c("Left Winger","Center Forward","Right Winger")] = "O"

data.player$pos[data.player$pos %in% 
                  c("Attacking Midfielder","Defensive Midfielder",
                    "Left Midfielder", "Right Midfielder",
                    "Center Midfielder" )] ='M'

data.player$pos[data.player$pos %in% 
                  c("Left Fullback","Center Back","Right Fullback")]='D'

data.player$pos[data.player$pos=="Goalkeeper"]='G'

data.player$pos = as.factor(data.player$pos)

# create clean z-scored data set for modelling 
data.playerz = na.omit(data.frame(cbind(scale(
  data.player[c("skintone","height","weight","age","wins.loss","goals")]),
  data.player[c("pos","totalReds","tonesplit","games","country","totalYellows")])))

# set effects coded contrasts for position
contrasts(data.playerz$pos) = contr.sum(length(levels(data.playerz$pos)))
# add names
colnames(contrasts(data.playerz$pos)) = levels(data.playerz$pos)[-length(levels(data.playerz$pos))]

# set effects coded contrasts for country
contrasts(data.playerz$country) = contr.sum(length(levels(data.playerz$country)))
# add names
colnames(contrasts(data.playerz$country)) = levels(data.playerz$country)[-length(
  levels(data.playerz$country))]

