library(ggplot2)
library(plyr)
library(sandwich)
library(msm)
library(MASS)
library(pscl)
library(AER)
library(car)
data = read.csv("data.csv")

# Research Question 1: Are soccer referees more likely to give red cards to dark skin toned players than light skin toned players?
# 
# Research Question 2: Are soccer referees from countries high in skin-tone prejudice more likely to award red cards to dark skin toned players?

#Check interrater agreement
library(irr)
icc(data[c('rater1','rater2')], model="oneway", type="agreement")
icc(data[c('rater1','rater2')], model="oneway", type="consistency")

ggplot(data,aes(x=rater1,y=rater2))+
  geom_point(alpha=.002,size=10)

#compute average rating
#first make sure there aren't ratings missing for one rater and not the other though
sum((is.na(data$rater2)&is.na(data$rater1)))

data$meanrating = rowMeans(data[c('rater1','rater2')],na.rm=T)

#total cards distributions by player
data.player = ddply(data,.(playerShort),summarize,
                    totalReds = sum(redCards),
                    totalYellows = sum(yellowCards),
                    totalyellowReds = sum(yellowReds),
                    skintone = mean(meanrating,na.rm=T),
                    height = height[1],
                    weight = weight[1],
        age = 2014-as.numeric(substr(as.character(birthday[1]),7,10)),
        wins = sum(victories),
        losses = sum(defeats),
        ties = sum(ties),
        wins.loss = wins-losses,
        goals = sum(goals),
        position = position[1],
        club = club[1],
        country = leagueCountry[1],
        games = sum(games)
                    )
data.player= na.omit(data.player)
data.player$tonesplit = ifelse(data.player$skintone>3.5,
                               "dark",ifelse(data.player$skintone<3,"light","neither"))
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

# data is very much poisson distributed
hist(data.player$totalReds)
hist(data.player$totalyellowReds)
hist(data.player$totalYellows)


# very skewed, way more light skinned players than dark
hist(data.player$skintone)

table(data.player$tonesplit)

#compare card distributions between skin tones
ggplot(data.player[data.player$tonesplit!='neither',],aes(x=totalReds,fill=tonesplit))+
  geom_density(alpha=.5)

median(data.player$totalReds[data.player$tonesplit=="light"],na.rm=T)
median(data.player$totalReds[data.player$tonesplit=="dark"],na.rm=T)

mean(data.player$totalReds[data.player$tonesplit=="light"],na.rm=T)
mean(data.player$totalReds[data.player$tonesplit=="dark"],na.rm=T)

ggplot(data.player[data.player$tonesplit!='neither',],aes(x=totalYellows,fill=tonesplit))+
  geom_density(alpha=.5)

median(data.player$totalYellows[data.player$tonesplit=="light"],na.rm=T)
median(data.player$totalYellows[data.player$tonesplit=="dark"],na.rm=T)

mean(data.player$totalYellows[data.player$tonesplit=="light"],na.rm=T)
mean(data.player$totalYellows[data.player$tonesplit=="dark"],na.rm=T)

ggplot(data.player[data.player$tonesplit!='neither',],aes(x=totalyellowReds,fill=tonesplit))+
  geom_density(alpha=.5)

median(data.player$totalyellowReds[data.player$tonesplit=="light"],na.rm=T)
median(data.player$totalyellowReds[data.player$tonesplit=="dark"],na.rm=T)

mean(data.player$totalyellowReds[data.player$tonesplit=="light"],na.rm=T)
mean(data.player$totalyellowReds[data.player$tonesplit=="dark"],na.rm=T)

#poisson regression totalReds

summary(m1 <- glm(totalReds ~ tonesplit, family = "poisson", data = data.player))


##hierarchcial testing
data.playerz = na.omit(data.frame(cbind(scale(
  data.player[c("skintone","height","weight","age","wins.loss","goals")]),
                     data.player[c("position","totalReds","tonesplit")])))
m1 <- glm(totalReds ~ tonesplit+height+weight+age+wins.loss+goals+position
          , family = "poisson", data = data.playerz)
summary(m1)
fit <- step(m1)
summary(fit)
dispersiontest(fit)
dispersiontest(m1)
fit1=fit



##
##hierarchcial testing
data.playerz = na.omit(data.frame(cbind(scale(
  data.player[c("skintone","height","weight","age","wins.loss","goals")]),
  data.player[c("position","totalReds","tonesplit")])))
m1 <- glm.nb(totalReds ~ skintone+height+weight+age+wins.loss+goals+position
          , data = data.playerz)
summary(m1)
fit <- step(m1,scope=~.^2)
summary(fit)
dispersiontest(fit)
dispersiontest(m1)
fit2 = fit

m1 <- hurdle(totalReds ~ tonesplit+height+weight+age+wins.loss+goals+position
             ,dist='negbin', data = data.playerz)
summary(m1)
fit <- step(m1)
summary(fit)
dispersiontest(fit)
dispersiontest(m1)
fit3 = fit


AIC(fit3)
AIC(fit2)
AIC(fit1)

##neg bin is the winner
data.playerz = na.omit(data.frame(cbind(scale(
  data.player[c("skintone","height","weight","age","wins.loss","goals")]),
  data.player[c("pos","totalReds","tonesplit","games","country","totalYellows")])))
contrasts(data.playerz$pos) = contr.sum(length(levels(data.playerz$pos)))
colnames(contrasts(data.playerz$pos)) = levels(data.playerz$pos)[-length(levels(data.playerz$pos))]

contrasts(data.playerz$country) = contr.sum(length(levels(data.playerz$country)))
colnames(contrasts(data.playerz$country)) = levels(data.playerz$country)[-length(levels(data.playerz$country))]

m2 <-glm(totalReds ~ skintone+height+weight+age+wins.loss+goals+pos+country+offset(log(games)),
            family="poisson",data = data.playerz)
  
summary(m2)
fit <- step(m2,scope=~.^2,trace=0)
summary(fit)
dispersiontest(fit)
fit1 = fit

hist(data.playerz$skintone)
hist(data.playerz$height)
hist(data.playerz$weight)
hist(data.playerz$age)
hist(data.playerz$wins.loss)
hist(data.playerz$goals)

m1 <- glm.nb(totalReds ~ skintone+totalYellows+height+weight+age+
               wins.loss+goals+pos+country+offset(log(games))
           ,data = data.playerz)
 
summary(m1)
fit <- step(m1,scope=~.^2,trace=0)
summary(fit)

fit2 = fit
AIC(fit1)
AIC(fit2)
# nb wins
m1=fit

(est <- cbind(Estimate = coef(m1), confint(m1)))
exp(est)
sd(data.player$skintone,na.rm=T)

plot(resid(fit)~data.playerz$totalReds)
plot(data.playerz$skintone,data.playerz$totalReds)

x = xtabs(totalReds~tonesplit+country,data=data.playerz)
colSums(x)
94/384
60/274
47/341
28/479




m0 <-lm(totalReds ~skintone+totalYellows+height+weight+age+
           wins.loss+goals+pos+country+offset(log(games)),data = data.playerz)

fit0 <- step(m0,scope=~.^2,trace=0)

m1 <-glm(totalReds ~skintone+totalYellows+height+weight+age+
           wins.loss+goals+pos+country+offset(log(games)),
         family="poisson",data = data.playerz)

fit1 <- step(m1,scope=~.^2,trace=0)

m2 <- glm.nb(totalReds ~ skintone+totalYellows+height+weight+age+
                    wins.loss+goals+pos+country+offset(log(games))
                  ,data = data.playerz)

fit2 <- step(m2,scope=~.^2,trace=0)

m3 <-zeroinfl(totalReds ~ skintone+totalYellows+height+age+
                wins.loss+goals+pos+country+skintone:country+totalYellows:age+
                age:goals+totalYellows:pos+
                offset(log(games)),
                dist="poisson",data = data.playerz)

fit3 <- m3

m4 <-zeroinfl(totalReds ~ skintone+totalYellows+height+age+
                wins.loss+goals+pos+country+skintone:country+totalYellows:age+
                age:goals+
                offset(log(games)),
              dist="negbin",data = data.playerz)

fit4 <- m4

anova(fit1,fit2,fit3,fit4)

AIC(fit0)
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
summary(fit4)

(est <- cbind(Estimate = coef(m4), confint(m4)))
round(exp(est),2)
