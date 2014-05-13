source("DataPrep.R")
##################### Look at some descriptive plots of DV distribution #################


# data is very skewed
hist(data.player$totalReds)
hist(data.player$totalyellowReds)
hist(data.player$totalYellows)


# very skewed, way more light skinned players than dark
hist(data.player$skintone)

table(data.player$tonesplit)

# compare Red card distributions between skin tones
ggplot(data.player[data.player$tonesplit!='neither',],aes(x=totalReds,fill=tonesplit))+
  geom_density(alpha=.5)

median(data.player$totalReds[data.player$tonesplit=="light"],na.rm=T)
median(data.player$totalReds[data.player$tonesplit=="dark"],na.rm=T)

mean(data.player$totalReds[data.player$tonesplit=="light"],na.rm=T)
mean(data.player$totalReds[data.player$tonesplit=="dark"],na.rm=T)
