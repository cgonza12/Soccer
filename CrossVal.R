source("DataPrep.R")
source("modelling1.R")

# hold iteration output of coefficients
thetalist = list()
# hold iteration output of AIC and prediction error
fitlist = list()
# number of iterations
nReps = 500

# start progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(nReps)

for( i in 1:nReps){
  # train model on 80% of data
  trainData = sample(1:nrow(data.playerz),nrow(data.playerz)*.80)
  # validate model on remaining 20%
  valData = setdiff(1:nrow(data.playerz),trainData)
  
  trainData <- data.playerz[trainData,]
  valData  <- data.playerz[valData,]
  
  # run neg bin model, zero inflated model is too complex for smaller data subsets :,,(
  tempmodel = glm.nb(totalReds ~ skintone+totalYellows+height+age+
                          wins.loss+goals+pos+country+skintone:country+
                          offset(log(games)),
                          data = trainData)
  # grab coefs and CIs
  est <- exp(cbind(Estimate = coef(tempmodel), confint(tempmodel)))
  #save to list
  thetalist[[i]]  <- est
  
  # predict 20% of witheld data with temp model
  prediction = predict(tempmodel,valData[-8])
  
  # compute root mean squared prediction error
  RMSEi = sqrt(mean((prediction - valData[8])^2))
  # get models AIC value
  AICi = AIC(tempmodel)
  # save to fit list
  fitlist[[i]] = cbind(RMSEi,AICi)
  progress.bar$step()
  
}

# combine list of iteration output
thetadf = do.call(rbind,thetalist)
iteration = rep(1:nReps,each=16)
thetadf = cbind(thetadf,iteration)
predictors = as.character(row.names(thetadf))
thetadf = as.data.frame(thetadf)
thetadf$predictor = predictors

fitdf = data.frame(do.call(rbind,fitlist))

write.csv(thetadf,"CrossValthetaout.csv")
write.csv(thetadf,"CrossValfitout.csv")

hist(fitdf$AICi)
hist(fitdf$RMSEi)

AIC(m2)/nrow(data.playerz)
mean(fitdf$AICi/(nrow(data.playerz)*.80))
hist(fitdf$AICi/(nrow(data.playerz)*.80))

mean(fitdf$RMSEi)
sqrt(mean(residuals(m2)^2))


skintoneest = thetadf[thetadf$predictor=="skintone",]
hist(skintoneest$Estimate)
mean(skintoneest$Estimate)
quantile(skintoneest$Estimate,c(.025,.975))

hist(skintoneest$'2.5 %')
sum(skintoneest$'2.5 %'<1)/nReps
quantile(skintoneest$'2.5 %',c(.025,.975))

skintoneint = thetadf[thetadf$predictor=="skintone:countryGermany",]
hist(skintoneint$Estimate)
hist(skintoneint$'2.5 %')
sum(skintoneint$'2.5 %'<1)/nReps

round(exp(cbind(Estimate = coef(m2), confint(m2))),2)
