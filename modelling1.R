# source("DataPrep.R")

# OLS Full model
m0 <-lm(totalReds~skintone+totalYellows+height+age+
          wins.loss+goals+pos+country+skintone:country+offset(log(games)),data = data.playerz)

# OlS backwards and forwards stepwise fit by AIC testing all 2 way interactions
# fit0 <- step(m0,scope=~.^2,trace=0)

# Poisson Full model
m1 <-glm(totalReds ~skintone+totalYellows+height+age+
           wins.loss+goals+pos+country+skintone:country++offset(log(games)),
         family="poisson",data = data.playerz)

# Poisson backwards and forwards stepwise fit by AIC testing all 2 way interactions
# fit1 <- step(m1,scope=~.^2,trace=0)

# Negative binomial Full model
m2 <- glm.nb(totalReds ~ skintone+totalYellows+height+age+
               wins.loss+goals+pos+country+skintone:country++offset(log(games))
                  ,data = data.playerz)

# Negative binomial backwards and forwards stepwise fit by AIC testing all 2 way interactions
# fit2 <- step(m2,scope=~.^2,trace=0)

# Zero-inflated Poisson, no step function compatability so predictors are determined via 
# best fit stanard Poisson model
m3 <-zeroinfl(totalReds ~ skintone+totalYellows+height+age+
                wins.loss+goals+pos+country+skintone:country+
                
                offset(log(games)),
                dist="poisson",data = data.playerz)

# fit3 <- m3
# Zero-inflated negative binomial, no step function compatability so predictors are determined via 
# best fit standard negbin model.  However, there were optimization problems so any non-significant
# two way interactions were removed to aid convergence

m4 <-zeroinfl(totalReds ~ skintone+totalYellows+height+age+
                wins.loss+goals+pos+country+skintone:country+
                offset(log(games)),
              dist="negbin",data = data.playerz)

# fit4 <- m4

# Compare AIC
# AIC(fit0,fit1,fit2,fit3,fit4)
AIC(m0,m1,m2,m3,m4)


# Zero inflateded neg bin appears most parsimonious of the five models tested

# look at final model estimates transformed into incident rates
# est <- cbind(Estimate = coef(fit4), confint(fit4))
# round(exp(est),2)
