# FMSN40project2
Project 2 in the course FMSN40 at Lund University

# Project 2 

# Load data
hospital <- read.delim("Data/hospital.txt", sep = ";")
head(hospital)
summary(hospital)
hospital$hosp_cat <- factor(hospital$hosp,
                          levels = c(0, 1),
                          labels = c("0 days", "1+ days"))
library(ggplot2)
library(xtable)
ggplot(hospital, aes(x = age, y = hosp, color = hosp_cat)) + geom_point(size = 1)

#### Part 1a #### 
table(hospital$health, hospital$hosp)
prop.table(table(hospital$health, hospital$hosp), margin = 1)

# Turning the categorical variables into factors
hospital$health <- factor(hospital$health,
                        levels = c(1, 2, 3),
                        labels = c("Good", "Bad", "Somewhere in between"))
table(hospital$health)

# Fit a model to health
model.health <- glm(hosp ~ health, family = "binomial", data = hospital)
model.health$coefficients
summary(model.health)

# beta: log-odds(ratio) with c.i.:
model.health$coefficients
(ci.beta <- confint(model.health))

# Odds (exp(beta0)) and OR, odds ratio, exp(beta1)
(oddsratio <- exp(model.health$coefficients))
(ci.or <- exp(ci.beta))

# McFaddens Pseudo R2
# Null model
(model.0 <- glm(hosp ~ 1, family = "binomial", data = hospital))
(lnL0 <- logLik(model.0))

bic <- BIC(model.0, model.health)
aic <- AIC(model.0, model.health)

(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(model.0)[1],
    logLik(model.health)[1])

(collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0)
(collect.AIC)

# compare health model with null model using deviance and chi2
(sum.health <- summary(model.health))

# Deviance to compare with chi2 quantile
(D_diff <- sum.health$null.deviance - sum.health$deviance)
qchisq(1 - 0.05, df_diff)

# P-value to compare with alpha
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Predict the probability of having at least one day in the hospital 
phat = cbind(
  hospital, 
  predict(model.health, type = "response"))
head(phat)



#### Part 1b ####
ggplot(hospital, aes(x = age, y = hosp)) + geom_point(size = 1) +
  geom_smooth(size = 0.5) +
  labs(title = "Hospital 1+ days (=1) or Hospital 0 days (=0) vs age") +
  theme(text = element_text(size = 10))

# Fit a log model for hosp as a function of age 
(model.age <- glm(hosp ~ age, family = "binomial", data = hospital))
exp(model.age$coefficients)
confint(model.age)
exp(confint(model.age))

# McFaddens Pseudo R2, AIC, BIC
(model.0 <- glm(hosp ~ 1, family = "binomial", data = hospital))
(lnL0 <- logLik(model.0))

bic <- BIC(model.0, model.age)
aic <- AIC(model.0, model.age)

(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(model.0)[1],
    logLik(model.age)[1])

(collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0)
(collect.AIC)

# Test to see if age is significant using a Wald test
(sum.age <- summary(model.age))

# Odds of having at least one day in hospital - when age increase by 1 and 5 years
(x1 <- exp(sum.age$coefficients[2]))
(cix1 <- exp(confint(model.age)))
(x5 <- x1^5)
(cix5 <-cix1^5)

# predict for plotting#
# phat = estimated probabilities p
hosp.pred <- cbind(
  hospital,
  phat = predict(model.age, type = "response"))

ggplot(hosp.pred, aes(age, hosp)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  xlab("age") +
  ylab("hospital") +
  labs(title = "hospital (=1) or Not hospital (=0) vs age",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 10))

# logit = logodds with s.e. for constructing C.I.
hosp.pred <- cbind(
  hosp.pred,
  logit = predict(model.age, se.fit = TRUE))
head(hosp.pred)
# An unnecessary variable:
hosp.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds#
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
hosp.pred$logit.lwr <- hosp.pred$logit.fit - lambda*hosp.pred$logit.se.fit
hosp.pred$logit.upr <- hosp.pred$logit.fit + lambda*hosp.pred$logit.se.fit
head(hosp.pred)

# transform the log-odds intervals into C.I. for odds#
hosp.pred$odds.lwr <- exp(hosp.pred$logit.lwr)
hosp.pred$odds.upr <- exp(hosp.pred$logit.upr)
head(hosp.pred)

# transform the odds intervals into C.I. for p#
hosp.pred$p.lwr <- hosp.pred$odds.lwr/(1 + hosp.pred$odds.lwr)
hosp.pred$p.upr <- hosp.pred$odds.upr/(1 + hosp.pred$odds.upr)
head(hosp.pred)

# plot the intervals:
ggplot(hosp.pred, aes(age, hosp)) +
  geom_point(size = 1) +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Age") +
  ylab("Hospital") +
  labs(title = "Hospital 1+ days (=1) or Hospital 0 days (=0) vs age",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 10))

#### Part 1c ####
# Fit a log model for hosp as a function of age-squared
(model.sqrt.age <- glm(hosp ~ age + I(age^2), family = "binomial", data = hospital))
beta <- model.sqrt.age$coefficients
(betas.age.sq <- data.frame(beta))
betas.age.sq$expbeta <- c(exp(model.sqrt.age$coefficients))
betas.age.sq$ci <- confint(model.sqrt.age)
betas.age.sq$expci <- exp(confint(model.sqrt.age))
betas.age.sq


# McFaddens Pseudo R2, AIC, BIC
(model.0 <- glm(hosp ~ 1, family = "binomial", data = hospital))
(lnL0 <- logLik(model.0))

bic <- BIC(model.0, model.sqrt.age)
aic <- AIC(model.0, model.sqrt.age)

(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(model.0)[1],
    logLik(model.sqrt.age)[1])

(collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0)
(collect.AIC)


# Test sigificance using a Ward test
summary(model.sqrt.age)



# predict for plotting#
# phat = estimated probabilities p
hosp.pred.sq <- cbind(
  hospital,
  phat = predict(model.age, type = "response"),
  phat.sq = predict(model.sqrt.age, type = "response"))

ggplot(hosp.pred.sq, aes(age, hosp)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5) +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  geom_line(aes(y = phat.sq), color = "green", size = 0.5) +
  xlab("age") +
  ylab("hospital") +
  labs(title = "hospital (=1) or Not hospital (=0) vs age",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 10))

# logit = logodds with s.e. for constructing C.I.
hosp.pred.sq <- cbind(
  hosp.pred.sq,
  logit = predict(model.sqrt.age, se.fit = TRUE))
head(hosp.pred.sq)
# An unnecessary variable:
hosp.pred.sq$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds#
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
hosp.pred.sq$logit.lwr <- hosp.pred.sq$logit.fit - lambda*hosp.pred.sq$logit.se.fit
hosp.pred.sq$logit.upr <- hosp.pred.sq$logit.fit + lambda*hosp.pred.sq$logit.se.fit
head(hosp.pred.sq)

# transform the log-odds intervals into C.I. for odds#
hosp.pred.sq$odds.lwr <- exp(hosp.pred.sq$logit.lwr)
hosp.pred.sq$odds.upr <- exp(hosp.pred.sq$logit.upr)
head(hosp.pred.sq)

# transform the odds intervals into C.I. for p#
hosp.pred.sq$p.lwr.sq <- hosp.pred.sq$odds.lwr/(1 + hosp.pred.sq$odds.lwr)
hosp.pred.sq$p.upr.sq <- hosp.pred.sq$odds.upr/(1 + hosp.pred.sq$odds.upr)
hosp.pred.sq$p.upr <- hosp.pred$p.upr
hosp.pred.sq$p.lwr <- hosp.pred$p.lwr
head(hosp.pred.sq)

# plot the intervals:
ggplot(hosp.pred.sq, aes(age, hosp)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  geom_line(aes(y = phat.sq), color = "green", size = 0.5) +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.1) +
  geom_ribbon(aes(ymin = p.lwr.sq, ymax = p.upr.sq), alpha = 0.3) +
  xlab("Age") +
  ylab("Hospital") +
  labs(title = "Hospital 1+ days (=1) or Hospital 0 days (=0) vs age",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 10))



