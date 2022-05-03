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
