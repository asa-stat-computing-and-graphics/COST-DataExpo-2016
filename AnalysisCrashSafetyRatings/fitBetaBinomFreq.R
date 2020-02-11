#####################################
## This file performs the modeling for injuries
## There is also a few plots generated from the model output.
## We also have in the model selection.

library(dplyr)
library(gamlss)
library(ggplot2)
library(boot)
library(broom)

source("dataProcess.R")

########################################
## 
## Below is my model fitting
## First, Beta-Binomial with NHTSA as a predictor
nhtsa.fit1 <-  gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
## Now let the variance term also be influenced by the treatement
nhtsa.fit2 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA,
                     sigma.fo = ~ NHTSA,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
## Add on whether alcohol is involved
nhtsa.fit3 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + VEH_ALCH + isSpeed + highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
## I tried to interact NHTSA and alcohol, it has no effect
## Here we take on speeding
nhtsa.fit4 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + VEH_ALCH + isSpeed + highway,
                     sigma.fo = ~ NHTSA+ VEH_ALCH+ isSpeed + highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
## And now alcohol and speeding
nhtsa.fit5 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + VEH_ALCH + isSpeed + highway +
                       NHTSA*VEH_ALCH + NHTSA*isSpeed + NHTSA*highway + 
                       VEH_ALCH*isSpeed + VEH_ALCH*highway + isSpeed*highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))

nhtsa.fit6 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway + isSpeed*highway,
                     sigma.fo = ~ alchSpeed + highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))


nhtsa.fit7 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway + isSpeed*highway,
                     sigma.fo = ~ isSpeed + highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))

nhtsa.fit8 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway,
                     sigma.fo = ~ isSpeed + highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))

GAIC(nhtsa.fit1, nhtsa.fit2,
     nhtsa.fit3, nhtsa.fit4, nhtsa.fit5,
     nhtsa.fit6, nhtsa.fit7,nhtsa.fit8,
     k=0)   # Likelihood functions
GAIC(nhtsa.fit1, nhtsa.fit2,
     nhtsa.fit3, nhtsa.fit4, nhtsa.fit5,
     nhtsa.fit6, nhtsa.fit7,nhtsa.fit8,
     k=2)   # AIC values
GAIC(nhtsa.fit1, nhtsa.fit2, 
     nhtsa.fit3, nhtsa.fit4, nhtsa.fit5, 
     nhtsa.fit6, nhtsa.fit7,nhtsa.fit8,
     k=log(dim(acc.data.trim)[1]))  # BIC values


acc.data.trim$NHTSA <- factor(acc.data.trim$NHTSA, levels=c("2-Star", "1-Star", "3-Star", "4-Star", "5-Star"))
nhtsa.fit8.alt2 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.data.trim,
                          family=BB(mu.link="logit"))

acc.data.trim$NHTSA <- factor(acc.data.trim$NHTSA, levels=c("3-Star", "1-Star", "2-Star", "4-Star", "5-Star"))
nhtsa.fit8.alt3 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.data.trim,
                          family=BB(mu.link="logit"))

acc.data.trim$NHTSA <- factor(acc.data.trim$NHTSA, levels=c("4-Star", "1-Star", "2-Star", "3-Star", "5-Star"))
nhtsa.fit8.alt4 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.data.trim,
                          family=BB(mu.link="logit"))

acc.data.trim$NHTSA <- factor(acc.data.trim$NHTSA, levels=c("5-Star", "1-Star", "2-Star", "3-Star", "4-Star"))
nhtsa.fit8.alt5 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.data.trim,
                          family=BB(mu.link="logit"))



# The model with sigma as a function of NHTSA is slightly better by AIC
# but much worse by BIC. We'll go with the sigma not modeled
term.plot(nhtsa.fit8, se=TRUE)
## I do not understand the y-axis on the above plot...

out <- tidy(nhtsa.fit8)
mu1 <- out$estimate[1] + out$estimate[2]
lo.fit <- out$estimate - 2.576*out$std.error
up.fit <- out$estimate + 2.576*out$std.error
mu.fit <- out$estimate

eff <- c(mu.fit[1], mu.fit[1]+mu.fit[2], mu.fit[1]+mu.fit[3], mu.fit[1]+mu.fit[4], mu.fit[1]+mu.fit[5])
eff.lo <- c(lo.fit[1], lo.fit[1]+lo.fit[2], lo.fit[1]+lo.fit[3],
            lo.fit[1]+lo.fit[4], lo.fit[1]+lo.fit[5])
eff.up <- c(up.fit[1], up.fit[1]+up.fit[2], up.fit[1]+up.fit[3],
            up.fit[1]+up.fit[4], up.fit[1]+up.fit[5])

effect.data.nhtsa <- data.frame(NHTSA = 1:5, eff=inv.logit(eff), eff.lo=inv.logit(eff.lo), eff.up=inv.logit(eff.up) )


p.nhtsa <- ggplot(effect.data.nhtsa)+
  geom_point(aes(x=NHTSA, y=eff)) +
  geom_line(aes(x=NHTSA, y=eff)) + 
  geom_ribbon(aes(x=NHTSA, ymin=eff.lo, ymax=eff.up),alpha=0.3) +
  scale_x_continuous(labels=c("1-Star", "2-Star", "3-Star", "4-Star", "5-Star")) +
  ylim(c(0.2, 0.4) ) + ylab("Probability of Injury") + xlab("NHTSA Safety Rating") + 
  theme_classic()
p.nhtsa

iihs.fit1 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ (as.factor(IIHS)),
                 data=acc.data.trim,
                 family=BB(mu.link="logit"))
iihs.fit2 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ (as.factor(IIHS)),
                    sigma.fo = ~as.factor(IIHS),
                    data=acc.data.trim,
                    family=BB(mu.link="logit"))
iihs.fit3 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ (as.factor(IIHS)) + VEH_ALCH +as.factor(NHTSA)*VEH_ALCH,
                     sigma.fo = ~ as.factor(NHTSA)+VEH_ALCH,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
iihs.fit4 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ (as.factor(IIHS)) + VEH_ALCH + isSpeed,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
iihs.fit5 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ (as.factor(IIHS)) + VEH_ALCH + isSpeed +VEH_ALCH*isSpeed,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
iihs.fit7 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ IIHS + alchSpeed + highway + alchSpeed*highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))
iihs.fit8 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ IIHS + alchSpeed + highway,
                     sigma.fo = ~ isSpeed + highway,
                     data=acc.data.trim,
                     family=BB(mu.link="logit"))


acc.data.trim$IIHS <- factor(acc.data.trim$IIHS, levels=c("Marginal", "Poor", "Acceptable", "Good"))
iihs.fit8.alt2 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ IIHS + alchSpeed + highway,
                         sigma.fo = ~ isSpeed + highway,
                         data=acc.data.trim,
                         family=BB(mu.link="logit"))

acc.data.trim$IIHS <- factor(acc.data.trim$IIHS, levels=c("Acceptable", "Poor", "Marginal", "Good"))
iihs.fit8.alt3 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ IIHS + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.data.trim,
                          family=BB(mu.link="logit"))

acc.data.trim$IIHS <- factor(acc.data.trim$IIHS, levels=c("Good", "Poor", "Marginal", "Acceptable"))
iihs.fit8.alt4 <- gamlss(cbind(NUM_INJV, NUMOCCS-NUM_INJV) ~ IIHS + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.data.trim,
                          family=BB(mu.link="logit"))

out <- tidy(iihs.fit8)
mu1 <- out$estimate[1] + out$estimate[2]
lo.fit <- out$estimate - 2.576*out$std.error
up.fit <- out$estimate + 2.576*out$std.error
mu.fit <- out$estimate

eff <- c(mu.fit[1], mu.fit[1]+mu.fit[2], mu.fit[1]+mu.fit[3], mu.fit[1]+mu.fit[4])
eff.lo <- c(lo.fit[1], lo.fit[1]+lo.fit[2], lo.fit[1]+lo.fit[3],
            lo.fit[1]+lo.fit[4])
eff.up <- c(up.fit[1], up.fit[1]+up.fit[2], up.fit[1]+up.fit[3],
            up.fit[1]+up.fit[4])

effect.data.iihs <- data.frame(IIHS = 1:4, eff=inv.logit(eff), eff.lo=inv.logit(eff.lo), eff.up=inv.logit(eff.up) )

p.iihs <- ggplot(effect.data.iihs)+
  geom_point(aes(x=IIHS, y=eff)) +
  geom_line(aes(x=IIHS, y=eff)) + 
  geom_ribbon(aes(x=IIHS, ymin=eff.lo, ymax=eff.up),alpha=0.3) +
  scale_x_continuous(labels=c("Poor", "Marginal", "Acceptable", "Good")) +
  ylim(c(0.2, 0.4) ) + ylab("Probability of Injury") + xlab("IIHS Crashworthiness Rating") + 
  theme_classic()
p.iihs

ggsave("injuryPerformanceNHTSA.pdf", device="pdf", plot=p.nhtsa, width=6, height=5, units="in")
ggsave("injuryPerformanceIIHS.pdf", device="pdf", plot=p.iihs, width=6, height=5, units="in")

GAIC(iihs.fit1, iihs.fit2,
     iihs.fit3, iihs.fit4, iihs.fit5,
     k=0)   # Likelihood functions
GAIC(iihs.fit1, iihs.fit2,
     iihs.fit3, iihs.fit4, iihs.fit5,
     k=2)   # AIC values
GAIC(iihs.fit1, iihs.fit2,
     iihs.fit3, iihs.fit4, iihs.fit5,
     k=log(dim(acc.data.trim)[1]))  # BIC values

# The model with sigma as a function of NHTSA is slightly better by AIC
# but much worse by BIC. We'll go with the sigma not modeled
tidy(iihs.fit4)
tidy(iihs.fit5)  # the sigma terms are not significant...
term.plot(iihs.fit4, se=TRUE)
fittedPlot(iihs.fit1, x=acc.data.trim$IIHS)

acc.data.trim %>% group_by(NHTSA) %>% summarize(avg=mean(prop), std=sd(prop))
acc.data.trim %>% group_by(IIHS) %>% summarize(avg=mean(prop), std=sd(prop))




