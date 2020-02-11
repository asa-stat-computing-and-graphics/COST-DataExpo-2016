#####################################
## This file performs the modeling for severe injuries & death
## The file is essentially a copy/psate of fitBetaBinomFreq.R
## but we have changed the necessary variables
##
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
## Use the same model as for overall injury

nhtsa.severe <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ NHTSA + alchSpeed + highway,
                     sigma.fo = ~ isSpeed + highway,
                     data=acc.sev.data.trim,
                     family=BB(mu.link="logit"))


acc.sev.data.trim$NHTSA <- factor(acc.sev.data.trim$NHTSA, levels=c("2-Star", "1-Star", "3-Star", "4-Star", "5-Star"))
nhtsa.alt2 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.sev.data.trim,
                          family=BB(mu.link="logit"))

acc.sev.data.trim$NHTSA <- factor(acc.sev.data.trim$NHTSA, levels=c("3-Star", "1-Star", "2-Star", "4-Star", "5-Star"))
nhtsa.alt3 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.sev.data.trim,
                          family=BB(mu.link="logit"))

acc.sev.data.trim$NHTSA <- factor(acc.sev.data.trim$NHTSA, levels=c("4-Star", "1-Star", "2-Star", "3-Star", "5-Star"))
nhtsa.alt4 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.sev.data.trim,
                          family=BB(mu.link="logit"))

acc.sev.data.trim$NHTSA <- factor(acc.sev.data.trim$NHTSA, levels=c("5-Star", "1-Star", "2-Star", "3-Star", "4-Star"))
nhtsa.alt5 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ NHTSA + alchSpeed + highway,
                          sigma.fo = ~ isSpeed + highway,
                          data=acc.sev.data.trim,
                          family=BB(mu.link="logit"))



# The model with sigma as a function of NHTSA is slightly better by AIC
# but much worse by BIC. We'll go with the sigma not modeled
## I do not understand the y-axis on the above plot...

out <- tidy(nhtsa.severe)
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


p.nhtsa.severe <- ggplot(effect.data.nhtsa)+
  geom_point(aes(x=NHTSA, y=eff)) +
  geom_line(aes(x=NHTSA, y=eff)) + 
  geom_ribbon(aes(x=NHTSA, ymin=eff.lo, ymax=eff.up),alpha=0.3) +
  scale_x_continuous(labels=c("1-Star", "2-Star", "3-Star", "4-Star", "5-Star")) +
  ylim(c(0.03, 0.1) ) + 
  ylab("Probability of Severe Injury or Death") + 
  xlab("NHTSA Safety Rating") + 
  theme_classic()
p.nhtsa.severe

iihs.severe <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ IIHS + alchSpeed + highway,
                     sigma.fo = ~ isSpeed + highway,
                     data=acc.sev.data.trim,
                     family=BB(mu.link="logit"))


acc.sev.data.trim$IIHS <- factor(acc.sev.data.trim$IIHS, levels=c("Marginal", "Poor", "Acceptable", "Good"))
iihs.alt2 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ IIHS + alchSpeed + highway,
                         sigma.fo = ~ isSpeed + highway,
                         data=acc.sev.data.trim,
                         family=BB(mu.link="logit"))

acc.sev.data.trim$IIHS <- factor(acc.sev.data.trim$IIHS, levels=c("Acceptable", "Poor", "Marginal", "Good"))
iihs.alt3 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ IIHS + alchSpeed + highway,
                         sigma.fo = ~ isSpeed + highway,
                         data=acc.sev.data.trim,
                         family=BB(mu.link="logit"))

acc.sev.data.trim$IIHS <- factor(acc.sev.data.trim$IIHS, levels=c("Good", "Poor", "Marginal", "Acceptable"))
iihs.alt4 <- gamlss(cbind(NUMSEVINJ, NUMOCCS-NUMSEVINJ) ~ IIHS + alchSpeed + highway,
                         sigma.fo = ~ isSpeed + highway,
                         data=acc.sev.data.trim,
                         family=BB(mu.link="logit"))

out <- tidy(iihs.severe)
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

p.iihs.severe <- ggplot(effect.data.iihs)+
  geom_point(aes(x=IIHS, y=eff)) +
  geom_line(aes(x=IIHS, y=eff)) + 
  geom_ribbon(aes(x=IIHS, ymin=eff.lo, ymax=eff.up),alpha=0.3) +
  scale_x_continuous(labels=c("Poor", "Marginal", "Acceptable", "Good")) +
  ylim(c(0.03, 0.1) ) + 
  ylab("Probability of Severe Injury or Death") + 
  xlab("IIHS Crashworthiness Rating") + 
  theme_classic()
p.iihs.severe

ggsave("severePerformanceNHTSA.pdf", device="pdf", plot=p.nhtsa.severe, width=6, height=5, units="in")
ggsave("severePerformanceIIHS.pdf", device="pdf", plot=p.iihs.severe, width=6, height=5, units="in")

acc.data.trim %>% group_by(NHTSA) %>% summarize(avg=mean(prop), std=sd(prop))
acc.data.trim %>% group_by(IIHS) %>% summarize(avg=mean(prop), std=sd(prop))


acc.data.trim$NHTSA <- factor(acc.data.trim$NHTSA, levels=c("1-Star", "2-Star", "3-Star", "4-Star", "5-Star"))
acc.data.trim$IIHS <- factor(acc.data.trim$IIHS, levels=c("Poor", "Marginal", "Acceptable", "Good"))
colSums(xtabs(~IIHS+NHTSA, data=acc.data.trim))
rowSums(xtabs(~IIHS+NHTSA, data=acc.data.trim))
sum(colSums(xtabs(~IIHS+NHTSA, data=acc.data.trim)))
sum(rowSums(xtabs(~IIHS+NHTSA, data=acc.data.trim)))
xtabs(~IIHS+NHTSA, data=acc.data.trim)
