####################################
## This file contains the code to generate
## the plots included in the paper
## An Analysis of Crash-Safety ratings and the true assessment 
## of injury by vehicle

library(dplyr)
library(gamlss)
library(scales)
library(ggplot2)
library(boot)
library(broom)

source('dataProcess.R')

####
## Bubbble plots
## First, trim the data again, removing the single observation with 26 passengers
## Facet based on the NHTSA or IIHS rating
acc.data.trim.plot <- filter(acc.data.trim, NUMOCCS < 8)

plot.acc.numocc.hist <- ggplot(acc.data.trim.plot) +
  geom_histogram(aes(x=NUMOCCS), binwidth = 1) +
  xlab("Number of Occupants") + ylab("Count") + 
  scale_y_continuous(labels = comma, limits=c(0,150000),
                     breaks=c(50000,100000, 150000), minor_breaks = c(25000,75000,125000)) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme_classic()
plot.acc.numocc.hist
plot.acc.numocc.hist + scale_y_log10()

# ggsave("num_occ_distr.eps", device="eps", plot=plot.acc.numocc.hist)
ggsave("num_occ_distr.pdf", device="pdf", plot=plot.acc.numocc.hist, width=6, height=5, units="in")
# ggsave("num_occ_log_distr.eps", device="eps", plot=plot.acc.numocc.hist+scale_y_log10())

plot.acc.numinj.hist <- ggplot(acc.data.trim.plot) +
  geom_histogram(aes(x=NUM_INJV), binwidth = 1) +
  xlab("Number of Injuries") + ylab("Count") + 
  scale_y_continuous(labels = comma, limits=c(0,150000),
                     breaks=c(50000,100000, 150000), minor_breaks = c(25000,75000,125000)) +
  scale_x_continuous(breaks=seq(0,7,1)) + 
  theme_classic()
plot.acc.numinj.hist

plot.acc.numinj.hist + scale_y_log10()
# ggsave("num_inj_distr.eps", device="eps", plot=plot.acc.numinj.hist)
ggsave("num_inj_distr.pdf", device="pdf", plot=plot.acc.numinj.hist, width=6, height=5, units="in")
# ggsave("num_inj_log_distr.eps", device="eps", plot=plot.acc.numinj.hist+scale_y_log10())


acc.data.trim.plot <- filter(acc.data.trim, NUMOCCS < 7)
plot.acc.nhtsa <- ggplot(acc.data.trim.plot) + 
  geom_count(aes(x=NUMOCCS, y=NUM_INJV), alpha=0.75) +
  scale_size(breaks=c(5000,10000,15000,20000,25000,30000,35000,40000))+
  facet_grid(~NHTSA) + 
  scale_x_continuous(name ="Number of Occupants", breaks=c(seq(1,13,1) ), labels=paste(seq(1,13,1))  ) + 
  ylab("Numer of Injuries") +
  theme_classic()
plot.acc.nhtsa
# ggsave("nhtsaInjuryDistro.eps", device="eps", plot=plot.acc.nhtsa, width=8, units="in")
ggsave("injuryDistroNHTSA.pdf", device="pdf", plot=plot.acc.nhtsa, width=12, height=5, units="in")

plot.acc.iihs <- ggplot(acc.data.trim.plot) + 
  geom_count(aes(x=NUMOCCS, y=NUM_INJV), alpha=0.75) +
  scale_size(breaks=c(5000,10000,15000,20000,25000,30000,35000,40000))+
  facet_grid(~IIHS) + 
  scale_x_continuous(name ="Number of Occupants", breaks=c(seq(1,13,1) ), labels=paste(seq(1,13,1))  ) + 
  ylab("Numer of Injuries") +
  theme_classic()
plot.acc.iihs
ggsave("injuryDistroIIHS.pdf", device="pdf", plot=plot.acc.iihs, width=12, height=5, units="in")
#######################
## In this plot we see...
##
## Many zero injuries. Also see most of the time the occupant levels are small
## The likelihood of injury will need to be adjusted by number of occupants
## this contributes to the overdispersion, why we need Beta-Binomial

plot.acc2 <- ggplot(acc.data.trim) + 
  geom_jitter(aes(x=as.factor(NHTSA), y=prop), alpha=0.5 ) +
  scale_y_continuous(trans="log1p") +
  ylim(0, 1) + 
  theme_classic()
plot.acc2

library(alluvial)


acc.alluvial <- acc.data.trim %>%
  filter(NHTSA=="3-Star"|NHTSA=="4-Star"|NHTSA=="5-Star") %>%
  group_by(OCCCAT, NHTSA, highway,  alchSpeed, INJCAT ) %>%
  summarize(counts=n())
head(acc.alluvial)
names(acc.alluvial) <- c("Occupants", "NHTSA Safety Rating", "Road Type", "Alcohol or Speeding", "Injured", "counts")


pdf("injuredAlluvialNHTSA.pdf", width=16, height=12)
png("injuredAlluvialNHTSA.png", width=1360, heigh=1020)
alluvial(as.data.frame(acc.alluvial)[,c(1,2,3,4,5)], freq=acc.alluvial$counts,
         col = ifelse(acc.alluvial$Injured==0, "lightgrey", "red"),
         border = ifelse(acc.alluvial$Injured==0, "gray70", "red"),
         axis_labels=c("Occupants", "NHTSA Rating", "Road Type", "Alcohol or Speeding", "Injured")
)
dev.off()


acc.alluvial <- acc.data.trim %>%
  #filter(NHTSA=="3 Star"|NHTSA=="4 Star"|NHTSA=="5 Star") %>%
  group_by(OCCCAT, IIHS, highway,  alchSpeed, INJCAT ) %>%
  summarize(counts=n())
head(acc.alluvial)
names(acc.alluvial) <- c("Occupants", "IIHS Rating", "Road Type", "Alcohol or Speeding", "Injured", "counts")


pdf("injuredAlluvialIIHS.pdf", width=16, height=12)
png("injuredAlluvialIIHS.png", width=1360, heigh=1020)
alluvial(as.data.frame(acc.alluvial)[,c(1,2,3,4,5)], freq=acc.alluvial$counts,
         col = ifelse(acc.alluvial$Injured==0, "lightgrey", "red"),
         border = ifelse(acc.alluvial$Injured==0, "gray70", "red"),
         axis_labels=c("Occupants", "IIHS Rating", "Road Type", "Alcohol or Speeding", "Injured")
)
dev.off()




################################################## 
# severe injury or death
##################################################

acc.alluvial <- acc.sev.data.trim %>%
  filter(NHTSA=="3-Star"|NHTSA=="4-Star"|NHTSA=="5-Star") %>%
  group_by(OCCCAT, NHTSA, highway,  alchSpeed, INJCAT ) %>%
  summarize(counts=n())
head(acc.alluvial)

names(acc.alluvial) <- c("Occupants", "NHTSA Safety Rating", "Road Type", "Alcohol or Speeding", "Severe Injured", "counts")


pdf("severeAlluvialNHTSA.pdf", width=16, height=12)
alluvial(as.data.frame(acc.alluvial)[,c(1,2,3,4,5)], freq=acc.alluvial$counts,
         col = ifelse(acc.alluvial$`Severe Injured`==0, "lightgrey", "red"),
         border = ifelse(acc.alluvial$`Severe Injured`==0, "gray70", "red"),
         axis_labels=c("Occupants", "NHTSA Rating", "Road Type", "Alcohol or Speeding", "Severe Injury")
)
dev.off()

acc.alluvial <- acc.sev.data.trim %>%
  #filter(NHTSA=="3 Star"|NHTSA=="4 Star"|NHTSA=="5 Star") %>%
  group_by(OCCCAT, IIHS, highway,  alchSpeed, INJCAT ) %>%
  summarize(counts=n())
head(acc.alluvial)
names(acc.alluvial) <- c("Occupants", "IIHS Rating", "Road Type", "Alcohol or Speeding", "Severe Injured", "counts")

pdf("severeAlluvialIIHS.pdf", width=16, height=12)
alluvial(as.data.frame(acc.alluvial)[,c(1,2,3,4,5)], freq=acc.alluvial$counts,
         col = ifelse(acc.alluvial$`Severe Injured`==0, "lightgrey", "red"),
         border = ifelse(acc.alluvial$`Severe Injured`==0, "gray70", "red"),
         axis_labels=c("Occupants", "IIHS Rating", "Road Type", "Alcohol or Speeding", "Severe Injury")
)
dev.off()

library(tidyr)

acc.nhtsa.summary <- acc.sev.data.trim %>%
  group_by(NHTSA) %>%
  dplyr::summarise(Total=sum(NUMOCCS, na.rm=TRUE),
                   Non.Injured=sum(no.injured, na.rm=TRUE),
                   Moderate.Injured=sum(mod.injured, na.rm=TRUE),
                   Severe.Injured=sum(NUMSEVINJ, na.rm=TRUE),
                   Total.cars=n()) %>%
  mutate(Prop.non.injured=Non.Injured/Total,
         Prop.mod.injured=Moderate.Injured/Total,
         Prop.sev.injured=Severe.Injured/Total)

acc.nhtsa.summary <- acc.nhtsa.summary %>%
  gather(key=Severity, value=Proportion,
         Prop.non.injured, Prop.mod.injured, Prop.sev.injured) %>%
  dplyr::select(NHTSA, Severity, Proportion, Total.cars)


acc.iihs.summary <- acc.sev.data.trim %>%
  group_by(IIHS) %>%
  dplyr::summarise(Total=sum(NUMOCCS, na.rm=TRUE),
                   Non.Injured=sum(no.injured, na.rm=TRUE),
                   Moderate.Injured=sum(mod.injured, na.rm=TRUE),
                   Severe.Injured=sum(NUMSEVINJ, na.rm=TRUE),
                   Total.cars=n() ) %>%
  mutate(Prop.non.injured=Non.Injured/Total,
         Prop.mod.injured=Moderate.Injured/Total,
         Prop.sev.injured=Severe.Injured/Total)

acc.iihs.summary <- acc.iihs.summary %>%
  gather(key=Severity, value=Proportion,
         Prop.non.injured, Prop.mod.injured, Prop.sev.injured) %>%
  dplyr::select(IIHS, Severity, Proportion, Total.cars)


acc.nhtsa.summary$Rating.System <- "NHTSA"
acc.iihs.summary$Rating.System <- "IIHS"
names(acc.nhtsa.summary)[1] <- "Rating"
names(acc.iihs.summary)[1] <- "Rating"

acc.severity.summary <- rbind(acc.nhtsa.summary, acc.iihs.summary)

levels(acc.severity.summary$Rating) <- c("1-Star", "2-Star", "3-Star", "4-Star", "5-Star", 
                                         "Poor", "Marginal", "Acceptable", "Good")
acc.severity.summary$Severity <- as.factor(acc.severity.summary$Severity)
acc.severity.summary$Severity <- factor(acc.severity.summary$Severity,
                                     levels=c("Prop.non.injured", "Prop.mod.injured", "Prop.sev.injured"),
                                     labels=c("No Injuries", "Moderate Injuries", "Severe Injury or Death") )
acc.severity.summary$Rating.System <- factor(acc.severity.summary$Rating.System,
                                             levels=c("NHTSA", "IIHS"))
prop.injury.distro <- ggplot(acc.severity.summary, aes(x=Rating, y=Proportion, fill=Severity, width=Total.cars/47500)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ Rating.System, scales="free", space="free") + 
  theme_classic() +
  scale_fill_brewer(palette="Greys") +
  labs(fill="Injury Severity:", x="") + 
  theme(legend.position = "bottom")
prop.injury.distro
ggsave("injurySeverityDistro2.pdf", device="pdf", plot=prop.injury.distro, width=12, height=5, units="in")
