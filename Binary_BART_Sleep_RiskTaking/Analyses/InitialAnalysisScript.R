
# Initial Analysis Script To Analyze OneShotBART data #
library(tidyverse)
library(here)
library(R.utils)
library(patchwork)

# first look at the current data to understand 
CurrentData <- read_csv("CurrentData.csv")
#View(CurrentData)
# look at initial data to understand
InitialData <- read_csv("InitialData.csv")
#View(InitialData)


# Initial Data Cleaned Up -------------------------------------------------


#get initial data cleaned up

InitialData$PopProb<-as.numeric(InitialData$PopProb)

# remove redundant participants
InitialData<-filter(InitialData,run_id<89)


# now get the filtering variable for the AttCheck

# Filter Initial Analyses  ------------------------------------------------

AttentionCheckVec<- c("images/AttentionCheckL.png","images/AttentionCheckR.png")

AttentionCheckTrials <- InitialData %>% 
  select(stimulus, participant, key_press) %>% 
  filter(stimulus %in% AttentionCheckVec)

# now divide into left and right so that you can then designate correct

AttentionCheckLeft <- filter(AttentionCheckTrials, AttentionCheckTrials$stimulus=="images/AttentionCheckL.png")
AttentionCheckRight <- filter(AttentionCheckTrials, AttentionCheckTrials$stimulus=="images/AttentionCheckR.png")

AttentionCheckLeft$correct <- ifelse(AttentionCheckLeft$key_press=="37",1,0)
AttentionCheckRight$correct <- ifelse(AttentionCheckRight$key_press=="39",1,0)

# then bring back together

AttentionCheckTrials <-rbind(AttentionCheckLeft,AttentionCheckRight)

AttentionCheckAccuracy <- AttentionCheckTrials %>% 
  group_by(participant) %>% 
  summarise(Accuracy=mean(correct))


#test to see what the raw accuracy looks like
mean(AttentionCheckAccuracy$Accuracy)

#filter out participant data that does not meet criterion on Attention Check Trials
AttentionCriterion <- filter(AttentionCheckAccuracy, Accuracy >= .75)

InitialData <- filter(InitialData, participant %in% AttentionCriterion$participant)


# Look at main Trial Behavior for Initial Analyses ------------------------


MainTrials <- InitialData %>% 
  select(run_id,participant,stimulus,uncertain,key_press,PopProb, rt, trial_index, popped) %>% 
  arrange(participant,PopProb) %>% 
  filter(stimulus=="images/redBalloon.png")

# add in size variable to the initial participants

Pops<-c(1:200)+150
Pops<-sort(c(Pops,Pops))
Pops<-rep(Pops,35)

MainTrials$size<-Pops

#33 participants#



# Filter the Current Analyses ---------------------------------------------


# Filter Current Analyses
AttentionCheckVec<- c("images/AttentionCheckL.png","images/AttentionCheckR.png")

AttentionCheckTrials <- CurrentData %>% 
  select(stimulus, participant, key_press) %>% 
  filter(stimulus %in% AttentionCheckVec)

# now divide into left and right so that you can then designate correct

AttentionCheckLeft <- filter(AttentionCheckTrials, AttentionCheckTrials$stimulus=="images/AttentionCheckL.png")
AttentionCheckRight <- filter(AttentionCheckTrials, AttentionCheckTrials$stimulus=="images/AttentionCheckR.png")

AttentionCheckLeft$correct <- ifelse(AttentionCheckLeft$key_press=="37",1,0)
AttentionCheckRight$correct <- ifelse(AttentionCheckRight$key_press=="39",1,0)

# then bring back together

AttentionCheckTrials <-rbind(AttentionCheckLeft,AttentionCheckRight)

AttentionCheckAccuracy <- AttentionCheckTrials %>% 
  group_by(participant) %>% 
  summarise(Accuracy=mean(correct))


#test to see what the raw accuracy looks like
mean(AttentionCheckAccuracy$Accuracy)

#filter out participant data that does not meet criterion on Attention Check Trials
AttentionCriterion <- filter(AttentionCheckAccuracy, Accuracy >= .75)

CurrentData <- filter(CurrentData, participant %in% AttentionCriterion$participant)



# Look at Main Trial Behavior for Current Analyses ------------------------

CurrentData$PopProb<-as.numeric(CurrentData$PopProb)

MainTrialsCurrent <- CurrentData %>% 
  select(run_id, participant,stimulus,uncertain,key_press,PopProb, rt, trial_index, popped,size) %>% 
  arrange(participant,PopProb) %>% 
  filter(stimulus=="images/redBalloon.png")




# Combine the data sets ---------------------------------------------------
MainTrialsCombo<-rbind(MainTrials,MainTrialsCurrent)
MainTrials<-MainTrialsCombo


MainTrialsCertain<-filter(MainTrials, uncertain=="false")
MainTrialsCertain$points<-5

MainTrialsUncertain<-filter(MainTrials, uncertain=="true")
MainTrialsUncertain$points<-ifelse(MainTrialsUncertain$popped=="true",0,10)

MainTrials <- rbind(MainTrialsUncertain,MainTrialsCertain)

MainTrials$ChooseUncertain <- ifelse(MainTrials$uncertain=="true",1,0)
MainTrials$rt<-as.numeric(MainTrials$rt)

#TrialIndexSummary <- MainTrials %>% 
#  group_by(run_id,participant) %>% 
#  summarise(Min=min(trial_index),Max=max(trial_index))

# Overall Measures
ParticipantSummary <- MainTrials %>% 
  group_by(run_id,participant) %>% 
  summarise(Points=sum(points),RT=mean(rt),RTsd=sd(rt),PropUncertain=mean(ChooseUncertain),TotalUncertain=sum(ChooseUncertain))
  
ParticipantSummary <- ParticipantSummary %>% 
  filter(TotalUncertain > 1)

#create easy variable to break it down into blocks
Index<-rep(c(1:400),length(unique(MainTrials$participant)))
Index2<-rep(c(1:400),3)

Index<-c(Index, Index2)

MainTrials<-MainTrials %>% 
  arrange(run_id,participant,trial_index) %>% 
  group_by(run_id,participant) %>%
  mutate(lag_Uncertain = dplyr::lag(uncertain, n = 1, default = NA),lag_Popped = dplyr::lag(popped, n = 1, default = NA),lag_size = dplyr::lag(size, n = 1, default = NA),lag_rt = dplyr::lag(rt, n = 1, default = NA))

MainTrials$Index<-Index


#create variable to judge whether last balloon was bigger or smaller than the current one
MainTrials$RelativeSize<-ifelse(MainTrials$size>MainTrials$lag_size,"Bigger","Smaller")

MainTrials$Block<-ifelse(MainTrials$Index>200,2,1)

# Break participant measures down by Block
ParticipantSummaryPopped <- MainTrials %>% 
  group_by(run_id,participant,lag_Popped) %>% 
  summarise(Points=sum(points),RT=mean(rt),RTsd=sd(rt),PropUncertain=mean(ChooseUncertain),TotalUncertain=sum(ChooseUncertain))




ParticipantSummaryPopped <- na.omit(ParticipantSummaryPopped)


PoppedSum<-subset(ParticipantSummaryPopped,ParticipantSummaryPopped$lag_Popped=="true")



NotPoppedSum<-subset(ParticipantSummaryPopped,ParticipantSummaryPopped$lag_Popped=="false")

NotPoppedSum <- NotPoppedSum %>% 
  filter(run_id != 136)

NotPoppedSum  <- filter(NotPoppedSum, participant %in% PoppedSum$participant)


PopDiff <-NotPoppedSum$PropUncertain - PoppedSum$PropUncertain
RTDiff <-NotPoppedSum$RT - PoppedSum$RT
RTsdDiff <- NotPoppedSum$RTsd - PoppedSum$RTsd



ParticipantSummary$PopDiff <- PopDiff
ParticipantSummary$RTDiff  <- RTDiff
ParticipantSummary$RTsdDiff<- RTsdDiff

ggplot(ParticipantSummaryPopped, aes(x=as.factor(lag_Popped), y=Points, fill=as.factor(RelativeSize))) + 
  geom_boxplot()

ggplot(ParticipantSummaryPopped, aes(x=as.factor(lag_Popped), y=log(RT), fill=as.factor(RelativeSize))) + 
  geom_boxplot()

ggplot(ParticipantSummaryPopped, aes(x=as.factor(lag_Popped), y=log(RTsd), fill=as.factor(RelativeSize))) + 
  geom_boxplot()

ggplot(ParticipantSummaryPopped, aes(x=as.factor(lag_Popped), y=PropUncertain, fill=as.factor(RelativeSize))) + 
  geom_boxplot(notch=TRUE) +
  ggtitle("The Effect of Previous Trial Outcome on the Current Trial Decision" ) +
  xlab(" Did the balloon pop on the last trial?") +
  ylab("Inflation Proportion") +
  theme(plot.title = element_text(hjust = 0.5))

anova1<-aov(PropUncertain~lag_Popped*RelativeSize, data=ParticipantSummaryPopped)
summary(anova1)









# Overall Performance -----------------------------------------------------
SizeSummary <- MainTrials %>% 
  group_by(size) %>% 
  summarise(RT=mean(rt),PropUncertain=mean(ChooseUncertain),PopProb=mean(PopProb)/1000)


ggplot() +
  geom_point(data = SizeSummary, aes(x=size, y=1-PropUncertain, color= 'darkblue', alpha=0.9, size=2)) +
  geom_point(data = SizeSummary, aes(x=size, y=PopProb, color= 'red', alpha=0.9, size=2)) +
  scale_color_manual(values=c("darkblue", "red")) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    legend.position="none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
  )

ggplot() +
  #geom_point(data = SizeSummary, aes(x=size, y=1-PropUncertain, alpha=0.9, size=2)) +
  geom_point(data = SizeSummary, aes(x=size, color='red', y=PopProb, alpha=0.9, size=2)) + 
  scale_color_manual(values=c("red")) +
theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change axis line
  axis.line = element_line(colour = "black"),
  legend.position="none",
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
)


SizeSummary <- MainTrials %>% 
  group_by(size,lag_Popped) %>% 
  summarise(RT=mean(rt),PropUncertain=mean(ChooseUncertain),PopProb=mean(PopProb)/1000)

SizeSummary<-na.omit(SizeSummary)

ggplot() +
  geom_point(data = SizeSummary, aes( x=size, y=1-PopProb, color= "red", alpha=0.9)) +
  geom_point(data = SizeSummary, aes( x=size, y=PropUncertain, color= "black", alpha=0.9)) +
   facet_wrap(~lag_Popped)


ggplot() +
  geom_point(data = SizeSummary, aes( x=size, y=RT, color= "black", alpha=0.9))



### By Blocks ###

SizeSummaryBlock <- MainTrials %>% 
  group_by(size, Block) %>% 
  summarise(RT=mean(rt),PropUncertain=mean(ChooseUncertain),PopProb=mean(PopProb)/1000)

#SizeSummary1 <- SizeSummaryBlock %>% 
#  filter(Block==1)


ggplot() +
  geom_point(data = SizeSummaryBlock, aes( x=size, y=1-PopProb, color= "red", alpha=0.9)) +
  geom_point(data = SizeSummaryBlock, aes( x=size, y=PropUncertain, color= "black", alpha=0.9)) +
  facet_wrap(~Block)

ggplot() +
  geom_point(data = SizeSummaryBlock, aes( x=size, y=RT, color= "red", alpha=0.9)) +
  facet_wrap(~Block)

#will need to combine variables between first and second half

DataTogether <- merge(ParticipantSummary, DataForBehavior)



DataTogetherSurveyRaw <- merge(DataTogether,Survey_Data)

write_csv(DataTogetherSurveyRaw,"SurveyRaw.csv")

#summary(aov(PropUncertain~SEMs, data=DataTogether))

DataTogether <- DataTogether %>% 
  filter(PropUncertain > 0.10) %>%
  filter(PropUncertain < .90) %>%
  filter(WeeklySleepEfficiency < 1.0) %>%
  filter(WeeklySleepEfficiency > 0.0) %>% 
  filter(SleepEfficiencyWE > 0.0) %>%
  filter(SleepEfficiencyWD > 0.0)

summary()
  
summary(DataTogether)
  
write.csv(DataTogether,"DataNicolesThesis.csv")

cor.test(DataTogether$ACESSumTotal,DataTogether$SCITotal)
cor.test(DataTogether$ACESSumTotal,DataTogether$PHQ8)
cor.test(DataTogether$ACESSumTotal,DataTogether$GAD7)
cor.test(DataTogether$ACESSumTotal,DataTogether$RISQMonth)


???
cor.test(DataTogether$ACESSumTotal,DataTogether$PropUncertain)
cor.test(DataTogether$ACESSumTotal,DataTogether$PopDiff)
cor.test(DataTogether$ACESSumTotal,DataTogether$RTDiff)
cor.test(DataTogether$ACESSumTotal,DataTogether$RTsdDiff)




summary(DataTogether)


#Strong Correlation here, Sleep and PCL (PTSD), as well as sleep and PHQ8 (depression), and Anxiety (GAD7)
cor.test(DataTogether$SCITotal,DataTogether$PCLTotal)
cor.test(DataTogether$SCITotal,DataTogether$PHQ8)
cor.test(DataTogether$SCITotal,DataTogether$GAD7)

cor.test(DataTogether$SleepEfficiency,DataTogether$PCLTotal)
cor.test(DataTogether$SleepEfficiency,DataTogether$PHQ8)
cor.test(DataTogether$SleepEfficiency,DataTogether$GAD7)

cor.test(DataTogether$TotalSleepTime,DataTogether$PCLTotal)
cor.test(DataTogether$TotalSleepTime,DataTogether$PHQ8)
cor.test(DataTogether$TotalSleepTime,DataTogether$GAD7)



# Strong positive correlation between Anxiety and Depression
cor.test(DataTogether$PHQ8,DataTogether$GAD7)
plot(DataTogether$PHQ8,DataTogether$GAD7)

#
cor.test(DataTogether$PHQ8,DataTogether$RISQMonth)
cor.test(DataTogether$GAD7,DataTogether$RISQMonth)
cor.test(DataTogether$PCLTotal,DataTogether$RISQMonth)
#none for ACES, ACES doesn't really associate with things
cor.test(DataTogether$ACESSumTotal,DataTogether$RISQMonth)




cor.test(DataTogether$time,DataTogether$SCITotal)





cor.test(DataTogether$SCITotal,DataTogether$SleepEfficiency)




cor.test(DataTogether$RISQMonth,DataTogether$PopDiff)


hist(DataTogether$PropUncertain)

cor.test(DataTogether$SCITotal,DataTogether$RT)
cor.test(DataTogether$SCITotal,DataTogether$RTsd)
cor.test(DataTogether$SCITotal,DataTogether$PropUncertain)
cor.test(DataTogether$SCITotal,DataTogether$Points)
cor.test(DataTogether$SCITotal,DataTogether$PopDiff)


cor.test(DataTogether$SleepEfficiency,DataTogether$RT)
cor.test(DataTogether$SleepEfficiency,DataTogether$RTsd)

cor.test(DataTogether$SleepEfficiency,DataTogether$Points)
cor.test(DataTogether$SleepEfficiency,DataTogether$PropUncertain)
cor.test(DataTogether$SleepEfficiency,DataTogether$PopDiff)

plot(DataTogether$ACESSumTotal,DataTogether$PropUncertain)



cor.test(DataTogether$SleepEfficiency,DataTogether$Points)

cor.test(DataTogether$TotalSleepTime,DataTogether$RT)
cor.test(DataTogether$TotalSleepTime,DataTogether$RTsd)
cor.test(DataTogether$TotalSleepTime,DataTogether$PropUncertain)
plot(DataTogether$TotalSleepTime,DataTogether$Points)

cor.test(DataTogether$TotalSleepTime,DataTogether$ACESSumTotal)


summary(aov(PropUncertain~SCIInsomniaLikely,data=DataTogether))

cor.test(DataTogether$PCLTotal,DataTogether$RT)
cor.test(DataTogether$PCLTotal,DataTogether$RTsd)
cor.test(DataTogether$PCLTotal,DataTogether$PropUncertain)
cor.test(DataTogether$PCLTotal,DataTogether$Points)


cor.test(DataTogether$GAD7,DataTogether$RT)
cor.test(DataTogether$GAD7,DataTogether$RTsd)
cor.test(DataTogether$GAD7,DataTogether$PropUncertain)
cor.test(DataTogether$GAD7,DataTogether$Points)

cor.test(DataTogether$PHQ8,DataTogether$RT)
cor.test(DataTogether$PHQ8,DataTogether$RTsd)
cor.test(DataTogether$PHQ8,DataTogether$PropUncertain)
cor.test(DataTogether$PHQ8,DataTogether$Points)

TukeyHSD(aov(PropUncertain~AnxietyLikely,data=DataTogether))

p <- ggplot(DataTogether, aes(Group, PopDiff))
p + geom_violin()

cor.test(DataTogether$total2to12,DataTogether$PropUncertain)

summary(aov(PropUncertain~AnxietyLikely,data=DataTogether))
summary(aov(PopDiff~SCIInsomniaLikely,data=DataTogether))


cor.test

DataTogether$
tukey

hist(DataTogether$PropUncertain)


max(DataTogether$RISQMonth)
#cor.test(DataTogether$RISQMonth,DataTogether$RT)
#cor.test(DataTogether$RISQMonth,DataTogether$RTsd)
cor.test(DataTogether$RISQMonth,DataTogether$PropUncertain)
#cor.test(DataTogether$RISQMonth,DataTogether$Points)






ggplot(data = DataTogetherTest, aes(x=RISQMonth, y=PropUncertain)) +
  geom_jitter(alpha=0.8, size=4, color="darkblue") + 
  geom_smooth(method=lm) +
  scale_color_manual(values=c("darkblue")) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    legend.position="none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  )



NewSurvey <- Survey_Data %>% 
  filter()




