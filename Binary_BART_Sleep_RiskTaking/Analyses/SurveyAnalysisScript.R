library(naniar)


Survey_Data <- read_csv("Survey.csv")

#View(Survey_Data)

# JM this is just an example: to get Sleep Score, take average of columns 15_1 to 17_1, 15_3 is reverse coded



# Quality Control, remove bad participants --------------------------------




# ND Data cleaning- Remove participants with <4/6 attention check questions corr

sort(colnames(Survey_Data))

AttCheck<-c("Q100","Q101","Q14_1","Q103","Q104","Q105","Q16")

AttCheckSurvey<- Survey_Data %>% 
  select(Q57_1,Q58_1,Q100,Q101,Q14_1,Q14_5,Q103,Q104,Q105_1,Q16_1,participant)

AttCheckSurvey<-na.omit(AttCheckSurvey)

AttCheckSurvey$Q57_1<-as.numeric(AttCheckSurvey$Q57_1)
AttCheckSurvey$Q58_1<-as.numeric(AttCheckSurvey$Q58_1)

AttCheckSurvey$Q14_1<-as.numeric(AttCheckSurvey$Q14_1)
AttCheckSurvey$Q14_5<-as.numeric(AttCheckSurvey$Q14_5)

AttCheckSurvey$Q16_1<-as.numeric(AttCheckSurvey$Q16_1)
AttCheckSurvey$Q105_1<-as.numeric(AttCheckSurvey$Q105_1)


AttCheckSurvey$Q16_1<- 6 - AttCheckSurvey$Q16_1

AttCheckSurvey$Q16Match<-ifelse(AttCheckSurvey$Q16_1==AttCheckSurvey$Q105_1,1,0)

AttCheckSurvey$Q14_5<- 6 - AttCheckSurvey$Q14_5

AttCheckSurvey$Q14Match<-ifelse(AttCheckSurvey$Q14_1==AttCheckSurvey$Q14_5,1,0)




AttCheckSurvey <- AttCheckSurvey %>% 
  filter(Q100 == "1") %>% 
  filter(Q101 == "2") %>% 
  filter(Q57_1 > 50) %>%
  filter(Q58_1 > 50) %>% 
  filter(Q14Match==1) %>% 
  filter(Q16Match==1)

# attention check questions and answers are: Q100= 1 (yes), Q101= 2 (no), Q14.5= reverse code of Q14.1, Q103= 1 (yes), Q104= 1 (yes), Q105= reverse code of Q16

# remove participants who answered <50 on Q57 and Q58 (honesty questions)


Survey_Data <- filter(Survey_Data, participant %in% AttCheckSurvey$participant )


library(lubridate)

TimeOfDay<-Survey_Data$StartDate

format(TimeOfDay, format="%m/%d/%Y %H:%M")

dates <- as.POSIXct(TimeOfDay, format = "%m/%d/%Y %H:%M")

time <- format(dates, format = "%H:%M")

time<- as.numeric(hm(time))

Survey_Data$time<-time

# Descriptives ------------------------------------------------------------


# Demographics data- take average and std dev of Q45 (age) and Q56 (SES ladder)
summary(as.numeric(Survey_Data$Q45))
sd(as.numeric(Survey_Data$Q45),na.rm = TRUE)

summary(as.numeric(Survey_Data$Q56))
sd(as.numeric(Survey_Data$Q56))

# Take frequency values (percentages) and std dev of Q44, Q46, Q47,Q48 Q49, Q50 (will need to test for any significant differences in demographics between groups later on)

# ACES Scores: Sum of 1 (yes values) Q2-Q6, then dichotomize Q7-Q12 with following groups (1= 2 or 3) (0=1, 4, or 5), add values of 1 from Q1-Q6 to values of 1 from Q7-Q12 to find total ACES Score
ACES<- Survey_Data %>% 
  select(Q2:Q12, participant) 

ACES<-mutate_all(ACES, function(x) as.numeric(as.character(x)))

#ACES$Sum<-sum(ACES,na.rm=TRUE)

# Yes is 1, everything else no
ACES$Q2Binary<-ifelse(ACES$Q2==1,1,0)
ACES$Q3Binary<-ifelse(ACES$Q3==1,1,0)
ACES$Q4Binary<-ifelse(ACES$Q4==1,1,0)
ACES$Q5Binary<-ifelse(ACES$Q5==1,1,0)
ACES$Q6Binary<-ifelse(ACES$Q6==1,1,0)

# Yes is 2 or 3, no is everything else
ACES$Q7Binary<-ifelse( ACES$Q7==2 | ACES$Q7==3,1,0)
ACES$Q8Binary<-ifelse( ACES$Q8==2 | ACES$Q8==3,1,0)
ACES$Q9Binary<-ifelse( ACES$Q9==2 | ACES$Q9==3,1,0)
ACES$Q10Binary<-ifelse( ACES$Q10==2 | ACES$Q10==3,1,0)
ACES$Q11Binary<-ifelse( ACES$Q11==2 | ACES$Q11==3,1,0)
ACES$Q12Binary<-ifelse( ACES$Q12==2 | ACES$Q12==3,1,0)


rf <- ACES %>% rowwise(participant)
ACESSum<- rf %>% summarise(total2to12 = sum(c_across(Q2Binary:Q12Binary)))
#ACESSum <-na.omit(ACESSum)

hist(ACESSum$total2to12)

mean(ACESSum$total2to12, na.rm=TRUE)
sd(ACESSum$total2to12, na.rm=TRUE)
# From Total Aces Score, take mean and std dev., find group individuals into following groups: Total Score of 0, 1, 2, 3, 4 or more

#will need to do group ACES later


ACESSumZero <- ACESSum %>% 
  filter(total2to12 == 0)

ACESSumZero$Group <- "GroupZero"

###

ACESSumZero$Group <- "GroupZero"
###
ACESSumOne <- ACESSum %>% 
  filter(total2to12==1)

ACESSumOne$Group <- "GroupOne"
###
ACESSumTwo <- ACESSum %>% 
  filter(total2to12==2)

ACESSumTwo$Group <- "GroupTwo"
###
ACESSumThree <- ACESSum %>% 
  filter(total2to12==3)

ACESSumThree$Group <- "GroupThree"

ACESSumFour <- ACESSum %>% 
  filter(total2to12>3)

ACESSumFour$Group <- "GroupFour"

ACESSum<-rbind(ACESSumZero,ACESSumOne,ACESSumTwo,ACESSumThree,ACESSumFour)



# Find Frequencies of Different Abuses from ACES- Percentage Frequency of 1 values in Q2-Q12 of dichotomized variables. Future thing.



# PCL-5 Scores- recode variables 1=0, 2=1, 3=2, 4=3, 5=4; sum of Q17_1-Q17_20; if sum < 32, create group of "PTSD Unlikely", if sum > or = to 32, create group of "PTSD Likely"

PCL_5 <- Survey_Data %>% 
  select(Q17_1:Q17_20, participant) 

PCL_5<-mutate_all(PCL_5, function(x) as.numeric(as.character(x)))


  
rf <- PCL_5 %>% rowwise(participant) %>%
summarise(total = sum(c_across(Q17_1:Q17_20)))

PCL_5$Total<-rf$total - 20

hist(PCL_5$Total)

#1 is likely, 0 is unlikely
PCL_5$PTSDLikely<-ifelse(PCL_5$Total>=32,1,0)

mean(PCL_5$PTSDLikely, na.rm=TRUE)

plot(PCL_5$Total,ACESSum$total2to12)

cor.test(PCL_5$Total,ACESSum$total2to12)


# Insomnia, 
# SCI Scores ignore Q14_5, recode Q14_1-Q14_4, Q15_1-Q15_3, and Q16_1 to 1=0, 1=0, 2=1, 3=2, 4=3, 5=4

# SCI Scores: reverse code  Q14_1, Q14_2, Q14_3, Q14_4, Q15_1, Q15_2, Q15_3, Q16_1, sum recoded Q14_1, Q14_2, Q14_3, Q14_4, Q15_1, Q15_2, Q15_3, Q16_1 and create total SCI sum value, find mean and std dev of total SCI Sum value, create group of "insomnia probale" for score > or = to 16, create group of "Insomnia improbable" for sum scores > 16


SCI <- Survey_Data %>% 
  select(Q14_1:Q14_4,Q15_1:Q15_3,Q16_1, participant) 

SCI<-mutate_all(SCI, function(x) as.numeric(as.character(x)))


SCI$Q14_1 <- 6 - SCI$Q14_1
SCI$Q14_2 <- 6 - SCI$Q14_2
SCI$Q14_3 <- 6 - SCI$Q14_3
SCI$Q14_4 <- 6 - SCI$Q14_4

SCI$Q15_1 <- 6 - SCI$Q15_1
SCI$Q15_2 <- 6 - SCI$Q15_2
SCI$Q15_3 <- 6 - SCI$Q15_3

SCI$Q16_1 <- 6 - SCI$Q16_1


rf <- SCI %>% rowwise(participant) %>%
  summarise(total = sum(c_across(Q14_1:Q16_1)))


SCI$Total <- rf$total - 8

hist(SCI$Total)

SCI$InsomniaLikely<- ifelse(SCI$Total<=16,1,0)

mean(SCI$InsomniaLikely)


plot(SCI$Total,PCL_5$Total)
cor.test(SCI$Total,PCL_5$Total)


# GAD-7 Scoring- for some reason there are two Q58_1, need to fix, I am referring to the Q58_1 later on- Sum Q58_1-Q58_10 to create value of GAD-7 Sum Score, find mean and std dev. of GAD-7 Sum Score, create groups from GAD-7 Sum Score- mild anxiety at threshold of 5, moderate anxiety at threshold of 10, severe anxiety at threshold of 15, cut off point of 10
GAD7 <- Survey_Data %>% 
  select(Q58_1_1:Q58_10, participant) 

GAD7<-mutate_all(GAD7, function(x) as.numeric(as.character(x)))

rf <- GAD7 %>% rowwise(participant) %>%
  summarise(total = sum(c_across(Q58_1_1:Q58_10)))

GAD7$Total <- rf$total

hist(GAD7$Total)

GAD7$AnxietyLikely<- ifelse(GAD7$Total<=10,0,1)

mean(GAD7$AnxietyLikely, na.rm = TRUE)
#sort(colnames(Survey_Data))



# PHQ-8 Scoring, sum Q57_1 through Q57_8 to create variable "PHQ-8 Total Sum Score", find mean and std dev. of "PHQ-8 Total Sum Score", create groups from "PHQ-8 Total Sum Score": "Unlikely Major Depression" = score < 10, "Likely Major Depression" = score > or = to 10, "Likely Severe Major Depression" = score > or = to 20
PHQ8 <- Survey_Data %>% 
  select(Q57_1_1:Q57_8, participant) 

PHQ8<-mutate_all(PHQ8, function(x) as.numeric(as.character(x)))

rf <- PHQ8 %>% rowwise(participant) %>%
  summarise(total = sum(c_across(Q57_1_1:Q57_8)))


PHQ8$Total <- rf$total

hist(PHQ8$Total)

PHQ8$DepressionLikely<- ifelse(PHQ8$Total<=10,0,1)

mean(PHQ8$DepressionLikely, na.rm = TRUE)


#### SASSY CODE ####
SASSYWD <- Survey_Data %>% 
  select(Q19:Q27_3, participant) 

SASSYWD<-mutate_all(SASSYWD, function(x) as.numeric(as.character(x)))

SASSYBed<- SASSYWD %>% 
  select(Q19,Q25,participant)

#Time In Bed
SASSYBed$TimeInBed<-abs(SASSYWD$Q19-SASSYWD$Q25)
SASSYBed$TimeInBed<- ifelse(SASSYBed$TimeInBed > 12,24-SASSYBed$TimeInBed,SASSYBed$TimeInBed)

SASSYBed$SleepOnsetLatency<-SASSYWD$Q21_1 + (SASSYWD$Q21_2/60)

# Q24 is what time you finally woke up, Q25 is when you got out of bed
SASSYBed$TerminalWakefulnessTime <- SASSYWD$Q25 - SASSYWD$Q24

#got weird values here

SASSYBed$WakeAfterSleepOnset<- SASSYWD$Q22 * (SASSYWD$Q23_1 + (SASSYWD$Q23_2/60))

#Time they went to sleep is Latency plus Q19#

SASSYBed$TotalWakeTime <- SASSYBed$SleepOnsetLatency + SASSYBed$TerminalWakefulnessTime + SASSYBed$WakeAfterSleepOnset

SASSYBed$TotalSleepTime <- SASSYBed$TimeInBed - SASSYBed$TotalWakeTime

SASSYBed$SleepEfficiency <-  SASSYBed$TotalSleepTime / SASSYBed$TimeInBed

med<-median(SASSYBed$SleepEfficiency)

SASSYBed$SEMs<-ifelse(SASSYBed$)


SASSYWD$NumberOfAwakenings<-
SASSYWD$SleepEfficiency<-
SASSYWD$TotalSleepTime<-
SASSYWD$Latency
SASSYWD$SleepOnset
#Latency,SleepOnset


SASSYWE <- Survey_Data %>% 
  select(Q29:Q37_3, participant) 

SASSYWE<-mutate_all(SASSYWE, function(x) as.numeric(as.character(x)))


SASSYBedWE<- SASSYWE %>% 
  select(Q29,Q35,participant)

#Time In Bed
SASSYBedWE$TimeInBed<-abs(SASSYWE$Q35-SASSYWE$Q29)
SASSYBedWE$TimeInBed<- ifelse(SASSYBedWE$TimeInBed > 12,24-SASSYBedWE$TimeInBed,SASSYBedWE$TimeInBed)

SASSYBedWE$SleepOnsetLatency<-SASSYWE$Q31_1 + (SASSYWE$Q31_2/60)

# Q24 is what time you finally woke up, Q25 is when you got out of bed
SASSYBedWE$TerminalWakefulnessTime <- SASSYWE$Q35 - SASSYWE$Q34

#got weird values here

SASSYBedWE$WakeAfterSleepOnset<- SASSYWE$Q32 * (SASSYWE$Q33_1 + (SASSYWE$Q33_2/60))

#Time they went to sleep is Latency plus Q19#

SASSYBedWE$TotalWakeTime <- SASSYBedWE$SleepOnsetLatency + SASSYBedWE$TerminalWakefulnessTime + SASSYBedWE$WakeAfterSleepOnset

SASSYBedWE$TotalSleepTime <- SASSYBedWE$TimeInBed - SASSYBedWE$TotalWakeTime

SASSYBedWE$SleepEfficiency <-  SASSYBedWE$TotalSleepTime / SASSYBedWE$TimeInBed


###############
WeeklySleepTime<-((SASSYBed$TotalSleepTime * 5) + (SASSYBedWE$TotalSleepTime *2))/7

WeeklySleepEfficiency<-((SASSYBed$SleepEfficiency * 5) + (SASSYBedWE$SleepEfficiency *2))/7




#####


DataForBehavior<-as.data.frame(cbind(Survey_Data$participant,Survey_Data$time,SCI$Total,SCI$InsomniaLikely,PCL_5$PTSDLikely,GAD7$AnxietyLikely,PHQ8$DepressionLikely ,PCL_5$Total,GAD7$Total,PHQ8$Total,SASSYBed$SleepEfficiency,SASSYBed$TotalSleepTime))
colnames(DataForBehavior) <- c("participant","time", "SCITotal","SCIInsomniaLikely","PTSDLikely","AnxietyLikely","DepressionLikely", "PCLTotal","GAD7","PHQ8", "SleepEfficiency", "TotalSleepTime")
DataForBehavior<-mutate_all(DataForBehavior, function(x) as.numeric(as.character(x)))



DataForBehavior<- inner_join(DataForBehavior, ACESSum, by="participant")


summary(aov(PCLTotal~Group,data=DataForBehavior))
summary(aov(SleepEfficiency~SCIInsomniaLikely, data=DataForBehavior))




summary(aov(PHQ8~Group,data=DataForBehavior))

DataForBehavior$ACESGroup <- ACESSum$Group
DataForBehavior<-subset(DataForBehavior, DataForBehavior$SleepEfficiency!="-Inf") 

med<-median(DataForBehavior$SleepEfficiency)

DataForBehavior$SEMs<-ifelse(DataForBehavior$SleepEfficiency>med,"High","Low")


# 1. Nicole get me info on SASSY

# 2. Time of Day of testing

# 3. Time of Getting into Bed, Trying to sleep, how long it takes, AM or PM time for their sleep, What time did you wake, then get out of bed. Average Quality of sleep, Weekdays and Weekends




# RISQ --------------------------------------------------------------------

colnames(Survey_Data)

sort(colnames(Survey_Data))

RISQLifetime<- Survey_Data %>% 
  select(Q61_1, Q62_1, Q64_1,Q65_1, Q66_1, Q67_1, Q68_1,
 Q69_1, Q70_1, Q71_1, Q72_1, Q73_1, Q74_1, Q75_1, Q76_1, Q77_1, Q78_1, Q79_1, Q80_1, Q81_1,
 Q82_1, Q83_1, Q84_1, Q85_1, Q86_1, Q87_1, Q88_1, Q89_1, Q90_1, Q91_1, Q92_1, Q93_1, Q94_1,
 Q95_1, Q96_1, Q97_1, Q98_1, Q99_1, participant)

RISQLifetime<-mutate_all(RISQLifetime, function(x) as.numeric(as.character(x)))

RISQLifetime$Sum <- rowSums(RISQLifetime[,1:38])

RISQLifetime$AlcoholSum <-rowSums(RISQLifetime[,c(11,22)])


RISQLifetime$RiskySexSum <-rowSums(RISQLifetime[,c(7,10,23,29)])


RISQLifetime$Reckless <-rowSums(RISQLifetime[,c(2,6,31,38)])

RISQLifetime$DrugSum <-rowSums(RISQLifetime[,c(4,5,12,20,27,31,34)])
#RISQLifetime$DrugBehaviorAffectiveApproach <- rowSums(RISQLifetime[,c(3,4,11,19,26,30,33)])
#RISQLifetime$GamblingTotal <- rowSums(RISQLifetime[,c(,4,11,19,26,30,33)])

Survey_Data$Q63

RISQLifetimeGambling <- Survey_Data %>% 
  select(Q64_1, Q78_1,Q93_1,Q94_1)

RISQLifetimeGambling<-mutate_all(RISQLifetimeGambling, function(x) as.numeric(as.character(x)))

RISQLifetimeGambling$Sum <- rowSums(RISQLifetimeGambling)



Survey_Data$Q63_1


RISQMonth<- Survey_Data %>% 
  select(Q61_2, Q62_2, Q64_2,Q65_2, Q66_2, Q67_2, 
  Q68_2, Q69_2, Q70_2, Q71_2, Q72_2, Q73_2, Q74_2, Q75_2, Q76_2, Q77_2, Q78_2, Q79_2, Q80_2,
  Q81_2, Q82_2, Q83_2, Q84_2, Q85_2, Q86_2, Q87_2, Q88_2, Q89_2, Q90_2, Q91_2, Q92_2, Q93_2,
  Q94_2, Q95_2, Q96_1, Q97_2, Q98_2, Q99_2, participant)

RISQMonth<-mutate_all(RISQMonth, function(x) as.numeric(as.character(x)))

#Breaks
b <- c(-Inf,1,10,50,100,Inf)
names<-c(0,1,2,3,4)

RISQMonthTest<- cut(RISQMonth, breaks = b, labels = names)


RISQMonthTest<-as.data.frame(lapply(RISQMonth[,1:38], function(x){cut(x, breaks=b, labels=names)}))
RISQMonthTest$participant<-RISQMonth$participant

RISQMonthTest<-mutate_all(RISQMonthTest, function(x) as.numeric(as.character(x)))


RISQMonth$Reckless <-rowSums(RISQMonth[,c(2,6,31,38)])


RISQMonth$Sum <- rowSums(RISQMonthTest[,1:38], na.rm=TRUE)

RISQMonth$DrugSum <-rowSums(RISQMonth[,c(4,5,12,20,27,31,34)])

RISQMonth$AlcoholSum <-rowSums(RISQMonth[,c(11,22)])


RISQMonthGambling <- Survey_Data %>% 
  select(Q64_2, Q78_2,Q93_2,Q94_2)

RISQMonthGambling<-mutate_all(RISQMonthGambling, function(x) as.numeric(as.character(x)))

RISQMonthGambling$Sum <- rowSums(RISQMonthGambling)

RISQMonth$RiskySexSum <-rowSums(RISQMonth[,c(7,10,23,29)])




####

ggplot() +
  geom_jitter(data = DataForBehavior, aes( x=ACESSumTotal, y=SCITotal, color= SCIInsomniaLikely, size=4, alpha=0.9)) 



#####
Survey_Data$Q44
Survey_Data$Q45
Survey_Data$Q46
Survey_Data$Q47
Survey_Data$Q48
SurveyLadder<-as.numeric(Survey_Data$Q56)




DataForBehavior<-as.data.frame(cbind(Survey_Data$Q44,Survey_Data$Q45,Survey_Data$Q46,Survey_Data$Q47,Survey_Data$Q48,SurveyLadder,Survey_Data$participant,Survey_Data$time, SCI$Total,SCI$InsomniaLikely,PCL_5$Total,ACESSum$total2to12,GAD7$Total,PHQ8$Total,RISQLifetime$Sum,RISQMonth$Sum,RISQLifetime$Reckless,RISQMonth$Reckless,RISQLifetime$AlcoholSum, RISQMonth$AlcoholSum, RISQLifetime$RiskySexSum, RISQMonth$RiskySexSum, RISQLifetimeGambling$Sum,RISQMonthGambling$Sum,RISQLifetime$DrugSum,RISQMonth$DrugSum, SASSYBed$SleepEfficiency,SASSYBed$TotalSleepTime,SASSYBedWE$SleepEfficiency,SASSYBedWE$TotalSleepTime,WeeklySleepTime,WeeklySleepEfficiency))
DataForBehavior<-mutate_all(DataForBehavior, function(x) as.numeric(as.character(x)))
colnames(DataForBehavior) <- c("Sex","Age","School Year","Race","Military","SES","participant","time", "SCITotal","SCIInsomniaLikely", "PCLTotal", "ACESSumTotal","GAD7","PHQ8","RISQLifetime","RISQMonth","RISQLifetimeReckless","RISQMonthReckless","RISQLifetimeAlcohol","RISQMonthAlcohol","RISQLifetimeRiskySex","RISQMonthRiskySex", "RISQLifetimeGambling","RISQMonthGambling","RISQLifetimeDrugUse","RISQMonthlyDrugUse","SleepEfficiencyWD","TotalSleepTimeWD","SleepEfficiencyWE","TotalSleepTimeWE","WeeklySleepTime","WeeklySleepEfficiency")


DataForBehavior<-subset(DataForBehavior, DataForBehavior$SleepEfficiencyWD!="-Inf") 

# 1. Nicole get me info on SASSY

# 2. Time of Day of testing

# 3. Time of Getting into Bed, Trying to sleep, how long it takes, AM or PM time for their sleep, What time did you wake, then get out of bed. Average Quality of sleep, Weekdays and Weekends


hist(DataForBehavior$RISQLifetime)
hist(DataForBehavior$RISQMonth)




DataForBehavior <- DataForBehavior %>% 
  filter(RISQMonth < 10000) 


cor.test(DataForBehavior$RISQLifetime,DataForBehavior$PCLTotal)

RISQRaw<- Survey_Data %>% 
  select(Q61_1:Q99_6, participant) 


NewSurvey <-m
