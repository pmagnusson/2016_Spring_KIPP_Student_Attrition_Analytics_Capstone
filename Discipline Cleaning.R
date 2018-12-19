



# BAT 4301 Predictive Team Project - Discipline and Newness Data cleaning, Full and Clustered Set Predictive Modeling

# requires R version 3.2.3 at minimum, as well as Java JDK

# WD on home PC, will differ for other computers
setwd("C:/Users/Patrick Magnusson/Google Drive/School/TU 03 Spring 2016/BAT Consulting Capstone/KIPP Predictive Model/Trinity Data")
load("C:/Users/Patrick Magnusson/Google Drive/School/TU 03 Spring 2016/BAT Consulting Capstone/KIPP Predictive Model/Trinity Data/1.RData")

library(xlsxjars)
library(rJava)
library(xlsx)
library(plyr)
library(dplyr)
library(data.table)
library(zoo)
library(caTools)


#---------------------------------------------------------------------------------------------------------------------------------


#data loading - initial tables as provided, with necessary fixes

attrition2014 = read.xlsx("Attrition/Final2_2013-14_Student_Attrition_Template - Copy.xlsm", sheetName="Data")
attrition2014 = attrition2014[1:1483,]

attrition2015 = read.xlsx("Attrition/2014-15_Student_Attrition_Template_FINAL - ALL.xlsx", sheetName="Data")
attrition2015 = attrition2015[1:806,1:16]

discipline0 = read.xlsx("Discipline Data/rsccc_SR_DISCPLN.xlsx", sheetName="rsccc_SR_DISCPLN")
discipline0$SCH_YR = as.numeric(as.character(discipline0$SCH_YR))

#create count of disciplinary incidents by student ID
disc.count = as.data.frame(table(discipline0$STU_ID))
colnames(disc.count) = c("STU_ID", "NumberDiscIncidents")



#---------------------------------------------------------------------------------------------------------------------------------




# Student newness - length of enrollment cleaning

enroll = read.xlsx("rsccc_SR_STU_ENROLL.xlsx", sheetName="rsccc_SR_STU_ENROLL")
#format dates
enroll$DT_ENTRY = as.character(enroll$DT_ENTRY)
enroll$DT_ENTRY = as.Date(enroll$DT_ENTRY, "%Y%m%d")
enroll$DT_WITHDRAW = as.character(enroll$DT_WITHDRAW)
enroll$DT_WITHDRAW = as.Date(enroll$DT_WITHDRAW, "%Y%m%d")

#create concise table, with today's date as replacement for NA withdrawal date values, and calculate enrollment time per row
newness = enroll[,c("STU_ID", "DT_ENTRY", "DT_WITHDRAW")]


#newness$DT_WITHDRAW[is.na(newness$DT_WITHDRAW)] = as.Date("20160328", "%Y%m%d")
newness$DT_WITHDRAW = as.character(newness$DT_WITHDRAW)
newness$DT_WITHDRAW = as.Date(newness$DT_WITHDRAW, "%Y%m%d")
newness$DT_ENTRY = as.character(newness$DT_ENTRY)
newness$DT_ENTRY = as.Date(newness$DT_ENTRY, "%m/%d/%Y")

#calc difference btwn dates
newness$days_enrolled = as.numeric(difftime(newness$DT_WITHDRAW, newness$DT_ENTRY, units="days"))
newness$weeks_enrolled = as.numeric(round(difftime(newness$DT_WITHDRAW, newness$DT_ENTRY, units="weeks"),2))
newness$months_enrolled = as.numeric(round((newness$DT_WITHDRAW - newness$DT_ENTRY)/(365.25/12)))
newness$years_enrolled = as.numeric(round(newness$days_enrolled/365,2))

#filter down to total time enrolled per student ID only (corrects for multiple entries/rows)
newness1 = aggregate(newness[,4:7],newness["STU_ID"],sum)


#---------------------------------------------------------------------------------------------------------------------------------

# write to .csv files

write.csv(disc.count, file="disc.count.csv")
write.csv(newness1, file="newness.csv")


#---------------------------------------------------------------------------------------------------------------------------------


#Full dataset

full.set = read.xlsx("COMBINED DATASET v1.0.xlsx", sheetName = "Sheet1")

#clean
full.set$Number_Absent[is.na(full.set$Number_Absent)] = 0
full.set$Number_Disc_Incidents[is.na(full.set$Number_Disc_Incidents)] = 0
full.set$STU_ID = as.factor(full.set$STU_ID)
full.set$MAP.Score = as.numeric(as.character(full.set$MAP.Score))
full.set$MAP.Score = round(full.set$MAP.Score, 2)
full.set$Grades = round(full.set$Grades, 2)
full.set$Attrition_Y_N[is.na(full.set$Attrition_Y_N)] = 0

write.xlsx(full.set, "COMBINED DATASET v2.0.xlsx", sheetName = "Sheet1")

# divide train/test sets

set.seed(101) 
sample = sample.split(full.set, SplitRatio = .75)
train.set = subset(full.set, sample == TRUE)
test.set = subset(full.set, sample == FALSE)



#---------------------------------------------------------------------------------------------------------------------------------

# linear models, one for each newness measure

set.seed(101)
lm.train.days = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+days_enrolled+Miles_Distance_House_School,
         data=train.set)
plot(lm.train.days)
summary(lm.train.days)

set.seed(101)
lm.train.weeks = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+weeks_enrolled+Miles_Distance_House_School,
                   data=train.set)
plot(lm.train.weeks)
summary(lm.train.weeks)

set.seed(101)
lm.train.months = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+months_enrolled+Miles_Distance_House_School,
                   data=train.set)
plot(lm.train.months)
summary(lm.train.months)

set.seed(101)
lm.train.years = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
                   data=train.set)
plot(lm.train.years)
summary(lm.train.years)




#prediction

tested = as.data.frame(predict(lm.train.days, test.set, level=.95, type="response"))
colnames(tested) = "Prediction"
tested$Prediction1 = trunc(trunc(tested$Prediction+sign(tested$Prediction)*0.5))

comparison = as.data.frame(cbind(test.set$STU_ID,test.set$Attrition_Y_N, tested$Prediction1))
colnames(comparison) = c("STU_ID", "Test_set", "Predicted")


table(comparison$Test_set,comparison$Predicted)/length(comparison$Test_set)




#---------------------------------------------------------------------------------------------------------------------------------


# Clustered predictions


setwd("C:/Users/Patrick Magnusson/Google Drive/School/TU 03 Spring 2016/BAT Consulting Capstone/KIPP Predictive Model/based_on_all_var")


#Cluster 1
set.seed(101) 
sample1 = sample.split(cluster1, SplitRatio = .75)
train1 = subset(cluster1, sample1 == TRUE)
test1 = subset(cluster1, sample1 == FALSE)

set.seed(101)
lm.train1 = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
                   data=train1)
plot(lm.train1)
summary(lm.train1)

tested1 = as.data.frame(predict(lm.train1, test1, level=.95, type="response"))
colnames(tested1) = "Prediction"
tested1$Prediction1 = trunc(trunc(abs(tested1$Prediction+sign(tested1$Prediction)*0.5)))

comparison1 = as.data.frame(cbind(test1$X,test1$Attrition_Y_N, tested1$Prediction1))
colnames(comparison1) = c("STU_ID", "Test_set", "Predicted")


table(comparison1$Test_set,comparison1$Predicted)/length(comparison1$Test_set)



#Cluster 2
set.seed(101) 
sample2 = sample.split(cluster2, SplitRatio = .75)
train2 = subset(cluster2, sample2 == TRUE)
test2 = subset(cluster2, sample2 == FALSE)

set.seed(101)
lm.train2 = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
               data=train2)
plot(lm.train2)
summary(lm.train2)

tested2 = as.data.frame(predict(lm.train2, test2, level=.95, type="response"))
colnames(tested2) = "Prediction"
tested2$Prediction1 = trunc(trunc(abs(tested2$Prediction+sign(tested2$Prediction)*0.5)))

comparison2 = as.data.frame(cbind(test2$X, test2$Attrition_Y_N, tested2$Prediction1))
colnames(comparison2) = c("STU_ID", "Test_set", "Predicted")


table(comparison2$Test_set,comparison2$Predicted)/length(comparison2$Test_set)




#Cluster 3
set.seed(101) 
sample3 = sample.split(cluster3, SplitRatio = .75)
train3 = subset(cluster3, sample3 == TRUE)
test3 = subset(cluster3, sample3 == FALSE)

set.seed(101)
lm.train3 = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
               data=train3)
plot(lm.train3)
summary(lm.train3)

tested3 = as.data.frame(predict(lm.train3, test3, level=.95, type="response"))
colnames(tested3) = "Prediction"
tested3$Prediction1 = trunc(trunc(abs(tested3$Prediction+sign(tested3$Prediction)*0.5)))

comparison3 = as.data.frame(cbind(test3$X, test3$Attrition_Y_N, tested3$Prediction1))
comparison3 = comparison3[-50,]
colnames(comparison3) = c("STU_ID", "Test_set", "Predicted")


table(comparison3$Test_set,comparison3$Predicted)/length(comparison3$Test_set)



#Cluster 4
set.seed(101) 
sample4 = sample.split(cluster4, SplitRatio = .75)
train4 = subset(cluster4, sample4 == TRUE)
test4 = subset(cluster4, sample4 == FALSE)

set.seed(101)
lm.train4 = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
               data=train4)
plot(lm.train4)
summary(lm.train4)

tested4 = as.data.frame(predict(lm.train4, test4, level=.95, type="response"))
colnames(tested4) = "Prediction"
tested4$Prediction1 = trunc(trunc(abs(tested4$Prediction+sign(tested4$Prediction)*0.5)))

comparison4 = as.data.frame(cbind(test4$X, test4$Attrition_Y_N, tested4$Prediction1))
colnames(comparison4) = c("STU_ID", "Test_set", "Predicted")


table(comparison4$Test_set,comparison4$Predicted)/length(comparison4$Test_set)



#Cluster 5
set.seed(101) 
sample5 = sample.split(cluster5, SplitRatio = .75)
train5 = subset(cluster5, sample5 == TRUE)
test5 = subset(cluster5, sample5 == FALSE)

set.seed(101)
lm.train5 = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
               data=train5)
plot(lm.train5)
summary(lm.train5)

tested5 = as.data.frame(predict(lm.train5, test5, level=.95, type="response"))
colnames(tested5) = "Prediction"
tested5$Prediction1 = trunc(trunc(abs(tested5$Prediction+sign(tested5$Prediction)*0.5)))

comparison5 = as.data.frame(cbind(test5$X, test5$Attrition_Y_N, tested5$Prediction1))
colnames(comparison5) = c("STU_ID", "Test_set", "Predicted")


table(comparison5$Test_set,comparison5$Predicted)/length(comparison5$Test_set)




#Cluster 6
set.seed(101) 
sample6 = sample.split(cluster6, SplitRatio = .75)
train6 = subset(cluster6, sample6 == TRUE)
test6 = subset(cluster6, sample6 == FALSE)

set.seed(101)
lm.train6 = lm(Attrition_Y_N ~ Grades+MAP.Score+Number_Absent+Number_Disc_Incidents+years_enrolled+Miles_Distance_House_School,
               data=train6)
plot(lm.train6)
summary(lm.train6)

tested6 = as.data.frame(predict(lm.train6, test6, level=.95, type="response"))
colnames(tested6) = "Prediction"
tested6$Prediction1 = trunc(trunc(abs(tested6$Prediction+sign(tested6$Prediction)*0.5)))

comparison6 = as.data.frame(cbind(test6$X, test6$Attrition_Y_N, tested6$Prediction1))
colnames(comparison6) = c("STU_ID", "Test_set", "Predicted")


table(comparison6$Test_set,comparison6$Predicted)/length(comparison6$Test_set)



#---------------------------------------------------------------------------------------------------------------------------------


