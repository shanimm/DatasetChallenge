df1 <- read.csv("train.csv",header=T, na.strings=c("",NA))
#as.is helps save as character instead of factor
#na.strings is used here to create NA values on import on blank insteead of empty cells which won't be classified as missing otherwise
#taking the header now
#using excel every instance of '/' was change to '-' to avoid ambiguity
# transformed the dates using excel to get the dates in uniform format. works fine now but its currently a factor
head(df1)
#checking the head
class(df1$First_Activity)
anyNA(df1$First_Activity)
anyNA(df1$Last_Activity)
#nchar(df1$First_Activity[4])
#nchar(df1$First_Activity[2])
# interestingly some of the number of characters were different. 

#How long does it take bills to be processed?
#Can you analyze the reason why it takes long/short according to the dataset?
#Compare and contrast the different teams.
#Compare and contrast the different bill types.
#Is there a bottleneck in processing of this set of bills and if so, what is it?
#For State and Client data, can you identify any interesting data patterns?
summary(df1)
install.packages("Hmisc")
#better summary technique
require(Hmisc)
Hmisc::describe(df1)
library(tidyverse)
table(df1$State)
str(df1)
ggplot(data = df1, aes(x =State, y =Closed)) +
  geom_point()
# <- ggplot(data = df1,
 #           mapping = aes(x = State, y = Original_Charge))
#p + geom_point()
require(zoo)
library(lubridate)
require(lubridate)
install.packages("date")
require(date)
class(df1$First_Activity)
class(df1$Last_Activity)

df1$First_Activity



#df1$First_Activity
# converting first and last activity to character and taking their diffenrence. We will do same for every pair before analyzing bottlenecks
df1$Last_Activity <- strptime(x = as.character(df1$Last_Activity),
                              format = "%d-%m-%y %H:%M")

df1$Last_Activity
df1$First_Activity <- strptime(x = as.character(df1$First_Activity),
                              format = "%d-%m-%y %H:%M")

df1$TotalTime=difftime(df1$First_Activity,df1$Last_Activity)
df1$TotalTime

str(df1)

df1$Team_1_Start <- strptime(x = as.character(df1$Team_1_Start),
                               format = "%d-%m-%y %H:%M")
df1$Team_1_End  <- strptime(x = as.character(df1$Team_1_End),
                             format = "%d-%m-%y %H:%M")
df1$Team1Time=difftime(df1$Team_1_End,df1$Team_1_Start)
df1$Team1Time
df1$Team_2_Start<- strptime(x = as.character(df1$Team_2_Start),
                             format = "%d-%m-%y %H:%M")
df1$Team_2_End<- strptime(x = as.character(df1$Team_2_End),
                            format = "%d-%m-%y %H:%M")
df1$Team2Time=difftime(df1$Team_2_End,df1$Team_2_Start)
df1$Team_3_Start <- strptime(x = as.character(df1$Team_3_Start),
                             format = "%d-%m-%y %H:%M")
df1$Team_3_End  <- strptime(x = as.character(df1$Team_3_End),
                            format = "%d-%m-%y %H:%M")
df1$Team3Time=difftime(df1$Team_3_End,df1$Team_3_Start)
df1$Completed <- strptime(x = as.character(df1$Completed),
                            format = "%d-%m-%y %H:%M")
df1$Closed <- strptime(x = as.character(df1$Closed),
                           format = "%d-%m-%y %H:%M")
df1$Closed
# difference in time between bill being closed and completed
df1$CloseComplete=difftime(df1$Closed,df1$Completed)
# change represents difference between original charge and reduced charge
df1$Change=df1$Original_Charge-df1$Reduction
# Delayteam12 represents end of team 1's activities and team 2's start
df1$Delayteam12=difftime(df1$Team_1_End,df1$Team_2_Start)
# Delayteam23 represents end of team 2's activities and team 3's start
df1$Delayteam23=difftime(df1$Team_2_End,df1$Team_3_Start)
#Delayteam3Completed represents timedifference between team 3's end and completed time
df1$Delayteam3Completed=difftime(df1$Team_3_End,df1$Completed)
head(df1)

#df2=write.csv(df1,"yorknew.csv")
#df1$Team2Time
#df1$Team_2_Start
#str(df1)
#all values to be converted to minutes
# Total time is in minutes
#Team1time is in seconds, so divide by 60
df1$Team1Time=as.numeric(df1$Team1Time/60)
#Team2time is in seconds, so divide by 60
df1$Team2Time=as.numeric(df1$Team2Time/60)
#Team3time is in seconds, so divide by 60
df1$Team3Time=as.numeric(df1$Team3Time/60)
#CloseComplete is in hours, so multiply by 60
df1$CloseComplete=as.numeric(df1$CloseComplete*60)
#Change is a numeric value so leave as is
# Delayteam12 is in minutes, so leave as is
# Delayteam23 is in seconds , so divide by 60
df1$Delayteam23=as.numeric(df1$Delayteam23/60)
# Delayteam3Completed delay is in minutes so fine 
df1$TotalTime=-df1$TotalTime
df1$Delayteam12=-df1$Delayteam12
df1$Delayteam3Completed=-df1$Delayteam3Completed
head(df1)
df1$TotalTime=as.numeric(df1$TotalTime)
df1$Delayteam12=as.numeric(df1$Delayteam12)
df1$Delayteam3Completed=as.numeric(df1$Delayteam3Completed)
# all transformed and all time differences in minutes
head(df1)

df2=write.csv(df1,"yorknew1.csv")
#encoding values for 0 as when team1 didn't work on bill and 1 when it did
for (i in 1:nrow(df1)){
  if(is.na(df1[i,"Team1Time"])==TRUE)
     {
    df1[i,"Team1Worked"]=0
  } 
  else df1[i,"Team1Worked"]=1
}
str(df1$Team1Worked)

df1$Team1Worked
# encoding for team2 as 0 and 1 with same logic
for (i in 1:nrow(df1)){
  if(is.na(df1[i,"Team2Time"])==TRUE)
  {
    df1[i,"Team2Worked"]=0
  } 
  else df1[i,"Team2Worked"]=1
}
df1$Team2Worked
for (i in 1:nrow(df1)){
  if(is.na(df1[i,"Team3Time"])==TRUE)
  {
    df1[i,"Team3Worked"]=0
  } 
  else df1[i,"Team3Worked"]=1
}
head(df1)
df3=write.csv(df1,"yorknew5.csv")
library(DataExplorer)
plot_str(df1)
#introduce(df1)
#plot_intro(df1)
plot_missing(df1)
#since  delayteam2end might be empty we are considering that as well
df1$Delayteam13=difftime(df1$Team_3_Start,df1$Team_1_End)
df1$Delayteam13
plot_missing(df1)
#histogram plots
plot_histogram(df1)
#correlation matrix
plot_correlation(df1,maxcat = 50)
pcadf1 <- na.omit(df1[, c("Team3Worked","Team2Worked","Team1Worked","TotalTime","Change","Original_Charge","State","Type_A","Type_B","Type_C","Type_C","Type_D","Type_E","Type_F")])
plot_prcomp(pcadf1, variance_cap = 0.9, nrow = 2L, ncol = 2L)
pcadf1
plot_correlation(na.omit(df1[, c("Team3Worked","Team2Worked","Team1Worked","TotalTime","Change","Original_Charge","State","Type_A","Type_B","Type_C","Type_C","Type_D","Type_E","Type_F")]))
df6=write.csv(df1,"yorknew6.csv")
require(ggplot2)
#team1TimeVsChange
p <- ggplot(data = df1,
            mapping = aes(x = Team1Time, y = Change))
p + geom_point()+ ggtitle("Team1Time Vs Change")

p1 <- ggplot(data = df1,
            mapping = aes(x = Team2Time, y = Change))
p1 + geom_point()+ ggtitle("Team2Time Vs Change")

p2 <- ggplot(data = df1,
             mapping = aes(x = Team3Time, y = Change))
p2 + geom_point()+ ggtitle("Team3Time Vs Change")
#geom_smooth also can be used to get a line with some error in between
head(df1)
#group by state, time vs change
p3 <- ggplot(data = df1,
            mapping = aes(x =TotalTime,
                          y = Change))
p3 + geom_point() + facet_wrap(~State)
table(df1$Client_ID,df1$State)
#statecount
p4 <- ggplot(data = df1,
            mapping = aes(x = State, fill =State))
p4 + geom_bar() + guides(fill = FALSE)
head(df1)
df1$Delayteam13=as.numeric(df1$Delayteam13)
#type A vs States
p5 <- ggplot(data = df1,
             mapping = aes(x=State,y=Type_A))
p5 + geom_bar(stat = "identity")
#typeB vs State
p6 <- ggplot(data = df1,
             mapping = aes(x=State,y=Type_B))
p6 + geom_bar(stat = "identity")
#typeC vs State
p7 <- ggplot(data = df1,
             mapping = aes(x=State,y=Type_C))
p7 + geom_bar(stat = "identity")
#typeD vs State
p8 <- ggplot(data = df1,
             mapping = aes(x=State,y=Type_D))
p8 + geom_bar(stat = "identity")
#typeE vs State
p9 <- ggplot(data = df1,
             mapping = aes(x=State,y=Type_E))
p9 + geom_bar(stat = "identity")
#typeF vs State
p10 <- ggplot(data = df1,
             mapping = aes(x=State,y=Type_F))
p10 + geom_bar(stat = "identity")
head(df1)
library(caret)
install.packages("caret")
require(caret)
library(rpart)
library(party)
#conditional Inference Tree
fit <- ctree(Change ~Original_Charge+State+TotalTime+Team3Time+Team2Time+Team1Time+Type_A+Type_B+Type_C+Type_D+Type_E+Type_F+Delayteam12+Delayteam13+Delayteam23, 
             data=df1)
plot(fit, main="Conditional Inference Tree for Change in charge")
#rpart tree for change 
tree4 <- rpart(Change ~Original_Charge+State+TotalTime+Team3Time+Team2Time+Team1Time+Type_A+Type_B+Type_C+Type_D+Type_E+Type_F+Delayteam12+Delayteam13+Delayteam23, 
              data=df1,method="anova")
require(rpart.plot)
install.packages("rpart.plot")
rpart.plot(tree4)
plotcp(tree4)
tree4$cptable
# 62% accuracy model
#random forest model with importance and 300 trees,
require(randomForest)
randforest1=randomForest(Change ~Original_Charge+State+TotalTime+Team3Time+Team2Time+Team1Time+Type_A+Type_B+Type_C+Type_D+Type_E+Type_F+Delayteam12+Delayteam13+Delayteam23, 
     data=df1,na.action = na.omit,importance=TRUE,ntrees=300,type="regression")
importance(randforest1)
require(Hmisc)
library(dplyr)
#trying to get certain values of state vs number of clients
df1[, c("Team3Worked","Team2Worked","Team1Worked","TotalTime","Change","Original_Charge","State","Type_A","Type_B","Type_C","Type_C","Type_D","Type_E","Type_F","Client_ID")] %>% group_by(State) %>% count(Client_ID)
df1
head(df1)
df4=write.csv(df1,"york8.csv")
randforest1
df2 <- as.data.frame(lapply(df1, unlist))
Hmisc::describe(df2)
