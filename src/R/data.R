setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("readr")
library("stats")
library("dplyr")
library("apollo")

rawData = read.csv(
  "C:/Users/kemakino/OneDrive - University of California, Davis/Class/TTP289A/HW3/HW3 Part 1 Team Assignment/chicorr3.csv"
)

seqEl = seq(1,dim(rawData)[1]-2,3)
seqTrain = seqEl + 1
seqAuto = seqEl + 2

data = data.frame(matrix(0,dim(rawData)[1] / 3 ,1))

data$id = 1:dim(rawData)[1]/3
data$choice = (rawData$idalt * rawData$ichoslh) %>% .[. != 0]
data$lhfs1_el = rawData$lhfs1[seqEl]
data$lhfs1_train = rawData$lhfs1[seqTrain]
data$lhfs1_auto = rawData$lhfs1[seqAuto]
data$lhfs3_el = rawData$lhfs3[seqEl]
data$lhfs3_train = rawData$lhfs3[seqTrain]
data$lhfs3_auto = rawData$lhfs3[seqAuto]
data$minttime_el = rawData$minttime[seqEl]
data$minttime_train = rawData$minttime[seqTrain]
data$minttime_auto = rawData$minttime[seqAuto]
data = data[,-1]

write.csv(data,"data.csv")

