cat("\014")
rm(list=ls())

library(stringr)
setwd("C:/Users/colet/Downloads/Adelphi/Data Visualization")

mlb <- read.csv("Main_Mlb_Payroll_Data.csv")
str(mlb)
summary(mlb)

#string normalization
table(mlb$Team)
table(mlb$Position)
table(mlb$Playoffs)
table(mlb$Position)
table(mlb$WSWin)
table(mlb$HighestPayroll)
table(mlb$LowestPayroll)
