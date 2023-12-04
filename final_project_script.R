## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$`length:seconds`)
sd(data$`length:seconds`)
table(data$`length:seconds`)
describe(data$`length:seconds`)
summary(data$`length:seconds`)

mean(data$`streams:millions`)
sd(data$`streams:millions`)
table(data$`streams:millions`)
describe(data$`streams:millions`)
summary(data$`streams:millions`)

mean(data$`Mood`)
sd(data$`Mood`)
table(data$`Mood`)
describe(data$`Mood`)
summary(data$`Mood`)

mean(data$`album`)
sd(data$`album`)
table(data$`album`)
describe(data$`album`)
summary(data$`album`)
##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Mood, data$album)
##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Mood, data$album))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(data$`streams:millions` ~ data$Mood, data = data)
summary(anova_adapted)
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$`streams:millions`, data$Mood)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(data$`streams:millions` ~ data$`length:seconds`)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(data$`length:seconds`, data$`streams:millions`)
print(linear_plot)
abline(linear_relationship, col = 'red')
abline(v=236.4769, col = 'pink')
abline(h=260.0677, col = 'blue')

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$`streams:millions`, residuals(linear_relationship))
abline(h=260.0677, col = 'blue')