library(WriteXLS)
library(survival)
library(survminer)
library(dplyr)
library(openxlsx)
library(ggplot2)


rm(list=ls())

## Ler arquivo xlsx
rawdata <- read.xlsx("FAN1.xlsx", sheet =2)
data_filter <- rawdata
rownames(data_filter) <- data_filter$Name
data_filter <- data_filter[,-1]


data_filter$status <- factor(data_filter$Status, 
                         levels = c("0", "1"), 
                         labels = c("alive", "dead"))

#data_filter$status <- factor(data_filter$CDK7.Group, 
                             #levels = c("0", "1"), 
                             #labels = c("low", "high"))

hist(data_filter$FAN1) 
hist(data_filter$Overall.Survival) 

#data_filter <- data_filter %>% mutate(FAN1 = ifelse
                                      #(data_filter$CDK7.Group >=data_filter$X8, "high", "low"))

data_filter$FAN1 <- factor(data_filter$FAN1.Group.Name)


# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = data_filter$Overall.Survival, 
                    event = data_filter$Status)
surv_object 

data_esp <- data_filter$Overall.Survival
fit1 <- survfit(surv_object ~ FAN1.Group.Name, data = data_filter)
summary(fit1)

ggsurvplot (fit1 , pval = TRUE)



dev.off()

  



