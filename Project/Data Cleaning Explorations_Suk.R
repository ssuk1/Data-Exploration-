library(ggplot2)
library(fixest)
library(wooldridge)
library(multcomp)
library(vtable)
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(jtools)
library(readr)
library(tidyverse)
library(Ecdat)
library(car)
library(plm)
library(vtable)
library(DT)
library(readxl)
library(lubridate)


#Reade in CSV
scorecard <- read.csv("Most+Recent+Cohorts+(Scorecard+Elements).csv", header = TRUE) 


id_name_link <- read.csv("id_name_link.csv", header = TRUE)
id_name_link <- id_name_link %>%
  group_by(schname) %>%
  mutate(N=n()) %>%
  filter(N==1)
colnames(id_name_link) <- c("UNITID", "OPEID", "schname","N")

trends <- list.files(pattern = "trends_", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()

##Joining trends to id name link by column "schname".

id_trends <- left_join(x=id_name_link, y= trends, by = "schname")

##Joining id_trends to scorecard by "unitid".

alldata <- left_join(x = id_trends, y= scorecard, by= "UNITID")

##All data that has universities that grant bachelor degrees

alldata <- alldata %>%
  filter(PREDDEG==3)

alldata <- alldata %>%
  group_by(schname, keyword) %>%
  mutate(index_std = (index - mean(index))/sd(index, na.rm = TRUE))

##changing the time
alldata <- alldata %>%
  mutate(date = str_sub(monthorweek, 1, 10)) %>%
  mutate(date = ymd(date)) %>%
  mutate(beforeSC = date < ymd('2015-08-31')) %>%
  mutate(afterSC = date > ymd('2015-09-01'))

##Before and after treatment (scorecard)
month_week <- alldata %>% 
  group_by(monthorweek) %>% 
  summarize(n = n())
#remove dates that are marked as 'NA'
month_week_clean <- na.omit(month_week, c("monthorweek"))
#filtering out dates after September 2015
pre_scorecard <- month_week_clean$monthorweek[1:128]
pre_scorecard <- as.data.frame(pre_scorecard)

earnings <- alldata %>% 
  group_by(UNITID, schname) %>% 
  summarize(mean(as.numeric(md_earn_wne_p10.REPORTED.EARNINGS))) %>% 
  na.omit(earnings, c("md_earn_wne_p10.REPORTED.EARNINGS"))

earnings <- rename(earnings, avg_earnings = "mean(as.numeric(md_earn_wne_p10.REPORTED.EARNINGS))")

alldata <- left_join(x= alldata, y=earnings, by= "UNITID", "schname")
alldata <- alldata[,-c(16:103)]

mean(alldata$avg_earnings)
##43716.1
max(alldata$avg_earnings)
##166200
min(alldata$avg_earnings)
##16700
median(alldata$avg_earnings)
##42000

dnorm(alldata$avg_earnings, mean= 43716.1, sd = 10000)

##Splitting earnings
H_earnings <- filter(alldata, between(avg_earnings, 60000, 110000))
L_earnings <- filter(alldata, between(avg_earnings, 25000, 45000))

write_csv(H_earnings, "high_dataclean.csv")

write_csv(L_earnings, "low_dataclean.csv")
