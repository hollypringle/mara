#Load data and packages

meta_v2<-read.csv("Kenya_CT2018_metadata_withdims.csv")

library(dplyr)
library(tidyverse)

#Check total range of survey

meta_v2_range <-as.data.frame( meta_v2 %>%
                                 group_by(CT_site) %>%
                                 summarize(DateTime = range(DateTime)))
meta_v2_range<-as.data.frame(meta_v2_range %>%
                               mutate(variable = rep(c("Start", "End"), nrow(meta_v2_range) / 2),
                                      key = rep(1:(nrow(meta_v2_range) / 2), each = 2)) %>%
                               pivot_wider(id_cols = CT_site, names_from = variable, values_from = DateTime))

#Check maintenance times in metadata

meta_servicing_time<-meta_v2[grepl("000001|020001", meta_v2$ImageID),]

meta_servicing_time_check<-meta_servicing_time %>%   filter(time_hour< 10|time_hour>18)

#Record correction notes in excel (camera date fix)

meta_v2$datecheck<-"notassigned" #set everything to not checked

meta_v2<-meta_v2%>%
  mutate(datecheck = ifelse(grepl("2016", Date), "incorrect", meta_v2$datecheck)) #anything with 2016 marked as incorrect

meta_v2<-meta_v2%>%
  mutate(datecheck = ifelse(grepl("2017", Date), "incorrect", meta_v2$datecheck)) #anything with 2016 marked as incorrect

meta_v2<-meta_v2%>%
  mutate(datecheck = ifelse(grepl("NB07", CT_site), "remove", meta_v2$datecheck)) #NB07 needs to be corrected

meta_v2<-meta_v2%>%
  mutate(datecheck = ifelse(grepl("NB02", CT_site), "remove", meta_v2$datecheck)) #NB02 cannot be corrected

meta_v2<-meta_v2%>%
  mutate(datecheck = ifelse(grepl("MN49", CT_site), "incorrect", meta_v2$datecheck))

meta_v2$DateTime_corrected<-meta_v2$DateTime


################scrap######################
require(lubridate)
# ymd function parses dates in year-month-day format
startDate <- ymd('2013-01-31')
# The %m+% adds months to dates without exceeding the last day
myDates <- startDate %m+% months(c(0:6))

