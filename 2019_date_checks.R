###Load packages and data

library(dplyr)
library(tidyverse)
library(lubridate)
meta_2019<-read.csv("C:/Users/Holly/Documents/Biome_Health_Project/Data/Kenya_CT2019_metadata_withdims.csv")

###Put date in correct format
meta_2019$DateTime<-sub(":","-",meta_2019$DateTime) #run twice to replace first two instances
meta_2019$DateTime<-sub(":","-",meta_2019$DateTime) 
meta_2019$Date<-sub(":","-",meta_2019$Date) #run twice to replace first two instances
meta_2019$Date<-sub(":","-",meta_2019$Date) 
meta_2019$Date<-as.Date(meta_2019$Date, format ="%Y-%m-%d")
head(meta_2019)
str(meta_2019)

#Check total range of dates at each camera trap site
names(meta_2019)
nrow(meta_2019) #3746475
meta_2019<- meta_2019[!is.na(meta_2019$Date), ] #remove rows where date is NA
nrow(meta_2019) #3746465 #10 images had no date

meta_2019_range <-as.data.frame( meta_2019 %>%
                                 group_by(CT_site) %>%
                                 summarize(Date = range(Date)))
meta_2019_range<-as.data.frame(meta_2019_range %>%
                               mutate(variable = rep(c("Start", "End"), nrow(meta_2019_range) / 2),
                                      key = rep(1:(nrow(meta_2019_range) / 2), each = 2)) %>%
                               pivot_wider(id_cols = CT_site, names_from = variable, values_from = Date))

#Check maintenance times in metadata

meta_servicing_time<-meta_2019[grepl("000001|020001|040001", meta_2019$ImageID),]
meta_servicing_time_check<-meta_servicing_time %>%   filter(time_hour< 10|time_hour>18)


#Record correction notes in excel (camera date fix)

#onto correcting

meta_2019$datecheck<-"notassigned" #set everything to not checked
meta_2019$DateTime_corrected<-meta_2019$DateTime #add column for corrected date
meta_2019$ImageID_corrected<-meta_2019$ImageID #add column for corrected date

###LIST OF ERRORS###
#MN16- Actually MN35 up from 2019_MN16_00001 until 2019_MN16_005960-done
which(meta_2019$ImageID == '2019_MN16_000001.JPG') #289020 #locating misnamed images
which(meta_2019$ImageID == '2019_MN16_005960.JPG') #294979
meta_2019[289020:294979, 16]= "remove_misnamed_files"  #removing as they are already in the correct place, these are duplicates
meta_2019[289020:294979, 18]= "NA"  #removing as they are already in the correct place, these are duplicates
meta_2019[289020:294979, 17]= "NA"  #removing as they are already in the correct place, these are duplicates


#MN17- add 12 hours to first, round, 24 hours to second -done
which(meta_2019$ImageID == '2019_MN17_000001.JPG') #316044 #locating first round
which(meta_2019$ImageID == '2019_MN17_012441.JPG') #328483
meta_2019[316044:328483, 16]= "plus_12_hours"

which(meta_2019$ImageID == '2019_MN17_020001.JPG') #328484 #locating second round
which(meta_2019$ImageID == '2019_MN17_023437.JPG') #331920
meta_2019[328484:331920, 16]= "plus_24_hours" #maintenance put it back 12 hours, was meant to go forward 12 hours, so therefore need to add 24 hours

#MN30- add 24 hours to first round -done
which(meta_2019$ImageID == '2019_MN30_000001.JPG') #565520 #locating first round
which(meta_2019$ImageID == '2019_MN30_005508.JPG') #571022
meta_2019[565520:571022, 16]= "plus_24_hours" #wrong set up day

#MN31- set of 2017 photos on setup that need removing- done
which(meta_2019$ImageID == '2019_MN31_000001.JPG') #583404 #locating misnamed images
which(meta_2019$ImageID == '2019_MN31_000008.JPG') #583411
meta_2019[583404:583411, 16]= "remove_setup_files"  
meta_2019[583404:583411, 18]= "NA"  #removing as they are already in the correct place, these are duplicates
meta_2019[583404:583411, 17]= "NA"  #removing as they are already in the correct place, these are duplicates

#MN35- add 24 hours to first and second round- done
which(meta_2019$ImageID == '2019_MN35_000001.JPG') #677913 #locating first round
which(meta_2019$ImageID == '2019_MN35_005960.JPG') #683872
meta_2019[677913:683872, 16]= "plus_24_hours" #wrong set up 

which(meta_2019$ImageID == '2019_MN35_020001.JPG') #683873 #locating second round
which(meta_2019$ImageID == '2019_MN35_024913.JPG') #688785
meta_2019[683873:688785, 16]= "plus_24_hours" #wrong set up day

#MT03- add 12 hours to first round - done
which(meta_2019$ImageID == '2019_MT03_000001.JPG') #1068002 #locating first round
which(meta_2019$ImageID == '2019_MT03_015129.JPG') #1083129
meta_2019[1068002:1083129, 16]= "plus_12_hours" #wrong set up 

#MT11- change year from 2017 to 2019 - done
meta_2019<-meta_2019%>%
  mutate(datecheck = ifelse(grepl("MT11", CT_site), "change_to_2019", meta_2019$datecheck)) #wrong year

#MT20- second round goes forward 24 hours -done
which(meta_2019$ImageID == '2019_MT20_020001.JPG') #1611994 #locating second round
which(meta_2019$ImageID == '2019_MT20_030166.JPG') #1622158
meta_2019[1611994:1622158, 16]= "plus_24_hours" #wrong set up day

#MT42- add 12 hours to first round - done
which(meta_2019$ImageID == '2019_MT42_000001.JPG') #2076991 #locating first round
which(meta_2019$ImageID == '2019_MT42_001483.JPG') #2078473
meta_2019[2076991:2078473, 16]= "plus_12_hours" #wrong set up 

#MT46- add 12 hours to first round - done
which(meta_2019$ImageID == '2019_MT46_000001.JPG') #2137607 #locating first round
which(meta_2019$ImageID == '2019_MT46_000472.JPG') #2138078
meta_2019[2137607:2138078, 16]= "plus_12_hours" #wrong set up 

#MT47 - add 12 hours to first round - done
which(meta_2019$ImageID == '2019_MT47_000001.JPG') #2149911 #locating first round
which(meta_2019$ImageID == '2019_MT47_014841.JPG') #2164750
meta_2019[2149911:2164750, 16]= "plus_12_hours" #wrong set up 

#NB14- change from 2018 to 2019 - done
meta_2019<-meta_2019%>%
  mutate(datecheck = ifelse(grepl("NB14", CT_site), "change_to_2019", meta_2019$datecheck)) #wrong year

#NB16- rounds 1 and 2 go back 12 hours - done  #year needs correcting but deal with in subset
which(meta_2019$ImageID == '2019_NB16_000001.JPG') #3052558 #locating first round
which(meta_2019$ImageID == '2019_NB16_007326.JPG') #3059883
meta_2019[3052558:3059883, 16]= "minus_12_hours" #wrong set up 

which(meta_2019$ImageID == '2019_NB16_020001.JPG') #3059884 #locating first round
which(meta_2019$ImageID == '2019_NB16_030512.JPG') #3070394
meta_2019[3059884:3070394, 16]= "minus_12_hours" #wrong set up 

#NB22- set of 2017 photos on setup that need removing- done
which(meta_2019$ImageID == '2019_NB22_000001.JPG') #3174013 #locating misnamed images
which(meta_2019$ImageID == '2019_NB22_000028.JPG') #3174040
meta_2019[3174013:3174040, 16]= "remove_setup_files"  
meta_2019[3174013:3174040, 18]= "NA"  
meta_2019[3174013:3174040, 17]= "NA"  


#NB27- First two rounds need to go 12 hours forward - done
which(meta_2019$ImageID == '2019_NB27_000001.JPG') #3256634 #locating first round
which(meta_2019$ImageID == '2019_NB27_002330.JPG') #3258963
meta_2019[3256634:3258963, 16]= "plus_12_hours" #wrong set up 

which(meta_2019$ImageID == '2019_NB27_020001.JPG') #3258964 #locating first round
which(meta_2019$ImageID == '2019_NB27_023010.JPG') #3261973
meta_2019[3258964:3261973, 16]= "plus_12_hours" #wrong set up 


#NB29 - first round needs to go 12 hours back, NB29_020001 until NB29_027321 is actually NB22- done
which(meta_2019$ImageID == '2019_NB29_000001.JPG') #3287637 #locating first round
which(meta_2019$ImageID == '2019_NB29_005570.JPG') #3293206
meta_2019[3287637:3293206, 16]= "minus_12_hours" #wrong set up 

which(meta_2019$ImageID == '2019_NB29_020001.JPG') #3293207 #locating misnamed images
which(meta_2019$ImageID == '2019_NB29_027321.JPG') #3300527
meta_2019[3293207:3300527, 16]= "rename_as_NB22" #misnamed files

#NB32- images NB32_000001 up to NB32_007123 is actually NB12 - done
which(meta_2019$ImageID == '2019_NB32_000001.JPG') #3334726 #locating misnamed images
which(meta_2019$ImageID == '2019_NB32_007123.JPG') #3341848
meta_2019[3334726:3341848, 16]= "rename_as_NB12" #misnamed files  #not done survey effort yet

#NB35- round 1 and 2 go forward 12 hours - done
which(meta_2019$ImageID == '2019_NB35_000001.JPG') #3408280 #locating first round
which(meta_2019$ImageID == '2019_NB35_007492.JPG') #3415771
meta_2019[3408280:3415771, 16]= "plus_12_hours" #wrong set up 

which(meta_2019$ImageID == '2019_NB35_020001.JPG') #3415772 #locating first round
which(meta_2019$ImageID == '2019_NB35_025837.JPG') #3421608
meta_2019[3415772:3421608, 16]= "plus_12_hours" #wrong set up 

#NB39- round 1 and 2 go forward 12 hours - done
which(meta_2019$ImageID == '2019_NB39_000001.JPG') #3499273 #locating first round
which(meta_2019$ImageID == '2019_NB39_005849.JPG') #3505121
meta_2019[3499273:3505121, 16]= "plus_12_hours" #wrong set up 

which(meta_2019$ImageID == '2019_NB39_025851.JPG') #3505122 #locating first round
which(meta_2019$ImageID == '2019_NB39_033356.JPG') #3512533
meta_2019[3505122:3512533, 16]= "plus_12_hours" #wrong set up 

#NB50- first round and second to go forward 12 hours - done
which(meta_2019$ImageID == '2019_NB50_000001.JPG') #3729801 #locating first round
which(meta_2019$ImageID == '2019_NB50_006014.JPG') #3735814
meta_2019[3729801:3735814, 16]= "plus_12_hours" #wrong set up 

which(meta_2019$ImageID == '2019_NB50_020001.JPG') #3735815 #locating first round
which(meta_2019$ImageID == '2019_NB50_024266.JPG') #3740080
meta_2019[3735815:3740080, 16]= "plus_12_hours" #wrong set up 

#OMC05- first round to go forward 12 hours- done
which(meta_2019$ImageID == '2019_OMC05_020001.JPG') #2299732 #locating first round
which(meta_2019$ImageID == '2019_OMC05_035297.JPG') #2315027
meta_2019[2299732:2315027, 16]= "plus_12_hours" #wrong set up 

#OMC36- change year from 2018 to 2019
meta_2019<-meta_2019%>%
  mutate(datecheck = ifelse(grepl("OMC36", CT_site), "change_to_2019", meta_2019$datecheck)) #wrong year


#allcams fine
#meta_v2$yearcheck<-format(as.Date(meta_v2$Date, format="%Y-%m-%d"),"%Y")
#meta_v2$datecheck <- ifelse(grepl("MN10", meta_v2$CT_site) & grepl("2017", meta_v2$yearcheck),
#                   "remove_wrong_year",meta_v2$datecheck)
#    meta_v2$DateTime_corrected<- ifelse(grepl("MN10", meta_v2$CT_site) & grepl("2017", meta_v2$yearcheck),
#                            "remove",meta_v2$DateTime_corrected)

####################################################################
#Correcting 'minus 12 hours subset"  
minus12<-subset(meta_2019, datecheck == "minus_12_hours") #23407
sort(unique(minus12$CT_site))
# "NB16"yes  "NB29"yes
minus12$DateTime_corrected<- as_datetime(minus12$DateTime_corrected)    
minus12$DateTime_corrected<-minus12$DateTime_corrected - hours(12)    



#Correcting 'plus 12 hours subset"  
plus12<-subset(meta_2019, datecheck == "plus_12_hours") #101869
sort(unique(plus12$CT_site))
#"MN17"yes  "MT03" yes  "MT42"yes  "MT46"yes  "MT47"yes  "NB27"yes  "NB35"yes  "NB39"yes  "NB50"yes  "OMC05"yes
plus12$DateTime_corrected<- as_datetime(plus12$DateTime_corrected)    
plus12$DateTime_corrected<-plus12$DateTime_corrected + hours(12)  

#Correcting 'plus 24 hours subset"  
plus24<-subset(meta_2019, datecheck == "plus_24_hours") #29978
sort(unique(plus24$CT_site))
#"MN17"yes "MN30"yes "MN35"yes "MT20"yes
plus24$DateTime_corrected<- as_datetime(plus24$DateTime_corrected)    
plus24$DateTime_corrected<-plus24$DateTime_corrected + hours(24)


#Correcting 'rename as NB12 subset"
rename_as_NB12<-subset(meta_2019, datecheck == "rename_as_NB12") #7123
sort(unique(rename_as_NB12$CT_site))
#NB32- correct
rename_as_NB12$ImageID_corrected<-gsub("NB32", "NB12", rename_as_NB12$ImageID_corrected)

#Correcting 'rename as NB22 subset"
rename_as_NB22<-subset(meta_2019, datecheck == "rename_as_NB22") #7321
sort(unique(rename_as_NB22$CT_site))
#NB29- correct
rename_as_NB22$ImageID_corrected<-gsub("NB29", "NB22", rename_as_NB22$ImageID_corrected)

sort(unique(meta_2019$datecheck))
# "change_to_2019" - do afterwards       "minus_12_hours" done           
# "plus_12_hours" done        "plus_24_hours" done        "remove_misnamed_files"-done
# "remove_setup_files" done    "rename_as_NB12" done       "rename_as_NB22" done

#Corrections done on
#NB16
#NB27
#NB29 (SEPARATE ROUNDS) 
#NB32 #MN17(TWO SEPARATE ROUNDS)
#NB35
#NB39
#NB50
#MN30 
#MN35 
#MT03
#MT20  
#MT42 
#MT46 
#MT47
#OMC05

#######################################################
corrected_rows<-rbind(minus12, plus12, plus24, rename_as_NB12, rename_as_NB22) 
nrow(corrected_rows)#169698 (23407+101869+29978+7123+7321= 169698- correct)
n_occur <- data.frame(table(corrected_rows$ImageID_corrected))
#gives you a data frame with a list of ids and the number of times they occurred.
n_occur[n_occur$Freq > 1,] #no images are duplicated
corrected_rows[duplicated(corrected_rows$ImageID_corrected),]#no images are duplicated

removed_corrected_meta_2019<- anti_join(meta_2019, corrected_rows,by = "ImageID") 
nrow(meta_2019) #3746465
nrow(corrected_rows) #169698
nrow(removed_corrected_meta_2019) #3576767 (3746465-169698=3576767 - correct)

N.img.meta_2019<- length(unique(meta_2019$ImageID)) #3746465
N.img.removed_corrected_meta_2019<- length(unique(removed_corrected_meta_2019$ImageID)) #3576767
N.img.corrected_rows<- length(unique(corrected_rows$ImageID)) #169698 - correct, no rows duplicated

removed_corrected_meta_2019$DateTime_corrected<- as_datetime(removed_corrected_meta_2019$DateTime_corrected)    #5996 failed to parse #ok as these are to be removed
meta_2019_corrected<- bind_rows(removed_corrected_meta_2019,corrected_rows) #2357374 obs - correct
nrow(meta_2019_corrected) #3746465 - correct
N.img.meta_2019_corrected<- length(unique(meta_2019_corrected$ImageID_corrected)) #3740470- ok as some were removed
N.img.meta_2019_corrected_origimage<- length(unique(meta_2019_corrected$ImageID)) #3746465- correct
#3746465-3740470= 5995 
#one additional date failed to parse- why?
sum(is.na(meta_2019_corrected$DateTime_corrected)) #5996
which_table<-as.data.frame(which(is.na(meta_2019_corrected$DateTime_corrected)))


meta_2019_corrected$yearcheck<-format(as.Date(meta_2019_corrected$Date, format="%Y-%m-%d"),"%Y")
sort(unique(meta_2019_corrected$yearcheck))
#"2017" "2018" "2019"
meta_2019_corrected$datecheck <- ifelse(grepl("OMC06", meta_2019_corrected$CT_site) & grepl("2018", meta_2019_corrected$yearcheck),
                                               "change_to_2019",meta_2019_corrected$datecheck)
meta_2019_corrected$datecheck <- ifelse(grepl("NB16", meta_2019_corrected$CT_site) & grepl("2018", meta_2019_corrected$yearcheck),
                                        "minus_12_hours_change_to_2019",meta_2019_corrected$datecheck)
meta_2019_corrected$datecheck <- ifelse(grepl("NB25", meta_2019_corrected$CT_site) & grepl("2017", meta_2019_corrected$yearcheck),
                                        "change_to_2019",meta_2019_corrected$datecheck)

meta_2019_corrected_year<-meta_2019_corrected
str(meta_2019_corrected_year)
year(meta_2019_corrected_year$DateTime_corrected)
unique(year(meta_2019_corrected_year$DateTime_corrected))
unique(year(meta_2019_corrected_year$DateTime))
year(meta_2019_corrected_year$DateTime_corrected)<-2019


# get time from datetime using format
meta_2019_corrected_year$Time_corrected<-format(meta_2019_corrected_year$DateTime_corrected, format = "%H:%M:%S")
meta_2019_corrected_year$Hour_corrected<-format(meta_2019_corrected_year$DateTime_corrected, format = "%H")
# get date from datetime using format
meta_2019_corrected_year$Date_corrected<-format(meta_2019_corrected_year$DateTime_corrected, format = "%Y-%m-%d")
nrow(meta_2019_corrected_year)
#Checking
meta_2019_corrected_year_newcheck<- meta_2019_corrected_year[complete.cases(meta_2019_corrected_year$DateTime_corrected),] #15283 to remove #2357374-15283=  2342091 #2342091 obs: correct
nrow(meta_2019_corrected_year_newcheck) #5996 to remove
#Check maintenance times in metadata

newcheck_servicing_time<-meta_2019_corrected_year_newcheck[grepl("000001|020001|040001", meta_2019_corrected_year_newcheck$ImageID),]
newcheck_time_check<-newcheck_servicing_time %>%   filter(Hour_corrected< 10|time_hour>18) #only one left is 2018_MT36_020001.JPG : this is okay, maintenance images missing but rest of times seem ok
#all between 8am and 7pm

#Check total range of survey

meta_2019_corrected_year_newcheck_range <-as.data.frame(meta_2019_corrected_year_newcheck%>%
                                group_by(CT_site) %>%
                                summarize(DateTime_corrected = range(DateTime_corrected)))
meta_2019_corrected_year_newcheck_range<-as.data.frame(meta_2019_corrected_year_newcheck_range %>%
                               mutate(variable = rep(c("Start", "End"), nrow(meta_2019_corrected_year_newcheck_range) / 2),
                                     key = rep(1:(nrow(meta_2019_corrected_year_newcheck_range) / 2), each = 2)) %>%
                              pivot_wider(id_cols = CT_site, names_from = variable, values_from = DateTime_corrected))

### MISSED ONE ! Of course... :') Correcting first round of MT38 - needs to go forward 31 days
which(meta_2019_corrected_year$ImageID == '2019_MT38_000001.JPG') #1954884 #locating first round
which(meta_2019_corrected_year$ImageID == '2019_MT38_002552.JPG') #1957435
names(meta_2019_corrected_year)
meta_2019_corrected_year[1954884:1957435, 16]= "plus_1_month" #wrong set up 

plusmonth<-subset(meta_2019_corrected_year, datecheck == "plus_1_month") #29978
sort(unique(plusmonth$CT_site))
#"MT38" - yes
plusmonth$DateTime_corrected<- as_datetime(plusmonth$DateTime_corrected)    
plusmonth$DateTime_corrected<-plusmonth$DateTime_corrected + days(31)
plusmonth$Date_corrected<-format(plusmonth$DateTime_corrected, format = "%Y-%m-%d")

#plusmonth is 2552 obs
removed_plusmonth_meta_2019<- anti_join(meta_2019_corrected_year, plusmonth,by = "ImageID") 
nrow(meta_2019_corrected_year) #3746465 -yes
nrow(plusmonth) #2552 - yes
nrow(removed_plusmonth_meta_2019) #3743913 (3746465-2552= 3743913 - correct)

N.img.meta_2019_corrected_year<- length(unique(meta_2019_corrected_year$ImageID)) #3746465 - yes
N.img.removed_plusmonth_meta_2019<- length(unique(removed_plusmonth_meta_2019$ImageID)) #3743913 -yes
N.img.plusmonth<- length(unique(plusmonth$ImageID)) #2552 - correct, no rows duplicated
meta_2019_corrected_v1<- bind_rows(removed_plusmonth_meta_2019,plusmonth) #2357374 obs - correct
nrow(meta_2019_corrected_v1) #3746465 - correct
N.img.meta_2019_corrected_v1<- length(unique(meta_2019_corrected_v1$ImageID_corrected)) #3740470- ok as some were removed
N.img.meta_2019_corrected_v1_origimage<- length(unique(meta_2019_corrected_v1$ImageID)) #3746465- correct
#3746465-3740470= 5995 

#check times and ranges for a final time

#Checking
meta_2019_corrected_v1_newcheck<- meta_2019_corrected_v1[complete.cases(meta_2019_corrected_v1$DateTime_corrected),] #15283 to remove #2357374-15283=  2342091 #2342091 obs: correct
nrow(meta_2019_corrected_v1_newcheck) #5996 to remove #3740469
#Check maintenance times in metadata

v1_servicing_time<-meta_2019_corrected_v1_newcheck[grepl("000001|020001|040001", meta_2019_corrected_v1_newcheck$ImageID_corrected),]
v1_time_check<-newcheck_servicing_time %>%   filter(Hour_corrected< 10|Hour_corrected>18) 
#all between 8am and 7pm =ok

#Check total range of survey

meta_2019_corrected_v1_newcheck_range <-as.data.frame(meta_2019_corrected_v1_newcheck%>%
                                                          group_by(CT_site) %>%
                                                          summarize(DateTime_corrected = range(DateTime_corrected)))
meta_2019_corrected_v1_newcheck_range<-as.data.frame(meta_2019_corrected_v1_newcheck_range %>%
                                                         mutate(variable = rep(c("Start", "End"), nrow(meta_2019_corrected_v1_newcheck_range) / 2),
                                                                key = rep(1:(nrow(meta_2019_corrected_v1_newcheck_range) / 2), each = 2)) %>%
                                                         pivot_wider(id_cols = CT_site, names_from = variable, values_from = DateTime_corrected))



###All ranges seem ok- fall between 11/07 and 01/12
write.csv(meta_2019_corrected_v1, "Kenya_CT2019_metadata_withdims_v2.csv")


#head(removedcorrectedmeta2)
#head(correcteddates)
#removedcorrectedmeta2$Date<-as.Date(removedcorrectedmeta2$Date)
#removedcorrectedmeta2$DateTime<-as.POSIXct(removedcorrectedmeta2$DateTime, format="%Y-%m-%d %H:%M:%S")
#setdiff(correctedmeta2dates, meta2)
#correctedmeta2dates$Date<-as.character(correctedmeta2dates$Date)
#correctedmeta2dates$DateTime<-as.character(correctedmeta2dates$DateTime, format="%Y-%m-%d %H:%M:%S")

