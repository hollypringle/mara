#Load data and packages

meta_v2<-read.csv("Kenya_CT2018_metadata_withdims.csv")

library(dplyr)
library(tidyverse)
library(lubridate)

#Check total range of survey

      #meta_v2_range <-as.data.frame( meta_v2 %>%
      #                                 group_by(CT_site) %>%
      #                                 summarize(DateTime = range(DateTime)))
      #meta_v2_range<-as.data.frame(meta_v2_range %>%
      #                               mutate(variable = rep(c("Start", "End"), nrow(meta_v2_range) / 2),
      #                                      key = rep(1:(nrow(meta_v2_range) / 2), each = 2)) %>%
      #                               pivot_wider(id_cols = CT_site, names_from = variable, values_from = DateTime))

#Check maintenance times in metadata

      #meta_servicing_time<-meta_v2[grepl("000001|020001", meta_v2$ImageID),]
      #meta_servicing_time_check<-meta_servicing_time %>%   filter(time_hour< 10|time_hour>18)

#Record correction notes in excel (camera date fix)

meta_v2$datecheck<-"notassigned" #set everything to not checked
meta_v2$DateTime_corrected<-meta_v2$DateTime #add column for corrected date

###Correcting dates/times
###Identifying and adding error decription first

    #NB02- DONE
    meta_v2<-meta_v2%>%
    mutate(datecheck = ifelse(grepl("NB02", CT_site), "remove_cannot_correct", meta_v2$datecheck)) #NB02 cannot be corrected
        meta_v2<-meta_v2%>%
    mutate(DateTime_corrected = ifelse(grepl("NB02", CT_site), "remove", meta_v2$DateTime_corrected)) #NB02 cannot be corrected
    
    #NB07- first round set to November not October (first round 2018_NB07_020001.JPG to 2018_NB07_022549.JPG)- DONE
    which(meta_v2$ImageID == '2018_NB07_020001.JPG') #1388859
    which(meta_v2$ImageID == '2018_NB07_022549.JPG') #1391407
    meta_v2[1388859:1391407, 21]= "minus_1_month"
       
    #NB14- second round needs to go forward 12 hours (second round 2018_NB07_020001.JPG to 2018_NB14_026059.JPG)-DONE
    which(meta_v2$ImageID == '2018_NB14_020001.JPG') #1513833 #locating second round
    which(meta_v2$ImageID == '2018_NB14_026059.JPG') #1519891
    meta_v2[1513833:1519891, 21]= "plus_12_hours"
    
    #NB41- needs to go back 12 hours -DONE
    meta_v2<-meta_v2%>%
    mutate(datecheck = ifelse(grepl("NB41", CT_site), "minus_12_hours", meta_v2$datecheck))
    
    #MT01- needs to go back 12 hours- both rounds-DONE
    meta_v2<-meta_v2%>%
    mutate(datecheck = ifelse(grepl("MT01", CT_site), "minus_12_hours", meta_v2$datecheck)) 
    
    #MT16- needs to go forward 12 hours- both rounds-DONE
    meta_v2<-meta_v2%>%
    mutate(datecheck = ifelse(grepl("MT16", CT_site), "plus_12_hours", meta_v2$datecheck)) 
    
    #MT44- needs to go back 12 hours- both rounds-DONE
    meta_v2<-meta_v2%>%
    mutate(datecheck = ifelse(grepl("MT44", CT_site), "minus_12_hours", meta_v2$datecheck)) 
    
    #OMC28- NEEDS TO GO BACK 12 HOURS- both rounds-DONE
    meta_v2<-meta_v2%>%
    mutate(datecheck = ifelse(grepl("OMC28", CT_site), "minus_12_hours", meta_v2$datecheck)) #NB02 cannot be corrected
             
    
    #OMC33: first round need to go back 12 hours-DONE
    which(meta_v2$ImageID == '2018_OMC33_000001.JPG') #2281315 #locating first round
    which(meta_v2$ImageID == '2018_OMC33_018043.JPG') #2299356
    meta_v2[2281315:2299356, 21]= "minus_12_hours"
    
    which(meta_v2$ImageID == '2018_OMC33_020001.JPG') #2299357 #locating few images which were incorrect while setting up camera
    which(meta_v2$ImageID == '2018_OMC33_020005.JPG') #2299361
    meta_v2[2299357:2299361, 21]= "remove_camera_maintenance"
    meta_v2[2299357:2299361, 22]= "remove"
    
    #OMC37: -DONE
    meta_v2<-meta_v2%>%
      mutate(datecheck = ifelse(grepl("OMC37", CT_site), "minus_12_hours", meta_v2$datecheck)) #NB02 cannot be corrected
    
    #MN10: a number of 2017 images nees removing- DONE
    meta_v2$yearcheck<-format(as.Date(meta_v2$Date, format="%Y-%m-%d"),"%Y")
    meta_v2$datecheck <- ifelse(grepl("MN10", meta_v2$CT_site) & grepl("2017", meta_v2$yearcheck),
                   "remove_wrong_year",meta_v2$datecheck)
    meta_v2$DateTime_corrected<- ifelse(grepl("MN10", meta_v2$CT_site) & grepl("2017", meta_v2$yearcheck),
                            "remove",meta_v2$DateTime_corrected)

####################################################################
#Correcting 'minus 12 hours subset"  
minus12<-subset(meta_v2, datecheck == "minus_12_hours") #83843
minus12$DateTime_corrected<- as_datetime(minus12$DateTime_corrected)    
minus12$DateTime_corrected<-minus12$DateTime_corrected - hours(12)    
    
#Correcting 'plus 12 hours subset"  
plus12<-subset(meta_v2, datecheck == "plus_12_hours") #23857 (6059 NB14 added)
plus12$DateTime_corrected<- as_datetime(plus12$DateTime_corrected)    
plus12$DateTime_corrected<-plus12$DateTime_corrected + hours(12)  

#Correcting 'minus 1 month subset"  
minus_month<-subset(meta_v2, datecheck == "minus_1_month")
minus_month$DateTime_corrected<- as_datetime(minus_month$DateTime_corrected)    
minus_month$DateTime_corrected<-minus_month$DateTime_corrected - days(31)   #use days() function not months()!! - didn't work with months

#######################################################
#meta_v1<-read.csv("Kenya_CT2018_metadata.csv")- check compared to original

corrected_rows<-rbind(minus12, plus12, minus_month) #2549+83843+23857=110249 #correct:110249 obs
removed_corrected_meta2<- anti_join(meta_v2, corrected_rows,by = "ImageID") #2357374-110249=2247125#correct : 2247125 obs
N.img.removed_corrected_meta2<- length(unique(removed_corrected_meta2$ImageID)) 
N.img.corrected_rows<- length(unique(corrected_rows$ImageID)) 
removed_corrected_meta2$DateTime_corrected<- as_datetime(removed_corrected_meta2$DateTime_corrected)    #15283 failed to parse #ok as these are to be removed
meta_v2_corrected<- bind_rows(removed_corrected_meta2,corrected_rows) #2357374 obs - correct

# get time from datetime using format
meta_v2_corrected$Time_corrected<-format(meta_v2_corrected$DateTime_corrected, format = "%H:%M:%S")
meta_v2_corrected$Hour_corrected<-format(meta_v2_corrected$DateTime_corrected, format = "%H")
# get date from datetime using format
meta_v2_corrected$Date_corrected<-format(meta_v2_corrected$DateTime_corrected, format = "%Y-%m-%d")

#Checking
meta2_newcheck<- meta_v2_corrected[complete.cases(meta_v2_corrected$DateTime_corrected),] #15283 to remove #2357374-15283=  2342091 #2342091 obs: correct

#Check maintenance times in metadata

newcheck_servicing_time<-meta2_newcheck[grepl("000001|020001", meta2_newcheck$ImageID),]
newcheck_time_check<-newcheck_servicing_time %>%   filter(Hour_corrected< 10|time_hour>18) #only one left is 2018_MT36_020001.JPG : this is okay, maintenance images missing but rest of times seem ok

#Check total range of survey

meta2_newcheck_range <-as.data.frame( meta2_newcheck%>%
                                group_by(CT_site) %>%
                                summarize(DateTime_corrected = range(DateTime_corrected)))
meta2_newcheck_range<-as.data.frame(meta2_newcheck_range %>%
                               mutate(variable = rep(c("Start", "End"), nrow(meta2_newcheck_range) / 2),
                                     key = rep(1:(nrow(meta2_newcheck_range) / 2), each = 2)) %>%
                              pivot_wider(id_cols = CT_site, names_from = variable, values_from = DateTime_corrected))


###All ranges seem ok
write.csv(meta_v2_corrected, "Kenya_CT2018_metadata_withdims_v2.csv")


#head(removedcorrectedmeta2)
#head(correcteddates)
#removedcorrectedmeta2$Date<-as.Date(removedcorrectedmeta2$Date)
#removedcorrectedmeta2$DateTime<-as.POSIXct(removedcorrectedmeta2$DateTime, format="%Y-%m-%d %H:%M:%S")
#setdiff(correctedmeta2dates, meta2)
#correctedmeta2dates$Date<-as.character(correctedmeta2dates$Date)
#correctedmeta2dates$DateTime<-as.character(correctedmeta2dates$DateTime, format="%Y-%m-%d %H:%M:%S")


