setwd("G:/CSVs")
MT1<-read.csv("MT_1-export.csv")
MT2<-read.csv("MT_2-export.csv")
MT3<-read.csv("MT_3-export.csv")
MT4<-read.csv("MT4_New-export.csv")

OMC1<-read.csv("OMC_1_Emily-export.csv")
OMC2<-read.csv("OMC_2_Emily-export.csv")
OMC3<-read.csv("OMC3_Liam_output.csv")

MN1<-read.csv("MN1-export_new.csv")
MN2<-read.csv("MN2-export_new.csv")
MN3<-read.csv("MN3-export.csv")
MN4<-read.csv("NEW_MN4-export.csv")

NB1<-read.csv("NB_1_StrattonHatfield-export.csv")
NB2<-read.csv("NB2-export.csv")
NB4<-read.csv("NB4-export.csv")

###NB3
NB3Georgia1<-read.csv("NB3_Georgia--export.csv")
NB3Taras1<-read.csv("NB3_Taras-export.csv")
NB3Taras2<-read.csv("NB2_Taras_set2-export.csv")
NB3Taras3<-read.csv("Taras_set3-export.csv")
NB3Taras4<-read.csv("Taras_set4-export.csv")


MN1$tagger<-"sarah"
MN2$tagger<-"sarah"
MN3$tagger<-"sarah"
MN4$tagger<-"holly"

MT1$tagger<-"georgia"
MT2$tagger<-"georgia"
MT3$tagger<-"georgia"
MT4$tagger<-"holly"

OMC1$tagger<-"emily"
OMC2$tagger<-"emily"
OMC3$tagger<-"liam"

NB1$tagger<-"stratton"
NB2$tagger<-"holly"
NB3Georgia1$tagger<-"georgia"
NB3Taras1$tagger<-"taras"
NB3Taras2$tagger<-"taras"
NB3Taras3$tagger<-"taras"
NB3Taras4$tagger<-"taras"
NB4$tagger<-"holly"

NB3<-rbind(NB3Georgia1,NB3Taras1,NB3Taras2,NB3Taras3,NB3Taras4)

head(MT1) #5000 images
head(MT2) #5000 images
head(MT3) #5000 images
head(MT4) #5885 images

head(OMC1) #5000 images
head(OMC2) #5000 images
head(OMC3) #3794 images

head(MN1) #5000 images
head(MN2) #5000 images
head(MN3) #5000 images
head(MN4) #2904 images

head(NB1) #5000 images
head(NB2) #5000 images
head(NB3) #5000 images
head(NB4) #2133 images

library(tidyverse)
OMC3 <- rename(OMC3, image = ImageID)
OMC3 <- rename(OMC3, label = CommonName)
OMC3$xmin <- NA 
OMC3$ymin <- NA 
OMC3$xmax <- NA 
OMC3$ymax <- NA

OMC3<-OMC3[,c(1,2,5,6,7,8,3,4)]
OMC3$X<-NULL

library(exiftoolr)

#MN
MN1list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MN/MN1", full.names = TRUE, pattern = "*.JPG")
MN1_exif<-exif_read(MN1list,tags=c("SourceFile", "DateTimeOriginal"))
head(MN1_exif)
write.csv(MN1_exif, "MN1_exif.csv")

MN2list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MN/MN2", full.names = TRUE, pattern = "*.JPG")
MN2_exif<-exif_read(MN2list,tags=c("SourceFile", "DateTimeOriginal"))
head(MN2_exif)
write.csv(MN2_exif, "MN2_exif.csv")

MN3list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MN/MN3", full.names = TRUE, pattern = "*.JPG")
MN3_exif<-exif_read(MN3list,tags=c("SourceFile", "DateTimeOriginal"))
head(MN3_exif)
write.csv(MN3_exif, "MN3_exif.csv")

MN4list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MN/MN4", full.names = TRUE, pattern = "*.JPG")
MN4_exif<-exif_read(MN4list,tags=c("SourceFile", "DateTimeOriginal"))
head(MN4_exif)
write.csv(MN4_exif, "MN4_exif.csv")
#MT
MT1list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MT/MT1", full.names = TRUE, pattern = "*.JPG")
MT1_exif<-exif_read(MT1list,tags=c("SourceFile", "DateTimeOriginal"))
head(MT1_exif)
write.csv(MT1_exif, "MT1_exif.csv")

MT2list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MT/MT2", full.names = TRUE, pattern = "*.JPG")
MT2_exif<-exif_read(MT2list,tags=c("SourceFile", "DateTimeOriginal"))
head(MT2_exif)
write.csv(MT2_exif, "MT2_exif.csv")

MT3list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MT/MT3", full.names = TRUE, pattern = "*.JPG")
MT3_exif<-exif_read(MT3list,tags=c("SourceFile", "DateTimeOriginal"))
head(MT3_exif)
write.csv(MT3_exif, "MT3_exif.csv")

MT4list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/MT/MT4", full.names = TRUE, pattern = "*.JPG")
MT4_exif<-exif_read(MT4list,tags=c("SourceFile", "DateTimeOriginal"))
head(MT4_exif)
write.csv(MT4_exif, "MT4_exif.csv")

#NB
NB1list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/NB/NB1", full.names = TRUE, pattern = "*.JPG")
NB1_exif<-exif_read(NB1list,tags=c("SourceFile", "DateTimeOriginal"))
head(NB1_exif)
write.csv(NB1_exif, "NB1_exif.csv")

NB2list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/NB/NB2", full.names = TRUE, pattern = "*.JPG")
NB2_exif<-exif_read(NB2list,tags=c("SourceFile", "DateTimeOriginal"))
head(NB2_exif)
write.csv(NB2_exif, "NB2_exif.csv")

NB3list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/NB/NB3", full.names = TRUE, pattern = "*.JPG")
NB3_exif<-exif_read(NB3list,tags=c("SourceFile", "DateTimeOriginal"))
head(NB3_exif)
write.csv(NB3_exif, "NB3_exif.csv")

NB4list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/NB/NB4", full.names = TRUE, pattern = "*.JPG")
NB4_exif<-exif_read(NB4list,tags=c("SourceFile", "DateTimeOriginal"))
head(NB4_exif)
write.csv(NB4_exif, "NB4_exif.csv")

#OMC
OMC1list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/OMC/OMC1", full.names = TRUE, pattern = "*.JPG")
OMC1_exif<-exif_read(OMC1list,tags=c("SourceFile", "DateTimeOriginal"))
head(OMC1_exif)
write.csv(OMC1_exif, "OMC1_exif.csv")

OMC2list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/OMC/OMC2", full.names = TRUE, pattern = "*.JPG")
OMC2_exif<-exif_read(OMC2list,tags=c("SourceFile", "DateTimeOriginal"))
head(OMC2_exif)
write.csv(OMC2_exif, "OMC2_exif.csv")

OMC3list <- list.files("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/OMC/OMC3", full.names = TRUE, pattern = "*.JPG")
OMC3_exif<-exif_read(OMC3list,tags=c("SourceFile", "DateTimeOriginal"))
head(OMC3_exif)
write.csv(OMC3_exif, "OMC3_exif.csv")
                     
#### MN #####
MN1_exif1<-MN1_exif
MN2_exif1<-MN2_exif
MN3_exif1<-MN3_exif
MN4_exif1<-MN4_exif

MN1_exif1$image = str_sub(MN1_exif1$SourceFile,-20) 
head(MN1_exif1)
MN2_exif1$image = str_sub(MN2_exif1$SourceFile,-20) 
head(MN2_exif1)
MN3_exif1$image = str_sub(MN3_exif1$SourceFile,-20) 
head(MN3_exif1)
MN4_exif1$image = str_sub(MN4_exif1$SourceFile,-20) 
head(MN4_exif1)

MN_exif_complete<-rbind(MN1_exif1, MN2_exif1,MN3_exif1,MN4_exif1)
MN_labels_complete<-rbind(MN1,MN2,MN3,MN4)

MN_metadata<-merge(MN_labels_complete,MN_exif_complete,by="image",all=TRUE)
write.csv(MN_metadata, "MN_metadata_tagger_NOV.csv")

head(MN_metadata)
tail(MN_metadata)
MN_metadata$stage<-"nov" #stage
MN_metadata$ConservancyID<-"MN" #ConservancyID
MN_metadata$Conservancy_name<-"mara_north" #Conservancy_name
MN_metadata$CT_site = str_sub(MN_metadata$image,6,-12) #CT_site
MN_metadata$CT_id<-NA #CT_id
MN_metadata$Date <- sub(pattern=":", replacement= "/", MN_metadata$Date, ignore.case=F)
MN_metadata$Date <- sub(pattern=":", replacement= "/", MN_metadata$Date, ignore.case=F)
MN_metadata$Date<-as.Date(MN_metadata$Date)
MN_metadata$month<-months(MN_metadata$Date)
require(tidyverse)
install.packages("activity")
require(activity)
MN_metadata %>% mutate(month = tolower(month))
MN_metadata<- separate(MN_metadata, col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) #Date #Time
MN_metadata$time_rad <- gettime(MN_metadata$Time, "%H:%M:%S")  # transforming time in radians
MN_metadata$time_hour <- gettime(MN_metadata$Time, "%H:%M:%S", "hour") # transforming time in hours
MN_metadata$time_prop <- gettime(MN_metadata$Time, "%H:%M:%S", "proportion") # transforming time in proportion
summary(MN_metadata$time_hour)
summary(MN_metadata$time_prop)

#### MT #####
MT1_exif1<-MT1_exif
MT2_exif1<-MT2_exif
MT3_exif1<-MT3_exif
MT4_exif1<-MT4_exif

MT1_exif1$image = str_sub(MT1_exif1$SourceFile,-20) 
head(MT1_exif1)
MT2_exif1$image = str_sub(MT2_exif1$SourceFile,-20) 
head(MT2_exif1)
MT3_exif1$image = str_sub(MT3_exif1$SourceFile,-20) 
head(MT3_exif1)
MT4_exif1$image = str_sub(MT4_exif1$SourceFile,-20) 
head(MT4_exif1)

MT_exif_complete<-rbind(MT1_exif1, MT2_exif1,MT3_exif1,MT4_exif1)
MT_labels_complete<-rbind(MT1,MT2,MT3,MT4)

MT_metadata<-merge(MT_labels_complete,MT_exif_complete,by="image",all=TRUE)
write.csv(MT_metadata, "MT_metadata_tagger_NOV.csv")

head(MT_metadata)
tail(MT_metadata)
MT_metadata$stage<-"nov" #stage
MT_metadata$ConservancyID<-"MT" #ConservancyID
MT_metadata$Conservancy_name<-"mara_triangle" #Conservancy_name
MT_metadata$CT_site = str_sub(MT_metadata$image,6,-12) #CT_site
MT_metadata$CT_id<-NA #CT_id
MT_metadata$Date <- sub(pattern=":", replacement= "/", MT_metadata$Date, ignore.case=F)
MT_metadata$Date <- sub(pattern=":", replacement= "/", MT_metadata$Date, ignore.case=F)
MT_metadata$Date<-as.Date(MT_metadata$Date)
MT_metadata$month<-months(MT_metadata$Date)
require(tidyverse)
install.packages("activity")
require(activity)
MT_metadata %>% mutate(month = tolower(month))
MT_metadata<- separate(MT_metadata, col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) #Date #Time
MT_metadata$time_rad <- gettime(MT_metadata$Time, "%H:%M:%S")  # transforming time in radians
MT_metadata$time_hour <- gettime(MT_metadata$Time, "%H:%M:%S", "hour") # transforming time in hours
MT_metadata$time_prop <- gettime(MT_metadata$Time, "%H:%M:%S", "proportion") # transforming time in proportion
summary(MT_metadata$time_hour)
summary(MT_metadata$time_prop)


#### NB #####
NB1_exif1<-NB1_exif
NB2_exif1<-NB2_exif
NB3_exif1<-NB3_exif
NB4_exif1<-NB4_exif

NB1_exif1$image = str_sub(NB1_exif1$SourceFile,-20) 
head(NB1_exif1)
NB2_exif1$image = str_sub(NB2_exif1$SourceFile,-20) 
head(NB2_exif1)
NB3_exif1$image = str_sub(NB3_exif1$SourceFile,-20) 
head(NB3_exif1)
NB4_exif1$image = str_sub(NB4_exif1$SourceFile,-20) 
head(NB4_exif1)

NB_exif_complete<-rbind(NB1_exif1, NB2_exif1,NB3_exif1,NB4_exif1)
NB_labels_complete<-rbind(NB1,NB2,NB3,NB4)

NB_metadata<-merge(NB_labels_complete,NB_exif_complete,by="image",all=TRUE)
write.csv(NB_metadata, "NB_metadata_tagger_NOV.csv")

head(NB_metadata)
tail(NB_metadata)
NB_metadata$stage<-"nov" #stage
NB_metadata$ConservancyID<-"NB" #ConservancyID
NB_metadata$Conservancy_name<-"naboisho" #Conservancy_name
NB_metadata$CT_site = str_sub(NB_metadata$image,6,-12) #CT_site
NB_metadata$CT_id<-NA #CT_id
NB_metadata$Date <- sub(pattern=":", replacement= "/", NB_metadata$Date, ignore.case=F)
NB_metadata$Date <- sub(pattern=":", replacement= "/", NB_metadata$Date, ignore.case=F)
NB_metadata$Date<-as.Date(NB_metadata$Date)
NB_metadata$month<-months(NB_metadata$Date)
require(tidyverse)
install.packages("activity")
require(activity)
NB_metadata %>% mutate(month = tolower(month))
NB_metadata<- separate(NB_metadata, col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) #Date #Time
NB_metadata$time_rad <- gettime(NB_metadata$Time, "%H:%M:%S")  # transforming time in radians
NB_metadata$time_hour <- gettime(NB_metadata$Time, "%H:%M:%S", "hour") # transforming time in hours
NB_metadata$time_prop <- gettime(NB_metadata$Time, "%H:%M:%S", "proportion") # transforming time in proportion
summary(NB_metadata$time_hour)
summary(NB_metadata$time_prop)

#### OMC #####
OMC1_exif1<-OMC1_exif
OMC2_exif1<-OMC2_exif
OMC3_exif1<-OMC3_exif


OMC1_exif1$image = str_sub(OMC1_exif1$SourceFile,-21) 
head(OMC1_exif1)
OMC2_exif1$image = str_sub(OMC2_exif1$SourceFile,-21) 
head(OMC2_exif1)
OMC3_exif1$image = str_sub(OMC3_exif1$SourceFile,-21) 
head(OMC3_exif1)

OMC_exif_complete<-rbind(OMC1_exif1, OMC2_exif1,OMC3_exif1)
OMC_labels_complete<-rbind(OMC1,OMC2,OMC3)

OMC_metadata<-merge(OMC_labels_complete,OMC_exif_complete,by="image",all=TRUE)
write.csv(OMC_metadata,"OMC_metadata_tagger_NOV.csv")

head(OMC_metadata)
tail(OMC_metadata)
OMC_metadata$stage<-"nov" #stage
OMC_metadata$ConservancyID<-"OMC" #ConservancyID
OMC_metadata$Conservancy_name<-"omc" #Conservancy_name
OMC_metadata$CT_site = str_sub(OMC_metadata$image,6,-12) #CT_site
OMC_metadata$CT_id<-NA #CT_id
OMC_metadata$Date <- sub(pattern=":", replacement= "/", OMC_metadata$Date, ignore.case=F)
OMC_metadata$Date <- sub(pattern=":", replacement= "/", OMC_metadata$Date, ignore.case=F)
OMC_metadata$Date<-as.Date(OMC_metadata$Date)
OMC_metadata$month<-months(OMC_metadata$Date)
require(tidyverse)
install.packages("activity")
require(activity)
OMC_metadata %>% mutate(month = tolower(month))
OMC_metadata<- separate(OMC_metadata, col = DateTimeOriginal, into = c("Date","Time"), sep = " ", remove = F) #Date #Time
OMC_metadata$time_rad <- gettime(OMC_metadata$Time, "%H:%M:%S")  # transforming time in radians
OMC_metadata$time_hour <- gettime(OMC_metadata$Time, "%H:%M:%S", "hour") # transforming time in hours
OMC_metadata$time_prop <- gettime(OMC_metadata$Time, "%H:%M:%S", "proportion") # transforming time in proportion
summary(OMC_metadata$time_hour)
summary(OMC_metadata$time_prop)

MetadataComplete_November<-rbind(MT_metadata,MN_metadata,OMC_metadata,NB_metadata)
#time_rad
#time_hour
#time_prop

ALL_METADATA_OLD<-read.csv("Kenya_2018_5minInterval_Full_CORRECTED_metadata.csv")


head(ALL_METADATA_OLD)
head(MetadataComplete_November_CF)


length(unique(ALL_METADATA_OLD$image))
MetadataComplete_November_CF<-MetadataComplete_November[,c(1,2,3,4,5,6,7,12,13,14,15,16,20,10,9,11,8,17,18,19)]
MetadataComplete_November_CF <- rename(MetadataComplete_November_CF, FilePath = SourceFile)
MetadataComplete_November_CF <- rename(MetadataComplete_November_CF, DateTime = DateTimeOriginal)
head(MetadataComplete_November_CF)
tail(MetadataComplete_November_CF)
MetadataComplete_November_CF$Date<-as.factor(MetadataComplete_November_CF$Date)
Full_Metadata_2018_November<-rbind(MetadataComplete_November_CF,ALL_METADATA_OLD)
ALL_METADATA_OLD$X<-NULL
head(Full_Metadata_2018_November)
tail(Full_Metadata_2018_November)
write.csv(Full_Metadata_2018_November, "FullMetadata_2018_VNov.csv")
length(unique(Full_Metadata_2018_November$image))

unique(Full_Metadata_2018_November$tagger)










MN_original<-subset(ALL_METADATA_OLD, ConservancyID=="MN")
head(MN_original)
MN_original$DateTime<-as.Date(MN_original$DateTime,"%d/%m/%Y")
MN_original$Date<-as.Date(MN_original$Date,"%d/%m/%Y")
MN_original$DateTime<-as.factor(MN_original$DateTime)
MN_original$Date<-as.factor(MN_original$Date)
head(MN_original)
tail(MN_original)
head(MTOMC)

MN_metadata2<-MN_metadata[,c(1,2,3,4,5,6,7,12,13,14,15,16,20,10,9,11,8,17,18,19)]
write.csv(MN_metadata, "MN_Metadata_new_correctformat.csv")

MTOMC_MN_Original<-rbind(MTOMC,MN_original)
head(MTOMC_MN_Original)
tail(MTOMC_MN_Original)
MTOMC_MN_Original$X<-NULL

MN_Holly<-subset(ALL_METADATA_OLD, ConservancyID=="MN")


MN_metadata <- rename(MN_metadata, FilePath = SourceFile)
MN_metadata <- rename(MN_metadata, DateTime = DateTimeOriginal)
head(MN_metadata)
MN_metadata$Date<-as.factor(MN_metadata$Date)
MTOMCMN_COMPLETE<-rbind(MTOMC_MN_Original, MN_metadata)
head(MTOMCMN_COMPLETE)
tail(MTOMCMN_COMPLETE)
write.csv(MTOMCMN_COMPLETE, "MTOMCMN_COMPLETE2.csv")
