#### Now have a column "new_tag" with consistent labels used for all taggers 

# Summary of tags

N.img <- length(unique(mergeddf$image)) # number of images tagged

N.labels <- length(unique(mergeddf$new_tag)) # number of different tags
list.labels <- unique(mergeddf$new_tag) # back engineering the list of tags to double check
list.labels


# creating a new col joining tags and tagger
mergeddf <- unite(data=mergeddf, # dataframe
                  col="label_tagger", #name of the new col
                  c("new_tag", "tagger"), # cols to be joined
                  sep="_", remove=FALSE)

head(mergeddf)

label.tagger <- as.data.frame(sort(table(mergeddf$label_tagger))) # freq of unique label+tagger
tail(label.tagger)
N.labtag <- length(unique(mergeddf$label_tagger))

label.tagger[label.tagger$Freq>100,] # just exploring data

#read csv with Kenya 2018 metadata to join file path to 'mergeddf' dataframe
library(data.table)

img <- fread("Z:/biome_health_project_files/country_files/kenya/kenya_data/ImageTbl_no_head.csv")
colnames(img) <- c("ConservancyID", "Conservancy_name", "CT_site", "CT_id", "month", "Date", 
                   "DateTime","ImageID","FilePath")
head(img)

# adding a col with time only
library(tidyr)

imgB <- separate(img, col = DateTime, into = c("Date2","Time"), sep = " ", remove = F)
head(imgB)
imgB$Date2 <- NULL # eliminating duplicated Date col

# now transforming time in numbers using function from Marcus' Githyub - activity
#gettime function
#Converts character, POSIXct or POSIXlt time of day data to numeric
#ARGUMENTS
# x: vector of character, POSIXct or POSIXlt time data to convert
# format: used only if x is character, see strptime
# scale: scale on which to return times (see below)
#VALUE
#A vector of numeric times of day in units defined by scale:
# radian: [0,2*pi]
# hours: [0,24]
# proportion: [0,1]
# example: ptime <- gettime(BCItime$date, "%d/%m/%Y %H:%M", "proportion")
# SOLAR TIME FUNCTIONS ####
install.packages("insol")
require(insol)

gettime <- function(x, format="%Y-%m-%d %H:%M:%S", scale=c("radian","hour","proportion")){
  if(class(x)[1]=="character") x <- strptime(x, format, "UTC") else
    if(class(x)[1]=="POSIXct") x <- as.POSIXlt(x) else
      if(class(x)[1]!="POSIXlt") stop("x must be character or POSIXt class")
    scale <- match.arg(scale)
    res <- x$hour + x$min/60 + x$sec/3600
    if(scale=="radian") res <- res*pi/12
    if(scale=="proportion") res <- res/24
    if(all(res==0, na.rm=T)) warning("All times are 0: may be just strptime default?")
    res
}


imgB$time_rad <- gettime(imgB$Time, "%H:%M:%S")  # transforming time in radians
imgB$time_hour <- gettime(imgB$Time, "%H:%M:%S", "hour") # transforming time in hours
imgB$time_prop <- gettime(imgB$Time, "%H:%M:%S", "proportion") # transforming time in proportion
summary(imgB$time_hour)
summary(imgB$time_prop)

#saving metadata with separate time cols (charcater, radisn, proportion, and hour)
write.csv(imgB,"Z:/biome_health_project_files/country_files/kenya/processed_data/Kenya_CT2018_metadata.csv")

#merging DF with tags and DF with metadata
tag.img <- merge(mergeddf,imgB, by.x="image", by.y="ImageID", all.x=T)
head(tag.img)

#now creating col w/ 2 distinct period (nigth & day)  using proportional time
str(imgB)
tag.img$Period <- tag.img$time_prop 
tag.img$Period[tag.img$Period <0.25 | tag.img$Period >=0.75] <- "night" 
tag.img$Period[tag.img$Period >=0.25 & tag.img$Period <0.75] <- "day"
head(tag.img)
unique(tag.img$Period)
which(is.na(tag.img$Period), arr.ind=TRUE) 
# rows 596, 597 and 598 are NA, need to understand why
check.na <- tag.img[c(596:598),]
# all 3 are images with humans without metadata, they can be excluded from DF
tag.img2 <- tag.img[-c(596:598),]

#now creating col w/ unique ID with spp_tagger_period
tag.img2 <- unite(data=tag.img2, # dataframe
                  col="label_tagger_period", #name of the new col
                  c("label_tagger", "Period"), # cols to be joined
                  sep="_", remove=FALSE) # keep original cols
head(tag.img2)

#changing driver letter from file path
tag.img2$FilePath2 <- sub(pattern="M:", replacement= "Z:", tag.img2$FilePath, ignore.case=F)
head(tag.img2)

write.csv(tag.img2,"Kenya2018_5minTags_metadataMK2.csv")

##### copying files ####
# will use the col FilePath2 to copy files 
# example with "other_birds"

birdpath <- tag.img2[(tag.img2$new_tag=="other_bird"), "FilePath2"]  # file paths for all other birds for retagging
head(birdpath)

# eliminating duplicated files - i.e. photos with more than 1 tag
birdpath <- unique(birdpath)

# creating folder to paste files
# 1st a general folder to check tags
dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/", 
                  "checking_tags"))
# now a folder for birds only
dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/",
                  "other_birds"))

file.copy(birdpath, 
          to= "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/other_birds", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE) 

# copying "unidentified" tags for checking

noidpath <- tag.img2[(tag.img2$new_tag=="unidentified"), "FilePath2"]  # file paths for all unidentified
head(noidpath)

# eliminating duplicated files - i.e. photos with more than 1 tag
noidpath <- unique(noidpath)

# creating folder to paste unidentified files

dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/",
                  "unidentified"))

file.copy(noidpath, 
          to= "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/unidentified", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

# copying "mongoose" tags for checking

mongpath <- tag.img2[(tag.img2$new_tag=="mongoose_other"), "FilePath2"]  # file paths for all mongoose
head(mongpath)

# eliminating duplicated files - i.e. photos with more than 1 tag
mongpath <- unique(mongpath)

# creating folder to paste mongoose files

dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/",
                  "mongoose_other"))

file.copy(mongpath, 
          to= "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/mongoose_other", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

### Generating list of images to be checked

#Load file
originalmetadata<-read.csv("Kenya2018_5minTags_metadataMK2.csv",header=TRUE,stringsAsFactors=FALSE)

#Duplicates have to be removed at this stage to avoid clash later on between image sample size and tag sample size (causes replacement to be needed)
removedduplicates<-originalmetadata %>% distinct(image , label_tagger, .keep_all = TRUE) #Remove duplicated rows i.e. rows with images with more than one individual of a species. Still leaves one row for each.
removedmultispecies<-removedduplicates[!(duplicated(removedduplicates$image) | rev(duplicated(rev(removedduplicates$image)))), ] #Remove multiple species images. Removes ALL files with more than one species in.

#Calculate sample sizes needed
totalstable<-table(removedmultispecies$new_tag,removedmultispecies$tagger)
totalsmatrix<-as.data.frame.matrix(totalstable)
totals10<-apply(totalsmatrix, 2, function(x) ifelse(x < 10, x, x*0.1)) #convert to 10% if larger than 10 #perhaps simpler: totals10<-totalsmatrix*0.1 #convert to 10%
totalsrounded<-ceiling(totals10) #round to whole number
maxcap<-apply(totalsrounded, 2, function(x) ifelse(x > 100, 100, x)) #add upper cap of 100
mincap<-apply(maxcap, 2, function(x) ifelse(x > 0 & x < 10, 10, x)) #add lower cap of 10
samplesizes <- ifelse(totalstable < 10, totalstable, mincap) #if total is less than 10, check all images and match to original metadata count
samplesizes <- cbind(rownames(samplesizes), data.frame(samplesizes, row.names=NULL))
head(samplesizes)

library(tidyr)
dfsample<-gather(samplesizes, key="tagger",value= "samplesize", alex:liam, na.rm = FALSE, convert = FALSE)
dfsample <- unite(data=dfsample, # dataframe #adding label_tagger column
                  col="label_tagger", #name of the new col
                  c("rownames(samplesizes)", "tagger"), # cols to be joined
                  sep="_", remove=FALSE) 
head(dfsample)

#Amend metadata
removedmultispecies$samplesize <- dfsample$samplesize[match(removedmultispecies$label_tagger, dfsample$label_tagger)] #add samplesize column to original metadata
head(removedmultispecies)

#Generate file list
library(dplyr)
new_df <- removedmultispecies %>% group_by(label_tagger) %>% sample_n(samplesize, replace=FALSE) 
write.csv(new_df, file = "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/new_df.csv")

### sorting images into folders by species ###
# file list
file_list <- read.csv("new_df.csv", header = TRUE)

head(file_list)

# creating multiple folders for the differnt species images to be checked

foldername_list <- read.csv("tagging_check_foldernames.csv", header = TRUE)
subfolder_names<- (foldername_list$folder_name) 

for (j in 1:length(subfolder_names)){
  folder<-dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/",subfolder_names[j]))
}

# add column of target diectory to list of images ot be checked
file_list$targetpath <- "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/"
file_list$fulltargertpath <- paste0(file_list$targetpath,file_list$new_tag)
head(file_list)


# move images into species folders 

for (i in 1:nrow(file_list)) {
  file.copy(from = paste0(file_list[i,27]), # [1,27] is the current file pathway
            to=paste0(file_list[i,30], "/", file_list[i,3]), overwrite = TRUE, recursive = FALSE, copy.date=TRUE) } # [i,30] is the target pathway, [i,3] is the image name
library(beepr)
beep(sound = 1, expr = NULL)

# create output folders

for (j in 1:length(subfolder_names)){
  folder<-dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/outputs/",subfolder_names[j],"_output"))
}



### Firgure out how to pull species only in multi species images ###
check = as.data.frame(setdiff(originalmetadata$label_tagger, new_df$label_tagger)) #find labels only captured in multispecies images- these will need to be pulled out separately
colnames(check)[colnames(check) == "setdiff(originalmetadata$label_tagger, new_df$label_tagger)"]<- "label_tagger"
multispeciescheck <-subset(originalmetadata, label_tagger %in% check$label_tagger)


### Assessing the checked tags

# collate and merge csvs
library(readbulk)

checkedtags <- read_bulk(directory = "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/outputs", subdirectories = TRUE, extension = "*.csv",
                         data = NULL, verbose = TRUE, fun = utils::read.csv)
head(checkedtags)
summary(checkedtags) # check overall proportion of "n" tags 

write.csv(checkedtags, file = "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/checkedtags.csv")

# mish mash of code for assessing the checked tags results and separating out the species to be retagged
checkedtags<- unite(data=checkedtags, # dataframe
                    col="folder_mark", #name of the new col
                    c("Subdirectory", "label"), # cols to be joined
                    sep="_", remove=FALSE)
head(checkedtags)
summary(checkedtags)
summary(checkedtags$folder_mark)


w <- table(removedduplicates$new_tag)
write.csv(w, file = "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/tagcount.csv" )

# example with "other_birds"

mongothpath <- tag.img2[(tag.img2$new_tag=="mongoose_other"), "FilePath2"]  # file paths for all other birds for retagging
head(mongothpath)

# eliminating duplicated files - i.e. photos with more than 1 tag
oribipath <- unique(mongothpath)

# creating folder to paste files
dir.create(paste0("Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/", 
                  "checking_tags/retagging/","mongoose_other"))

file.copy(mongothpath,
          to= "Z:/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/checking_tags/retagging/mongoose_other", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE) 

#creating new csv overwriting old tags with updated tags
library(dplyr)
new <- read.csv("Z:\\biome_health_project_files\\country_files\\kenya\\tagging_photos\\5_minute_2018\\retag.csv", header=TRUE)
old <- read.csv("Z:\\biome_health_project_files\\country_files\\kenya\\tagging_photos\\5_minute_2018\\Kenya_2018_5minInterval_Full_newtag.csv", header=TRUE)

removed <- anti_join(old, new, by = "image") #removing the old tags

new$stage <- c("new")
removed$stage <- c("old")
corrected <- bind_rows(removed,new) #combine the old and new
write.csv(corrected, "Z:\\biome_health_project_files\\country_files\\kenya\\tagging_photos\\5_minute_2018\\Kenya_2018_5minInterval_Full_CORRECTED.csv")

meta <- read.csv("Z:/biome_health_project_files/country_files/kenya/processed_data/Kenya_CT2018_metadata.csv")

correctedmeta <- merge(corrected,meta, by.x="image", by.y="ImageID", all.x=T)

write.csv(correctedmeta, "Z:\\biome_health_project_files\\country_files\\kenya\\tagging_photos\\5_minute_2018\\Kenya_2018_5minInterval_Full_CORRECTED_metadata.csv")
