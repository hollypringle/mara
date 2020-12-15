#### Now have a column "new_tag" with consistent labels used for all taggers 
FullMetadata<-read.csv("FullMetadata_2018_VNov.csv")
incorrect<-subset(FullMetadata, image=="2018_MN06_002474.JPG")
# Summary of tags

N.img <- length(unique(FullMetadata$image)) # number of images tagged
N.labels <- length(unique(FullMetadata$label)) # number of different tags
list.labels <- unique(FullMetadata$label) # back engineering the list of tags to double check
list.labels
N.img.new <- length(unique(NewSubset$image))

###Correcting tags
FullMetadata$label <- sub(pattern="ververt_monkey", replace="vervet_monkey",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="domestic_dog ", replace="domestic_dog",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="hyaena_spotted", replace="hyena_spotted",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="juvenille", replace="juvenile",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="motorbike", replace="vehicle",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="other_rodents", replace="other_rodent",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="southern_groundhornbill", replace="southern_ground_hornbill",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="spring_hare", replace="springhare",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="unidentified ", replace="unidentified",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="vechicle", replace="vehicle",FullMetadata$label) # correcting typo
sort(unique(FullMetadata$label))

# creating a new col joining tags and tagger
library(tidyr)
FullMetadata <- unite(data=FullMetadata, # dataframe
                  col="label_tagger", #name of the new col
                  c("label", "tagger"), # cols to be joined
                  sep="_", remove=FALSE)
head(FullMetadata)
label.tagger <- as.data.frame(sort(table(FullMetadata$label_tagger))) # freq of unique label+tagger
tail(label.tagger)
N.labtag <- length(unique(FullMetadata$label_tagger))
label.tagger[label.tagger$Freq>100,] # just exploring data

#now creating col w/ 2 distinct period (nigth & day)  using proportional time
str(FullMetadata)
FullMetadata$Period <- FullMetadata$time_prop 
FullMetadata$Period[FullMetadata$Period <0.25 | FullMetadata$Period >=0.75] <- "night" 
FullMetadata$Period[FullMetadata$Period >=0.25 & FullMetadata$Period <0.75] <- "day"
head(FullMetadata)
unique(FullMetadata$Period)
which(is.na(FullMetadata$Period), arr.ind=TRUE) 
# rows 153463:153465 are NA, need to understand why
check.na <- FullMetadata4[c(153460:153470),]
# all 3 are images with humans without metadata, they can be excluded from DF
FullMetadata2 <- FullMetadata[-c(153463:153465),]

#now creating col w/ unique ID with spp_tagger_period
FullMetadata2 <- unite(data=FullMetadata2, # dataframe
                  col="label_tagger_period", #name of the new col
                  c("label_tagger", "Period"), # cols to be joined
                  sep="_", remove=FALSE) # keep original cols
head(FullMetadata2)

###JSON extract

# Set working directory specific to your files

library(dplyr) #bind_rows
library(here)
library(lubridate)
library(knitr)
library(stringr)
library(taxize)
library(profvis)
library(jsonlite)
library(tidyr)
library(stringr)
library(pbapply)

install.packages("taxize")
install.packages("profvis")


OMC3listfiles <- list.files("D:/OMC3_output", recursive= TRUE, full.names = TRUE, pattern = "*asset.json")

###JSON extraction function - this can be resued for each folder###
json_extract<-function(json_file){
  jsin<-jsonlite::fromJSON(json_file) 
  ImageID<-jsin$asset$name
  ImageWidth<-jsin$asset$size$width
  ImageHeight<-jsin$asset$size$height
  # TagID<-gsub("-asset.json", "",basename(json_file))
  
  json_loop_out<-NULL
  for (i in 1:nrow(jsin$regions)){  #length(jsin$regions)){
    
    CommonName<-as.character(jsin$regions$tags[[i]]) # (jsin$regions[[i]]$tags) sometimes works if this doesn't
    box_id<-jsin$regions$id[i]
    box_width <- jsin$regions$boundingBox$width[i]
    box_height<-jsin$regions$boundingBox$height[i]
    xmin<-min(jsin$regions$points[[i]]$x)
    ymin<-min(jsin$regions$points[[i]]$y)
    xmax<-max(jsin$regions$points[[i]]$x)
    ymax<-max(jsin$regions$points[[i]]$y)
    
    
    jlo_out<-data.frame(CommonName,box_id, box_width, box_height, xmin, ymin, xmax, ymax)
    json_loop_out<-rbind(json_loop_out, jlo_out)
  }
  
  #TagID<-paste(TagID, json_loop_out$box_id, sep = "_")
  
  json_out<-data.frame( ImageID = ImageID, ImageWidth = ImageWidth, ImageHeight = ImageHeight, json_loop_out, JSON_filepath = json_file)
  return(json_out)
}

json_OMC3 <- pblapply(OMC3listfiles,json_extract) 
df_OMC3<- do.call("rbind", json_OMC3)
write.csv(df_OMC3, "OMC3_coords.csv")

#Adding Liam's coordinates
tag.img2$FilePath2 <- sub(pattern="M:", replacement= "Z:", tag.img2$FilePath, ignore.case=F)
head(tag.img2)
head(FullMetadata2)
head(df_OMC3)
df_OMC3<-read.csv("OMC3_coords.csv")
df_OMC3 <- rename(df_OMC3, image = ImageID)
FullMetadata2_OMCcoords<-merge(FullMetadata2, df_OMC3, all=TRUE, by="image")
FullMetadata3<-FullMetadata2
FullMetadata3$xmin <- ifelse(is.na(FullMetadata3$xmin), df_OMC3$xmin, FullMetadata3$xmin)
FullMetadata3$xmax <- ifelse(is.na(FullMetadata3$xmax), df_OMC3$xmax, FullMetadata3$xmax)
FullMetadata3$ymin <- ifelse(is.na(FullMetadata3$ymin), df_OMC3$ymin, FullMetadata3$ymin)
FullMetadata3$ymax <- ifelse(is.na(FullMetadata3$ymax), df_OMC3$ymax, FullMetadata3$ymax)
head(FullMetadata3)
write.csv(FullMetadata3,"Kenya2018_5minTags_metadata_Complete_Nov.csv")
which(is.na(FullMetadata3$image), arr.ind=TRUE) 
str(FullMetadata3)
NewSubset<-subset(FullMetadata3,stage=="nov")
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
removedduplicates<-NewSubset %>% distinct(image , label_tagger, .keep_all = TRUE) #Remove duplicated rows i.e. rows with images with more than one individual of a species. Still leaves one row for each.
removedmultispecies<-removedduplicates[!(duplicated(removedduplicates$image) | rev(duplicated(rev(removedduplicates$image)))), ] #Remove multiple species images. Removes ALL files with more than one species in.

#Calculate sample sizes needed
totalstable<-table(removedmultispecies$label,removedmultispecies$tagger)
totalsmatrix<-as.data.frame.matrix(totalstable)
totals10<-apply(totalsmatrix, 2, function(x) ifelse(x < 10, x, x*0.1)) #convert to 10% if larger than 10 #perhaps simpler: totals10<-totalsmatrix*0.1 #convert to 10%
totalsrounded<-ceiling(totals10) #round to whole number
maxcap<-apply(totalsrounded, 2, function(x) ifelse(x > 100, 100, x)) #add upper cap of 100
mincap<-apply(maxcap, 2, function(x) ifelse(x > 0 & x < 10, 10, x)) #add lower cap of 10
samplesizes <- ifelse(totalstable < 10, totalstable, mincap) #if total is less than 10, check all images and match to original metadata count
samplesizes <- cbind(rownames(samplesizes), data.frame(samplesizes, row.names=NULL))
head(samplesizes)

library(tidyr)
dfsample<-gather(samplesizes, key="tagger",value= "samplesize", emily:taras, na.rm = FALSE, convert = FALSE)
dfsample <- unite(data=dfsample, # dataframe #adding label_tagger column
                  col="label_tagger", #name of the new col
                  c("rownames(samplesizes)", "tagger"), # cols to be joined
                  sep="_", remove=FALSE) 
head(dfsample)
sum(dfsample$samplesize)

#Amend metadata
removedmultispecies$samplesize <- dfsample$samplesize[match(removedmultispecies$label_tagger, dfsample$label_tagger)] #add samplesize column to original metadata
head(removedmultispecies)

#Generate file list
library(dplyr)
new_df <- removedmultispecies %>% group_by(label_tagger) %>% sample_n(samplesize, replace=FALSE) 
write.csv(new_df, "new_df2.csv")

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
