
metadata<-read.csv("FullMetadata_2018_VDECEMBER_FINAL.csv")


###DECEMBER CHECkED TAGS
checkedtagscorrect<-read.csv("checked_tagsDECEMBER.csv")
wrongfiles<-subset(checkedtagscorrect, new =="n")
correctfiles<-subset(checkedtagscorrect, new =="y")

#Remove aardwolf, hartebeest, 'animalremoved' and grants gazelle, as we have already added the retagged ones of these,. 
wrongfile_detail <- anti_join(checkedtagscorrect, correctfiles, by = "image")
wrongfile_detail<-wrongfile_detail[!grepl("aardwolf", wrongfile_detail$Original),]
wrongfile_detail<-wrongfile_detail[!grepl("hartebeest_cokes", wrongfile_detail$Original),]
wrongfile_detail<-wrongfile_detail[!grepl("gazelle_grants", wrongfile_detail$Original),]
wrongfile_detail<-wrongfile_detail[!grepl("animalremoved", wrongfile_detail$Original),]

#Remove extra tags
wrongfile_detail<-wrongfile_detail[!grepl("n", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("missed", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("misidentified", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("other_bird", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("misidentified", wrongfile_detail$new),]
N.img.wrongfile_detail<-length(unique(wrongfile_detail$image)) #14

newwrongfile<-wrongfile_detail
N.img.newwrongfile<- length(unique(newwrongfile$image)) #14
newwrongfile$stage <- c("novretag") #change stage name
newwrongfile$T.F<-NULL
newwrongfile$Original<-NULL
newwrongfile <- newwrongfile %>% rename(label = new) 
correctednewwrongfile<- merge(newwrongfile,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewwrongfile$X<-NULL
correctednewwrongfile$Subdirectory<-NULL
correctednewwrongfile$File<-NULL
correctednewwrongfile$count<-NULL
head(correctednewwrongfile)
N.img.correctednewwrongfile<- length(unique(correctednewwrongfile$image)) #14

correctednewwrongfile$tagger<-c("holly")
head(correctednewwrongfile)
N.img.correctednewwrongfile<- length(unique(correctednewwrongfile$image)) #2220
correctednewwrongfile$Period <- correctednewwrongfile$time_prop 
correctednewwrongfile$Period[correctednewwrongfile$Period <0.25 | correctednewwrongfile$Period >=0.75] <- "night" 
correctednewwrongfile$Period[correctednewwrongfile$Period >=0.25 & correctednewwrongfile$Period <0.75] <- "day"
unique(correctednewwrongfile$Period)

correctednewwrongfile <- unite(data=correctednewwrongfile, # dataframe
                               col="label_tagger", #name of the new col
                               c("label", "tagger"), # cols to be joined
                               sep="_", remove=FALSE)

correctednewwrongfile <- unite(data=correctednewwrongfile, # dataframe
                               col="label_tagger_period", #name of the new col
                               c("label_tagger", "Period"), # cols to be joined
                               sep="_", remove=FALSE) 
head(correctednewwrongfile)

###EMILY CHECkED TAGS

checkedtagsemily<-read.csv("checked_tags_EMILY.csv")
wrongfiles.e<-subset(checkedtagsemily, new =="n")
correctfiles.e<-subset(checkedtagsemily, new =="y")
#Remove aardwolf, hartebeest, 'animalremoved' and grants gazelle, as we have already added the retagged ones of these,. 
wrongfile_detail.e <- anti_join(checkedtagsemily, correctfiles.e, by = "image")
wrongfile_detail.e<-wrongfile_detail.e[!grepl("aardvark", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("aardwolf", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("bateared_fox", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("caracal", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("gazelle_grants", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("honey_badger", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("hyena_striped", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("leopard", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("mongoose_banded", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("mongoose_other", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("oribi", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("reedbuck", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("secretary_bird", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("springhare", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("unidentified", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("wildcat", wrongfile_detail.e$original),]


wrongfile_detail.e<-wrongfile_detail.e[!grepl("n", wrongfile_detail.e$new),]
N.img.wrongfile_detail.e<-length(unique(wrongfile_detail.e$image)) #42

newwrongfile.e<-wrongfile_detail.e

N.img.newwrongfile.e<- length(unique(newwrongfile.e$image)) #42
newwrongfile.e$stage <- c("novretag_emily") #change stage name
newwrongfile.e$T.F<-NULL
newwrongfile.e$original<-NULL
newwrongfile.e$count<-NULL
newwrongfile.e <- newwrongfile.e %>% rename(label = new) 
correctednewwrongfile.e<- merge(newwrongfile.e,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewwrongfile.e$X<-NULL
correctednewwrongfile.e$Subdirectory<-NULL
correctednewwrongfile.e$File<-NULL
head(correctednewwrongfile.e)

correctednewwrongfile.e$tagger<-c("emily")
head(correctednewwrongfile.e)
N.img.correctednewwrongfile.e<- length(unique(correctednewwrongfile.e$image)) #2220
correctednewwrongfile.e$Period <- correctednewwrongfile.e$time_prop 
correctednewwrongfile.e$Period[correctednewwrongfile.e$Period <0.25 | correctednewwrongfile.e$Period >=0.75] <- "night" 
correctednewwrongfile.e$Period[correctednewwrongfile.e$Period >=0.25 & correctednewwrongfile.e$Period <0.75] <- "day"
unique(correctednewwrongfile.e$Period)

correctednewwrongfile.e <- unite(data=correctednewwrongfile.e, # dataframe
                                 col="label_tagger", #name of the new col
                                 c("label", "tagger"), # cols to be joined
                                 sep="_", remove=FALSE)

correctednewwrongfile.e <- unite(data=correctednewwrongfile.e, # dataframe
                                 col="label_tagger_period", #name of the new col
                                 c("label_tagger", "Period"), # cols to be joined
                                 sep="_", remove=FALSE) 
head(correctednewwrongfile.e)


###Combine and add to main metadata

allcorrectedchecks <- bind_rows(correctednewwrongfile.e,correctednewwrongfile)

head(allcorrectedchecks)
allcorrectedchecks$X.x<-NULL
allcorrectedchecks$X.y<-NULL

N.img.allcorrectedchecks<- length(unique(allcorrectedchecks$image)) #56
N.img.metadata<- length(unique(metadata$image)) #132126

removedfullmetachecks <- anti_join(metadata, allcorrectedchecks, by = "image")
N.img.removedfullmetachecks<- length(unique(removedfullmetachecks$image)) #132126

head(metadata)
metadata$X.1<-NULL
metadata$X<-NULL
N.img.metadata<- length(unique(metadata$image))


correctedfullmeta_final <- bind_rows(removedfullmetachecks,allcorrectedchecks)
N.img.correctedfullmeta_final<-length(unique(correctedfullmeta_final$image)) #132126
head(correctedfullmeta_final)

write.csv(correctedfullmeta_final, "Full2018Metadata_VDECEMBER_FINAL_WITHCHECKS.csv")

