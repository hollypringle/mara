library(dplyr)
#Step 1: load databases
oldtags_orig<-read.csv("Final2018Kenya_TagsOnly_withremoved.csv")
oldtags<-read.csv("Final2018Kenya_TagsOnly_withremoved.csv")
strattontags<-read.csv("Kenya_2018_birds_StrattonHatfield_input-export.csv") #2885
origstrattontags<-read.csv("Kenya_2018_birds_StrattonHatfield_input-export.csv") #2885

#Adding tagger, stage, filepath information to Stratton's tags
strattontags$tagger<-"stratton"
strattontags$stage<-"new_birds"
filepath_info<-oldtags%>%select(image, FilePath, ReasonRemoved, dupfilepath, Two_Labels, Duplicated_Row)
filepath_info<-filepath_info[!duplicated(filepath_info$image), ]
strattontags <- merge(strattontags, filepath_info, by = 'image', all.x=TRUE)

#Check for any boxes with more than 1 species tag in Stratton's update

updated_birds_dupelabel<- strattontags  %>% 
  group_by(image, xmin, ymin,xmax, ymax) %>% 
  mutate(dupelabel = n()>1)

n.img.updated_birds_dupelabel<-length(unique(updated_birds_dupelabel$image)) #1001 images
morethan1tag_dupelabel<-subset(updated_birds_dupelabel, dupelabel=="TRUE") 
length(unique(morethan1tag_dupelabel$image)) #1 image
nrow(morethan1tag_dupelabel) #2 tags
#only duplicated because of 'juvenile' labe, therefore no duplicates for boxes in Stratton tags


strattontags$Two_Labels<-FALSE #remove original checks from current dataset as labels are now different, and replace with 'FALSE' as there are no duplicates in this set
strattontags<-strattontags %>%
  mutate(ReasonRemoved = na_if(ReasonRemoved, "two_labels_same_box")) 



##Step 2: remove mammals from Stratton's tags. We only want to update the birds. Mammal tags have already been checked
strattontags<-strattontags[!grepl("gazelle", strattontags$label),] #2775
strattontags<-strattontags[!grepl("cattle", strattontags$label),] #2638
strattontags<-strattontags[!grepl("human", strattontags$label),] #2633
strattontags<-strattontags[!grepl("baboon", strattontags$label),] #2632
strattontags<-strattontags[!grepl("eland", strattontags$label),] #2618
strattontags<-strattontags[!grepl("hartebeest_cokes", strattontags$label),] #2617
strattontags<-strattontags[!grepl("hyena", strattontags$label),] #2616
strattontags<-strattontags[!grepl("elephant", strattontags$label),] #2611
strattontags<-strattontags[!grepl("topi", strattontags$label),] #2586
strattontags<-strattontags[!grepl("motorbike", strattontags$label),] #2584
strattontags<-strattontags[!grepl("zebra", strattontags$label),] #2357
strattontags<-strattontags[!grepl("warthog", strattontags$label),] #2337
strattontags<-strattontags[!grepl("hare", strattontags$label),] #2334
strattontags<-strattontags[!grepl("reptiles", strattontags$label),] #2333
strattontags<-strattontags[!grepl("vehicle", strattontags$label),] #2331
strattontags<-strattontags[!grepl("blank", strattontags$label),] #2328
strattontags<-strattontags[!grepl("buffalo", strattontags$label),] #2309
strattontags<-strattontags[!grepl("giraffe", strattontags$label),] #2296
strattontags<-strattontags[!grepl("impala", strattontags$label),] #2266
strattontags<-strattontags[!grepl("jackal", strattontags$label),] #2265
strattontags<-strattontags[!grepl("juvenile", strattontags$label),] #2264
strattontags<-strattontags[!grepl("mongoose", strattontags$label),] #2261
strattontags<-strattontags[!grepl("shoats", strattontags$label),] #2235
strattontags<-strattontags[!grepl("wildebeest", strattontags$label),] #1343

# Adding column based on other column:

oldtags$IfBird <- ifelse(grepl("other_bird|heron|stork_other|stork_saddle_billed|crane|secretary_bird|guineafowl|francolin_coqui|francolin_redwinged|francolin_rednecked_spurfowl|francolin_other|collared_dove|bustard_kori|bustard_denhams|bustard_black_bellied|bustard_white_bellied", oldtags$Species), "Bird", "NotBird")
oldbirdtags<-oldtags[!grepl("NotBird", oldtags$IfBird),] #1308 number of old bird tags
nrow(oldbirdtags) #1308 number of old bird tags
n.img.oldbirdtags<-length(unique(oldbirdtags$image)) #1006 image

nrow(strattontags) #1343 tags with extra mammals removed
n.img.strattontags<-length(unique(strattontags$image)) #995 images when extra mammals removed

nrow(origstrattontags) #2885 tags including extra mammals (won't replace)
n.img.origstrattontags<-unique(origstrattontags$image) #1001 with extra mammals tags

commonID<-intersect(strattontags$image,oldbirdtags$image) #list of images that are in both the bird (old tags and stratton tags) #993 images
mismatch<-oldbirdtags[!oldbirdtags$image%in% commonID,] #list of images that are mismatched
n.img.mismatch<-unique(mismatch$image)
match<-as.data.frame(oldbirdtags[oldbirdtags$image %in% intersect(origstrattontags$image,oldbirdtags$image),])
n.img.match<-unique(match$image)
commonDF<-as.data.frame(commonID)
nrow(strattontags)


test_oldbird <- oldtags[(oldtags$IfBird == "Bird" & oldtags$image %in% strattontags$image),]
#1287 that meet condition of being labelled 'Bird' in old dataset and match image in stratton tags

removedoldbird <- oldtags[!(oldtags$IfBird == "Bird" & oldtags$image %in% strattontags$image),] 
#266553  :  yes (267840-1287)
#CHECK THAT THOSE REMOVED ARE CORRECT

head(removedoldbird)
head(strattontags)
strattontags$IfBird <- ifelse(grepl("unidentified", strattontags$label), "Unidentified", "Bird")
strattontags<-strattontags %>% rename(Species = label)
removedoldbird$X.1<-NULL
removedoldbird$X<-NULL

NEWTAGS<-rbind(removedoldbird, strattontags) #267896 tags
NEWTAGS.ORIG<-rbind(removedoldbird, strattontags) #267896 tags
n.img.NEWTAGS<-length(unique(NEWTAGS$image)) #132126 images
nrow(NEWTAGS) #267896
orignewtagtable<-as.data.frame(table(NEWTAGS.ORIG$Species))
write.csv(orignewtagtable, "orignewtagtable.csv")

#Step 7: correct hartebeest/topi labels
which(NEWTAGS$image == '2018_NB05_009873.JPG'&NEWTAGS$Species == "hartebeest_cokes")
NEWTAGS[158671, 6]= "topi"
NEWTAGS[158672, 6]= "topi"

which(NEWTAGS$image == '2018_MT26_003463.JPG'&NEWTAGS$Species == "topi")
NEWTAGS[123888, 6]= "eland"

which(NEWTAGS$image == '2018_MT30_007936.JPG'&NEWTAGS$Species == "topi")
NEWTAGS <- NEWTAGS[-c(131715),]
nrow(NEWTAGS) #267895

which(NEWTAGS$image == '2018_MN45_006709.JPG'&NEWTAGS$Species == "topi")
NEWTAGS[59254, 6]= "hartebeest_cokes"

which(NEWTAGS$image == '2018_MN38_002108.JPG'&NEWTAGS$Species == "topi")
NEWTAGS[49557, 6]= "wildebeest"
NEWTAGS[49558, 6]= "wildebeest"
NEWTAGS[49559, 6]= "wildebeest"
NEWTAGS[49560, 6]= "wildebeest"
NEWTAGS[49561, 6]= "wildebeest"
NEWTAGS[49562, 6]= "wildebeest"
NEWTAGS[49567, 6]= "wildebeest"

which(NEWTAGS$image == '2018_MN38_002108.JPG'&NEWTAGS$Species == "topi")

which(NEWTAGS$image == '2018_MT14_021577.JPG'&NEWTAGS$Species == "hartebeest_cokes")
NEWTAGS[104126, 6]= "topi"
NEWTAGS[104127, 6]= "topi"
NEWTAGS[104128, 6]= "topi"
NEWTAGS[104129, 6]= "topi"
NEWTAGS[104130, 6]= "topi"
NEWTAGS[104131, 6]= "topi"
NEWTAGS[104132, 6]= "topi"
NEWTAGS[104133, 6]= "topi"
NEWTAGS[104134, 6]= "topi"
NEWTAGS[104135, 6]= "topi"
NEWTAGS[104136, 6]= "topi"

which(NEWTAGS$image == '2018_NB25_021560.JPG'&NEWTAGS$Species == "hartebeest_cokes")
NEWTAGS[184463, 6]= "topi"

nrow(NEWTAGS)
correctednewtagtable<-as.data.frame(table(NEWTAGS$Species))
write.csv(correctednewtagtable, "correctednewtagtable.csv")

#Step 6: group species into new ML labels
NEWTAGS$label <- ifelse(grepl("aardvark",NEWTAGS$Species), "aardvark", "unassigned")
NEWTAGS$label <- ifelse(grepl("babbler",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("baboon",NEWTAGS$Species), "baboon", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("barbet",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bat",NEWTAGS$Species), "bat", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bateared_fox",NEWTAGS$Species), "bateared_fox", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("beeeater",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("blank",NEWTAGS$Species), "blank", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("boubou",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("buffalo",NEWTAGS$Species), "buffalo", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bulbul",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bushbaby",NEWTAGS$Species), "bushbaby", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bushbuck",NEWTAGS$Species), "bushbuck", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bustard_black_bellied",NEWTAGS$Species), "bustard_other", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bustard_kori",NEWTAGS$Species), "bustard_kori", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("bustard_white_bellied",NEWTAGS$Species), "bustard_other", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("canary",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("caracal",NEWTAGS$Species), "caracal", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("cattle",NEWTAGS$Species), "cattle", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("cheetah",NEWTAGS$Species), "cheetah", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("cisticola",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("collared_dove",NEWTAGS$Species), "dove_pigeon", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("coucal",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("crane",NEWTAGS$Species), "crane", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("dikdik",NEWTAGS$Species), "dikdik", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("domestic_dog",NEWTAGS$Species), "domestic_dog", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("donkey",NEWTAGS$Species), "donkey", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("dove_other",NEWTAGS$Species), "dove_pigeon", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("drongo",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("eagle",NEWTAGS$Species), "other_raptor", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("eland",NEWTAGS$Species), "eland", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("elephant",NEWTAGS$Species), "elephant", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("feral_cat",NEWTAGS$Species), "feral_cat", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("finch",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("flycatcher",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("francolin_coqui",NEWTAGS$Species), "francolin_coqui", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("francolin_crested",NEWTAGS$Species), "francolin_crested", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("francolin_other",NEWTAGS$Species), "francolin_other", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("francolin_rednecked_spurfowl",NEWTAGS$Species), "francolin_rednecked_spurfowl", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("francolin_redwinged",NEWTAGS$Species), "francolin_redwinged", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("gazelle_grants",NEWTAGS$Species), "gazelle_grants", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("gazelle_thomsons",NEWTAGS$Species), "gazelle_thomsons", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("genet",NEWTAGS$Species), "genet", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("gerbil",NEWTAGS$Species), "gerbil", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("giraffe",NEWTAGS$Species), "giraffe", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("goose",NEWTAGS$Species), "goose", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("guineafowl",NEWTAGS$Species), "guineafowl", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("hamerkop",NEWTAGS$Species), "hamerkop", NEWTAGS$label) #heron?
NEWTAGS$label <- ifelse(grepl("hare",NEWTAGS$Species), "hare", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("hartebeest_cokes",NEWTAGS$Species), "hartebeest_cokes", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("heron",NEWTAGS$Species), "heron", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("hippopotamus",NEWTAGS$Species), "hippopotamus", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("honey_badger",NEWTAGS$Species), "honey_badger", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("hoopoe",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("hornbill_ground",NEWTAGS$Species), "southern_ground_hornbill", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("human",NEWTAGS$Species), "human", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("hyena_spotted|hyena_striped|aardwolf",NEWTAGS$Species), "hyena", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("ibis",NEWTAGS$Species), "ibis", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("impala",NEWTAGS$Species), "impala", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("insect",NEWTAGS$Species), "insect", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("jackal",NEWTAGS$Species), "jackal", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("juvenile",NEWTAGS$Species), "juvenile", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("kingfisher",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("lapwing",NEWTAGS$Species), "wader", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("lark",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("leopard",NEWTAGS$Species), "leopard", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("lion_female",NEWTAGS$Species), "lion", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("lion_male",NEWTAGS$Species), "lion", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("longclaw",NEWTAGS$Species), "other_passerine", NEWTAGS$label) #############
NEWTAGS$label <- ifelse(grepl("mongoose_banded",NEWTAGS$Species), "mongoose", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("mongoose_white_tailed",NEWTAGS$Species), "mongoose", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("mongoose_other",NEWTAGS$Species), "mongoose", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("motorbike",NEWTAGS$Species), "motorbike", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("nightjar",NEWTAGS$Species), "nightjar", NEWTAGS$label) #########
NEWTAGS$label <- ifelse(grepl("oribi",NEWTAGS$Species), "oribi", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("ostrich",NEWTAGS$Species), "ostrich", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("other_bird",NEWTAGS$Species), "unidentified_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("other_rodents",NEWTAGS$Species), "other_rodents", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("oxpecker",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("pigeon",NEWTAGS$Species), "dove_pigeon", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("pipit",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("plover",NEWTAGS$Species), "wader", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("porcupine",NEWTAGS$Species), "porcupine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("query",NEWTAGS$Species), "unidentified", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("raptor_other",NEWTAGS$Species), "other_raptor", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("reedbuck",NEWTAGS$Species), "reedbuck", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("reptiles",NEWTAGS$Species), "reptiles", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("robin",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("roller",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("secretary_bird",NEWTAGS$Species), "secretary_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("serval",NEWTAGS$Species), "serval", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("shoat",NEWTAGS$Species), "shoat", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("shoats",NEWTAGS$Species), "shoat", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("shrike",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("southern_ground_hornbil",NEWTAGS$Species), "southern_ground_hornbil", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("sparrow",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("starling",NEWTAGS$Species), "starling", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("stork_other",NEWTAGS$Species), "stork_other", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("stork_saddle_billed",NEWTAGS$Species), "stork_saddle_billed", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("starling",NEWTAGS$Species), "starling", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("swallow",NEWTAGS$Species), "swallow", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("tchagra",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("thickknee",NEWTAGS$Species), "wader", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("topi",NEWTAGS$Species), "topi", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("unidentified",NEWTAGS$Species), "unidentified", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("vehicle",NEWTAGS$Species), "vehicle", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("vervet_monkey",NEWTAGS$Species), "vervet_monkey", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("vulture",NEWTAGS$Species), "vulture", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("warthog",NEWTAGS$Species), "warthog", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("waterbuck",NEWTAGS$Species), "waterbuck", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("weaver",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("wheatear",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("widowbird",NEWTAGS$Species), "other_passerine", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("wildebeest",NEWTAGS$Species), "wildebeest", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("woodpecker",NEWTAGS$Species), "other_bird", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("zebra",NEWTAGS$Species), "zebra", NEWTAGS$label)
NEWTAGS$label <- ifelse(grepl("zorilla",NEWTAGS$Species), "zorilla", NEWTAGS$label)

#607 have 'animal'- remove them
nrow(NEWTAGS) #267895
n.img.NEWTAGS<-length(unique(NEWTAGS$image)) #132126 images

NEWTAGS<-NEWTAGS[!grepl("animal", NEWTAGS$Species),] #267288 #607 rows have been removed
n.img.NEWTAGS<-length(unique(NEWTAGS$image)) #still 132126 images
#all tags have been assigned a ML label

#Step 5: check bird labels
birdsnotchecked<-subset(NEWTAGS, tagger!="stratton" & IfBird=="Bird")
table(NEWTAGS$Species)
table(oldtags_orig$Species)

#general checking
sort(unique(NEWTAGS$Species))
library(stringr)
NEWTAGS$Species <- str_replace(NEWTAGS$Species, "hornbill_ground", "southern_ground_hornbill")
NEWTAGS$Species <- str_replace(NEWTAGS$Species, "other_bird", "other_bird_noid")
NEWTAGS$Species <- str_replace(NEWTAGS$Species, "shoats", "shoat")

which(NEWTAGS$image == '2018_MN14_004398.JPG'&NEWTAGS$Species == "francolin_crested"&NEWTAGS$tagger=="emily")
NEWTAGS.corrections <- NEWTAGS[-c(18673),] #remove duplicated row


which(NEWTAGS.corrections$image == '2018_MN14_008022.JPG'&NEWTAGS.corrections$Species == "francolin_crested"&NEWTAGS.corrections$tagger=="emily")
NEWTAGS.corrections <- NEWTAGS.corrections[-c(19007),] #remove duplicated row

which(NEWTAGS.corrections$image == '2018_MT18_025040.JPG'&NEWTAGS.corrections$Species == "francolin_other"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[1210, 6]= "francolin_rednecked_spurfowl"
NEWTAGS.corrections[1210, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MT18_025045.JPG'&NEWTAGS.corrections$Species == "francolin_other"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[1212, 6]= "francolin_rednecked_spurfowl"
NEWTAGS.corrections[1212, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MT28_003102.JPG'&NEWTAGS.corrections$Species == "francolin_coqui"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[1403, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MT28_003105.JPG'&NEWTAGS.corrections$Species == "francolin_coqui"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[1405, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MT28_003106.JPG'&NEWTAGS.corrections$Species == "francolin_coqui"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[1407, 8]= "new_birds"

sort(unique(NEWTAGS.corrections$Species))
nrow(NEWTAGS.corrections)

which(NEWTAGS.corrections$image == '2018_MN37_000095.JPG'&NEWTAGS.corrections$Species == "other_bird_noid"&NEWTAGS.corrections$tagger=="emily")
NEWTAGS.corrections[45960, 6]= "lapwing"
NEWTAGS.corrections[45960, 7]= "holly"
NEWTAGS.corrections[45960, 8]= "new_birds"

NEWTAGS.corrections[45961, 6]= "lapwing"
NEWTAGS.corrections[45961, 7]= "holly"
NEWTAGS.corrections[45961, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MT10_006903.JPG'&NEWTAGS.corrections$Species == "other_bird_noid"&NEWTAGS.corrections$tagger=="georgia")
NEWTAGS.corrections[90357, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_NB22_001268.JPG'&NEWTAGS.corrections$Species == "other_bird_noid"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[176022, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_NB22_001956.JPG'&NEWTAGS.corrections$Species == "other_bird_noid"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[176129, 6]= "guineafowl"
NEWTAGS.corrections[176129, 8]= "new_birds"

NEWTAGS.corrections[176130, 6]= "guineafowl"
NEWTAGS.corrections[176130, 8]= "new_birds"

NEWTAGS.corrections[176131, 6]= "guineafowl"
NEWTAGS.corrections[176131, 8]= "new_birds"

NEWTAGS.corrections[176132, 6]= "guineafowl"
NEWTAGS.corrections[176132, 8]= "new_birds"

NEWTAGS.corrections[176133, 6]= "guineafowl"
NEWTAGS.corrections[176133, 8]= "new_birds"

NEWTAGS.corrections[176134, 6]= "guineafowl"
NEWTAGS.corrections[176134, 8]= "new_birds"


NEWTAGS.corrections[176135, 6]= "guineafowl"
NEWTAGS.corrections[176135, 8]= "new_birds"

NEWTAGS.corrections[176136, 6]= "guineafowl"
NEWTAGS.corrections[176136, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MN34_024094.JPG'&NEWTAGS.corrections$Species == "other_bird_noid"&NEWTAGS.corrections$tagger=="sarah")
NEWTAGS.corrections[45039, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_MT30_007762.JPG'&NEWTAGS.corrections$Species == "bustard_black_bellied"&NEWTAGS.corrections$tagger=="holly")
NEWTAGS.corrections[1419, 8]= "new_birds"


NOTBIRD<-subset(NEWTAGS.corrections, IfBird=="NotBird")
table(NOTBIRD$Species)


NOTBIRDCHECK<-subset(NEWTAGS.corrections, IfBird=="NotBird")
table(NOTBIRDCHECK$Species)

NEWBIRDSSTAGECHECK<-subset(NEWTAGS.corrections, IfBird=="Bird")
table(NEWBIRDSSTAGECHECK$stage)


which(NEWTAGS.corrections$image == '2018_MN33_009027.JPG'&NEWTAGS.corrections$Species == "guineafowl"&NEWTAGS.corrections$tagger=="alex")
NEWTAGS.corrections[42012, 8]= "new_birds"

which(NEWTAGS.corrections$image == '2018_OMC21_002058.JPG'&NEWTAGS.corrections$Species == "crane"&NEWTAGS.corrections$tagger=="emily")
NEWTAGS.corrections <- NEWTAGS.corrections[-c(246007),] #remove duplicated row


nrow(NEWTAGS.corrections)
NEWTAGSFINAL<-NEWTAGS.corrections
NEWTAGSFINAL$IfBird<-NULL

#Step 6: group species into new ML labels
NEWTAGSFINAL$label <- ifelse(grepl("aardvark",NEWTAGSFINAL$Species), "aardvark", "unassigned")
NEWTAGSFINAL$label <- ifelse(grepl("babbler",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("baboon",NEWTAGSFINAL$Species), "baboon", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("barbet",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bat",NEWTAGSFINAL$Species), "bat", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bateared_fox",NEWTAGSFINAL$Species), "bateared_fox", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("beeeater",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("blank",NEWTAGSFINAL$Species), "blank", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("boubou",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("buffalo",NEWTAGSFINAL$Species), "buffalo", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bulbul",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bushbaby",NEWTAGSFINAL$Species), "bushbaby", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bushbuck",NEWTAGSFINAL$Species), "bushbuck", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bustard_black_bellied",NEWTAGSFINAL$Species), "bustard_other", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bustard_kori",NEWTAGSFINAL$Species), "bustard_kori", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("bustard_white_bellied",NEWTAGSFINAL$Species), "bustard_other", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("canary",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("caracal",NEWTAGSFINAL$Species), "caracal", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("cattle",NEWTAGSFINAL$Species), "cattle", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("cheetah",NEWTAGSFINAL$Species), "cheetah", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("cisticola",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("collared_dove",NEWTAGSFINAL$Species), "dove_pigeon", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("coucal",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("crane",NEWTAGSFINAL$Species), "crane", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("dikdik",NEWTAGSFINAL$Species), "dikdik", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("domestic_dog",NEWTAGSFINAL$Species), "domestic_dog", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("donkey",NEWTAGSFINAL$Species), "donkey", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("dove_other",NEWTAGSFINAL$Species), "dove_pigeon", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("drongo",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("eagle",NEWTAGSFINAL$Species), "other_raptor", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("eland",NEWTAGSFINAL$Species), "eland", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("elephant",NEWTAGSFINAL$Species), "elephant", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("feral_cat",NEWTAGSFINAL$Species), "feral_cat", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("finch",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("flycatcher",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("francolin_coqui",NEWTAGSFINAL$Species), "francolin", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("francolin_crested",NEWTAGSFINAL$Species), "francolin", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("francolin_other",NEWTAGSFINAL$Species), "francolin_other", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("francolin_rednecked_spurfowl",NEWTAGSFINAL$Species), "francolin", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("francolin_redwinged",NEWTAGSFINAL$Species), "francolin", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("gazelle_grants",NEWTAGSFINAL$Species), "gazelle_grants", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("gazelle_thomsons",NEWTAGSFINAL$Species), "gazelle_thomsons", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("genet",NEWTAGSFINAL$Species), "genet", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("gerbil",NEWTAGSFINAL$Species), "gerbil", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("giraffe",NEWTAGSFINAL$Species), "giraffe", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("goose",NEWTAGSFINAL$Species), "goose", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("guineafowl",NEWTAGSFINAL$Species), "guineafowl", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("hamerkop",NEWTAGSFINAL$Species), "hamerkop", NEWTAGSFINAL$label) #heron?
NEWTAGSFINAL$label <- ifelse(grepl("hare",NEWTAGSFINAL$Species), "hare", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("hartebeest_cokes",NEWTAGSFINAL$Species), "hartebeest_cokes", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("heron",NEWTAGSFINAL$Species), "heron", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("hippopotamus",NEWTAGSFINAL$Species), "hippopotamus", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("honey_badger",NEWTAGSFINAL$Species), "honey_badger", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("hoopoe",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("human",NEWTAGSFINAL$Species), "human", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("hyena_spotted|hyena_striped|aardwolf",NEWTAGSFINAL$Species), "hyena", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("ibis",NEWTAGSFINAL$Species), "ibis", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("impala",NEWTAGSFINAL$Species), "impala", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("insect",NEWTAGSFINAL$Species), "insect", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("jackal",NEWTAGSFINAL$Species), "jackal", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("juvenile",NEWTAGSFINAL$Species), "juvenile", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("kingfisher",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("lapwing",NEWTAGSFINAL$Species), "wader", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("lark",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("leopard",NEWTAGSFINAL$Species), "leopard", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("lion_female",NEWTAGSFINAL$Species), "lion", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("lion_male",NEWTAGSFINAL$Species), "lion", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("longclaw",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label) #############
NEWTAGSFINAL$label <- ifelse(grepl("mongoose_banded",NEWTAGSFINAL$Species), "mongoose", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("mongoose_white_tailed",NEWTAGSFINAL$Species), "mongoose", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("mongoose_other",NEWTAGSFINAL$Species), "mongoose", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("motorbike",NEWTAGSFINAL$Species), "motorbike", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("nightjar",NEWTAGSFINAL$Species), "nightjar", NEWTAGSFINAL$label) #########
NEWTAGSFINAL$label <- ifelse(grepl("oribi",NEWTAGSFINAL$Species), "oribi", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("ostrich",NEWTAGSFINAL$Species), "ostrich", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("other_bird",NEWTAGSFINAL$Species), "unidentified_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("other_rodents",NEWTAGSFINAL$Species), "other_rodents", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("oxpecker",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("pigeon",NEWTAGSFINAL$Species), "dove_pigeon", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("pipit",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("plover",NEWTAGSFINAL$Species), "wader", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("porcupine",NEWTAGSFINAL$Species), "porcupine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("query",NEWTAGSFINAL$Species), "unidentified", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("raptor_other",NEWTAGSFINAL$Species), "other_raptor", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("reedbuck",NEWTAGSFINAL$Species), "reedbuck", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("reptiles",NEWTAGSFINAL$Species), "reptiles", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("robin",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("roller",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("secretary_bird",NEWTAGSFINAL$Species), "secretary_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("serval",NEWTAGSFINAL$Species), "serval", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("shoat",NEWTAGSFINAL$Species), "shoat", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("shrike",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("southern_ground_hornbil",NEWTAGSFINAL$Species), "southern_ground_hornbil", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("sparrow",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("starling",NEWTAGSFINAL$Species), "starling", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("stork_other",NEWTAGSFINAL$Species), "stork_other", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("stork_saddle_billed",NEWTAGSFINAL$Species), "stork_saddle_billed", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("starling",NEWTAGSFINAL$Species), "starling", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("swallow",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("tchagra",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("thickknee",NEWTAGSFINAL$Species), "wader", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("topi",NEWTAGSFINAL$Species), "topi", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("unidentified",NEWTAGSFINAL$Species), "unidentified", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("vehicle",NEWTAGSFINAL$Species), "vehicle", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("vervet_monkey",NEWTAGSFINAL$Species), "vervet_monkey", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("vulture",NEWTAGSFINAL$Species), "vulture", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("warthog",NEWTAGSFINAL$Species), "warthog", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("waterbuck",NEWTAGSFINAL$Species), "waterbuck", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("weaver",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("wheatear",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("widowbird",NEWTAGSFINAL$Species), "other_passerine", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("wildebeest",NEWTAGSFINAL$Species), "wildebeest", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("woodpecker",NEWTAGSFINAL$Species), "other_bird", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("zebra",NEWTAGSFINAL$Species), "zebra", NEWTAGSFINAL$label)
NEWTAGSFINAL$label <- ifelse(grepl("zorilla",NEWTAGSFINAL$Species), "zorilla", NEWTAGSFINAL$label)

NEWTAGSFINAL.Speciestable<-as.data.frame(table(NEWTAGSFINAL$Species))
table(oldtags_orig$Species)
table(NEWTAGSFINAL$label)

table(NEWTAGSFINAL$ReasonRemoved)

#Step 8: write new file
write.csv(NEWTAGSFINAL, "2018_Kenya_Tags_withMLlabels.csv")

LabelUpdate1<-read.csv("2018_Kenya_Tags_withMLlabels.csv")
LabelUpdate1$label <- ifelse(grepl("lapwing",LabelUpdate1$Species), "lapwing_plover_thickknee", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("plover",LabelUpdate1$Species), "lapwing_plover_thickknee", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("thickknee",LabelUpdate1$Species), "lapwing_plover_thickknee", LabelUpdate1$label)


LabelUpdate1$label <- ifelse(grepl("heron",LabelUpdate1$Species), "heron_stork_crane_ibis", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("stork_other",LabelUpdate1$Species), "heron_stork_crane_ibis", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("stork_saddle_billed",LabelUpdate1$Species), "heron_stork_crane_ibis", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("crane",LabelUpdate1$Species), "heron_stork_crane_ibis", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("ibis",LabelUpdate1$Species), "heron_stork_crane_ibis", LabelUpdate1$label)

LabelUpdate1$label <- ifelse(grepl("bustard_kori",LabelUpdate1$Species), "bustard", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("bustard_black_bellied",LabelUpdate1$Species), "bustard", LabelUpdate1$label)
LabelUpdate1$label <- ifelse(grepl("bustard_white_bellied",LabelUpdate1$Species), "bustard", LabelUpdate1$label)

write.csv(LabelUpdate1, "2018_Kenya_Tags_withMLlabels_v2.csv")
table(LabelUpdate1$label)
