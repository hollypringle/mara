#Load packages
install.packages("rgeos")
install.packages("maptools")
install.packages("proj4")
install.packages("data.table")
install.packages("rgdal")
install.packages("raster")

#Import csv of occurrences
gbif<- read.csv("GBIF_sample.csv", header=T)
head(gbif)

#Import shapefile of area
shpfile <- "Study_area.shp"
shape <- readOGR(shpfile)
print(proj4string(shape))

#subset the GBIF data into a data frame
occ.map <- data.frame(gbif$decimalLongitude, gbif$decimalLatitude, gbif$gbifID)
print(str(occ.map, 1))

#simplify column names
names(occ.map)[1:3] <- c('decimalLongitude', 'decimalLatitude', 'gbifID')
print(head(occ.map, 10))

#Remove missing values
occ.map.omit <- na.omit(occ.map)

#turning the data frame into a "spatial points data frame"
coordinates(occ.map.omit) <- c("long", "lat")

#defining the datum 
proj4string(occ.map.omit) <- CRS(map_crs)

#reprojecting the 'gbif' data frame to the same as in the 'shape' object 
occ.map.proj <- spTransform(occ.map.omit, proj4string(shape))

#Identifying records from gbif that fall within the shape polygons
inside <- occ.map.proj[apply(gIntersects(occ.map.proj, shape, byid = TRUE), 2, any),]

#Prepare data frame for joining with the occcurrence df so only records 
#that fall inside the polygons get selected 
res.gbif <- data.frame(inside@data)
final.gbif <- gbif %>% semi_join(res.gbif, by = c(gbifID = "gbifid"))

return(final.gbif)

write.csv(final.gbif, "GBIF_studyarea.csv")