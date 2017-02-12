# Load libraries
library(rgdal)
library(rgeos)

# Read MapInfo TAB format (MapInfo Professional)
# "TAB" refers to a set of files with extensions .TAB, .DAT, .MAP, .ID, .IND
# http://stackoverflow.com/questions/20101083/how-can-i-read-mapinfo-files-in-r
nyko <- readOGR(dsn = "cleaning/data/nyko/Nycklekodsomr_3siffr.TAB", layer = "Nycklekodsomr_3siffr", use_iconv = TRUE, encoding = "latin1")

# List layers
# ogrListLayers(dsn = "data/nyko/Nycklekodsomr_3siffr.TAB")

# Read KML file
# nyko <- readOGR(dsn = "data/doc.kml", layer = "Area Features")

# Remove unecessary columns
nyko$Klass <- nyko$Klass_förklaring <- nyko$Z1 <- nyko$Z2 <- nyko$Text <- nyko$om_1 <- nyko$om_2 <- nyko$om_3 <- nyko$om_4 <- nyko$Socken <- nyko$Ursprungsdatum <- NULL

# Reproject
EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code
nyko84 <- spTransform(nyko, CRS("+init=epsg:4326")) # reproject nyko to nyko84
remove(EPSG)
remove(nyko)

# Clean/save nyko84
names(nyko84@data)[2] <- "Area" 
levels(nyko84@data$Area)[1] <- "Antuna/Älvsunda"
levels(nyko84@data$Area)[2] <- "Brunnby-Vik"

# Create centroids
centroids <- coordinates(nyko84) # creates a matrix object
centroids <- as.data.frame(centroids)
names(centroids)[1] <- "long"
names(centroids)[2] <- "lat"

# BIVAND ET AL. - 2013 - APPLIED SPATIAL DATA ANALYSIS
# 2.6.1. SpatialPolygonsDataFrame (p. 44)
# a polygon object has a slot "labpt" for label point/centroid

# Merge nyko84 with centroids
centroids$ID <- nyko84@data$ID # add common column
nyko84b <- sp::merge(x = nyko84, y = centroids, by = "ID")
saveRDS(object = nyko84b, file = "data/nyko/nyko84b.rds")
remove(centroids)
remove(nyko84)
remove(nyko84b)