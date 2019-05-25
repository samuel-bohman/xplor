# Load library and source files
library(rgdal)
debugSource("r/car.R")
source("r/car_ce.R")

# Load data
data <- read.csv2("data-derived/data.csv", header = TRUE, fileEncoding = "UTF-8")

# Run functions on data
data_car <- calculateCAR(data)
data_car_ce <- calculateCAR_CE(data_car)

# Read spatial data
nyko84 <- readRDS("data-derived/nyko84.Rds")

# Merge datasets
data_spdf <- sp::merge(x = nyko84, y = data_car_ce, by = "Area", duplicateGeoms = TRUE)

# Save new spdf object
saveRDS(object = data_spdf, file = "data-derived/data_spdf.Rds")

# Remove temporary objects
remove(data)
remove(data_spdf)
remove(nyko84)
