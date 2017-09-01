# Load libraries
library(rgdal)
# library(rgeos)

# Source functions
source("car.R")
source("bcar.R")

# Read survey data
results <- read.csv2("cleaning/data/results/results.csv", header = TRUE, fileEncoding = "UTF-8")

# Run calculateCAR on the results
results <- calculateCAR(results)

# Run calculateBCAR on the results
results <- calculateBCAR(results)

# Read GIS data
nyko84b <- readRDS("cleaning/data/nyko/nyko84b.rds")

# Merge the two data sets
results_spdf <- sp::merge(x = nyko84b, y = results, by = "Area", duplicateGeoms = TRUE)

# Save the final file
saveRDS(object = results_spdf, file = "cleaning/data/merged/results_spdf.rds")

# Remove temporary objects
remove(results)
remove(results_spdf)
remove(nyko84b)