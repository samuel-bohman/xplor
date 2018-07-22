# Load packages
library(shiny)
library(shinythemes)
library(leaflet)
library(ggvis)
library(dplyr)
library(forcats)
library(purrr)
library(tibble)
library(DT)
library(RColorBrewer)
library(lpSolveAPI)
library(sp)

# Read R code from files 
source("r/menu.R")
source("r/distance.R")
source("r/optimization.R")
source("r/sd.R")

# Bookmarking
enableBookmarking(store = "url")

# SpatialPolygonsDataFrame for map polygons
nyko <- readRDS("data-derived/nyko84.rds")

# SpatialPolygonsDataFrame for group 1
data_spdf1 <- readRDS("data-derived/data_spdf.rds")

# SpatialPolygonsDataFrame for group 2
data_spdf2 <- readRDS("data-derived/data_spdf.rds")

# Data frame for tabPanel "Table"
data_df <- read.table("data-derived/data.csv", header = TRUE, sep = ";", fileEncoding = "UTF-8")

# Background variables for map
b_area <- c("", "All", sort(unique(as.character(data_df[["Area"]]))))
b_gender <- c("", "All", "Woman", "Man", "Prefer not to disclose", "Other/No gender")
b_age <- c("", "All", sort(unique(as.character(data_df[["Age"]]))))
b_occupation <- c("", "All", sort(unique(as.character(data_df[["Occupation"]]))))
b_education <- c("", "All", sort(unique(as.character(data_df[["Education.level"]]))))
b_years <- c("", "All", "0-4 years", "5-9 years", "10 years or more")

# Background variables for data table
b_gender_t <- c("", "All", "Woman", "Man", "Other", "-")
b_occupation_t <- c("", "All", "Employee", "Job-seeker", "LOA", "LTSL", "Self-employed", "Senior", "SB", "Student", "Other")
b_education_t <- c("", "All", "Doctorate", "University", "High school", "Elem. school", "No elem. school", "Other")
b_years_t <- c("", "All", "0-4 years", "5-9 years", "10+ years")

# Used for dynamically generating tabPanels for Group 1 and Group 2 (tabset.R)
b_variables <- list(area = b_area, gender = b_gender, age = b_age, occupation = b_occupation, education = b_education, years = b_years)

# Used in menu
b_names <- names(b_variables)

# Corresponding dataframe column names
b_col_names <- c("Area", "Gender", "Age", "Occupation", "Education.level", "Year")

# Labels that will appear in the GUI
b_labels <- c("Geographic Area", "Gender", "Age", "Occupation", "Education", "Length of residency")

# Randomization for polygons just for development purposes
# rnd <- "All"
rnd <- sample(x = b_area[3:45], size = 8) %>% split(f = c(1, 2))

# Themes
theme <- colnames(data_df)[57:66] %>% gsub(pattern = ".", replacement = " ", x = ., fixed = TRUE) %>% paste(c(1:10), ., sep = ". ")

# Alternatives
actions1 <- c(
  "1a. Preserve existing large green spaces", 
  "1b. Build parks in existing urban districts", 
  "1c. Build homes close to green spaces", 
  "1d. Renovate existing parks", 
  "1e. Improve accessibility to major green spaces"
  )
actions2 <- c(
  "2a. Offer more residential building types", 
  "2b. Offer more apartment sizes", 
  "2c. Offer small-scale land ownership", 
  "2d. Preserve the conceptual foundations of the buildings from the 1970s", 
  "2e. Offer more waterfront residences"
  )
actions3 <- c(
  "3a. Enable more diverse traffic", 
  "3b. Enable car parking along the streets", 
  "3c. Face residential entrances toward the streets", 
  "3d. Make public ground floor premises transparent", 
  "3e. Build underground car parks in residential buildings"
  )
actions4 <- c(
  "4a. Pair new streets with existing ones to strengthen the connection to adjacent neighborhoods and to reduce the barriers that the large roads pose", 
  "4b. Improve late night public transport", 
  "4c. Improve public transport to and from Uppsala", 
  "4d. Improve the north-south and east-west routes through a fine-mesh transportation network", 
  "4e. Improve public transport to and from Stockholm"
  )
actions5 <- c(
  "5a. Expand the range of cultural, sporting, and recreational activities", 
  "5b. Create better opportunities for festivals and concerts", 
  "5c. Create better opportunities for outdoor recreation", 
  "5d. Organize public and farmers' markets", 
  "5e. Provide municipal grants for cultural and recreational projects"
  )
actions6 <- c(
  "6a. Renovate old schools", 
  "6b. Build new schools", 
  "6c. Refurbish school yards", 
  "6d. Improve the education in primary schools", 
  "6e. Improve the education in high schools"
  )
actions7 <- c(
  "7a. More cultural and recreational activities for the elderly", 
  "7b. More cultural and recreational activities for children and young people", 
  "7c. Improve care for the elderly", 
  "7d. More youth centres and field assistants", 
  "7e. Reduce preschool child groups"
  )
actions8 <- c(
  "8a. Reduce preschool child groups", 
  "8b. Raise the quality of teaching", 
  "8c. More professional development for school teachers", 
  "8d. More modern information technology (IT) in education", 
  "8e. Involve caretakers more in school"
  )
actions9 <- c(
  "9a. Increase safety around the train station", 
  "9b. More police officers in the city center", 
  "9c. Improve the lighting in the city center", 
  "9d. Restrict the opening hours of bars and restaurants who serve alcohol in the city centre", 
  "9e. Extend the opening hours of shops in the city center"
  )
actions10 <- c(
  "10a. Reduce energy consumption", 
  "10b. Reduce transport noise and sound pollution", 
  "10c. Increase climate adaptation and recycling", 
  "10d. Prioritize environmentally friendly transport modes (walking, cycling, public transport)", 
  "10e. Reducing environmental toxins and hazardous chemicals in nature"
  )

# Put all alternative string vectors in a list
actions_list <- list(actions1, actions2, actions3, actions4, actions5, actions6, actions7, actions8, actions9, actions10)
rm(actions1, actions2, actions3, actions4, actions5, actions6, actions7, actions8, actions9, actions10)

# Dictionary of criteria and matching criterion number
criterion_number <- vector(mode = "list", length = 10)
names(criterion_number) <- theme
criterion_number[[1]] <- 1
criterion_number[[2]] <- 2
criterion_number[[3]] <- 3
criterion_number[[4]] <- 4
criterion_number[[5]] <- 5
criterion_number[[6]] <- 6
criterion_number[[7]] <- 7
criterion_number[[8]] <- 8
criterion_number[[9]] <- 9
criterion_number[[10]] <- 10

p_actions <- c("Alt.a","Alt.b","Alt.c","Alt.d","Alt.e")
