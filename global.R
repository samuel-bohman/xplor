library(shiny)
library(shinydashboard)
library(leaflet)
library(sp)
library(RColorBrewer)

# SpatialPolygonsDataFrame for map polygons
nyko <- readRDS("data/nyko.rds") 

# SpatialPolygonsDataFrame for group 1
results_spdf1 <- readRDS("data/results_spdf.rds")

# SpatialPolygonsDataFrame for group 2
results_spdf2 <- readRDS("data/results_spdf.rds")

# data frame for tabPanel "Data"
results_df <- read.table("data/results.csv", header = TRUE, sep = ";", fileEncoding = "UTF-8")

# Background variables ----
vars_area <- c("No selection" = "", "All", sort(unique(as.character(results_df[["Area"]]))))
vars_gender <- c("No selection" = "", "All", "Woman", "Man", "Prefer not to disclose", "Other/No gender")
vars_age <- c("No selection" = "", "All", sort(unique(as.character(results_df[["Age"]]))))
vars_occupation <- c("No selection" = "", "All", sort(unique(as.character(results_df[["Occupation"]]))))
vars_education <- c("No selection" = "", "All", sort(unique(as.character(results_df[["Education.level"]]))))
vars_years <- c("No selection" = "", "All", "0-4 years", "5-9 years", "10 years or more")

# Needed for dynamic form generation (group forms)
background_choices <- list(vars_area, vars_gender, vars_age, vars_occupation, vars_education, vars_years)

# Use these for input matching - TODO: Generate dynamically / From file
# Names to be used as basis for group input dropdown ids
ui_names_bg <- c("area", "gender", "age", "occupation", "education", "years")

# Corresponding dataframe column names
df_names_bg <- c("Area", "Gender", "Age", "Occupation", "Education.level", "Year")

# The labels that will appear for the ui_names entries on the forms
dropdown_names_bg <- c("Area", "Gender", "Age", "Occupation", "Education", "Length of residency")
names(df_names_bg) <- ui_names_bg

# Themes ----
themes <- colnames(results_df)[57:66]
themes <- gsub(pattern = ".", replacement = " ", x = themes, fixed = TRUE)
themes <- paste(c(1:10), themes, sep = ". ")

# Alternatives ----
alt_theme_1 <- c("1a. Preserve existing large green areas", "1b. Build parks in existing urban districts", "1c. Build homes close to green areas", "1d. Renovate existing parks", "1e. Improve accessibility to major green areas")
alt_theme_2 <- c("2a. Offer more residential building types", "2b. Offer more apartment sizes", "2c. Offer small-scale land ownership", "2d. Preserve the conceptual foundations of the buildings from the 1970s", "2e. Offer more waterfront residences")
alt_theme_3 <- c("3a. Enable more diverse traffic", "3b. Enable car parking along the streets", "3c. Face residential entrances toward the streets", "3d. Make public ground floor premises transparent", "3e. Build underground car parks in residential buildings")
alt_theme_4 <- c("4a. Pair new streets with existing ones to strengthen the connection to adjacent neighborhoods and to reduce the barriers that the large roads pose", "4b. Improve late night public transport", "4c. Improve public transport to and from Uppsala", "4d. Improve the north-south and east-west routes through a fine-mesh transportation network", "4e. Improve public transport to and from Stockholm")
alt_theme_5 <- c("5a. Expand the range of cultural, sporting, and recreational activities", "5b. Create better opportunities for festivals and concerts", "5c. Create better opportunities for outdoor recreation", "5d. Organize public and farmers' markets", "5e. Provide municipal grants for cultural and recreational projects")
alt_theme_6 <- c("6a. Renovate old schools", "6b. Build new schools", "6c. Refurbish school yards", "6d. Improve the education in primary schools", "6e. Improve the education in high schools")
alt_theme_7 <- c("7a. More cultural and recreational activities for the elderly", "7b. More cultural and recreational activities for children and young people", "7c. Improve care for the elderly", "7d. More youth centres and field assistants", "7e. Reduce preschool child groups")
alt_theme_8 <- c("8a. Reduce preschool child groups", "8b. Raise the quality of teaching", "8c. More professional development for school teachers", "8d. More modern information technology (IT) in education", "8e. Involve caretakers more in school")
alt_theme_9 <- c("9a. Increase safety around the train station", "9b. More police officers in the city center", "9c. Improve the lighting in the city center", "9d. Restrict the opening hours of bars and restaurants who serve alcohol in the city centre", "9e. Extend the opening hours of shops in the city center")
alt_theme_10 <- c("10a. Reduce energy consumption", "10b. Reduce transport noise and sound pollution", "10c. Increase climate adaptation and recycling", "10d. Prioritize environmentally friendly transport modes (walking, cycling, public transport)", "10e. Reducing environmental toxins and hazardous chemicals in nature")
alt_list <- list(alt_theme_1, alt_theme_2, alt_theme_3, alt_theme_4, alt_theme_5, alt_theme_6, alt_theme_7, alt_theme_8, alt_theme_9, alt_theme_10)