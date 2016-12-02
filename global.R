# Load libraries
library(shiny)
library(leaflet)
library(sp)
library(RColorBrewer)

# Read data ----
# SpatialPolygonsDataFrame for group 1
results_spdf1 <- readRDS("data/results2.rds") 
# SpatialPolygonsDataFrame for group 2
results_spdf2 <- readRDS("data/results2.rds") 
# DataFrame for tabPanel "Data"
results_df <- read.table(
    "data/results.csv",
    header = TRUE,
    sep = ";",
    fileEncoding = "UTF-8"
  ) 
# SpatialPolygonsDataFrame for nyko polygons
nyko <- readRDS("data/nyko.rds") 

# Define background variables ----
vars_area <-
  c("Inget val" = "", "Alla", sort(unique(as.character(results_df[["Omrade"]]))))
vars_sex <-
  c("Inget val" = "",
    "Alla",
    "Kvinna",
    "Man",
    "Vill ej uppge",
    "Annat/inget kön")
vars_age <-
  c("Inget val" = "", "Alla", sort(unique(as.character(results_df[["Ålder"]]))))
vars_occupation <-
  c("Inget val" = "", "Alla", sort(unique(as.character(results_df[["Sysselsättning"]]))))
vars_education <-
  c(
    "Inget val" = "",
    "Alla",
    "Inte gått ut grundskola eller motsvarande obligatorisk skola",
    "Grundskola eller motsvarande obligatorisk skola",
    "Gymnasium, folkhögskola eller motsvarande",
    "Annan eftergymnasial utbildning",
    "Högskola/universitet",
    "Forskarutbildning"
  )

vars_years <- c("Inget val" = "", "Alla", "0-4 år", "5-9 år", "10 år eller mer")

# Needed for dynamic form generation (group forms)
background_choices <- list(vars_area, vars_sex, vars_age, vars_occupation, vars_education, vars_years)

# Use these for input matching - TODO: Generate dynamically / From file
# Names to be used as basis for group input dropdown ids
ui_names_bg <- c("area", "sex", "age", "occupation", "education", "years")

# Corresponding dataframe column names
df_names_bg <- c("Omrade", "Kön", "Ålder", "Sysselsättning", "Utbildningsnivå", "År")

# The labels that will appear for the ui_names entries on the forms
dropdown_names_bg <- c("Område", "Kön", "Ålder", "Sysselsättning", "Utbildningsnivå", "Hur länge bott i kommunen")
names(df_names_bg) <- ui_names_bg

# Determines the number of controlled groups (generates a form for each)
group_amount = 2

# Define themes and alternatives variables ----
themes <- c("1. Parker & grönområden", "2. Mångfald i bostadsutbudet", "3. Levandegöra gemensamma platser", "4. Kommunikationer", "5. Kultur & fritid", "6. Utbildning", "7. Omsorg", "8. Skolan", "9. Trygghet", "10. Hållbar utveckling")
alt_theme_1 <- c("1a. Bevara existerande större grönområden", "1b. Anlägga parker i existerande stadsdelar", "1c. Bygga bostäder nära grönområden", "1d. Rusta upp befintliga parker", "1e. Skapa bättre tillgänglighet till större grönområden")
alt_theme_2 <- c("2a. Erbjuda fler bostadstyper", "2b. Erbjuda fler lägenhetsstorlekar", "2c. Erbjuda småskaligt markägande", "2d. Bevara de idémässiga grunderna för bebyggelsen från 1970-talet", "2e. Erbjuda fler bostäder nära vatten")
alt_theme_3 <- c("3a. Blanda trafikslagen", "3b. Förlägga parkering längs med gator", "3c. Vända entréer mot gator", "3d. Förlägga publika lokaler i transparenta bottenvåningar", "3e. Trygga parkeringslösningar under bostäder")
alt_theme_4 <- c("4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör", "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen", "4c. Förbättra kommunikationerna till och från Uppsala", "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät", "4e. Förbättra kommunikationerna till och från Stockholms innerstad")
alt_theme_5 <- c("5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter", "5b. Skapa bättre möjligheter för festivaler och konserter", "5c. Skapa fler förutsättningar för utomhussporter", "5d. Skapa marknadsplatser utomhus", "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt")
alt_theme_6 <- c("6a. Rusta upp äldre skolor", "6b. Bygg nya skolor", "6c. Förbättra skolgårdarnas fysiska miljöer", "6d. Höj kvaliteten i grundskolan", "6e. Höj kvaliteten på gymnasieutbildningarna")
alt_theme_7 <- c("7a. Fler kultur- och fritidsaktiviteter för äldre", "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar", "7c. Förbättra äldreomsorgen i kommunen", "7d. Fler ungdomsgårdar och fältassistenter", "7e. Minska barngrupperna i förskolan")
alt_theme_8 <- c("8a. Mindre barngrupper i förskolan", "8b. Höj kvaliteten i undervisningen", "8c. Mer kompetensutveckling för skolor och lärare", "8d. Mer modern informationsteknologi (IT) i undervisningen", "8e. Involvera vårdnadshavare mer i skolan")
alt_theme_9 <- c("9a. Öka tryggheten kring stationsområdet", "9b. Fler poliser i centrala Väsby", "9c. Förbättra belysningen i centrala Väsby", "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby", "9e. Förläng öppettider för affärsverksamhet i centrala Väsby")
alt_theme_10 <- c("10a. Minska förbrukningen av energi", "10b. Minska transporter och buller", "10c. Öka klimatanpassning och kretsloppstänkande", "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)", "10e. Minska miljögifter och farliga kemikalier i naturen")
alt_list <-
  list(
    alt_theme_1,
    alt_theme_2,
    alt_theme_3,
    alt_theme_4,
    alt_theme_5,
    alt_theme_6,
    alt_theme_7,
    alt_theme_8,
    alt_theme_9,
    alt_theme_10
  )