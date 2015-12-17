# load libraries ----
library(sp)
library(shiny)
library(leaflet)
library(RColorBrewer)

# global ----
# read data
results_spdf1 <- readRDS("data/results2.rds") # SpatialPolygonsDataFrame for group 1
results_spdf2 <- readRDS("data/results2.rds") # SpatialPolygonsDataFrame for group 2
results_df <-  read.table("data/results.csv", header = TRUE, sep = ";", fileEncoding = "UTF-8") # DataFrame for tabPanel "Data"
nyko <- readRDS("data/nyko.rds") # SpatialPolygonsDataFrame for nyko polygons

# define background variables
vars_area <- c("Inget val" = "", "Alla", sort(unique(as.character(results_df$Omrade))))
vars_sex <- c("Inget val" = "", "Alla", "Kvinna", "Man", "Vill ej uppge", "Annat/inget kön")
vars_age <- c("Inget val" = "", "Alla", sort(unique(as.character(results_df$Ålder))))
vars_occupation <- c("Inget val" = "", "Alla", sort(unique(as.character(results_df$Sysselsättning))))
vars_education <- c("Inget val" = "", "Alla", "Inte gått ut grundskola eller motsvarande obligatorisk skola", "Grundskola eller motsvarande obligatorisk skola", "Gymnasium, folkhögskola eller motsvarande", "Annan eftergymnasial utbildning", "Högskola/universitet", "Forskarutbildning")
vars_years <- c("Inget val" = "", "Alla", "0-4 år", "5-9 år", "10 år eller mer")

# define themes and alternatives variables
themes <- c("1. Parker & grönområden", "2. Mångfald i bostadsutbudet", "3. Levandegöra gemensamma platser", "4. Kommunikationer", "5. Kultur & fritid", "6. Utbildning", "7. Omsorg", "8. Skolan", "9. Trygghet", "10. Hållbar utveckling")
altTheme1 <- c("1a. Bevara existerande större grönområden", "1b. Anlägga parker i existerande stadsdelar", "1c. Bygga bostäder nära grönområden", "1d. Rusta upp befintliga parker", "1e. Skapa bättre tillgänglighet till större grönområden")
altTheme2 <- c("2a. Erbjuda fler bostadstyper", "2b. Erbjuda fler lägenhetsstorlekar", "2c. Erbjuda småskaligt markägande", "2d. Bevara de idémässiga grunderna för bebyggelsen från 1970-talet", "2e. Erbjuda fler bostäder nära vatten")
altTheme3 <- c("3a. Blanda trafikslagen", "3b. Förlägga parkering längs med gator", "3c. Vända entréer mot gator", "3d. Förlägga publika lokaler i transparenta bottenvåningar", "3e. Trygga parkeringslösningar under bostäder")
altTheme4 <- c("4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör", "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen", "4c. Förbättra kommunikationerna till och från Uppsala", "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät", "4e. Förbättra kommunikationerna till och från Stockholms innerstad")
altTheme5 <- c("5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter", "5b. Skapa bättre möjligheter för festivaler och konserter", "5c. Skapa fler förutsättningar för utomhussporter", "5d. Skapa marknadsplatser utomhus", "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt")
altTheme6 <- c("6a. Rusta upp äldre skolor", "6b. Bygg nya skolor", "6c. Förbättra skolgårdarnas fysiska miljöer", "6d. Höj kvaliteten i grundskolan", "6e. Höj kvaliteten på gymnasieutbildningarna")
altTheme7 <- c("7a. Fler kultur- och fritidsaktiviteter för äldre", "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar", "7c. Förbättra äldreomsorgen i kommunen", "7d. Fler ungdomsgårdar och fältassistenter", "7e. Minska barngrupperna i förskolan")
altTheme8 <- c("8a. Mindre barngrupper i förskolan", "8b. Höj kvaliteten i undervisningen", "8c. Mer kompetensutveckling för skolor och lärare", "8d. Mer modern informationsteknologi (IT) i undervisningen", "8e. Involvera vårdnadshavare mer i skolan")
altTheme9 <- c("9a. Öka tryggheten kring stationsområdet", "9b. Fler poliser i centrala Väsby", "9c. Förbättra belysningen i centrala Väsby", "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby", "9e. Förläng öppettider för affärsverksamhet i centrala Väsby")
altTheme10 <- c("10a. Minska förbrukningen av energi", "10b. Minska transporter och buller", "10c. Öka klimatanpassning och kretsloppstänkande", "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)", "10e. Minska miljögifter och farliga kemikalier i naturen")
contra <- c("12. Vatten eller bostäder", "13. Service eller grönområden", "14. Centralort eller mindre tätort")

# ui ----
ui <- navbarPage("xploR beta", id = "nav", position = "static-top", collapsible = TRUE, fluid = TRUE,
  tabPanel(
    "Karta", 
    div(
      class = "outer",
      tags$head(includeCSS("styles.css")),
      leafletOutput("map", width = "100%", height = "100%"),
      # panel 1
      absolutePanel(id = "panel1", class = "panel panel-default", draggable = TRUE, top = 30, left = 60, right = "auto", bottom = "auto", width = 320, height = "auto",
        h3("Grupp 1"),
        selectInput(inputId = "area1", label = "Område:", choices = vars_area, selected = c("Eds Glesbygd"), multiple = TRUE), # options = list(onDelete = I('function(value) {return false;}'))
        selectInput(inputId = "sex1", label = "Kön:", choices = vars_sex, selected = c("Alla"), multiple = TRUE),
        selectInput(inputId = "age1", label = "Ålder:", choices = vars_age, selected = c("Alla"), multiple = TRUE),
        selectInput(inputId = "occupation1", label = "Sysselsättning:", choices = vars_occupation, selected = "Alla", multiple = TRUE),
        selectInput(inputId = "education1", label = "Utbildningsnivå:", choices = vars_education, selected = "Alla", multiple = TRUE),
        selectInput(inputId = "years1", label = "Hur länge bott i kommunen:", choices = vars_years, selected = "Alla", multiple = TRUE),
        # uiOutput("weights1"),
        checkboxInput(inputId = "pop1", label = "Visa namn", value = FALSE),
        checkboxInput(inputId = "markers1", label = "Visa markörer", value = FALSE)
      ),
      # panel 2
      absolutePanel(id = "panel2", class = "panel panel-default", draggable = TRUE, top = 30, left = "auto", right = 20, bottom = "auto", width = 320, height = "auto",
        h3("Grupp 2"),
        selectInput(inputId = "area2", label = "Område:", choices = vars_area, selected = c("Fresta glesbygd"), multiple = TRUE),
        selectInput(inputId = "sex2", label = "Kön:", choices = vars_sex, selected = "Man", multiple = TRUE),
        selectInput(inputId = "age2", label = "Ålder:", choices = vars_age, selected = c("Alla"), multiple = TRUE),
        selectInput(inputId = "occupation2", label = "Sysselsättning:", choices = vars_occupation, selected = "Alla", multiple = TRUE),
        selectInput(inputId = "education2", label = "Utbildningsnivå:", choices = vars_education, selected = "Alla", multiple = TRUE),
        selectInput(inputId = "years2", label = "Hur länge bott i kommunen:", choices = vars_years, selected = "Alla", multiple = TRUE),
        # uiOutput("weights2"),
        checkboxInput(inputId = "pop2", label = "Visa namn", value = FALSE),
        checkboxInput(inputId = "markers2", label = "Visa markörer", value = FALSE)
      ),
      # panel 3
      absolutePanel(id = "panel3", class = "panel panel-default", draggable = TRUE, top = 30, left = 400, right = "auto", bottom = "auto", width = "auto", height = "auto",
        h3("Tema & Alternativ"), 
        selectInput(inputId = "themes", label = h5("Tema"), choices = themes, selected = "1. Parker & grönområden"),
        uiOutput("alternatives"),
        selectInput(inputId = "colors", label = "Färgschema", choices = rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "RdYlGn")
      )
    )
  ),
  tabPanel(
    "Tabell",
    fluidRow(
            column(width = 3, selectInput(inputId = "area", label = "Område:", choices = vars_area, selected = "Alla", multiple = TRUE)),
            column(width = 3, selectInput(inputId = "sex", label = "Kön:", choices = vars_sex, selected = "Alla", multiple = TRUE)),
            column(width = 3, selectInput(inputId = "age", label = "Ålder:", choices = vars_age, selected = "Alla", multiple = TRUE)),
            column(width = 3, selectInput(inputId = "occupation", label = "Sysselsättning:", choices = vars_occupation, selected = "Alla", multiple = TRUE)),
            column(width = 3, selectInput(inputId = "education", label = "Utbildningsnivå:", choices = vars_education, selected = "Alla", multiple = TRUE)),
            column(width = 3, selectInput(inputId = "years", label = "Hur länge bott i kommunen:", choices = vars_years, selected = "Alla", multiple = TRUE))
    ),
    fluidRow(DT::dataTableOutput(outputId = "table"))
  ),
  tabPanel(
  "Hjälp",
  fluidRow(includeMarkdown("help.Rmd"))
  )
)

# server ----
server <- function(input, output, session) {
  
  # create static map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 17.91128, lat = 59.51839, zoom = 12) %>% 
      addPolygons(data = nyko, fill = TRUE, fillOpacity = 0.1, fillColor = "black", stroke = TRUE, weight = 1, color = "black", group = "nyko")
  })
  
  # render alternatives menu
  output[["alternatives"]] <- renderUI({
    if (input[["themes"]] == "1. Parker & grönområden") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme1, multiple = FALSE))
    }
    if (input[["themes"]] == "2. Mångfald i bostadsutbudet") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme2, multiple = FALSE))
    }
    if (input[["themes"]] == "3. Levandegöra gemensamma platser") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme3, multiple = FALSE))
    }
    if (input[["themes"]] == "4. Kommunikationer") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme4, multiple = FALSE))
    }
    if (input[["themes"]] == "5. Kultur & fritid") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme5, multiple = FALSE))
    }
    if (input[["themes"]] == "6. Utbildning") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme6, multiple = FALSE))
    }
    if (input[["themes"]] == "7. Omsorg") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme7, multiple = FALSE))
    }
    if (input[["themes"]] == "8. Skolan") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme8, multiple = FALSE))
    }
    if (input[["themes"]] == "9. Trygghet") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme9, multiple = FALSE))
    }
    if (input[["themes"]] == "10. Hållbar utveckling") {
      return(selectInput(inputId = "alt", label = h5("Alternativ"), choices = altTheme10, multiple = FALSE))
    }
  })
  
  # color palette
  colorpal <- reactive({
    colorNumeric(palette = input[["colors"]], domain = c(0, 14), na.color = "gray")
    # colorNumeric(palette = c("red", "white", "darkgreen"), domain = c(0, 14), na.color = "gray")
  })
  
  ### GROUP 1 FILTERS AND CALCULATIONS ########################
  
  # subset: background variables
  group1Filter1 <- reactive({
    if (input[["area1"]] != "Alla") {
      for (i in 1:length(input[["area1"]])) { 
        results_spdf1 <- results_spdf1[results_spdf1[["Omrade"]] %in% input[["area1"]], ]
      }
    }
    if (input[["sex1"]] != "Alla") {
      for (j in 1:length(input[["sex1"]])) { 
        results_spdf1 <- results_spdf1[results_spdf1[["Kön"]] %in% input[["sex1"]], ]
      }
    }
    if (input[["age1"]] != "Alla") {
      for (k in 1:length(input[["age1"]])) { 
        results_spdf1 <- results_spdf1[results_spdf1[["Ålder"]] %in% input[["age1"]], ]
      }
    }
    if (input[["occupation1"]] != "Alla") {
      for (l in 1:length(input[["occupation1"]])) { 
        results_spdf1 <- results_spdf1[results_spdf1[["Sysselsättning"]] %in% input[["occupation1"]], ]
      }
    }
    if (input[["education1"]] != "Alla") {
      for (m in 1:length(input[["education1"]])) { 
        results_spdf1 <- results_spdf1[results_spdf1[["Utbildningsnivå"]] %in% input[["education1"]], ]
      }
    }
    if (input[["years1"]] != "Alla") {
      for (n in 1:length(input[["years1"]])) { 
        results_spdf1 <- results_spdf1[results_spdf1[["År"]] %in% input[["years1"]], ]
      }
    }
    results_spdf1
  })
  
  # subset: alternatives
  group1Filter2 <- reactive({
    # theme 1
    if (input[["alt"]] == "1a. Bevara existerande större grönområden") {
      return(as.matrix(group1Filter1()@data[, 10]))
    }
    if (input[["alt"]] == "1b. Anlägga parker i existerande stadsdelar") {
      return(as.matrix(group1Filter1()@data[, 11]))
    }
    if (input[["alt"]] == "1c. Bygga bostäder nära grönområden") {
      return(as.matrix(group1Filter1()@data[, 12]))
    }
    if (input[["alt"]] == "1d. Rusta upp befintliga parker") {
      return(as.matrix(group1Filter1()@data[, 13]))
    }
    if (input[["alt"]] == "1e. Skapa bättre tillgänglighet till större grönområden") {
      return(as.matrix(group1Filter1()@data[, 14]))
    }
    # theme 2
    if (input[["alt"]] == "2a. Erbjuda fler bostadstyper") {
      return(as.matrix(group1Filter1()@data[, 15]))
    }
    if (input[["alt"]] == "2b. Erbjuda fler lägenhetsstorlekar") {
      return(as.matrix(group1Filter1()@data[, 16]))
    }
    if (input[["alt"]] == "2c. Erbjuda småskaligt markägande") {
      return(as.matrix(group1Filter1()@data[, 17]))
    }
    if (input[["alt"]] == "2d. Bevara de idémässiga grunderna för bebyggelsen från 1970-talet") {
      return(as.matrix(group1Filter1()@data[, 18]))
    }
    if (input[["alt"]] == "2e. Erbjuda fler bostäder nära vatten") {
      return(as.matrix(group1Filter1()@data[, 19]))
    }
    # theme 3
    if (input[["alt"]] == "3a. Blanda trafikslagen") {
      return(as.matrix(group1Filter1()@data[, 20]))
    }
    if (input[["alt"]] == "3b. Förlägga parkering längs med gator") {
      return(as.matrix(group1Filter1()@data[, 21]))
    }
    if (input[["alt"]] == "3c. Vända entréer mot gator") {
      return(as.matrix(group1Filter1()@data[, 22]))
    }
    if (input[["alt"]] == "3d. Förlägga publika lokaler i transparenta bottenvåningar") {
      return(as.matrix(group1Filter1()@data[, 23]))
    }
    if (input[["alt"]] == "3e. Trygga parkeringslösningar under bostäder") {
      return(as.matrix(group1Filter1()@data[, 24]))
    }
    # theme 4
    if (input[["alt"]] == "4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör") {
      return(as.matrix(group1Filter1()@data[, 25]))
    }
    if (input[["alt"]] == "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen") {
      return(as.matrix(group1Filter1()@data[, 26]))
    }
    if (input[["alt"]] == "4c. Förbättra kommunikationerna till och från Uppsala") {
      return(as.matrix(group1Filter1()@data[, 27]))
    }
    if (input[["alt"]] == "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät") {
      return(as.matrix(group1Filter1()@data[, 28]))
    }
    if (input[["alt"]] == "4e. Förbättra kommunikationerna till och från Stockholms innerstad") {
      return(as.matrix(group1Filter1()@data[, 29]))
    }
    # theme 5
    if (input[["alt"]] == "5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter") {
      return(as.matrix(group1Filter1()@data[, 30]))
    }
    if (input[["alt"]] == "5b. Skapa bättre möjligheter för festivaler och konserter") {
      return(as.matrix(group1Filter1()@data[, 31]))
    }
    if (input[["alt"]] == "5c. Skapa fler förutsättningar för utomhussporter") {
      return(as.matrix(group1Filter1()@data[, 32]))
    }
    if (input[["alt"]] == "5d. Skapa marknadsplatser utomhus") {
      return(as.matrix(group1Filter1()@data[, 33]))
    }
    if (input[["alt"]] == "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt") {
      return(as.matrix(group1Filter1()@data[, 34]))
    }
    # theme 6
    if (input[["alt"]] == "6a. Rusta upp äldre skolor") {
      return(as.matrix(group1Filter1()@data[, 35]))
    }
    if (input[["alt"]] == "6b. Bygg nya skolor") {
      return(as.matrix(group1Filter1()@data[, 36]))
    }
    if (input[["alt"]] == "6c. Förbättra skolgårdarnas fysiska miljöer") {
      return(as.matrix(group1Filter1()@data[, 37]))
    }
    if (input[["alt"]] == "6d. Höj kvaliteten i grundskolan") {
      return(as.matrix(group1Filter1()@data[, 38]))
    }
    if (input[["alt"]] == "6e. Höj kvaliteten på gymnasieutbildningarna") {
      return(as.matrix(group1Filter1()@data[, 39]))
    }
    # theme 7
    if (input[["alt"]] == "7a. Fler kultur- och fritidsaktiviteter för äldre") {
      return(as.matrix(group1Filter1()@data[, 40]))
    }
    if (input[["alt"]] == "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar") {
      return(as.matrix(group1Filter1()@data[, 41]))
    }
    if (input[["alt"]] == "7c. Förbättra äldreomsorgen i kommunen") {
      return(as.matrix(group1Filter1()@data[, 42]))
    }
    if (input[["alt"]] == "7d. Fler ungdomsgårdar och fältassistenter") {
      return(as.matrix(group1Filter1()@data[, 43]))
    }
    if (input[["alt"]] == "7e. Minska barngrupperna i förskolan") {
      return(as.matrix(group1Filter1()@data[, 44]))
    }
    # theme 8
    if (input[["alt"]] == "8a. Mindre barngrupper i förskolan") {
      return(as.matrix(group1Filter1()@data[, 45]))
    }
    if (input[["alt"]] == "8b. Höj kvaliteten i undervisningen") {
      return(as.matrix(group1Filter1()@data[, 46]))
    }
    if (input[["alt"]] == "8c. Mer kompetensutveckling för skolor och lärare") {
      return(as.matrix(group1Filter1()@data[, 47]))
    }
    if (input[["alt"]] == "8d. Mer modern informationsteknologi (IT) i undervisningen") {
      return(as.matrix(group1Filter1()@data[, 48]))
    }
    if (input[["alt"]] == "8e. Involvera vårdnadshavare mer i skolan") {
      return(as.matrix(group1Filter1()@data[, 49]))
    }
    # theme 9
    if (input[["alt"]] == "9a. Öka tryggheten kring stationsområdet") {
      return(as.matrix(group1Filter1()@data[, 50]))
    }
    if (input[["alt"]] == "9b. Fler poliser i centrala Väsby") {
      return(as.matrix(group1Filter1()@data[, 51]))
    }
    if (input[["alt"]] == "9c. Förbättra belysningen i centrala Väsby") {
      return(as.matrix(group1Filter1()@data[, 52]))
    }
    if (input[["alt"]] == "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby") {
      return(as.matrix(group1Filter1()@data[, 53]))
    }
    if (input[["alt"]] == "9e. Förläng öppettider för affärsverksamhet i centrala Väsby") {
      return(as.matrix(group1Filter1()@data[, 54]))
    }
    # theme 10
    if (input[["alt"]] == "10a. Minska förbrukningen av energi") {
      return(as.matrix(group1Filter1()@data[, 55]))
    }
    if (input[["alt"]] == "10b. Minska transporter och buller") {
      return(as.matrix(group1Filter1()@data[, 56]))
    }
    if (input[["alt"]] == "10c. Öka klimatanpassning och kretsloppstänkande") {
      return(as.matrix(group1Filter1()@data[, 57]))
    }
    if (input[["alt"]] == "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)") {
      return(as.matrix(group1Filter1()@data[, 58]))
    }
    if (input[["alt"]] == "10e. Minska miljögifter och farliga kemikalier i naturen") {
      return(as.matrix(group1Filter1()@data[, 59]))
    }
  })
  
  # calculate mean
  group1Mean <- reactive({
    mean1 <- round(mean(group1Filter2()))
  })
  
  # subset: theme weights
  group1Filter3 <- reactive({
    if (input[["themes"]] == "1. Parker & grönområden") {
      return(as.matrix(group1Filter1()@data[, 60]))
    }
    if (input[["themes"]] == "2. Mångfald i bostadsutbudet") {
      return(as.matrix(group1Filter1()@data[, 61]))
    }
    if (input[["themes"]] == "3. Levandegöra gemensamma platser") {
      return(as.matrix(group1Filter1()@data[, 62]))
    }
    if (input[["themes"]] == "4. Kommunikationer") {
      return(as.matrix(group1Filter1()@data[, 63]))
    }
    if (input[["themes"]] == "5. Kultur & fritid") {
      return(as.matrix(group1Filter1()@data[, 64]))
    }
    if (input[["themes"]] == "6. Utbildning") {
      return(as.matrix(group1Filter1()@data[, 65]))
    }
    if (input[["themes"]] == "7. Omsorg") {
      return(as.matrix(group1Filter1()@data[, 66]))
    }
    if (input[["themes"]] == "8. Skolan") {
      return(as.matrix(group1Filter1()@data[, 67]))
    }
    if (input[["themes"]] == "9. Trygghet") {
      return(as.matrix(group1Filter1()@data[, 68]))
    }
    if (input[["themes"]] == "10. Hållbar utveckling") {
      return(as.matrix(group1Filter1()@data[, 69]))
    }
  })
  
  # render theme weight slider
#   output[["weights1"]] <- renderUI({
#     if (!is.null(input[["alt"]]) && !is.null(input[["area1"]]) && !is.null(input[["sex1"]]) && !is.null(input[["age1"]]) && !is.null(input[["occupation1"]]) && !is.null(input[["education1"]]) && !is.null(input[["years1"]])) {
#       min1 <- min(group1Filter3())
#       max1 <- max(group1Filter3())
#       sliderInput(inputId = "weights1", label = h5("Vikt tema"), min = min1, max = max1, value = c(min1, max1), step = 1, ticks = TRUE, dragRange = TRUE)
#     }
#   })
  
  ### GROUP 2 FILTERS AND CALCULATIONS ########################
  
  # subset: background variables
  group2Filter1 <- reactive({
    if (input[["area2"]] != "Alla") {
      for (a in 1:length(input[["area2"]])) { 
        results_spdf2 <- results_spdf2[results_spdf2[["Omrade"]] %in% input[["area2"]], ]
      }
    }
    if (input[["sex2"]] != "Alla") {
      for (b in 1:length(input[["sex2"]])) { 
        results_spdf2 <- results_spdf2[results_spdf2[["Kön"]] %in% input[["sex2"]], ]
      }
    }
    if (input[["age2"]] != "Alla") {
      for (c in 1:length(input[["age2"]])) { 
        results_spdf2 <- results_spdf2[results_spdf2[["Ålder"]] %in% input[["age2"]], ]
      }
    }
    if (input[["occupation2"]] != "Alla") {
      for (d in 1:length(input[["occupation2"]])) { 
        results_spdf2 <- results_spdf2[results_spdf2[["Sysselsättning"]] %in% input[["occupation2"]], ]
      }
    }
    if (input[["education2"]] != "Alla") {
      for (e in 1:length(input[["education2"]])) { 
        results_spdf2 <- results_spdf2[results_spdf2[["Utbildningsnivå"]] %in% input[["education2"]], ]
      }
    }
    if (input[["years2"]] != "Alla") {
      for (f in 1:length(input[["years2"]])) {
        results_spdf2 <- results_spdf2[results_spdf2[["År"]] %in% input[["years2"]], ]
      }
    }
    results_spdf2
  })
  
  # subset: alternatives
  group2Filter2 <- reactive({
    # theme 1
    if (input[["alt"]] == "1a. Bevara existerande större grönområden") {
      return(as.matrix(group2Filter1()@data[, 10]))
    }
    if (input[["alt"]] == "1b. Anlägga parker i existerande stadsdelar") {
      return(as.matrix(group2Filter1()@data[, 11]))
    }
    if (input[["alt"]] == "1c. Bygga bostäder nära grönområden") {
      return(as.matrix(group2Filter1()@data[, 12]))
    }
    if (input[["alt"]] == "1d. Rusta upp befintliga parker") {
      return(as.matrix(group2Filter1()@data[, 13]))
    }
    if (input[["alt"]] == "1e. Skapa bättre tillgänglighet till större grönområden") {
      return(as.matrix(group2Filter1()@data[, 14]))
    }
    # theme 2
    if (input[["alt"]] == "2a. Erbjuda fler bostadstyper") {
      return(as.matrix(group2Filter1()@data[, 15]))
    }
    if (input[["alt"]] == "2b. Erbjuda fler lägenhetsstorlekar") {
      return(as.matrix(group2Filter1()@data[, 16]))
    }
    if (input[["alt"]] == "2c. Erbjuda småskaligt markägande") {
      return(as.matrix(group2Filter1()@data[, 17]))
    }
    if (input[["alt"]] == "2d. Bevara de idémässiga grunderna för bebyggelsen från 1970-talet") {
      return(as.matrix(group2Filter1()@data[, 18]))
    }
    if (input[["alt"]] == "2e. Erbjuda fler bostäder nära vatten") {
      return(as.matrix(group2Filter1()@data[, 19]))
    }
    # theme 3
    if (input[["alt"]] == "3a. Blanda trafikslagen") {
      return(as.matrix(group2Filter1()@data[, 20]))
    }
    if (input[["alt"]] == "3b. Förlägga parkering längs med gator") {
      return(as.matrix(group2Filter1()@data[, 21]))
    }
    if (input[["alt"]] == "3c. Vända entréer mot gator") {
      return(as.matrix(group2Filter1()@data[, 22]))
    }
    if (input[["alt"]] == "3d. Förlägga publika lokaler i transparenta bottenvåningar") {
      return(as.matrix(group2Filter1()@data[, 23]))
    }
    if (input[["alt"]] == "3e. Trygga parkeringslösningar under bostäder") {
      return(as.matrix(group2Filter1()@data[, 24]))
    }
    #     # theme 4
    if (input[["alt"]] == "4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör") {
      return(as.matrix(group2Filter1()@data[, 25]))
    }
    if (input[["alt"]] == "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen") {
      return(as.matrix(group2Filter1()@data[, 26]))
    }
    if (input[["alt"]] == "4c. Förbättra kommunikationerna till och från Uppsala") {
      return(as.matrix(group2Filter1()@data[, 27]))
    }
    if (input[["alt"]] == "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät") {
      return(as.matrix(group2Filter1()@data[, 28]))
    }
    if (input[["alt"]] == "4e. Förbättra kommunikationerna till och från Stockholms innerstad") {
      return(as.matrix(group2Filter1()@data[, 29]))
    }
    #     # theme 5
    if (input[["alt"]] == "5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter") {
      return(as.matrix(group2Filter1()@data[, 30]))
    }
    if (input[["alt"]] == "5b. Skapa bättre möjligheter för festivaler och konserter") {
      return(as.matrix(group2Filter1()@data[, 31]))
    }
    if (input[["alt"]] == "5c. Skapa fler förutsättningar för utomhussporter") {
      return(as.matrix(group2Filter1()@data[, 32]))
    }
    if (input[["alt"]] == "5d. Skapa marknadsplatser utomhus") {
      return(as.matrix(group2Filter1()@data[, 33]))
    }
    if (input[["alt"]] == "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt") {
      return(as.matrix(group2Filter1()@data[, 34]))
    }
    #     # theme 6
    if (input[["alt"]] == "6a. Rusta upp äldre skolor") {
      return(as.matrix(group2Filter1()@data[, 35]))
    }
    if (input[["alt"]] == "6b. Bygg nya skolor") {
      return(as.matrix(group2Filter1()@data[, 36]))
    }
    if (input[["alt"]] == "6c. Förbättra skolgårdarnas fysiska miljöer") {
      return(as.matrix(group2Filter1()@data[, 37]))
    }
    if (input[["alt"]] == "6d. Höj kvaliteten i grundskolan") {
      return(as.matrix(group2Filter1()@data[, 38]))
    }
    if (input[["alt"]] == "6e. Höj kvaliteten på gymnasieutbildningarna") {
      return(as.matrix(group2Filter1()@data[, 39]))
    }
    #     # theme 7
    if (input[["alt"]] == "7a. Fler kultur- och fritidsaktiviteter för äldre") {
      return(as.matrix(group2Filter1()@data[, 40]))
    }
    if (input[["alt"]] == "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar") {
      return(as.matrix(group2Filter1()@data[, 41]))
    }
    if (input[["alt"]] == "7c. Förbättra äldreomsorgen i kommunen") {
      return(as.matrix(group2Filter1()@data[, 42]))
    }
    if (input[["alt"]] == "7d. Fler ungdomsgårdar och fältassistenter") {
      return(as.matrix(group2Filter1()@data[, 43]))
    }
    if (input[["alt"]] == "7e. Minska barngrupperna i förskolan") {
      return(as.matrix(group2Filter1()@data[, 44]))
    }
    #     # theme 8
    if (input[["alt"]] == "8a. Mindre barngrupper i förskolan") {
      return(as.matrix(group2Filter1()@data[, 45]))
    }
    if (input[["alt"]] == "8b. Höj kvaliteten i undervisningen") {
      return(as.matrix(group2Filter1()@data[, 46]))
    }
    if (input[["alt"]] == "8c. Mer kompetensutveckling för skolor och lärare") {
      return(as.matrix(group2Filter1()@data[, 47]))
    }
    if (input[["alt"]] == "8d. Mer modern informationsteknologi (IT) i undervisningen") {
      return(as.matrix(group2Filter1()@data[, 48]))
    }
    if (input[["alt"]] == "8e. Involvera vårdnadshavare mer i skolan") {
      return(as.matrix(group2Filter1()@data[, 49]))
    }
    #     # theme 9
    if (input[["alt"]] == "9a. Öka tryggheten kring stationsområdet") {
      return(as.matrix(group2Filter1()@data[, 50]))
    }
    if (input[["alt"]] == "9b. Fler poliser i centrala Väsby") {
      return(as.matrix(group2Filter1()@data[, 51]))
    }
    if (input[["alt"]] == "9c. Förbättra belysningen i centrala Väsby") {
      return(as.matrix(group2Filter1()@data[, 52]))
    }
    if (input[["alt"]] == "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby") {
      return(as.matrix(group2Filter1()@data[, 53]))
    }
    if (input[["alt"]] == "9e. Förläng öppettider för affärsverksamhet i centrala Väsby") {
      return(as.matrix(group2Filter1()@data[, 54]))
    }
    #     # theme 10
    if (input[["alt"]] == "10a. Minska förbrukningen av energi") {
      return(as.matrix(group2Filter1()@data[, 55]))
    }
    if (input[["alt"]] == "10b. Minska transporter och buller") {
      return(as.matrix(group2Filter1()@data[, 56]))
    }
    if (input[["alt"]] == "10c. Öka klimatanpassning och kretsloppstänkande") {
      return(as.matrix(group2Filter1()@data[, 57]))
    }
    if (input[["alt"]] == "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)") {
      return(as.matrix(group2Filter1()@data[, 58]))
    }
    if (input[["alt"]] == "10e. Minska miljögifter och farliga kemikalier i naturen") {
      return(as.matrix(group2Filter1()@data[, 59]))
    }
  })
  
  # calculate mean
  group2Mean <- reactive({
    mean2 <- round(mean(group2Filter2()))
  })
  
  # subset: theme weights
  group2Filter3 <- reactive({
    if (input[["themes"]] == "1. Parker & grönområden") {
      return(as.matrix(group2Filter1()@data[, 60]))
    }
    if (input[["themes"]] == "2. Mångfald i bostadsutbudet") {
      return(as.matrix(group2Filter1()@data[, 61]))
    }
    if (input[["themes"]] == "3. Levandegöra gemensamma platser") {
      return(as.matrix(group2Filter1()@data[, 62]))
    }
    if (input[["themes"]] == "4. Kommunikationer") {
      return(as.matrix(group2Filter1()@data[, 63]))
    }
    if (input[["themes"]] == "5. Kultur & fritid") {
      return(as.matrix(group2Filter1()@data[, 64]))
    }
    if (input[["themes"]] == "6. Utbildning") {
      return(as.matrix(group2Filter1()@data[, 65]))
    }
    if (input[["themes"]] == "7. Omsorg") {
      return(as.matrix(group2Filter1()@data[, 66]))
    }
    if (input[["themes"]] == "8. Skolan") {
      return(as.matrix(group2Filter1()@data[, 67]))
    }
    if (input[["themes"]] == "9. Trygghet") {
      return(as.matrix(group2Filter1()@data[, 68]))
    }
    if (input[["themes"]] == "10. Hållbar utveckling") {
      return(as.matrix(group2Filter1()@data[, 69]))
    }
  })
  
  # render theme weight slider
#   output[["weights2"]] <- renderUI({
#     if (!is.null(input[["alt"]]) && !is.null(input[["area2"]]) && !is.null(input[["sex2"]]) && !is.null(input[["age2"]]) && !is.null(input[["occupation2"]]) && !is.null(input[["education2"]]) && !is.null(input[["years2"]])) {
#       min2 <- min(group2Filter3())
#       max2 <- max(group2Filter3())
#       sliderInput(inputId = "weights2", label = h5("Vikt tema"), min = min2, max = max2, value = c(min2, max2), step = 1, ticks = TRUE, dragRange = TRUE)
#     }
#   })
  
  ### DYNAMIC MAP ELEMENTS ####################################
  
  # NULL checks
  nullChecks1 <- reactive({
    if (is.null(input[["alt"]]) || is.null(input[["area1"]]) || is.null(input[["sex1"]]) || is.null(input[["age1"]]) || is.null(input[["occupation1"]]) || is.null(input[["education1"]]) || is.null(input[["years1"]])) {
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  nullChecks2 <- reactive({
    if (is.null(input[["alt"]]) || is.null(input[["area2"]]) || is.null(input[["sex2"]]) || is.null(input[["age2"]]) || is.null(input[["occupation2"]]) || is.null(input[["education2"]]) || is.null(input[["years2"]])) {
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  # polygons
  observe({
    leafletProxy(mapId = "map") %>% 
      clearGroup(group = "group1Polygons") %>% 
      clearGroup(group = "group2Polygons")
    if (!is.null(nullChecks1())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(data = group1Filter1(), fill = TRUE, fillColor = ~ colorpal()(group1Mean()), fillOpacity = 0.8, stroke = TRUE, weight = 2, color = "red", layerId = group1Filter1()[["Omrade"]], group = "group1Polygons")
    }
    if (!is.null(nullChecks2())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(data = group2Filter1(), fill = TRUE, fillColor = ~ colorpal()(group2Mean()), fillOpacity = 0.8, stroke = TRUE, weight = 2, color = "blue", layerId = group2Filter1()[["Omrade"]], group = "group2Polygons")
    }
  })
  
  # polygons group 1
#   observe({
#     if (!is.null(nullChecks1())) {
#       leafletProxy(mapId = "map") %>%
#         clearGroup(group = "group1Polygons") %>% 
#         addPolygons(data = group1Filter1(), fill = TRUE, fillColor = ~ colorpal()(group1Mean()), fillOpacity = 0.8, stroke = TRUE, weight = 2, color = "red", layerId = group1Filter1()[["Omrade"]], group = "group1Polygons") 
#     } else {
#       leafletProxy(mapId = "map") %>% 
#         clearGroup(group = "group1Polygons")
#     }
# })
#   
#   # polygons group 2
#   observe({
#     if (!is.null(nullChecks2())) {
#       leafletProxy(mapId = "map") %>%
#         clearGroup(group = "group2Polygons") %>% 
#         addPolygons(data = group2Filter1(), fill = TRUE, fillColor = ~ colorpal()(group2Mean()), fillOpacity = 0.8, stroke = TRUE, weight = 2, color = "blue", layerId = group2Filter1()[["Omrade"]], group = "group2Polygons")
#     } else {
#       leafletProxy(mapId = "map") %>% 
#         clearGroup(group = "group2Polygons")
#     }
#   })
  
  # legend
  observe({
    leafletProxy(mapId = "map") %>% 
      clearControls() %>% 
      addLegend(position = "bottomleft", pal = colorpal(), values = c(0: 14))
  })
  
  # popups
  observe({
    leafletProxy(mapId = "map") %>% 
      clearPopups()
    if (!is.null(nullChecks1())) {
      if (input[["pop1"]]) {
        leafletProxy(mapId = "map") %>% 
          addPopups(data = group1Filter1(), lng = ~ long, lat = ~ lat, popup = group1Filter1()[["Omrade"]], layerId = group1Filter1()[["Omrade"]])
      }
    }
    if (!is.null(nullChecks2())) {
      if (input[["pop2"]]) {
        leafletProxy(mapId = "map") %>% 
          addPopups(data = group2Filter1(), lng = ~ long, lat = ~ lat, popup = group2Filter1()[["Omrade"]], layerId = group2Filter1()[["Omrade"]])
      }
    }
  })
  
  # markers
  observe({
    if (!is.null(nullChecks1())) {
      leafletProxy(mapId = "map") %>% 
        clearMarkers()
      if (input[["markers1"]]) {
        leafletProxy(mapId = "map") %>% 
          addMarkers(data = group1Filter1(), lng = ~ long, lat = ~ lat, popup = group1Filter1()[["Omrade"]], layerId = group1Filter1()[["Omrade"]])
      }
    }
    if (!is.null(nullChecks2())) {  
      if (input[["markers2"]]) {
        leafletProxy(mapId = "map") %>% 
          addMarkers(data = group2Filter1(), lng = ~ long, lat = ~ lat, popup = group2Filter1()[["Omrade"]], layerId = group2Filter1()[["Omrade"]])
      }
    }
  })
  
  ### DATA TABLE ##############################################
  
  # subset: background variables
  tableFilter <- reactive({
    if (input[["area"]] != "Alla" || is.null(input[["area"]]))
      for (o in 1:length(input[["area"]])) { 
        results_df <- head(results_df[results_df[["Omrade"]] %in% input[["area"]], ], n = 1040, drop = FALSE)
      }
    if (input[["sex"]] != "Alla" || is.null(input[["sex"]])) 
      for (p in 1:length(input[["sex"]])) { 
        results_df <- head(results_df[results_df[["Kön"]] %in% input[["sex"]], ], n = 1040, drop = FALSE)
      }
    if (input[["age"]] != "Alla" || is.null(input[["age"]])) 
      for (q in 1:length(input[["age"]])) { 
        results_df <- head(results_df[results_df[["Ålder"]] %in% input[["age"]], ], n = 1040, drop = FALSE)
      }
    if (input[["occupation"]] != "Alla" || is.null(input[["occupation"]])) 
      for (r in 1:length(input[["occupation"]])) { 
        results_df <- head(results_df[results_df[["Sysselsättning"]] %in% input[["occupation"]], ], n = 1040, drop = FALSE)
      }
    if (input[["education"]] != "Alla" || is.null(input[["education"]])) 
      for (s in 1:length(input[["education"]])) { 
        results_df <- head(results_df[results_df[["Utbildningsnivå"]] %in% input[["education"]], ], n = 1040, drop = FALSE)
      }
    if (input[["years"]] != "Alla" || is.null(input[["years"]])) 
      for (t in 1:length(input[["years"]])) { 
        results_df <- head(results_df[results_df[["År"]] %in% input[["years"]], ], n = 1040, drop = FALSE)
      }
    results_df
  })
  
  output$table <- DT::renderDataTable({
    tableFilter()
  }, server = TRUE)
}

# shinyApp ----
shinyApp(ui = ui, server = server)