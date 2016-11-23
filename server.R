shinyServer(function(input, output, session) {

  # Create static map
  output[["map"]] <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 17.91128,
              lat = 59.51839,
              zoom = 12) %>%
      addPolygons(
        data = nyko,
        fill = TRUE,
        fillOpacity = 0.1,
        fillColor = "black",
        stroke = TRUE,
        weight = 1,
        color = "black",
        group = "nyko"
      )
  })

  # Render alternatives menu
  output[["alternatives"]] <- renderUI({
    if (input[["themes"]] == "1. Parker & grönområden") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme1,
      ))
    }
    if (input[["themes"]] == "2. Mångfald i bostadsutbudet") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme2,
      ))
    }
    if (input[["themes"]] == "3. Levandegöra gemensamma platser") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme3,
      ))
    }
    if (input[["themes"]] == "4. Kommunikationer") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme4,
      ))
    }
    if (input[["themes"]] == "5. Kultur & fritid") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme5,
      ))
    }
    if (input[["themes"]] == "6. Utbildning") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme6,
      ))
    }
    if (input[["themes"]] == "7. Omsorg") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme7,
      ))
    }
    if (input[["themes"]] == "8. Skolan") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme8,
      ))
    }
    if (input[["themes"]] == "9. Trygghet") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme9,
      ))
    }
    if (input[["themes"]] == "10. Hållbar utveckling") {
      return(selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = altTheme10,
      ))
    }
  })

  ### GROUP 1 FILTERS AND CALCULATIONS ########################


  #Filters given spdf by reading numbered group input ids
  #Same as below: relies on unique input values per column in dataframe
  bgFilter <- function(groupNumber, spdf){
    for (i in seq_along(uiNamesBg1)){

      col <- paste(uiNamesBg1[i], groupNumber, sep="") #e.g. "area1"
      dfCol <- getCategory(input[[col]]) #e.g. "area1" -> "Omrade"


      if (input[[col]] != "Alla" && input[[col]] != "") {
        #browser()
        for (i in seq_along(input[[col]])) {
          spdf <-
            results_spdf1[spdf[[dfCol]] %in% input[[col]],]
        }
      }
    }
    return(spdf)
  }

  group1Filter1 <- reactive({
    bgFilter(1, results_spdf1)
    })

  #More basic solution using uiNamesBg1 (hardcoded) to eliminate ifs
  #Works!
  #Drawback: relies on unique input values per column
  group1Filter1old3working <- reactive({

    for (i in seq_along(uiNamesBg1)){

      col <- uiNamesBg1[i]
      dfCol <- getCategory(input[[col]]) #Gets name of column in results_df containing the input value


      if (input[[col]] != "Alla" && input[[col]] != "") {
        #browser()
        for (i in seq_along(input[[col]])) {
          results_spdf1 <-
            results_spdf1[results_spdf1[[dfCol]] %in% input[[col]],]
        }
      }
    }
    results_spdf1


    })

  #New test formula
  group1Filter1old2 <- reactive({


    ##TODO: Handle filtering dynamically

    #For every column name in input (area1, sex2 etc.)
    for (i in seq_along(names(input))){

      #Example: "area1"
      column <- names(input)[[i]]

      #Example: input[["area1"]]
      value <- input[[column]]

      allowed <- c("area1", "sex1", "age1", "occupation1", "education1", "years1")

      inputFilter = any(column == allowed)

      #Debug part
      if (is.na(value != "Alla" && inputFilter)) browser()

      if (value != "Alla" && inputFilter){

        if (typeof(value) != "character") browser()

        resultsColumn <- getCategory(value)

        #Temporary breakpoint
        if (resultsColumn == "404"){
          browser()
        }

        for (j in seq_along(value)){
            results_spdf1 <- results_spdf1[results_spdf1[[resultsColumn]] %in% value,]

        }

      }

    }

    results_spdf1
    })

  # Subset background variables
  group1Filter1old1 <- reactive({
    if (input[["area1"]] != "Alla") {
      for (i in seq_along(input[["area1"]])) {
        results_spdf1 <-
          results_spdf1[results_spdf1[["Omrade"]] %in% input[["area1"]],]
      }
    }

    if (input[["sex1"]] != "Alla") {
      for (i in seq_along(input[["sex1"]])) {
        results_spdf1 <-
          results_spdf1[results_spdf1[["Kön"]] %in% input[["sex1"]],]
      }
    }
    if (input[["age1"]] != "Alla") {
      for (k in seq_along(input[["age1"]])) {
        results_spdf1 <-
          results_spdf1[results_spdf1[["Ålder"]] %in% input[["age1"]],]
      }
    }
    if (input[["occupation1"]] != "Alla") {
      for (l in seq_along(input[["occupation1"]])) {
        results_spdf1 <-
          results_spdf1[results_spdf1[["Sysselsättning"]] %in% input[["occupation1"]],]
      }
    }
    if (input[["education1"]] != "Alla") {
      for (m in seq_along(input[["education1"]])) {
        results_spdf1 <-
          results_spdf1[results_spdf1[["Utbildningsnivå"]] %in% input[["education1"]],]
      }
    }
    if (input[["years1"]] != "Alla") {
      for (n in seq_along(input[["years1"]])) {
        results_spdf1 <-
          results_spdf1[results_spdf1[["År"]] %in% input[["years1"]],]
      }
    }
    results_spdf1
  })

  # Subset theme alternatives
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

  # Calculate mean
  group1Mean <- reactive({
    mean1 <- round(mean(group1Filter2()))
  })

  ### GROUP 2 FILTERS AND CALCULATIONS ########################

  group2Filter1 <- reactive({
    bgFilter(2, results_spdf2)
    })

  # Subset background variables
  group2Filter1old <- reactive({
    if (input[["area2"]] != "Alla") {
      browser()
      for (a in seq_along(input[["area2"]])) {
        results_spdf2 <-
          results_spdf2[results_spdf2[["Omrade"]] %in% input[["area2"]],]
      }
    }
    if (input[["sex2"]] != "Alla") {
      for (b in seq_along(input[["sex2"]])) {
        results_spdf2 <-
          results_spdf2[results_spdf2[["Kön"]] %in% input[["sex2"]],]
      }
    }
    if (input[["age2"]] != "Alla") {
      for (c in seq_along(input[["age2"]])) {
        results_spdf2 <-
          results_spdf2[results_spdf2[["Ålder"]] %in% input[["age2"]],]
      }
    }
    if (input[["occupation2"]] != "Alla") {
      for (d in seq_along(input[["occupation2"]])) {
        results_spdf2 <-
          results_spdf2[results_spdf2[["Sysselsättning"]] %in% input[["occupation2"]],]
      }
    }
    if (input[["education2"]] != "Alla") {
      for (e in seq_along(input[["education2"]])) {
        results_spdf2 <-
          results_spdf2[results_spdf2[["Utbildningsnivå"]] %in% input[["education2"]],]
      }
    }
    if (input[["years2"]] != "Alla") {
      for (f in seq_along(input[["years2"]])) {
        results_spdf2 <-
          results_spdf2[results_spdf2[["År"]] %in% input[["years2"]],]
      }
    }
    results_spdf2
  })

  # Subset theme alternatives
  group2Filter2 <- reactive({
    # Theme 1
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
    # Theme 2
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
    # Theme 3
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
    # Theme 4
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
    # Theme 5
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
    # Theme 6
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
    # Theme 7
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
    # Theme 8
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
    # Theme 9
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
    # Theme 10
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

  # Calculate mean
  group2Mean <- reactive({
    mean2 <- round(mean(group2Filter2()))
  })

  ### DYNAMIC MAP ELEMENTS ####################################

  # NULL checks
  nullChecks1 <- reactive({
    if (is.null(input[["alt"]]) ||
        is.null(input[["area1"]]) ||
        is.null(input[["sex1"]]) ||
        is.null(input[["age1"]]) ||
        is.null(input[["occupation1"]]) ||
        is.null(input[["education1"]]) ||
        is.null(input[["years1"]])) {
      return(NULL)
    } else {
      return(TRUE)
    }
  })

  nullChecks2 <- reactive({
    if (is.null(input[["alt"]]) ||
        is.null(input[["area2"]]) ||
        is.null(input[["sex2"]]) ||
        is.null(input[["age2"]]) ||
        is.null(input[["occupation2"]]) ||
        is.null(input[["education2"]]) ||
        is.null(input[["years2"]])) {
      return(NULL)
    } else {
      return(TRUE)
    }
  })

  # Create polygons
  observe({
    leafletProxy(mapId = "map") %>%
      clearGroup(group = "group1Polygons") %>%
      clearGroup(group = "group2Polygons")
    if (!is.null(nullChecks1())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(
          data = group1Filter1(),
          fill = TRUE,
          fillColor = ~ colorpal()(group1Mean()),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          color = "red",
          layerId = group1Filter1()[["Omrade"]],
          group = "group1Polygons"
        )
    }
    if (!is.null(nullChecks2())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(
          data = group2Filter1(),
          fill = TRUE,
          fillColor = ~ colorpal()(group2Mean()),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          color = "blue",
          layerId = group2Filter1()[["Omrade"]],
          group = "group2Polygons"
        )
    }
  })

  # Create color palette
  colorpal <- reactive({
    colorNumeric(palette = input[["colors"]],
                 domain = c(0, 14),
                 na.color = "gray")
  })

  # Add color legend to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearControls() %>%
      addLegend(
        position = "bottomleft",
        pal = colorpal(),
        values = c(0:14),
        labels = c("Red", "Yellow", "Green")
      )
  })

  # Add popups to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearPopups()
    if (!is.null(nullChecks1())) {
      if (input[["pop1"]]) {
        leafletProxy(mapId = "map") %>%
          addPopups(
            data = group1Filter1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group1Filter1()[["Omrade"]],
            layerId = group1Filter1()[["Omrade"]]
          )
      }
    }
    if (!is.null(nullChecks2())) {
      if (input[["pop2"]]) {
        leafletProxy(mapId = "map") %>%
          addPopups(
            data = group2Filter1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group2Filter1()[["Omrade"]],
            layerId = group2Filter1()[["Omrade"]]
          )
      }
    }
  })

  # Add markers to map
  observe({
    if (!is.null(nullChecks1())) {
      leafletProxy(mapId = "map") %>%
        clearMarkers()
      if (input[["markers1"]]) {
        leafletProxy(mapId = "map") %>%
          addMarkers(
            data = group1Filter1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group1Filter1()[["Omrade"]],
            layerId = group1Filter1()[["Omrade"]]
          )
      }
    }
    if (!is.null(nullChecks2())) {
      if (input[["markers2"]]) {
        leafletProxy(mapId = "map") %>%
          addMarkers(
            data = group2Filter1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group2Filter1()[["Omrade"]],
            layerId = group2Filter1()[["Omrade"]]
          )
      }
    }
  })

  ### DATA TABLE ##############################################

  # Subset: background variables
  tableFilter <- reactive({
    if (input[["area"]] != "Alla" || is.null(input[["area"]]))
      for (o in seq_along(input[["area"]])) {
        results_df <-
          head(results_df[results_df[["Omrade"]] %in% input[["area"]],], n = 1040, drop = FALSE)
      }
    if (input[["sex"]] != "Alla" || is.null(input[["sex"]]))
      for (p in seq_along(input[["sex"]])) {
        results_df <-
          head(results_df[results_df[["Kön"]] %in% input[["sex"]],], n = 1040, drop = FALSE)
      }
    if (input[["age"]] != "Alla" || is.null(input[["age"]]))
      for (q in seq_along(input[["age"]])) {
        results_df <-
          head(results_df[results_df[["Ålder"]] %in% input[["age"]],], n = 1040, drop = FALSE)
      }
    if (input[["occupation"]] != "Alla" ||
        is.null(input[["occupation"]]))
      for (r in seq_along(input[["occupation"]])) {
        results_df <-
          head(results_df[results_df[["Sysselsättning"]] %in% input[["occupation"]],], n = 1040, drop = FALSE)
      }
    if (input[["education"]] != "Alla" ||
        is.null(input[["education"]]))
      for (s in seq_along(input[["education"]])) {
        results_df <-
          head(results_df[results_df[["Utbildningsnivå"]] %in% input[["education"]],], n = 1040, drop = FALSE)
      }
    if (input[["years"]] != "Alla" || is.null(input[["years"]]))
      for (t in seq_along(input[["years"]])) {
        results_df <-
          head(results_df[results_df[["År"]] %in% input[["years"]],], n = 1040, drop = FALSE)
      }
    results_df
  })

  output[["table"]] <- DT::renderDataTable({
    tableFilter()
  }, server = TRUE)
})
