shinyServer(function(input, output, session) {
  # Returns the name of the column in results_df where the data value exists
  # Currently not used
  # get_category_unique <- function(entry_value) {
  #  a <- which(sapply(results_df, function(x)
  #    any(x == entry_value)))
  #  a <- names(a)
  #  # Null handling
  #  noVal <- character(0)
  #  if (identical(a, character(0))) {
  #    return("404")
  #  }
  #  return(a)
  # }
  
  # Returns corresponding dataframe column name of an input column name
  get_input_category <- function(ui_col_name) {
    # Error handling
    if (!is.element(ui_col_name, names(df_names_bg))) {
      return("404")
    }
    a <- df_names_bg[[ui_col_name]]
    # Null handling
    no_val <- character(0)
    if (identical(a, no_val)) {
      return("404")
    }
    return(a)
  }
  
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
    switch(
      input$themes,
      "1. Parker & grönområden" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[1]]
      ),
      "2. Mångfald i bostadsutbudet" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[2]]
      ),
      "3. Levandegöra gemensamma platser" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[3]]
      ),
      "4. Kommunikationer" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[4]]
      ),
      "5. Kultur & fritid" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[5]]
      ),
      "6. Utbildning" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[6]]
      ),
      "7. Omsorg" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[7]]
      ),
      "8. Skolan" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[8]]
      ),
      "9. Trygghet" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[9]]
      ),
      "10. Hållbar utveckling" = selectInput(
        inputId = "alt",
        label = h5("Alternativ"),
        choices = alt_list[[10]]
      )
    )
  })
  
  ### GROUP 1 FILTERS AND CALCULATIONS ########################
  
  # Filters given spdf by reading numbered group input ids
  bg_filter <- function(group_number, spdf) {
    for (i in seq_along(ui_names_bg)) {
      col <- paste(ui_names_bg[i], group_number, sep = "") # E.g. "area1"
      df_col <-
        get_input_category(substr(col, 1, nchar(col) - 1)) # E.g. "area1" -> "area" -> "Omrade"
      # Do not filter non-selection
      if (input[[col]] != "Alla") {
        for (i in seq_along(input[[col]])) {
          # Get rows in spdf where value matches input selection
          spdf <- spdf[spdf[[df_col]] %in% input[[col]],]
        }
      }
    }
    return(spdf)
  }
  
  # Subset background variables
  group_1_filter_1 <- reactive({
    bg_filter(1, results_spdf1)
  })
  
  # Subset theme alternatives
  group_1_filter_2 <- reactive({
    # Theme 1
    if (input[["alt"]] == "1a. Bevara existerande större grönområden") {
      return(as.matrix(group_1_filter_1()@data[, 10]))
    }
    if (input[["alt"]] == "1b. Anlägga parker i existerande stadsdelar") {
      return(as.matrix(group_1_filter_1()@data[, 11]))
    }
    if (input[["alt"]] == "1c. Bygga bostäder nära grönområden") {
      return(as.matrix(group_1_filter_1()@data[, 12]))
    }
    if (input[["alt"]] == "1d. Rusta upp befintliga parker") {
      return(as.matrix(group_1_filter_1()@data[, 13]))
    }
    if (input[["alt"]] == "1e. Skapa bättre tillgänglighet till större grönområden") {
      return(as.matrix(group_1_filter_1()@data[, 14]))
    }
    # Theme 2
    if (input[["alt"]] == "2a. Erbjuda fler bostadstyper") {
      return(as.matrix(group_1_filter_1()@data[, 15]))
    }
    if (input[["alt"]] == "2b. Erbjuda fler lägenhetsstorlekar") {
      return(as.matrix(group_1_filter_1()@data[, 16]))
    }
    if (input[["alt"]] == "2c. Erbjuda småskaligt markägande") {
      return(as.matrix(group_1_filter_1()@data[, 17]))
    }
    if (input[["alt"]] == "2d. Bevara de idémässiga grunderna för bebyggelsen från 1970-talet") {
      return(as.matrix(group_1_filter_1()@data[, 18]))
    }
    if (input[["alt"]] == "2e. Erbjuda fler bostäder nära vatten") {
      return(as.matrix(group_1_filter_1()@data[, 19]))
    }
    # Theme 3
    if (input[["alt"]] == "3a. Blanda trafikslagen") {
      return(as.matrix(group_1_filter_1()@data[, 20]))
    }
    if (input[["alt"]] == "3b. Förlägga parkering längs med gator") {
      return(as.matrix(group_1_filter_1()@data[, 21]))
    }
    if (input[["alt"]] == "3c. Vända entréer mot gator") {
      return(as.matrix(group_1_filter_1()@data[, 22]))
    }
    if (input[["alt"]] == "3d. Förlägga publika lokaler i transparenta bottenvåningar") {
      return(as.matrix(group_1_filter_1()@data[, 23]))
    }
    if (input[["alt"]] == "3e. Trygga parkeringslösningar under bostäder") {
      return(as.matrix(group_1_filter_1()@data[, 24]))
    }
    # Theme 4
    if (input[["alt"]] == "4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör") {
      return(as.matrix(group_1_filter_1()@data[, 25]))
    }
    if (input[["alt"]] == "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen") {
      return(as.matrix(group_1_filter_1()@data[, 26]))
    }
    if (input[["alt"]] == "4c. Förbättra kommunikationerna till och från Uppsala") {
      return(as.matrix(group_1_filter_1()@data[, 27]))
    }
    if (input[["alt"]] == "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät") {
      return(as.matrix(group_1_filter_1()@data[, 28]))
    }
    if (input[["alt"]] == "4e. Förbättra kommunikationerna till och från Stockholms innerstad") {
      return(as.matrix(group_1_filter_1()@data[, 29]))
    }
    # Theme 5
    if (input[["alt"]] == "5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter") {
      return(as.matrix(group_1_filter_1()@data[, 30]))
    }
    if (input[["alt"]] == "5b. Skapa bättre möjligheter för festivaler och konserter") {
      return(as.matrix(group_1_filter_1()@data[, 31]))
    }
    if (input[["alt"]] == "5c. Skapa fler förutsättningar för utomhussporter") {
      return(as.matrix(group_1_filter_1()@data[, 32]))
    }
    if (input[["alt"]] == "5d. Skapa marknadsplatser utomhus") {
      return(as.matrix(group_1_filter_1()@data[, 33]))
    }
    if (input[["alt"]] == "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt") {
      return(as.matrix(group_1_filter_1()@data[, 34]))
    }
    # Theme 6
    if (input[["alt"]] == "6a. Rusta upp äldre skolor") {
      return(as.matrix(group_1_filter_1()@data[, 35]))
    }
    if (input[["alt"]] == "6b. Bygg nya skolor") {
      return(as.matrix(group_1_filter_1()@data[, 36]))
    }
    if (input[["alt"]] == "6c. Förbättra skolgårdarnas fysiska miljöer") {
      return(as.matrix(group_1_filter_1()@data[, 37]))
    }
    if (input[["alt"]] == "6d. Höj kvaliteten i grundskolan") {
      return(as.matrix(group_1_filter_1()@data[, 38]))
    }
    if (input[["alt"]] == "6e. Höj kvaliteten på gymnasieutbildningarna") {
      return(as.matrix(group_1_filter_1()@data[, 39]))
    }
    # Theme 7
    if (input[["alt"]] == "7a. Fler kultur- och fritidsaktiviteter för äldre") {
      return(as.matrix(group_1_filter_1()@data[, 40]))
    }
    if (input[["alt"]] == "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar") {
      return(as.matrix(group_1_filter_1()@data[, 41]))
    }
    if (input[["alt"]] == "7c. Förbättra äldreomsorgen i kommunen") {
      return(as.matrix(group_1_filter_1()@data[, 42]))
    }
    if (input[["alt"]] == "7d. Fler ungdomsgårdar och fältassistenter") {
      return(as.matrix(group_1_filter_1()@data[, 43]))
    }
    if (input[["alt"]] == "7e. Minska barngrupperna i förskolan") {
      return(as.matrix(group_1_filter_1()@data[, 44]))
    }
    # Theme 8
    if (input[["alt"]] == "8a. Mindre barngrupper i förskolan") {
      return(as.matrix(group_1_filter_1()@data[, 45]))
    }
    if (input[["alt"]] == "8b. Höj kvaliteten i undervisningen") {
      return(as.matrix(group_1_filter_1()@data[, 46]))
    }
    if (input[["alt"]] == "8c. Mer kompetensutveckling för skolor och lärare") {
      return(as.matrix(group_1_filter_1()@data[, 47]))
    }
    if (input[["alt"]] == "8d. Mer modern informationsteknologi (IT) i undervisningen") {
      return(as.matrix(group_1_filter_1()@data[, 48]))
    }
    if (input[["alt"]] == "8e. Involvera vårdnadshavare mer i skolan") {
      return(as.matrix(group_1_filter_1()@data[, 49]))
    }
    # Theme 9
    if (input[["alt"]] == "9a. Öka tryggheten kring stationsområdet") {
      return(as.matrix(group_1_filter_1()@data[, 50]))
    }
    if (input[["alt"]] == "9b. Fler poliser i centrala Väsby") {
      return(as.matrix(group_1_filter_1()@data[, 51]))
    }
    if (input[["alt"]] == "9c. Förbättra belysningen i centrala Väsby") {
      return(as.matrix(group_1_filter_1()@data[, 52]))
    }
    if (input[["alt"]] == "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby") {
      return(as.matrix(group_1_filter_1()@data[, 53]))
    }
    if (input[["alt"]] == "9e. Förläng öppettider för affärsverksamhet i centrala Väsby") {
      return(as.matrix(group_1_filter_1()@data[, 54]))
    }
    # Theme 10
    if (input[["alt"]] == "10a. Minska förbrukningen av energi") {
      return(as.matrix(group_1_filter_1()@data[, 55]))
    }
    if (input[["alt"]] == "10b. Minska transporter och buller") {
      return(as.matrix(group_1_filter_1()@data[, 56]))
    }
    if (input[["alt"]] == "10c. Öka klimatanpassning och kretsloppstänkande") {
      return(as.matrix(group_1_filter_1()@data[, 57]))
    }
    if (input[["alt"]] == "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)") {
      return(as.matrix(group_1_filter_1()@data[, 58]))
    }
    if (input[["alt"]] == "10e. Minska miljögifter och farliga kemikalier i naturen") {
      return(as.matrix(group_1_filter_1()@data[, 59]))
    }
  })
  
  # Calculate mean
  group_1_mean <- reactive({
    mean1 <- round(mean(group_1_filter_2()))
  })
  
  ### GROUP 2 FILTERS AND CALCULATIONS ########################
  
  group_2_filter_1 <- reactive({
    bg_filter(2, results_spdf2)
  })
  
  # Subset theme alternatives
  group_2_filter_2 <- reactive({
    # Theme 1
    if (input[["alt"]] == "1a. Bevara existerande större grönområden") {
      return(as.matrix(group_2_filter_1()@data[, 10]))
    }
    if (input[["alt"]] == "1b. Anlägga parker i existerande stadsdelar") {
      return(as.matrix(group_2_filter_1()@data[, 11]))
    }
    if (input[["alt"]] == "1c. Bygga bostäder nära grönområden") {
      return(as.matrix(group_2_filter_1()@data[, 12]))
    }
    if (input[["alt"]] == "1d. Rusta upp befintliga parker") {
      return(as.matrix(group_2_filter_1()@data[, 13]))
    }
    if (input[["alt"]] == "1e. Skapa bättre tillgänglighet till större grönområden") {
      return(as.matrix(group_2_filter_1()@data[, 14]))
    }
    # Theme 2
    if (input[["alt"]] == "2a. Erbjuda fler bostadstyper") {
      return(as.matrix(group_2_filter_1()@data[, 15]))
    }
    if (input[["alt"]] == "2b. Erbjuda fler lägenhetsstorlekar") {
      return(as.matrix(group_2_filter_1()@data[, 16]))
    }
    if (input[["alt"]] == "2c. Erbjuda småskaligt markägande") {
      return(as.matrix(group_2_filter_1()@data[, 17]))
    }
    if (input[["alt"]] == "2d. Bevara de idémässiga grunderna för bebyggelsen från 1970-talet") {
      return(as.matrix(group_2_filter_1()@data[, 18]))
    }
    if (input[["alt"]] == "2e. Erbjuda fler bostäder nära vatten") {
      return(as.matrix(group_2_filter_1()@data[, 19]))
    }
    # Theme 3
    if (input[["alt"]] == "3a. Blanda trafikslagen") {
      return(as.matrix(group_2_filter_1()@data[, 20]))
    }
    if (input[["alt"]] == "3b. Förlägga parkering längs med gator") {
      return(as.matrix(group_2_filter_1()@data[, 21]))
    }
    if (input[["alt"]] == "3c. Vända entréer mot gator") {
      return(as.matrix(group_2_filter_1()@data[, 22]))
    }
    if (input[["alt"]] == "3d. Förlägga publika lokaler i transparenta bottenvåningar") {
      return(as.matrix(group_2_filter_1()@data[, 23]))
    }
    if (input[["alt"]] == "3e. Trygga parkeringslösningar under bostäder") {
      return(as.matrix(group_2_filter_1()@data[, 24]))
    }
    # Theme 4
    if (input[["alt"]] == "4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör") {
      return(as.matrix(group_2_filter_1()@data[, 25]))
    }
    if (input[["alt"]] == "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen") {
      return(as.matrix(group_2_filter_1()@data[, 26]))
    }
    if (input[["alt"]] == "4c. Förbättra kommunikationerna till och från Uppsala") {
      return(as.matrix(group_2_filter_1()@data[, 27]))
    }
    if (input[["alt"]] == "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät") {
      return(as.matrix(group_2_filter_1()@data[, 28]))
    }
    if (input[["alt"]] == "4e. Förbättra kommunikationerna till och från Stockholms innerstad") {
      return(as.matrix(group_2_filter_1()@data[, 29]))
    }
    # Theme 5
    if (input[["alt"]] == "5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter") {
      return(as.matrix(group_2_filter_1()@data[, 30]))
    }
    if (input[["alt"]] == "5b. Skapa bättre möjligheter för festivaler och konserter") {
      return(as.matrix(group_2_filter_1()@data[, 31]))
    }
    if (input[["alt"]] == "5c. Skapa fler förutsättningar för utomhussporter") {
      return(as.matrix(group_2_filter_1()@data[, 32]))
    }
    if (input[["alt"]] == "5d. Skapa marknadsplatser utomhus") {
      return(as.matrix(group_2_filter_1()@data[, 33]))
    }
    if (input[["alt"]] == "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt") {
      return(as.matrix(group_2_filter_1()@data[, 34]))
    }
    # Theme 6
    if (input[["alt"]] == "6a. Rusta upp äldre skolor") {
      return(as.matrix(group_2_filter_1()@data[, 35]))
    }
    if (input[["alt"]] == "6b. Bygg nya skolor") {
      return(as.matrix(group_2_filter_1()@data[, 36]))
    }
    if (input[["alt"]] == "6c. Förbättra skolgårdarnas fysiska miljöer") {
      return(as.matrix(group_2_filter_1()@data[, 37]))
    }
    if (input[["alt"]] == "6d. Höj kvaliteten i grundskolan") {
      return(as.matrix(group_2_filter_1()@data[, 38]))
    }
    if (input[["alt"]] == "6e. Höj kvaliteten på gymnasieutbildningarna") {
      return(as.matrix(group_2_filter_1()@data[, 39]))
    }
    # Theme 7
    if (input[["alt"]] == "7a. Fler kultur- och fritidsaktiviteter för äldre") {
      return(as.matrix(group_2_filter_1()@data[, 40]))
    }
    if (input[["alt"]] == "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar") {
      return(as.matrix(group_2_filter_1()@data[, 41]))
    }
    if (input[["alt"]] == "7c. Förbättra äldreomsorgen i kommunen") {
      return(as.matrix(group_2_filter_1()@data[, 42]))
    }
    if (input[["alt"]] == "7d. Fler ungdomsgårdar och fältassistenter") {
      return(as.matrix(group_2_filter_1()@data[, 43]))
    }
    if (input[["alt"]] == "7e. Minska barngrupperna i förskolan") {
      return(as.matrix(group_2_filter_1()@data[, 44]))
    }
    # Theme 8
    if (input[["alt"]] == "8a. Mindre barngrupper i förskolan") {
      return(as.matrix(group_2_filter_1()@data[, 45]))
    }
    if (input[["alt"]] == "8b. Höj kvaliteten i undervisningen") {
      return(as.matrix(group_2_filter_1()@data[, 46]))
    }
    if (input[["alt"]] == "8c. Mer kompetensutveckling för skolor och lärare") {
      return(as.matrix(group_2_filter_1()@data[, 47]))
    }
    if (input[["alt"]] == "8d. Mer modern informationsteknologi (IT) i undervisningen") {
      return(as.matrix(group_2_filter_1()@data[, 48]))
    }
    if (input[["alt"]] == "8e. Involvera vårdnadshavare mer i skolan") {
      return(as.matrix(group_2_filter_1()@data[, 49]))
    }
    # Theme 9
    if (input[["alt"]] == "9a. Öka tryggheten kring stationsområdet") {
      return(as.matrix(group_2_filter_1()@data[, 50]))
    }
    if (input[["alt"]] == "9b. Fler poliser i centrala Väsby") {
      return(as.matrix(group_2_filter_1()@data[, 51]))
    }
    if (input[["alt"]] == "9c. Förbättra belysningen i centrala Väsby") {
      return(as.matrix(group_2_filter_1()@data[, 52]))
    }
    if (input[["alt"]] == "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby") {
      return(as.matrix(group_2_filter_1()@data[, 53]))
    }
    if (input[["alt"]] == "9e. Förläng öppettider för affärsverksamhet i centrala Väsby") {
      return(as.matrix(group_2_filter_1()@data[, 54]))
    }
    # Theme 10
    if (input[["alt"]] == "10a. Minska förbrukningen av energi") {
      return(as.matrix(group_2_filter_1()@data[, 55]))
    }
    if (input[["alt"]] == "10b. Minska transporter och buller") {
      return(as.matrix(group_2_filter_1()@data[, 56]))
    }
    if (input[["alt"]] == "10c. Öka klimatanpassning och kretsloppstänkande") {
      return(as.matrix(group_2_filter_1()@data[, 57]))
    }
    if (input[["alt"]] == "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)") {
      return(as.matrix(group_2_filter_1()@data[, 58]))
    }
    if (input[["alt"]] == "10e. Minska miljögifter och farliga kemikalier i naturen") {
      return(as.matrix(group_2_filter_1()@data[, 59]))
    }
  })
  
  # Calculate mean
  group_2_mean <- reactive({
    mean2 <- round(mean(group_2_filter_2()))
  })
  
  ### DYNAMIC MAP ELEMENTS ####################################
  
  # NULL checks
  null_checks_1 <- reactive({
    return(null_check(1, ui_names_bg))
  })
  
  null_checks_2 <- reactive({
    return(null_check(2, ui_names_bg))
  })
  
  # Run a null check on all the input values of a given list of keys
  null_check <- function(input_group_number, ui_names_list) {
    # Check alt first as done in old null_check_n functions
    if (is.null(input[["alt"]])) {
      return(NULL)
    }
    for (i in seq_along(ui_names_list)) {
      input_key <-
        paste(ui_names_list[[i]], input_group_number, sep = "") # Add group number to key
      if (is.null(input[[input_key]])) {
        return(NULL)
      }
    }
    return(TRUE)
  }
  
  # Create polygons
  observe({
    leafletProxy(mapId = "map") %>%
      clearGroup(group = "group1Polygons") %>%
      clearGroup(group = "group2Polygons")
    if (!is.null(null_checks_1())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(
          data = group_1_filter_1(),
          fill = TRUE,
          fillColor = ~ colorpal()(group_1_mean()),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          color = "red",
          layerId = group_1_filter_1()[["Omrade"]],
          group = "group1Polygons"
        )
    }
    if (!is.null(null_checks_2())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(
          data = group_2_filter_1(),
          fill = TRUE,
          fillColor = ~ colorpal()(group_2_mean()),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          color = "blue",
          layerId = group_2_filter_1()[["Omrade"]],
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
    if (!is.null(null_checks_1())) {
      if (input[["pop1"]]) {
        leafletProxy(mapId = "map") %>%
          addPopups(
            data = group_1_filter_1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group_1_filter_1()[["Omrade"]],
            layerId = group_1_filter_1()[["Omrade"]]
          )
      }
    }
    if (!is.null(null_checks_2())) {
      if (input[["pop2"]]) {
        leafletProxy(mapId = "map") %>%
          addPopups(
            data = group_2_filter_1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group_2_filter_1()[["Omrade"]],
            layerId = group_2_filter_1()[["Omrade"]]
          )
      }
    }
  })
  
  # Add markers to map
  observe({
    if (!is.null(null_checks_1())) {
      leafletProxy(mapId = "map") %>%
        clearMarkers()
      if (input[["markers1"]]) {
        leafletProxy(mapId = "map") %>%
          addMarkers(
            data = group_1_filter_1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group_1_filter_1()[["Omrade"]],
            layerId = group_1_filter_1()[["Omrade"]]
          )
      }
    }
    if (!is.null(null_checks_2())) {
      if (input[["markers2"]]) {
        leafletProxy(mapId = "map") %>%
          addMarkers(
            data = group_2_filter_1(),
            lng = ~ long,
            lat = ~ lat,
            popup = group_2_filter_1()[["Omrade"]],
            layerId = group_2_filter_1()[["Omrade"]]
          )
      }
    }
  })
  
  ### DATA TABLE ##############################################
  
  # Subset background variables
  table_filter <- reactive({
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
    table_filter()
  }, server = TRUE)
})
