shinyServer(function(input, output, session) {
  
  # callModule(module = tabset_module, id = "one")
  
  output$alternatives <- renderUI({
    switch(
      input$theme,
      "1. Parks and green areas" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[1]]),
      "2. Diversity in housing supply" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[2]]),
      "3. Invest in public areas" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[3]]),
      "4. Communications" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[4]]),
      "5. Culture and leasure" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[5]]),
      "6. Education" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[6]]),
      "7. Care" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[7]]),
      "8. School" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[8]]),
      "9. Safety" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[9]]),
      "10. Ecological sustainability" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[10]])
    )
  })
  
  ####################################################################################################################
  
  ### ALL FILTERS ####################################################################################################
  
  ####################################################################################################################
  
  # return corresponding dataframe column name of an input column name
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
  
  # filter given spdf by reading numbered group input ids
  bg_filter <- function(group_number, spdf) {
    for (i in seq_along(ui_names_bg)) { # ui_names_bg <- c("area", "gender", "age", "occupation", "education", "years")
      col <- paste(ui_names_bg[i], group_number, sep = "") # E.g. "area1"
      df_col <- get_input_category(substr(col, 1, nchar(col) - 1)) # E.g. "area1" -> "area" -> "Omrade"
      # Do not filter non-selection
      if (input[[col]] != "All") {
        for (i in seq_along(input[[col]])) {
          # Get rows in spdf where value matches input selection
          spdf <- spdf[spdf[[df_col]] %in% input[[col]], ]
        }
      }
    }
    return(spdf)
  }
  
  ### GROUP 1 FILTERS ################################################
  
  # subset background variables
  group_1_filter_1 <- reactive({
    bg_filter(1, results_spdf1)
  })
  
  # subset theme alternatives
  group_1_filter_2 <- reactive({
    # theme 1
    if (input$alt == alt_theme_1[1]) {
      return(as.matrix(group_1_filter_1()@data[, 10]))
    }
    if (input$alt == alt_theme_1[2]) {
      return(as.matrix(group_1_filter_1()@data[, 11]))
    }
    if (input$alt == alt_theme_1[3]) {
      return(as.matrix(group_1_filter_1()@data[, 12]))
    }
    if (input$alt == alt_theme_1[4]) {
      return(as.matrix(group_1_filter_1()@data[, 13]))
    }
    if (input$alt == alt_theme_1[5]) {
      return(as.matrix(group_1_filter_1()@data[, 14]))
    }
    # theme 2
    if (input$alt == alt_theme_2[1]) {
      return(as.matrix(group_1_filter_1()@data[, 15]))
    }
    if (input$alt == alt_theme_2[2]) {
      return(as.matrix(group_1_filter_1()@data[, 16]))
    }
    if (input$alt == alt_theme_2[3]) {
      return(as.matrix(group_1_filter_1()@data[, 17]))
    }
    if (input$alt == alt_theme_2[4]) {
      return(as.matrix(group_1_filter_1()@data[, 18]))
    }
    if (input$alt == alt_theme_2[5]) {
      return(as.matrix(group_1_filter_1()@data[, 19]))
    }
    # theme 3
    if (input$alt == alt_theme_3[1]) {
      return(as.matrix(group_1_filter_1()@data[, 20]))
    }
    if (input$alt == alt_theme_3[2]) {
      return(as.matrix(group_1_filter_1()@data[, 21]))
    }
    if (input$alt == alt_theme_3[3]) {
      return(as.matrix(group_1_filter_1()@data[, 22]))
    }
    if (input$alt == alt_theme_3[4]) {
      return(as.matrix(group_1_filter_1()@data[, 23]))
    }
    if (input$alt == alt_theme_3[5]) {
      return(as.matrix(group_1_filter_1()@data[, 24]))
    }
    # theme 4
    if (input$alt == alt_theme_4[1]) {
      return(as.matrix(group_1_filter_1()@data[, 25]))
    }
    if (input$alt == alt_theme_4[2]) {
      return(as.matrix(group_1_filter_1()@data[, 26]))
    }
    if (input$alt == alt_theme_4[3]) {
      return(as.matrix(group_1_filter_1()@data[, 27]))
    }
    if (input$alt == alt_theme_4[4]) {
      return(as.matrix(group_1_filter_1()@data[, 28]))
    }
    if (input$alt == alt_theme_4[5]) {
      return(as.matrix(group_1_filter_1()@data[, 29]))
    }
    # theme 5
    if (input$alt == alt_theme_5[1]) {
      return(as.matrix(group_1_filter_1()@data[, 30]))
    }
    if (input$alt == alt_theme_5[2]) {
      return(as.matrix(group_1_filter_1()@data[, 31]))
    }
    if (input$alt == alt_theme_5[3]) {
      return(as.matrix(group_1_filter_1()@data[, 32]))
    }
    if (input$alt == alt_theme_5[4]) {
      return(as.matrix(group_1_filter_1()@data[, 33]))
    }
    if (input$alt == alt_theme_5[5]) {
      return(as.matrix(group_1_filter_1()@data[, 34]))
    }
    # theme 6
    if (input$alt == alt_theme_6[1]) {
      return(as.matrix(group_1_filter_1()@data[, 35]))
    }
    if (input$alt == alt_theme_6[2]) {
      return(as.matrix(group_1_filter_1()@data[, 36]))
    }
    if (input$alt == alt_theme_6[3]) {
      return(as.matrix(group_1_filter_1()@data[, 37]))
    }
    if (input$alt == alt_theme_6[4]) {
      return(as.matrix(group_1_filter_1()@data[, 38]))
    }
    if (input$alt == alt_theme_6[5]) {
      return(as.matrix(group_1_filter_1()@data[, 39]))
    }
    # theme 7
    if (input$alt == alt_theme_7[1]) {
      return(as.matrix(group_1_filter_1()@data[, 40]))
    }
    if (input$alt == alt_theme_7[2]) {
      return(as.matrix(group_1_filter_1()@data[, 41]))
    }
    if (input$alt == alt_theme_7[3]) {
      return(as.matrix(group_1_filter_1()@data[, 42]))
    }
    if (input$alt == alt_theme_7[4]) {
      return(as.matrix(group_1_filter_1()@data[, 43]))
    }
    if (input$alt == alt_theme_7[5]) {
      return(as.matrix(group_1_filter_1()@data[, 44]))
    }
    # theme 8
    if (input$alt == alt_theme_8[1]) {
      return(as.matrix(group_1_filter_1()@data[, 45]))
    }
    if (input$alt == alt_theme_8[2]) {
      return(as.matrix(group_1_filter_1()@data[, 46]))
    }
    if (input$alt == alt_theme_8[3]) {
      return(as.matrix(group_1_filter_1()@data[, 47]))
    }
    if (input$alt == alt_theme_8[4]) {
      return(as.matrix(group_1_filter_1()@data[, 48]))
    }
    if (input$alt == alt_theme_8[5]) {
      return(as.matrix(group_1_filter_1()@data[, 49]))
    }
    # theme 9
    if (input$alt == alt_theme_9[1]) {
      return(as.matrix(group_1_filter_1()@data[, 50]))
    }
    if (input$alt == alt_theme_9[2]) {
      return(as.matrix(group_1_filter_1()@data[, 51]))
    }
    if (input$alt == alt_theme_9[3]) {
      return(as.matrix(group_1_filter_1()@data[, 52]))
    }
    if (input$alt == alt_theme_9[4]) {
      return(as.matrix(group_1_filter_1()@data[, 53]))
    }
    if (input$alt == alt_theme_9[5]) {
      return(as.matrix(group_1_filter_1()@data[, 54]))
    }
    # theme 10
    if (input$alt == alt_theme_10[1]) {
      return(as.matrix(group_1_filter_1()@data[, 55]))
    }
    if (input$alt == alt_theme_10[2]) {
      return(as.matrix(group_1_filter_1()@data[, 56]))
    }
    if (input$alt == alt_theme_10[3]) {
      return(as.matrix(group_1_filter_1()@data[, 57]))
    }
    if (input$alt == alt_theme_10[4]) {
      return(as.matrix(group_1_filter_1()@data[, 58]))
    }
    if (input$alt == alt_theme_10[5]) {
      return(as.matrix(group_1_filter_1()@data[, 59]))
    }
  })
  
  # calculate mean
  group_1_mean <- reactive({
    round(mean(group_1_filter_2()), digits = 2)
  })
  
  ### GROUP 2 FILTERS ################################################
  
  group_2_filter_1 <- reactive({
    bg_filter(2, results_spdf2)
  })
  
  # subset theme alternatives
  group_2_filter_2 <- reactive({
    # theme 1
    if (input$alt == alt_theme_1[1]) {
      return(as.matrix(group_2_filter_1()@data[, 10]))
    }
    if (input$alt == alt_theme_1[2]) {
      return(as.matrix(group_2_filter_1()@data[, 11]))
    }
    if (input$alt == alt_theme_1[3]) {
      return(as.matrix(group_2_filter_1()@data[, 12]))
    }
    if (input$alt == alt_theme_1[4]) {
      return(as.matrix(group_2_filter_1()@data[, 13]))
    }
    if (input$alt == alt_theme_1[5]) {
      return(as.matrix(group_2_filter_1()@data[, 14]))
    }
    # theme 2
    if (input$alt == alt_theme_2[1]) {
      return(as.matrix(group_2_filter_1()@data[, 15]))
    }
    if (input$alt == alt_theme_2[2]) {
      return(as.matrix(group_2_filter_1()@data[, 16]))
    }
    if (input$alt == alt_theme_2[3]) {
      return(as.matrix(group_2_filter_1()@data[, 17]))
    }
    if (input$alt == alt_theme_2[4]) {
      return(as.matrix(group_2_filter_1()@data[, 18]))
    }
    if (input$alt == alt_theme_2[5]) {
      return(as.matrix(group_2_filter_1()@data[, 19]))
    }
    # theme 3
    if (input$alt == alt_theme_3[1]) {
      return(as.matrix(group_2_filter_1()@data[, 20]))
    }
    if (input$alt == alt_theme_3[2]) {
      return(as.matrix(group_2_filter_1()@data[, 21]))
    }
    if (input$alt == alt_theme_3[3]) {
      return(as.matrix(group_2_filter_1()@data[, 22]))
    }
    if (input$alt == alt_theme_3[4]) {
      return(as.matrix(group_2_filter_1()@data[, 23]))
    }
    if (input$alt == alt_theme_3[5]) {
      return(as.matrix(group_2_filter_1()@data[, 24]))
    }
    # theme 4
    if (input$alt == alt_theme_4[1]) {
      return(as.matrix(group_2_filter_1()@data[, 25]))
    }
    if (input$alt == alt_theme_4[2]) {
      return(as.matrix(group_2_filter_1()@data[, 26]))
    }
    if (input$alt == alt_theme_4[3]) {
      return(as.matrix(group_2_filter_1()@data[, 27]))
    }
    if (input$alt == alt_theme_4[4]) {
      return(as.matrix(group_2_filter_1()@data[, 28]))
    }
    if (input$alt == alt_theme_4[5]) {
      return(as.matrix(group_2_filter_1()@data[, 29]))
    }
    # theme 5
    if (input$alt == alt_theme_5[1]) {
      return(as.matrix(group_2_filter_1()@data[, 30]))
    }
    if (input$alt == alt_theme_5[2]) {
      return(as.matrix(group_2_filter_1()@data[, 31]))
    }
    if (input$alt == alt_theme_5[3]) {
      return(as.matrix(group_2_filter_1()@data[, 32]))
    }
    if (input$alt == alt_theme_5[4]) {
      return(as.matrix(group_2_filter_1()@data[, 33]))
    }
    if (input$alt == alt_theme_5[5]) {
      return(as.matrix(group_2_filter_1()@data[, 34]))
    }
    # theme 6
    if (input$alt == alt_theme_6[1]) {
      return(as.matrix(group_2_filter_1()@data[, 35]))
    }
    if (input$alt == alt_theme_6[2]) {
      return(as.matrix(group_2_filter_1()@data[, 36]))
    }
    if (input$alt == alt_theme_6[3]) {
      return(as.matrix(group_2_filter_1()@data[, 37]))
    }
    if (input$alt == alt_theme_6[4]) {
      return(as.matrix(group_2_filter_1()@data[, 38]))
    }
    if (input$alt == alt_theme_6[5]) {
      return(as.matrix(group_2_filter_1()@data[, 39]))
    }
    # theme 7
    if (input$alt == alt_theme_7[1]) {
      return(as.matrix(group_2_filter_1()@data[, 40]))
    }
    if (input$alt == alt_theme_7[2]) {
      return(as.matrix(group_2_filter_1()@data[, 41]))
    }
    if (input$alt == alt_theme_7[3]) {
      return(as.matrix(group_2_filter_1()@data[, 42]))
    }
    if (input$alt == alt_theme_7[4]) {
      return(as.matrix(group_2_filter_1()@data[, 43]))
    }
    if (input$alt == alt_theme_7[5]) {
      return(as.matrix(group_2_filter_1()@data[, 44]))
    }
    # theme 8
    if (input$alt == alt_theme_8[1]) {
      return(as.matrix(group_2_filter_1()@data[, 45]))
    }
    if (input$alt == alt_theme_8[2]) {
      return(as.matrix(group_2_filter_1()@data[, 46]))
    }
    if (input$alt == alt_theme_8[3]) {
      return(as.matrix(group_2_filter_1()@data[, 47]))
    }
    if (input$alt == alt_theme_8[4]) {
      return(as.matrix(group_2_filter_1()@data[, 48]))
    }
    if (input$alt == alt_theme_8[5]) {
      return(as.matrix(group_2_filter_1()@data[, 49]))
    }
    # theme 9
    if (input$alt == alt_theme_9[1]) {
      return(as.matrix(group_2_filter_1()@data[, 50]))
    }
    if (input$alt == alt_theme_9[2]) {
      return(as.matrix(group_2_filter_1()@data[, 51]))
    }
    if (input$alt == alt_theme_9[3]) {
      return(as.matrix(group_2_filter_1()@data[, 52]))
    }
    if (input$alt == alt_theme_9[4]) {
      return(as.matrix(group_2_filter_1()@data[, 53]))
    }
    if (input$alt == alt_theme_9[5]) {
      return(as.matrix(group_2_filter_1()@data[, 54]))
    }
    # theme 10
    if (input$alt == alt_theme_10[1]) {
      return(as.matrix(group_2_filter_1()@data[, 55]))
    }
    if (input$alt == alt_theme_10[2]) {
      return(as.matrix(group_2_filter_1()@data[, 56]))
    }
    if (input$alt == alt_theme_10[3]) {
      return(as.matrix(group_2_filter_1()@data[, 57]))
    }
    if (input$alt == alt_theme_10[4]) {
      return(as.matrix(group_2_filter_1()@data[, 58]))
    }
    if (input$alt == alt_theme_10[5]) {
      return(as.matrix(group_2_filter_1()@data[, 59]))
    }
  })
  
  # calculate mean
  group_2_mean <- reactive({
    round(mean(group_2_filter_2()), digits = 2)
  })
  
  ####################################################################################################################
  
  ### MAP ############################################################################################################
  
  ####################################################################################################################
  
  # Create static map and empty polygons
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 17.91128, lat = 59.51839, zoom = 13) %>%
      addPolygons(data = nyko, fill = TRUE, fillOpacity = 0.1, fillColor = "blue", stroke = TRUE, weight = 1, color = "black", group = "nyko")
  })
  
  # Define color palette
  colorpal <- reactive({
    colorNumeric(
      palette = input$colors,
      domain = c(0, 14),
      na.color = "gray"
    )
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
  
  # NULL checks
  null_checks_1 <- reactive({
    return(null_check(1, ui_names_bg))
  })
  
  null_checks_2 <- reactive({
    return(null_check(2, ui_names_bg))
  })
  
  # Run a NULL check on all the input values of a given list of keys
  null_check <- function(input_group_number, ui_names_bg) {
    if (is.null(input[["alt"]])) {
      return(NULL)
    }
    for (i in seq_along(ui_names_bg)) { # ui_names_bg <- c("area", "gender", "age", "occupation", "education", "years")
      input_key <- paste(ui_names_bg[[i]], input_group_number, sep = "") # Add group number to key
      if (is.null(input[[input_key]])) {
        return(NULL)
      }
    }
    return(TRUE)
  }
  
  # Add polygons to map
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
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          color = "red",
          layerId = group_1_filter_1()[["Area"]],
          group = "group1Polygons"
        )
    }
    if (!is.null(null_checks_2())) {
      leafletProxy(mapId = "map") %>%
        addPolygons(
          data = group_2_filter_1(),
          fill = TRUE,
          fillColor = ~ colorpal()(group_2_mean()),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          color = "blue",
          layerId = group_2_filter_1()[["Area"]],
          group = "group2Polygons"
        )
    }
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
            popup = group_1_filter_1()[["Area"]],
            layerId = group_1_filter_1()[["Area"]]
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
            popup = group_2_filter_1()[["Area"]],
            layerId = group_2_filter_1()[["Area"]]
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
            popup = group_1_filter_1()[["Area"]],
            layerId = group_1_filter_1()[["Area"]],
            icon = list(iconUrl = "marker-icon-red.png", iconWidth = 25, iconHeight = 41, iconAnchorX = 0, iconAnchorY = 0,  shadowUrl = "marker-shadow.png", shadowWidth = 41, shadowHeight = 41, shadowAnchorX = 12, shadowAnchorY = 22, popupAnchorX = 0, popupAnchorY = 0),
            options = markerOptions(title = paste(group_1_filter_1()[["Area"]], group_1_mean(), sep = ": "))
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
            popup = group_2_filter_1()[["Area"]],
            layerId = group_2_filter_1()[["Area"]],
            options = markerOptions(title = paste(group_2_filter_1()[["Area"]], group_2_mean(), sep = ": "))
          )
      }
    }
  })
  
  # Info box 1
  output$group_1_mean <- renderInfoBox({
    if (!is.null(null_checks_1())) {
      x <- group_1_mean()
      if (x >= 7) {
        thumb <- "thumbs-o-up"
      } else {
        thumb <- "thumbs-o-down"
      }
      infoBox(title = "Group 1", value = x, subtitle = "Mean Value", icon = icon(name = thumb), color = "light-blue", fill = FALSE)
    } else {
      infoBox(title = "Group 1", value = "-", subtitle = "Mean Value", icon = icon(name = ""), color = "light-blue", fill = FALSE)
    }
  })
  
  # Info box 2
  output$group_2_mean <- renderInfoBox({
    if (!is.null(null_checks_2())) {
      x <- group_2_mean()
      if (x >= 7) {
        thumb <- "thumbs-o-up"
      } else {
        thumb <- "thumbs-o-down"
      }
      infoBox(title = "Group 2", value = x, subtitle = "Mean Value", icon = icon(name = thumb), color = "light-blue", fill = FALSE)
    } else {
      infoBox(title = "Group 2", value = "-", subtitle = "Mean Value", icon = icon(name = ""), color = "light-blue", fill = FALSE)
    }
  })
  
  # Info box 3    
  output$disagreement <- renderInfoBox({
    if (!is.null(null_checks_1()) & !is.null(null_checks_2())) {
      x <- 0.4
      if (x >= 0.5) {
        thumb <- "bolt"
      } else {
        thumb <- "balance-scale"
      }
      infoBox(title = "Overall", value = x, subtitle = "Disagreement", icon = icon(name = thumb), color = "light-blue", fill = FALSE)
    } else {
      infoBox(title = "Overall", value = "-", subtitle = "Disagreement", color = "light-blue", fill = FALSE)
    }
  })
  
  ####################################################################################################################
  
  ### DATA TABLE #####################################################################################################
  
  ####################################################################################################################
  
  table_filter <- reactive({
    if (!identical(input$area3, "All")) {
      for (o in seq_along(input$area3)) {
        results_df <- head(results_df[results_df$Area %in% input$area3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$gender3, "All")) {
      for (p in seq_along(input$gender3)) {
        results_df <- head(results_df[results_df$Gender %in% input$gender3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$age3, "All")) {
      for (q in seq_along(input$age3)) {
        results_df <- head(results_df[results_df$Age %in% input$age3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$occupation3, "All")) {
      for (r in seq_along(input$occupation3)) {
        results_df <- head(results_df[results_df$Occupation %in% input$occupation3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$education3, "All")) {
      for (s in seq_along(input$education3)) {
        results_df <- head(results_df[results_df$Education.level %in% input$education3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$years3, "All")) {
      for (t in seq_along(input$years3)) {
        results_df <- head(results_df[results_df$Year %in% input$years3, ], n = 1040, drop = FALSE)
      }
    }
    results_df
  })
  
  output$table <- DT::renderDataTable({
    table_filter()
  }, server = TRUE)
  
})