shinyServer(function(input, output, session) {
  
  # Create static map ----
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
  
  # Render alternatives menu ----
  output[["alternatives"]] <- renderUI({
    switch(
      input[["themes"]],
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
  
  # Returns corresponding dataframe column name of an input column name ----
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
  
  ### GROUP 1 FILTERS AND CALCULATIONS ########################
  
  # Filters given spdf by reading numbered group input ids
  bg_filter <- function(group_number, spdf) {
    for (i in seq_along(ui_names_bg)) {
      col <- paste(ui_names_bg[i], group_number, sep = "") # E.g. "area1"
      df_col <-
        get_input_category(substr(col, 1, nchar(col) - 1)) # E.g. "area1" -> "area" -> "Omrade"
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
  
  # Subset background variables
  group_1_filter_1 <- reactive({
    bg_filter(1, results_spdf1)
  })
  
  # Subset theme alternatives
  group_1_filter_2 <- reactive({
    # Theme 1
    if (input[["alt"]] == alt_theme_1[1]) {
      return(as.matrix(group_1_filter_1()@data[, 10]))
    }
    if (input[["alt"]] == alt_theme_1[2]) {
      return(as.matrix(group_1_filter_1()@data[, 11]))
    }
    if (input[["alt"]] == alt_theme_1[3]) {
      return(as.matrix(group_1_filter_1()@data[, 12]))
    }
    if (input[["alt"]] == alt_theme_1[4]) {
      return(as.matrix(group_1_filter_1()@data[, 13]))
    }
    if (input[["alt"]] == alt_theme_1[5]) {
      return(as.matrix(group_1_filter_1()@data[, 14]))
    }
    # Theme 2
    if (input[["alt"]] == alt_theme_2[1]) {
      return(as.matrix(group_1_filter_1()@data[, 15]))
    }
    if (input[["alt"]] == alt_theme_2[2]) {
      return(as.matrix(group_1_filter_1()@data[, 16]))
    }
    if (input[["alt"]] == alt_theme_2[3]) {
      return(as.matrix(group_1_filter_1()@data[, 17]))
    }
    if (input[["alt"]] == alt_theme_2[4]) {
      return(as.matrix(group_1_filter_1()@data[, 18]))
    }
    if (input[["alt"]] == alt_theme_2[5]) {
      return(as.matrix(group_1_filter_1()@data[, 19]))
    }
    # Theme 3
    if (input[["alt"]] == alt_theme_3[1]) {
      return(as.matrix(group_1_filter_1()@data[, 20]))
    }
    if (input[["alt"]] == alt_theme_3[2]) {
      return(as.matrix(group_1_filter_1()@data[, 21]))
    }
    if (input[["alt"]] == alt_theme_3[3]) {
      return(as.matrix(group_1_filter_1()@data[, 22]))
    }
    if (input[["alt"]] == alt_theme_3[4]) {
      return(as.matrix(group_1_filter_1()@data[, 23]))
    }
    if (input[["alt"]] == alt_theme_3[5]) {
      return(as.matrix(group_1_filter_1()@data[, 24]))
    }
    # Theme 4
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_1_filter_1()@data[, 25]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_1_filter_1()@data[, 26]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_1_filter_1()@data[, 27]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_1_filter_1()@data[, 28]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_1_filter_1()@data[, 29]))
    }
    # Theme 5
    if (input[["alt"]] == alt_theme_5[1]) {
      return(as.matrix(group_1_filter_1()@data[, 30]))
    }
    if (input[["alt"]] == alt_theme_5[2]) {
      return(as.matrix(group_1_filter_1()@data[, 31]))
    }
    if (input[["alt"]] == alt_theme_5[3]) {
      return(as.matrix(group_1_filter_1()@data[, 32]))
    }
    if (input[["alt"]] == alt_theme_5[4]) {
      return(as.matrix(group_1_filter_1()@data[, 33]))
    }
    if (input[["alt"]] == alt_theme_5[5]) {
      return(as.matrix(group_1_filter_1()@data[, 34]))
    }
    # Theme 6
    if (input[["alt"]] == alt_theme_6[1]) {
      return(as.matrix(group_1_filter_1()@data[, 35]))
    }
    if (input[["alt"]] == alt_theme_6[2]) {
      return(as.matrix(group_1_filter_1()@data[, 36]))
    }
    if (input[["alt"]] == alt_theme_6[3]) {
      return(as.matrix(group_1_filter_1()@data[, 37]))
    }
    if (input[["alt"]] == alt_theme_6[4]) {
      return(as.matrix(group_1_filter_1()@data[, 38]))
    }
    if (input[["alt"]] == alt_theme_6[5]) {
      return(as.matrix(group_1_filter_1()@data[, 39]))
    }
    # Theme 7
    if (input[["alt"]] == alt_theme_7[1]) {
      return(as.matrix(group_1_filter_1()@data[, 40]))
    }
    if (input[["alt"]] == alt_theme_7[2]) {
      return(as.matrix(group_1_filter_1()@data[, 41]))
    }
    if (input[["alt"]] == alt_theme_7[3]) {
      return(as.matrix(group_1_filter_1()@data[, 42]))
    }
    if (input[["alt"]] == alt_theme_7[4]) {
      return(as.matrix(group_1_filter_1()@data[, 43]))
    }
    if (input[["alt"]] == alt_theme_7[5]) {
      return(as.matrix(group_1_filter_1()@data[, 44]))
    }
    # Theme 8
    if (input[["alt"]] == alt_theme_8[1]) {
      return(as.matrix(group_1_filter_1()@data[, 45]))
    }
    if (input[["alt"]] == alt_theme_8[2]) {
      return(as.matrix(group_1_filter_1()@data[, 46]))
    }
    if (input[["alt"]] == alt_theme_8[3]) {
      return(as.matrix(group_1_filter_1()@data[, 47]))
    }
    if (input[["alt"]] == alt_theme_8[4]) {
      return(as.matrix(group_1_filter_1()@data[, 48]))
    }
    if (input[["alt"]] == alt_theme_8[5]) {
      return(as.matrix(group_1_filter_1()@data[, 49]))
    }
    # Theme 9
    if (input[["alt"]] == alt_theme_9[1]) {
      return(as.matrix(group_1_filter_1()@data[, 50]))
    }
    if (input[["alt"]] == alt_theme_9[2]) {
      return(as.matrix(group_1_filter_1()@data[, 51]))
    }
    if (input[["alt"]] == alt_theme_9[3]) {
      return(as.matrix(group_1_filter_1()@data[, 52]))
    }
    if (input[["alt"]] == alt_theme_9[4]) {
      return(as.matrix(group_1_filter_1()@data[, 53]))
    }
    if (input[["alt"]] == alt_theme_9[5]) {
      return(as.matrix(group_1_filter_1()@data[, 54]))
    }
    # Theme 10
    if (input[["alt"]] == alt_theme_10[1]) {
      return(as.matrix(group_1_filter_1()@data[, 55]))
    }
    if (input[["alt"]] == alt_theme_10[2]) {
      return(as.matrix(group_1_filter_1()@data[, 56]))
    }
    if (input[["alt"]] == alt_theme_10[3]) {
      return(as.matrix(group_1_filter_1()@data[, 57]))
    }
    if (input[["alt"]] == alt_theme_10[4]) {
      return(as.matrix(group_1_filter_1()@data[, 58]))
    }
    if (input[["alt"]] == alt_theme_10[5]) {
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
    if (input[["alt"]] == alt_theme_1[1]) {
      return(as.matrix(group_2_filter_1()@data[, 10]))
    }
    if (input[["alt"]] == alt_theme_1[2]) {
      return(as.matrix(group_2_filter_1()@data[, 11]))
    }
    if (input[["alt"]] == alt_theme_1[3]) {
      return(as.matrix(group_2_filter_1()@data[, 12]))
    }
    if (input[["alt"]] == alt_theme_1[4]) {
      return(as.matrix(group_2_filter_1()@data[, 13]))
    }
    if (input[["alt"]] == alt_theme_1[5]) {
      return(as.matrix(group_2_filter_1()@data[, 14]))
    }
    # Theme 2
    if (input[["alt"]] == alt_theme_2[1]) {
      return(as.matrix(group_2_filter_1()@data[, 15]))
    }
    if (input[["alt"]] == alt_theme_2[2]) {
      return(as.matrix(group_2_filter_1()@data[, 16]))
    }
    if (input[["alt"]] == alt_theme_2[3]) {
      return(as.matrix(group_2_filter_1()@data[, 17]))
    }
    if (input[["alt"]] == alt_theme_2[4]) {
      return(as.matrix(group_2_filter_1()@data[, 18]))
    }
    if (input[["alt"]] == alt_theme_2[5]) {
      return(as.matrix(group_2_filter_1()@data[, 19]))
    }
    # Theme 3
    if (input[["alt"]] == alt_theme_3[1]) {
      return(as.matrix(group_2_filter_1()@data[, 20]))
    }
    if (input[["alt"]] == alt_theme_3[2]) {
      return(as.matrix(group_2_filter_1()@data[, 21]))
    }
    if (input[["alt"]] == alt_theme_3[3]) {
      return(as.matrix(group_2_filter_1()@data[, 22]))
    }
    if (input[["alt"]] == alt_theme_3[4]) {
      return(as.matrix(group_2_filter_1()@data[, 23]))
    }
    if (input[["alt"]] == alt_theme_3[5]) {
      return(as.matrix(group_2_filter_1()@data[, 24]))
    }
    # Theme 4
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_2_filter_1()@data[, 25]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_2_filter_1()@data[, 26]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_2_filter_1()@data[, 27]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_2_filter_1()@data[, 28]))
    }
    if (input[["alt"]] == alt_theme_4[1]) {
      return(as.matrix(group_2_filter_1()@data[, 29]))
    }
    # Theme 5
    if (input[["alt"]] == alt_theme_5[1]) {
      return(as.matrix(group_2_filter_1()@data[, 30]))
    }
    if (input[["alt"]] == alt_theme_5[2]) {
      return(as.matrix(group_2_filter_1()@data[, 31]))
    }
    if (input[["alt"]] == alt_theme_5[3]) {
      return(as.matrix(group_2_filter_1()@data[, 32]))
    }
    if (input[["alt"]] == alt_theme_5[4]) {
      return(as.matrix(group_2_filter_1()@data[, 33]))
    }
    if (input[["alt"]] == alt_theme_5[5]) {
      return(as.matrix(group_2_filter_1()@data[, 34]))
    }
    # Theme 6
    if (input[["alt"]] == alt_theme_6[1]) {
      return(as.matrix(group_2_filter_1()@data[, 35]))
    }
    if (input[["alt"]] == alt_theme_6[2]) {
      return(as.matrix(group_2_filter_1()@data[, 36]))
    }
    if (input[["alt"]] == alt_theme_6[3]) {
      return(as.matrix(group_2_filter_1()@data[, 37]))
    }
    if (input[["alt"]] == alt_theme_6[4]) {
      return(as.matrix(group_2_filter_1()@data[, 38]))
    }
    if (input[["alt"]] == alt_theme_6[5]) {
      return(as.matrix(group_2_filter_1()@data[, 39]))
    }
    # Theme 7
    if (input[["alt"]] == alt_theme_7[1]) {
      return(as.matrix(group_2_filter_1()@data[, 40]))
    }
    if (input[["alt"]] == alt_theme_7[2]) {
      return(as.matrix(group_2_filter_1()@data[, 41]))
    }
    if (input[["alt"]] == alt_theme_7[3]) {
      return(as.matrix(group_2_filter_1()@data[, 42]))
    }
    if (input[["alt"]] == alt_theme_7[4]) {
      return(as.matrix(group_2_filter_1()@data[, 43]))
    }
    if (input[["alt"]] == alt_theme_7[5]) {
      return(as.matrix(group_2_filter_1()@data[, 44]))
    }
    # Theme 8
    if (input[["alt"]] == alt_theme_8[1]) {
      return(as.matrix(group_2_filter_1()@data[, 45]))
    }
    if (input[["alt"]] == alt_theme_8[2]) {
      return(as.matrix(group_2_filter_1()@data[, 46]))
    }
    if (input[["alt"]] == alt_theme_8[3]) {
      return(as.matrix(group_2_filter_1()@data[, 47]))
    }
    if (input[["alt"]] == alt_theme_8[4]) {
      return(as.matrix(group_2_filter_1()@data[, 48]))
    }
    if (input[["alt"]] == alt_theme_8[5]) {
      return(as.matrix(group_2_filter_1()@data[, 49]))
    }
    # Theme 9
    if (input[["alt"]] == alt_theme_9[1]) {
      return(as.matrix(group_2_filter_1()@data[, 50]))
    }
    if (input[["alt"]] == alt_theme_9[2]) {
      return(as.matrix(group_2_filter_1()@data[, 51]))
    }
    if (input[["alt"]] == alt_theme_9[3]) {
      return(as.matrix(group_2_filter_1()@data[, 52]))
    }
    if (input[["alt"]] == alt_theme_9[4]) {
      return(as.matrix(group_2_filter_1()@data[, 53]))
    }
    if (input[["alt"]] == alt_theme_9[5]) {
      return(as.matrix(group_2_filter_1()@data[, 54]))
    }
    # Theme 10
    if (input[["alt"]] == alt_theme_10[1]) {
      return(as.matrix(group_2_filter_1()@data[, 55]))
    }
    if (input[["alt"]] == alt_theme_10[2]) {
      return(as.matrix(group_2_filter_1()@data[, 56]))
    }
    if (input[["alt"]] == alt_theme_10[3]) {
      return(as.matrix(group_2_filter_1()@data[, 57]))
    }
    if (input[["alt"]] == alt_theme_10[4]) {
      return(as.matrix(group_2_filter_1()@data[, 58]))
    }
    if (input[["alt"]] == alt_theme_10[5]) {
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
      input_key <- paste(ui_names_list[[i]], input_group_number, sep = "") # Add group number to key
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
          weight = 3,
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
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 3,
          color = "blue",
          layerId = group_2_filter_1()[["Area"]],
          group = "group2Polygons"
        )
    }
  })
  
  # Create color palette
  colorpal <- reactive({
    colorNumeric(
      palette = input[["colors"]],
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
            layerId = group_1_filter_1()[["Area"]]
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
            layerId = group_2_filter_1()[["Area"]]
          )
      }
    }
  })
  
  # Value boxes
  output$group_1_mean <- renderValueBox({
    x <- group_1_mean()
    if (!is.null(null_checks_1())) {
      valueBox(
        value = x, 
        subtitle = "Group 1 Mean", 
        icon = if (x >= 7) {
          icon(name = "thumbs-o-up") 
          } else {
            icon(name = "thumbs-o-down")
            }, 
        color = "light-blue"
      )
    } else {
      valueBox(
        value = 0, 
        subtitle = "Group 1 Mean", 
        icon = icon(name = ""), 
        color = "light-blue"
        )
    }
  })
  
    output$group_2_mean <- renderValueBox({
      x <- group_2_mean()
      if (!is.null(null_checks_2())) {
        valueBox(
          value = group_2_mean(), 
          subtitle = "Group 2 Mean", 
          icon = if (x >= 7) {
            icon(name = "thumbs-o-up") 
            } else {
              icon(name = "thumbs-o-down")
              }, 
          color = "light-blue")
      } else {
        valueBox(
          value = 0, 
          subtitle = "Group 2 Mean", 
          icon = icon("users"), 
          color = "light-blue")
      }
  })
    
    output$overall_mean <- renderValueBox({
      if (!is.null(null_checks_1()) & !is.null(null_checks_2())) {
        valueBox(
          value = (group_1_mean() + group_2_mean()) / 2, 
          subtitle = "Overall Mean", 
          icon = icon("hand-peace-o"), 
          color = "light-blue")
      } else {
        valueBox(
          value = 0, 
          subtitle = "Overall Mean", 
          icon = icon("users"), 
          color = "light-blue")
      }
  })
  
  ### DATA TABLE ##############################################
  
  # Subset background variables ----
  table_filter <- reactive({
    if (input[["area"]] != "All" || is.null(input[["area"]]))
      for (o in seq_along(input[["area"]])) {
        results_df <- head(results_df[results_df[["Area"]] %in% input[["area"]], ], n = 1040, drop = FALSE)
      }
    if (input[["gender"]] != "All" || is.null(input[["gender"]]))
      for (p in seq_along(input[["gender"]])) {
        results_df <- head(results_df[results_df[["Gender"]] %in% input[["gender"]], ], n = 1040, drop = FALSE)
      }
    if (input[["age"]] != "All" || is.null(input[["age"]]))
      for (q in seq_along(input[["age"]])) {
        results_df <- head(results_df[results_df[["Age"]] %in% input[["age"]], ], n = 1040, drop = FALSE)
      }
    if (input[["occupation"]] != "All" || is.null(input[["occupation"]]))
      for (r in seq_along(input[["occupation"]])) {
        results_df <- head(results_df[results_df[["Occupation"]] %in% input[["occupation"]], ], n = 1040, drop = FALSE)
      }
    if (input[["education"]] != "All" || is.null(input[["education"]]))
      for (s in seq_along(input[["education"]])) {
        results_df <- head(results_df[results_df[["Education.level"]] %in% input[["education"]], ], n = 1040, drop = FALSE)
      }
    if (input[["years"]] != "All" || is.null(input[["years"]]))
      for (t in seq_along(input[["years"]])) {
        results_df <- head(results_df[results_df[["Year"]] %in% input[["years"]], ], n = 1040, drop = FALSE)
      }
    results_df
  })
  
  output[["table"]] <- DT::renderDataTable({
    table_filter()
  }, server = TRUE)
  
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
  
})