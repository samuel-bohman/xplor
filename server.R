shinyServer(function(input, output, session) {
  
  callModule(module = tabset_module, id = "one")
  
  # output$alternatives <- renderUI({
  #   switch(
  #     input$theme,
  #     "1. Parks and green areas" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[1]]),
  #     "2. Diversity in housing supply" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[2]]),
  #     "3. Invest in public areas" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[3]]),
  #     "4. Communications" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[4]]),
  #     "5. Culture and leasure" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[5]]),
  #     "6. Education" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[6]]),
  #     "7. Care" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[7]]),
  #     "8. School" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[8]]),
  #     "9. Safety" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[9]]),
  #     "10. Ecological sustainability" = selectInput(inputId = "alt", label = "Alternatives", choices = alt_list[[10]])
  #   )
  # })
  
  ####################################################################################################################
  
  ### MAP ############################################################################################################
  
  ####################################################################################################################
  
  # Create static map and empty polygons
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 17.91128, lat = 59.51839, zoom = 12) %>%
      addPolygons(data = nyko, fill = TRUE, fillOpacity = 0.1, fillColor = "blue", stroke = TRUE, weight = 1, color = "black", group = "nyko")
  })
  
  # Define color palette
  colorpal <- reactive({
    colorNumeric(palette = "RdYlGn", domain = c(0, 14), na.color = "gray")
  })
  
  # Add color legend to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearControls() %>%
      addLegend(position = "bottomleft", pal = colorpal(), values = c(0:14), labels = c("Red", "Yellow", "Green"))
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
    if (is.null(input$alt)) {
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