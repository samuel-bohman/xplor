shinyServer(function(input, output, session) {
  
  tdata <- callModule(module = tabset, id = "one")
  
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
  
  # Add polygons to map
  observe({
    
    leafletProxy(mapId = "map") %>%
      clearGroup(group = "group1Polygons") %>%
      clearGroup(group = "group2Polygons")
    
    leafletProxy(mapId = "map") %>%
        addPolygons(
          data = tdata$group_1_filter_1(),
          fill = TRUE,
          fillColor = ~ colorpal()(tdata$group_1_mean()),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          color = "red",
          layerId = tdata$group_1_filter_1()$Area,
          group = "group1Polygons"
        )
    
    leafletProxy(mapId = "map") %>%
      addPolygons(
        data = tdata$group_2_filter_1(),
        fill = TRUE,
        fillColor = ~ colorpal()(tdata$group_2_mean()),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        color = "blue",
        layerId = tdata$group_2_filter_1()$Area,
        group = "group2Polygons"
      )
  })
  
  # Add markers to map
  observe({
    
    leafletProxy(mapId = "map") %>%
      clearMarkers()
    
    if (tdata$markers1() == TRUE) {
      leafletProxy(mapId = "map") %>%
        addMarkers(
          data = tdata$group_1_filter_1(),
          lng = ~ long,
          lat = ~ lat,
          popup = tdata$group_1_filter_1()$Area,
          layerId = tdata$group_1_filter_1()$Area,
          options = markerOptions(title = paste(tdata$group_1_filter_1()$Area, tdata$group_1_mean(), sep = ": "))
        )
    }
    
    if (tdata$markers2() == TRUE) {
      leafletProxy(mapId = "map") %>%
        addMarkers(
          data = tdata$group_2_filter_1(),
          lng = ~ long,
          lat = ~ lat,
          popup = tdata$group_2_filter_1()$Area,
          layerId = tdata$group_2_filter_1()$Area,
          options = markerOptions(title = paste(tdata$group_2_filter_1()$Area, tdata$group_2_mean(), sep = ": ")),
          icon = list(iconUrl = "marker-icon-red.png", iconWidth = 25, iconHeight = 41, iconAnchorX = 0, iconAnchorY = 0,  shadowUrl = "marker-shadow.png", shadowWidth = 41, shadowHeight = 41, shadowAnchorX = 12, shadowAnchorY = 22, popupAnchorX = 0, popupAnchorY = 0)
        )
    }
    
  })
  
  # Add popups to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearPopups()

    if (tdata$pop1() == TRUE) {  
      leafletProxy(mapId = "map") %>%
        addPopups(
          data = tdata$group_1_filter_1(),
          lng = ~ long,
          lat = ~ lat,
          popup = tdata$group_1_filter_1()$Area,
          layerId = tdata$group_1_filter_1()$Area
        )
    }
      
    if (tdata$pop2() == TRUE) { 
      leafletProxy(mapId = "map") %>%
        addPopups(
          data = tdata$group_2_filter_1(),
          lng = ~ long,
          lat = ~ lat,
          popup = tdata$group_2_filter_1()$Area,
          layerId = tdata$group_2_filter_1()$Area
        )
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