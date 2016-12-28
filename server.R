shinyServer(function(input, output, session) {
  #
  tdata <- callModule(module = tabset, id = "one")
  callModule(module = tabset, id = "two")
  
  ####################################################################################################################
  
  ### MAP ############################################################################################################
  
  ####################################################################################################################
  
  # Create static map and polygon wireframes
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
    
    # Group 1
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
    
    # Group 2
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
    
    # Group 1
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
    
    # Group 2
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
    
    # Group 1
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
    
    # Group 2
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
  
  ### PLOTS ##########################################################################################################
  
  ####################################################################################################################
  
  # Calculates BCAR for both groups
  observe({

    results.vec1 <- disagreement_data(tdata$theme(), results_spdf1)
    results.vec2 <- disagreement_data(tdata$theme(), results_spdf2)
    
    # if (tdata$theme() == "1. Parks and green areas") {
    #   results.vec1 <- disagreement_data(q1pseudo.name, q1colNames, q1aNames, q1question.name, q1criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q1pseudo.name, q1colNames, q1aNames, q1question.name, q1criterion.name, results_spdf2)
    # } else if (tdata$theme() == "2. Diversity in housing supply") {
    #   results.vec1 <- disagreement_data(q2pseudo.name, q2colNames, q2aNames, q2question.name, q2criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q2pseudo.name, q2colNames, q2aNames, q2question.name, q2criterion.name, results_spdf2)
    # } else if (tdata$theme() == "3. Invest in public areas") {
    #   results.vec1 <- disagreement_data(q3pseudo.name, q3colNames, q3aNames, q3question.name, q3criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q3pseudo.name, q3colNames, q3aNames, q3question.name, q3criterion.name, results_spdf2)
    # } else if (tdata$theme() == "4. Communications") {
    #   results.vec1 <- disagreement_data(q4pseudo.name, q4colNames, q4aNames, q4question.name, q4criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q4pseudo.name, q4colNames, q4aNames, q4question.name, q4criterion.name, results_spdf2)
    # } else if (tdata$theme() == "5. Culture and leasure") {
    #   results.vec1 <- disagreement_data(q5pseudo.name, q5colNames, q5aNames, q5question.name, q5criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q5pseudo.name, q5colNames, q5aNames, q5question.name, q5criterion.name, results_spdf2)
    # } else if (tdata$theme() == "6. Education") {
    #   results.vec1 <- disagreement_data(q6pseudo.name, q6colNames, q6aNames, q6question.name, q6criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q6pseudo.name, q6colNames, q6aNames, q6question.name, q6criterion.name, results_spdf2)
    # } else if (tdata$theme() == "7. Care") {
    #   results.vec1 <- disagreement_data(q7pseudo.name, q7colNames, q7aNames, q7question.name, q7criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q7pseudo.name, q7colNames, q7aNames, q7question.name, q7criterion.name, results_spdf2)
    # } else if (tdata$theme() == "8. School") {
    #   results.vec1 <- disagreement_data(q8pseudo.name, q8colNames, q8aNames, q8question.name, q8criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q8pseudo.name, q8colNames, q8aNames, q8question.name, q8criterion.name, results_spdf2)
    # } else if (tdata$theme() == "9. Safety") {
    #   results.vec1 <- disagreement_data(q9pseudo.name, q9colNames, q9aNames, q9question.name, q9criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q9pseudo.name, q9colNames, q9aNames, q9question.name, q9criterion.name, results_spdf2)
    # } else if (tdata$theme() == "10. Ecological sustainability") {
    #   results.vec1 <- disagreement_data(q0pseudo.name, q0colNames, q0aNames, q0question.name, q0criterion.name, results_spdf1)
    #   results.vec2 <- disagreement_data(q0pseudo.name, q0colNames, q0aNames, q0question.name, q0criterion.name, results_spdf2)
    # }
    
    # Disagreement between group 1 and group 2
    # dis1_2 <- lapply(seq(1, 25, by = 5), function(x) {
    #   c1GroupWeight <- results.vec1[x + 3] / (results.vec1[x + 3] + results.vec1[x + 4])
    #   p1GroupWeight <- results.vec1[x + 4] / (results.vec1[x + 3] + results.vec1[x + 4])
    #   c2GroupWeight <- results.vec2[x + 3] / (results.vec2[x + 3] + results.vec2[x + 4])
    #   p2GroupWeight <- results.vec2[x + 4] / (results.vec2[x + 3] + results.vec2[x + 4])
    #   conIdx1 <- results.vec1[x]
    #   conIdx2 <- results.vec2[x]
    #   proIdx1 <- results.vec1[x + 1]
    #   proIdx2 <- results.vec2[x + 1]
    #   if ((c1GroupWeight == 0 || p1GroupWeight == 0) ) {
    #     conIdx1 <- 0
    #     proIdx1 <- 0
    #   }
    #   if ((c2GroupWeight == 0 || p2GroupWeight == 0)) {
    #     conIdx2 <- 0
    #     proIdx2 <- 0
    #   }
    #   dDEij <- abs(conIdx1 - conIdx2) + abs(proIdx1 - proIdx2)
    # })
    # 
    # # Disagreement within group 1
    # dis1 <- lapply(seq(1, 25, by = 5), function(x) {
    #   cGroupWeight <- results.vec1[x + 3] / (results.vec1[x + 3] + results.vec1[x + 4])
    #   pGroupWeight <- results.vec1[x + 4] / (results.vec1[x + 3] + results.vec1[x + 4])
    #   conIdx = results.vec1[x]
    #   proIdx = results.vec1[x + 1]
    #   if (pGroupWeight == 0 || cGroupWeight == 0) {
    #     conIdx <- 0
    #     proIdx <- 0
    #   }
    #   dSij = conIdx + proIdx
    #   res <- dSij
    #   return(res)
    # })
    # 
    # # Disagreement within group 2
    # dis2 <- lapply(seq(1, 25, by = 5), function(x) {
    #   cGroupWeight <- results.vec2[x + 3] / (results.vec2[x + 3] + results.vec2[x + 4])
    #   pGroupWeight <- results.vec2[x + 4] / (results.vec2[x + 3] + results.vec2[x + 4])
    #   conIdx = results.vec2[x]
    #   proIdx = results.vec2[x + 1]
    #   if (pGroupWeight == 0 || cGroupWeight == 0) {
    #     conIdx <- 0
    #     proIdx <- 0
    #   }
    #   dSij = conIdx + proIdx
    #   res <- dSij
    #   return(res)
    # })
    # 
    # print(tdata$theme())
    # print(unlist(dis1_2))
    # print(unlist(dis1))
    # print(unlist(dis2))
    # 
    # # Plot disagreements
    # output$plot1 <- renderPlot({
    #   plotdf <- data.frame(Actions = frgLbls[[frgCho]], Disagreement = c(disagreements[1:5]))
    #   suppressWarnings(
    #     print(
    #       ggplot(
    #         data = plotdf, 
    #         aes(x = Actions, y = Disagreement)
    #       ) + 
    #       labs(title = temLbls[frgCho]) + 
    #       geom_bar(stat = "identity") + 
    #       coord_cartesian(ylim = c(0, 1)) + 
    #       scale_x_discrete(labels = function(x) str_wrap(x, width = 23))
    #     )
    #   )
    # })
    
  })

  ####################################################################################################################
  
  ### TABLE ##########################################################################################################
  
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