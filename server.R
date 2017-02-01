shinyServer(function(input, output, session) {
  
  tdata <- callModule(module = tabset, id = "one")
  
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
    colorNumeric(palette = "RdYlGn", domain = c(0, 14), na.color = "darkgray")
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
          fillColor = ~colorpal()(tdata$group_1_mean()),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          color = "steelblue",
          layerId = tdata$group_1_filter_1()$Area,
          group = "group1Polygons"
        )
    
    # Group 2
    leafletProxy(mapId = "map") %>%
      addPolygons(
        data = tdata$group_2_filter_1(),
        fill = TRUE,
        fillColor = ~colorpal()(tdata$group_2_mean()),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        color = "firebrick",
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
          lng = ~long,
          lat = ~lat,
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
          lng = ~long,
          lat = ~lat,
          popup = tdata$group_2_filter_1()$Area,
          layerId = tdata$group_2_filter_1()$Area,
          options = markerOptions(title = paste(tdata$group_2_filter_1()$Area, tdata$group_2_mean(), sep = ": ")),
          icon = list(
                      iconUrl = "marker-icon-red.png", 
                      iconWidth = 25, 
                      iconHeight = 41, 
                      iconAnchorX = 0, 
                      iconAnchorY = 0,
                      shadowUrl = "marker-shadow.png", 
                      shadowWidth = 41, 
                      shadowHeight = 41, 
                      shadowAnchorX = 12, 
                      shadowAnchorY = 22, 
                      popupAnchorX = 0, 
                      popupAnchorY = 0
                  )
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
          lng = ~long,
          lat = ~lat,
          popup = tdata$group_1_filter_1()$Area,
          layerId = tdata$group_1_filter_1()$Area
        )
    }
    
    # Group 2
    if (tdata$pop2() == TRUE) {
      leafletProxy(mapId = "map") %>%
        addPopups(
          data = tdata$group_2_filter_1(),
          lng = ~long,
          lat = ~lat,
          popup = tdata$group_2_filter_1()$Area,
          layerId = tdata$group_2_filter_1()$Area
        )
    }
  })
  
  ####################################################################################################################
  
  ### PLOTS ##########################################################################################################
  
  ####################################################################################################################
  
  observe({
    withProgress(message = "Making plots", value = 0, expr = {
  
      ### DESCRIPTIVES ###################################################################################################
        
      incProgress(amount = 0, detail = "")
      
      # Get data for group 1 and group 2
      des_group_1 <- tdata$group_1_filter_2() %>% as.vector()
      des_group_2 <- tdata$group_2_filter_2() %>% as.vector()
      
      # Concatenate group 1 and group 2 and coerce into data frame
      des_group_1_2 <- c(des_group_1, des_group_2) %>% data.frame()
      
      # Coerce into data frame
      des_group_1 <- data.frame(des_group_1)
      des_group_2 <- data.frame(des_group_2)
      
      # Plot histogram of group 1
      des_group_1 %>%
        ggvis(x = ~des_group_1, fill := "steelblue", stroke := "") %>%
        scale_numeric(property = "x", domain = c(0, 14)) %>%
        scale_numeric(property = "y", domain = c(0, NA)) %>%
        add_axis(type = "x", title = "Value", ticks = 14, grid = FALSE) %>%
        add_axis(type = "y", title = "Count", format = "d",  grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_histograms(width = 1) %>%
        bind_shiny("ggvis_1")
      incProgress(amount = 1/12, detail = "Plot 1")
      
      # Plot histogram of group 2
      des_group_2 %>%
        ggvis(x = ~des_group_2, fill := "firebrick", stroke := "") %>%
        scale_numeric(property = "x", domain = c(0, 14)) %>%
        scale_numeric(property = "y", domain = c(0, NA)) %>%
        add_axis(type = "x", title = "Value", ticks = 14, grid = FALSE) %>%
        add_axis(type = "y", title = "Count", format = "d",  grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_histograms(width = 1) %>%
        bind_shiny("ggvis_2")
      incProgress(amount = 2/12, detail = "Plot 2")
      
      # Plot histogram of group 1 and 2
      des_group_1_2 %>%
        ggvis(~., fill := "darkgray", stroke := "") %>%
        scale_numeric(property = "x", domain = c(0, 14)) %>%
        scale_numeric(property = "y", domain = c(0, NA)) %>%
        add_axis(type = "x", title = "Value", ticks = 14, grid = FALSE) %>%
        add_axis(type = "y", title = "Count", format = "d",  grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_histograms(width = 1) %>%
        bind_shiny("ggvis_3")
      incProgress(amount = 3/12, detail = "Plot 3")
    
      ### MEAN WEIGHTED VALUES ###########################################################################################
        
      # Comment needed here
      results.vec1 <- disagreement(tdata$theme(), tdata$group_1_filter_1())
      results.vec2 <- disagreement(tdata$theme(), tdata$group_2_filter_1())
      
      # Calculate mean weighted values for group 1
      val_group_1 <- lapply(seq(1, 25, by = 5), function(x) {
        return(results.vec1[x + 2])
      })
      
      # Calculate mean weighted values for group 2
      val_group_2 <- lapply(seq(1, 25, by = 5), function(x) {
        return(results.vec2[x + 2])
      })
      
      # Calculate mean weighted values for group 1 and 2 combined
      val_group_1_2 <- lapply(seq(1, 25, by = 5), function(x) {
        n_grp1 <- (results.vec1[x + 3] + results.vec1[x + 4])
        n_grp2 <- (results.vec2[x + 3] + results.vec2[x + 4])
        v_grp1 <- results.vec1[x + 2]
        v_grp2 <- results.vec1[x + 2]
        org_v_grp_1 <- v_grp1 / (1 / n_grp1)
        org_v_grp_2 <- v_grp2 / (1 / n_grp2)
        m_grp_1_2 <- (org_v_grp_1 + org_v_grp_2) / (n_grp1 + n_grp2)
        return(m_grp_1_2)
      })
      
      # Flatten lists and transform them into data frames
      val_group_1 <- flatten_dbl(val_group_1) %>% data.frame()
      val_group_2 <- flatten_dbl(val_group_2) %>% data.frame()
      val_group_1_2 <- flatten_dbl(val_group_1_2) %>% data.frame()
      
      # Create row names
      alternatives <- c("a", "b", "c", "d", "e") %>% data.frame() 
      
      # Add column names to data frames
      colnames(alternatives) <- "alternatives"
      colnames(val_group_1) <- "val_group_1"
      colnames(val_group_2) <- "val_group_2"
      colnames(val_group_1_2) <- "val_group_1_2"
      
      # Bind data frames together
      val_data <- bind_cols(alternatives, val_group_1, val_group_2, val_group_1_2)
      
      # Plot bar chart for group 1
      val_data %>%
        ggvis(x = ~alternatives, y = ~val_group_1 * 100, fill := "steelblue", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE) %>%
        add_axis(type = "y", title = "Mean Weighted Value", format = "d", grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny("ggvis_4")
      incProgress(amount = 4/12, detail = "Plot 4")
      
      # Plot bar chart for group 2
      val_data %>%
        ggvis(x = ~alternatives, y = ~val_group_2 * 100, fill := "firebrick", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE) %>%
        add_axis(type = "y", title = "Mean Weighted Value", format = "d", grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny("ggvis_5")
      incProgress(amount = 5/12, detail = "Plot 5")
      
      # Plot bar chart for group 1 and 2 
      val_data %>%
        ggvis(x = ~alternatives, y = ~val_group_1_2 * 100, fill := "darkgray", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE) %>%
        add_axis(type = "y", title = "Mean Weighted Value", format = "d", grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny("ggvis_6")
      incProgress(amount = 6/12, detail = "Plot 6")
    
      ### DISAGREEMENTS ##################################################################################################
      
      # Disagreement within group 1
      dis_within_1 <- lapply(seq(1, 25, by = 5), function(x) {
        cGroupWeight <- results.vec1[x + 3] / (results.vec1[x + 3] + results.vec1[x + 4])
        pGroupWeight <- results.vec1[x + 4] / (results.vec1[x + 3] + results.vec1[x + 4])
        conIdx = results.vec1[x]
        proIdx = results.vec1[x + 1]
        if (pGroupWeight == 0 || cGroupWeight == 0) {
          conIdx <- 0
          proIdx <- 0
        }
        dSij = conIdx + proIdx
        res <- dSij
        return(res)
      })
      
      # Disagreement within group 2
      dis_within_2 <- lapply(seq(1, 25, by = 5), function(x) {
        cGroupWeight <- results.vec2[x + 3] / (results.vec2[x + 3] + results.vec2[x + 4])
        pGroupWeight <- results.vec2[x + 4] / (results.vec2[x + 3] + results.vec2[x + 4])
        conIdx = results.vec2[x]
        proIdx = results.vec2[x + 1]
        if (pGroupWeight == 0 || cGroupWeight == 0) {
          conIdx <- 0
          proIdx <- 0
        }
        dSij = conIdx + proIdx
        res <- dSij
        return(res)
      })
      
      # Disagreement between group 1 and group 2
      dis_between_1_2 <- lapply(seq(1, 25, by = 5), function(x) {
        c1GroupWeight <- results.vec1[x + 3] / (results.vec1[x + 3] + results.vec1[x + 4])
        p1GroupWeight <- results.vec1[x + 4] / (results.vec1[x + 3] + results.vec1[x + 4])
        c2GroupWeight <- results.vec2[x + 3] / (results.vec2[x + 3] + results.vec2[x + 4])
        p2GroupWeight <- results.vec2[x + 4] / (results.vec2[x + 3] + results.vec2[x + 4])
        conIdx1 <- results.vec1[x]
        conIdx2 <- results.vec2[x]
        proIdx1 <- results.vec1[x + 1]
        proIdx2 <- results.vec2[x + 1]
        if ((c1GroupWeight == 0 || p1GroupWeight == 0) ) {
          conIdx1 <- 0
          proIdx1 <- 0
        }
        if ((c2GroupWeight == 0 || p2GroupWeight == 0)) {
          conIdx2 <- 0
          proIdx2 <- 0
        }
        dDEij <- (abs(conIdx1 - conIdx2) + abs(proIdx1 - proIdx2))/2
      })
      
      # Flatten lists and transform them into data frames
      dis_within_1 <- flatten_dbl(dis_within_1) %>% data.frame()
      dis_within_2 <- flatten_dbl(dis_within_2) %>% data.frame()
      dis_between_1_2 <- flatten_dbl(dis_between_1_2) %>% data.frame()
      
      # Create row names
      alternatives <- c("a", "b", "c", "d", "e") %>% data.frame() 
      
      # Add column names to data frames
      colnames(alternatives) <- "alternatives"
      colnames(dis_within_1) <- "dis_within_1"
      colnames(dis_within_2) <- "dis_within_2"
      colnames(dis_between_1_2) <- "dis_between_1_2"
      
      # Bind data frames together
      dis_data <- bind_cols(alternatives, dis_within_1, dis_within_2, dis_between_1_2)
      
      # Plot bar chart for group 1
      dis_data %>%
        ggvis(x = ~alternatives, y = ~dis_within_1 * 100, fill := "steelblue", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE) %>%
        add_axis(type = "y", title = "Disagreement", format = "d", grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny("ggvis_7")
      incProgress(amount = 7/12, detail = "Plot 7")
      
      # Plot bar chart for group 2
      dis_data %>%
        ggvis(x = ~alternatives, y = ~dis_within_2 * 100, fill := "firebrick", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE) %>%
        add_axis(type = "y", title = "Disagreement", format = "d", grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny("ggvis_8")
      incProgress(amount = 8/12, detail = "Plot 8")
      
      # Plot bar chart for group 1 and 2
      dis_data %>%
        ggvis(x = ~alternatives, y = ~dis_between_1_2 * 100, fill := "darkgray", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE) %>%
        add_axis(type = "y", title = "Disagreement", format = "d", grid = FALSE, title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny("ggvis_9")
      incProgress(amount = 9/12, detail = "Plot 9")
      
      ### PORTFOLIOS ##################################################################################################
      
      # Generate portfolios of actions
      # Prepare data for optimization
      # Unlist values and disagreements
      val_group_1_lst <- unlist(val_group_1)
      val_group_2_lst <- unlist(val_group_2)
      val_group_1_2_lst <- unlist(val_group_1_2)
      dis_within_1_lst <- unlist(dis_within_1)
      dis_within_2_lst <- unlist(dis_within_2)
      dis_between_1_2_lst <- unlist(dis_between_1_2)
      actions <- c("A1","A2","A3","A4","A5") # TODO! Change to the real names of the actions!
      
      # Set initial budget constraint
      budget_grp1 = sum(dis_within_1_lst)
      budget_grp2 = sum(dis_within_2_lst)
      budget_grp1_2 = sum(dis_between_1_2_lst)
    
      # Generate portfolios
      
      # Group 1 positive
      portfolios_grp1_pos <- getAllPortfolios(
        actions = actions, 
        values = val_group_1_lst, 
        disagreements = dis_within_1_lst, 
        initialBudgetConstraint = budget_grp1, 
        direction = "max")
      
      # Group 1 negative
      portfolios_grp1_neg <- getAllPortfolios(
        actions = actions, 
        values = val_group_1_lst, 
        disagreements = dis_within_1_lst, 
        initialBudgetConstraint = budget_grp1, 
        direction = "min")
      
      # Group 2 positive
      portfolios_grp2_pos <- getAllPortfolios(
        actions = actions, 
        values = val_group_2_lst, 
        disagreements = dis_within_2_lst, 
        initialBudgetConstraint = budget_grp2, 
        direction = "max")
      
      # Group 2 negative
      portfolios_grp2_neg <- getAllPortfolios(
        actions = actions, 
        values = val_group_2_lst, 
        disagreements = dis_within_2_lst, 
        initialBudgetConstraint = budget_grp2, 
        direction = "min")
      
      # Group 1 and 2 positive
      portfolios_grp_1_2_pos <- getAllPortfolios(
        actions = actions, 
        values = val_group_1_2_lst, 
        disagreements = dis_between_1_2_lst, 
        initialBudgetConstraint = budget_grp1_2, 
        direction = "max")
      
      # Group 1 and 2 negative
      portfolios_grp_1_2_neg <- getAllPortfolios(
        actions = actions, 
        values = val_group_1_2_lst, 
        disagreements = dis_between_1_2_lst, 
        initialBudgetConstraint = budget_grp1_2, 
        direction = "min")
      
      # Create a combined list of positive and negative portfolios
      portfolios_grp1_neg_rev <- portfolios_grp1_neg[rev(rownames(portfolios_grp1_neg)),]
      portfolios_grp1 <- rbind(portfolios_grp1_pos, portfolios_grp1_neg_rev)
      
      portfolios_grp2_neg_rev <- portfolios_grp2_neg[rev(rownames(portfolios_grp2_neg)),]
      portfolios_grp2 <- rbind(portfolios_grp2_pos, portfolios_grp2_neg_rev)
      
      portfolios_grp_1_2_neg_rev <- portfolios_grp_1_2_neg[rev(rownames(portfolios_grp_1_2_neg)),]
      portfolios_grp_1_2 <- rbind(portfolios_grp_1_2_pos, portfolios_grp_1_2_neg_rev)
      
      # Plot portfolios for group 1
      portfolios_grp1 %>%
        ggvis(x = ~disagreement * 100, y = ~value * 100, fill := "steelblue", stroke := "") %>%
        add_axis(type = "x", title = "Disagreement", grid = FALSE) %>%
        add_axis(type = "y", title = "Value", grid = FALSE) %>%
        set_options(width = "auto", height = "200") %>%
        layer_points() %>%
        bind_shiny("ggvis_10")
      incProgress(amount = 10/12, detail = "Plot 10")
      
      # Plot portfolios for group 2
      portfolios_grp2 %>%
        ggvis(x = ~disagreement * 100, y = ~value * 100, fill := "firebrick", stroke := "") %>%
        add_axis(type = "x", title = "Disagreement", grid = FALSE) %>%
        add_axis(type = "y", title = "Value", grid = FALSE) %>%
        set_options(width = "auto", height = "200") %>%
        layer_points() %>%
        bind_shiny("ggvis_11")
      incProgress(amount = 11/12, detail = "Plot 11")
  
      # Plot portfolios for group 1 and 2
      portfolios_grp_1_2 %>%
        ggvis(x = ~disagreement * 100, y = ~value * 100, fill := "darkgray", stroke := "") %>%
        add_axis(type = "x", title = "Disagreement", grid = FALSE) %>%
        add_axis(type = "y", title = "Value", grid = FALSE) %>%
        set_options(width = "auto", height = "200") %>%
        layer_points() %>%
        bind_shiny("ggvis_12")
      incProgress(amount = 12/12, detail = "Plot 12")
      
    })
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