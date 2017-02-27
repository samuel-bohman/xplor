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
      addPolygons(data = nyko, fill = TRUE, fillOpacity = 0.1, fillColor = "blue", stroke = TRUE, weight = 0.7, color = "black", group = "nyko") %>%
      addMeasure() %>%
      # addGraticule(interval = 1, group = "Graticule") %>%
      # addTerminator(group = "Daylight") %>%
      # addLayersControl(overlayGroups = c("Graticule", "Daylight"), options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap(toggleDisplay = TRUE)
  })
  
  # Define color palette
  colorpal <- reactive({
    colorNumeric(palette = tdata$colorpal(), domain = c(0, 14), na.color = "gray")
  })
  
  # Add color legend to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearControls() %>%
      addLegend(position = "bottomleft", pal = colorpal(), values = c(0:14), labels = c("Min", "Mean", "Max"))
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
          group = "group1Polygons", 
          highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE)
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
        group = "group2Polygons", 
        highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE)
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
      
      # Get data for group 1 and group 2
      des_group_1 <- tdata$group_1_filter_2() %>% as.vector()
      des_group_2 <- tdata$group_2_filter_2() %>% as.vector()
      
      # Concatenate group 1 and group 2 and coerce into data frame
      des_total <- c(des_group_1, des_group_2) %>% data.frame()
      
      # Coerce into data frame
      des_group_1 <- data.frame(des_group_1)
      des_group_2 <- data.frame(des_group_2)
      
      # Plot values group 1
      des_group_1 %>%
        ggvis(x = ~des_group_1, fill := "steelblue", stroke := "") %>%
        scale_numeric(property = "x", domain = c(0, 14)) %>%
        scale_numeric(property = "y", domain = c(0, NA)) %>%
        add_axis(type = "x", title = "Value", ticks = 7, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Count", ticks = 6, format = "d",  grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_histograms(width = 1) %>%
        bind_shiny(plot_id = "ggvis_1")
      incProgress(amount = 1/16, detail = "Plot 1")
      
      # Plot values group 2
      des_group_2 %>%
        ggvis(x = ~des_group_2, fill := "firebrick", stroke := "") %>%
        scale_numeric(property = "x", domain = c(0, 14)) %>%
        scale_numeric(property = "y", domain = c(0, NA)) %>%
        add_axis(type = "x", title = "Value", ticks = 7, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Count", ticks = 6, format = "d",  grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_histograms(width = 1) %>%
        bind_shiny(plot_id = "ggvis_2")
      incProgress(amount = 1/16, detail = "Plot 2")
      
      # Plot total values
      des_total %>%
        ggvis(~., fill := "darkslateblue", stroke := "") %>%
        scale_numeric(property = "x", domain = c(0, 14)) %>%
        scale_numeric(property = "y", domain = c(0, NA)) %>%
        add_axis(type = "x", title = "Value", ticks = 7, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Count", ticks = 6, format = "d", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_histograms(width = 1) %>%
        bind_shiny(plot_id = "ggvis_3")
      incProgress(amount = 1/16, detail = "Plot 3")
    
      ### MEAN WEIGHTED VALUES ###########################################################################################
        
      # Comment needed here
      results.vec1 <- disagreement(tdata$theme(), tdata$group_1_filter_1())
      results.vec2 <- disagreement(tdata$theme(), tdata$group_2_filter_1())
      
      # Calculate group 1 mean weighted values
      val_group_1 <- lapply(seq(1, 25, by = 5), function(x) {
        return(results.vec1[x + 2])
      })
      
      # Calculate group 2 mean weighted values 
      val_group_2 <- lapply(seq(1, 25, by = 5), function(x) {
        return(results.vec2[x + 2])
      })
      
      # Calculate total mean weighted values
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
      
      # Flatten lists and transform into data frames
      val_group_1_df <- val_group_1 %>% flatten_dbl() %>% data.frame()
      val_group_2_df <- val_group_2 %>% flatten_dbl() %>% data.frame()
      val_group_1_2_df <- val_group_1_2 %>% flatten_dbl() %>% data.frame()
      
      # Create row names
      alternatives <- c("a", "b", "c", "d", "e") %>% data.frame() 
      
      # Add column names to data frames
      colnames(alternatives) <- "x"
      colnames(val_group_1_df) <- "y1"
      colnames(val_group_2_df) <- "y2"
      colnames(val_group_1_2_df) <- "y3"
      
      # Bind data frames together
      val_data <- bind_cols(alternatives, val_group_1_df, val_group_2_df, val_group_1_2_df)
      
      # Plot group 1 mean weighted values
      val_data %>%
        ggvis(x = ~x, y = ~y1 * 100, fill := "steelblue", stroke := "") %>%
        scale_numeric(property = "y", domain = c(NA, 10)) %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Value", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny(plot_id = "ggvis_4")
      incProgress(amount = 1/16, detail = "Plot 4")
      
      # Plot group 2 mean weighted values 
      val_data %>%
        ggvis(x = ~x, y = ~y2 * 100, fill := "firebrick", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Value", format = "d", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny(plot_id = "ggvis_5")
      incProgress(amount = 1/16, detail = "Plot 5")
      
      # Plot total mean weighted values 
      val_data %>%
        ggvis(x = ~x, y = ~y3 * 100, fill := "darkslateblue", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Value", format = "d", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny(plot_id = "ggvis_6")
      incProgress(amount = 1/16, detail = "Plot 6")
    
      ### DISAGREEMENTS ##################################################################################################
      
      # Calculate disagreement within group 1
      dis_group_1 <- lapply(seq(1, 25, by = 5), function(x) {
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
      
      # Calculate disagreement within group 2
      dis_group_2 <- lapply(seq(1, 25, by = 5), function(x) {
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
      
      # Calculate total disagreement
      dis_total <- lapply(seq(1, 25, by = 5), function(x) {
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
      
      # Flatten list and coerce to data frame
      dis_group_1 <- flatten_dbl(dis_group_1) %>% data.frame()
      dis_group_2 <- flatten_dbl(dis_group_2) %>% data.frame()
      dis_total <- flatten_dbl(dis_total) %>% data.frame()
      
      # Create row names
      alternatives <- c("a", "b", "c", "d", "e") %>% data.frame() 
      
      # Add column names to data frames
      colnames(alternatives) <- "x"
      colnames(dis_group_1) <- "y1"
      colnames(dis_group_2) <- "y2"
      colnames(dis_total) <- "y3"
      
      # Bind data frames together
      dis_data <- bind_cols(alternatives, dis_group_1, dis_group_2, dis_total)
      
      # Plot group 1 disagreement 
      dis_data %>%
        ggvis(x = ~x, y = ~y1 * 100, fill := "steelblue", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Disagreement", format = "d", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny(plot_id = "ggvis_7")
      incProgress(amount = 1/16, detail = "Plot 7")
      
      # Plot group 2 disagreement 
      dis_data %>%
        ggvis(x = ~x, y = ~y2 * 100, fill := "firebrick", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Disagreement", format = "d", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny(plot_id = "ggvis_8")
      incProgress(amount = 1/16, detail = "Plot 8")
      
      # Plot total disagreement
      dis_data %>%
        ggvis(x = ~x, y = ~y3 * 100, fill := "darkslateblue", stroke := "") %>%
        add_axis(type = "x", title = "Alternatives", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Disagreement", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 40) %>%
        set_options(width = "auto", height = "200") %>%
        layer_bars() %>%
        bind_shiny(plot_id = "ggvis_9")
      incProgress(amount = 1/16, detail = "Plot 9")
      
      ### PORTFOLIOS ##################################################################################################
      
      # Unlist values
      val_group_1 <- unlist(val_group_1)
      val_group_2 <- unlist(val_group_2)
      val_group_1_2 <- unlist(val_group_1_2)
      dis_group_1 <- unlist(dis_group_1)
      dis_group_2 <- unlist(dis_group_2)
      dis_total <- unlist(dis_total)
      actions <- c("A1","A2","A3","A4","A5")
      
      # Set initial budget constraint
      budget_group_1 = sum(dis_group_1)
      budget_group_2 = sum(dis_group_2)
      budget_total = sum(dis_total)
    
      # Generate portfolios
      
      # Group 1 positive
      portfolios_group_1_pos <- get_all_portfolios(
        actions = actions, 
        values = val_group_1, 
        disagreements = dis_group_1, 
        initial_budget_constraint = budget_group_1, 
        direction = "max")
      
      # Group 1 negative
      portfolios_group_1_neg <- get_all_portfolios(
        actions = actions, 
        values = val_group_1, 
        disagreements = dis_group_1, 
        initial_budget_constraint = budget_group_1, 
        direction = "min")
      
      # Group 2 positive
      portfolios_group_2_pos <- get_all_portfolios(
        actions = actions, 
        values = val_group_2, 
        disagreements = dis_group_2, 
        initial_budget_constraint = budget_group_2, 
        direction = "max")
      
      # Group 2 negative
      portfolios_group_2_neg <- get_all_portfolios(
        actions = actions, 
        values = val_group_2, 
        disagreements = dis_group_2, 
        initial_budget_constraint = budget_group_2, 
        direction = "min")
      
      # Group total positive
      portfolios_total_pos <- get_all_portfolios(
        actions = actions, 
        values = val_group_1_2, 
        disagreements = dis_total, 
        initial_budget_constraint = budget_total, 
        direction = "max")
      
      # Group total negative
      portfolios_total_neg <- get_all_portfolios(
        actions = actions, 
        values = val_group_1_2, 
        disagreements = dis_total, 
        initial_budget_constraint = budget_total, 
        direction = "min")
      
      # Combine and prepare data frames
      portfolios_group_1_pos_rev <- portfolios_group_1_pos[rev(rownames(portfolios_group_1_pos)),]
      portfolios_group_2_pos_rev <- portfolios_group_2_pos[rev(rownames(portfolios_group_2_pos)),]
      portfolios_total_pos_rev <- portfolios_total_pos[rev(rownames(portfolios_total_pos)),]
      
      # Prepare portfolios for group 1
      portfolios_group_1 <- rbind(portfolios_group_1_pos_rev, portfolios_group_1_neg[-1,])
      portfolios_group_1$id <- 1:nrow(portfolios_group_1)
      portfolios_group_1$dx <- rep_len(c(-10, 5), length.out = nrow(portfolios_group_1))
      portfolios_group_1$dy <- rep_len(c(-5, 10), length.out = nrow(portfolios_group_1))

      # Prepare portfolios for group 2
      portfolios_group_2 <- rbind(portfolios_group_2_pos_rev, portfolios_group_2_neg[-1,])
      portfolios_group_2$id <- 1:nrow(portfolios_group_2)
      portfolios_group_2$dx <- rep_len(c(-10, 5), length.out = nrow(portfolios_group_2))
      portfolios_group_2$dy <- rep_len(c(-5, 10), length.out = nrow(portfolios_group_2))
      
      # Prepare portfolios for total
      portfolios_total <- rbind(portfolios_total_pos_rev, portfolios_total_neg[-1,])
      portfolios_total$id <- 1:nrow(portfolios_total)
      portfolios_total$dx <- rep_len(c(-10, 5), length.out = nrow(portfolios_total))
      portfolios_total$dy <- rep_len(c(-5, 10), length.out = nrow(portfolios_total))
      
      # Functions for tooltips
      tooltip_1 <- function(x) {
        if (is.null(x)) return(NULL)
        row <- portfolios_group_1[portfolios_group_1$id == x$id, ]
        row$dx <- row$dy <- NULL
        paste0(names(row), ": ", format(x = row, digits = 1), collapse = "<br />")
      }
      tooltip_2 <- function(x) {
        if (is.null(x)) return(NULL)
        row <- portfolios_group_2[portfolios_group_2$id == x$id, ]
        row$dx <- row$dy <- NULL
        paste0(names(row), ": ", format(x = row, digits = 1), collapse = "<br />")
      }
      tooltip_total <- function(x) {
        if (is.null(x)) return(NULL)
        row <- portfolios_total[portfolios_total$id == x$id, ]
        row$dx <- row$dy <- NULL
        paste0(names(row), ": ", format(x = row, digits = 1), collapse = "<br />")
      }
      
      # Plot group 1 portfolios
      portfolios_group_1 %>%
        ggvis(x = ~disagreement * 100, y = ~value * 100, key := ~id) %>%
        add_axis(type = "x", title = "Disagreement", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Value", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        set_options(width = "auto", height = "200") %>%
        layer_text(text := ~id, fill := "steelblue", fontSize := 8, dx := ~dx, dy := ~dy) %>%
        layer_points(fillOpacity := 0, stroke := "steelblue") %>%
        layer_paths(stroke := "steelblue") %>%
        add_tooltip(html = tooltip_1, on = "hover") %>%
        bind_shiny(plot_id = "ggvis_10")
      incProgress(amount = 1/16, detail = "Plot 10")
      
      # Plot group 2 portfolios
      portfolios_group_2 %>%
        ggvis(x = ~disagreement * 100, y = ~value * 100, key := ~id) %>%
        add_axis(type = "x", title = "Disagreement", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Value", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        set_options(width = "auto", height = "200") %>%
        layer_text(text := ~id, fill := "firebrick", fontSize := 8, dx := ~dx, dy := ~dy) %>%
        layer_points(fillOpacity := 0, stroke := "firebrick") %>%
        layer_paths(stroke := "firebrick") %>%
        add_tooltip(html = tooltip_2, on = "hover") %>%
        bind_shiny(plot_id = "ggvis_11")
      incProgress(amount = 1/16, detail = "Plot 11")
  
      # Plot total portfolios
      portfolios_total %>%
        ggvis(x = ~disagreement * 100, y = ~value * 100, key := ~id) %>%
        add_axis(type = "x", title = "Disagreement", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Value", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        set_options(width = "auto", height = "200") %>%
        layer_text(text := ~id, fill := "darkslateblue", fontSize := 8, dx := ~dx, dy := ~dy) %>%
        layer_points(fillOpacity := 0, stroke := "darkslateblue") %>%
        layer_paths(stroke := "darkslateblue") %>%
        add_tooltip(html = tooltip_total, on = "hover") %>%
        bind_shiny(plot_id = "ggvis_12")
      incProgress(amount = 1/16, detail = "Plot 12")
      
      ### DEMOGRAPHICS ###################################################################################################
      
      # output$pyramid_plot <- renderPlot({
      #   ggplot(data = gender_age, aes(x = Age, y = n, fill = Gender)) +
      #     geom_bar(data = filter(gender_age, Gender == "Woman"), fill = "steelblue", stat = "identity") +
      #     geom_bar(data = filter(gender_age, Gender == "Man"), fill = "firebrick", stat = "identity", position = "identity") +
      #     scale_y_continuous(breaks = seq(-30, 30, 10), labels = seq(-30, 30, 10)) +
      #     coord_flip()
      # })
      
      dem_group_1 <- tdata$group_1_filter_1()@data %>% tbl_df()
      dem_group_2 <- tdata$group_2_filter_1()@data %>% tbl_df()
      
      gender_group_1 <- dem_group_1 %>%
        select(Gender) %>%
        filter(Gender == "Woman" | Gender == "Man") %>%
        droplevels() %>%
        group_by(Gender) %>%
        count()
      
      gender_group_2 <- dem_group_2 %>%
        select(Gender) %>%
        filter(Gender == "Woman" | Gender == "Man") %>%
        droplevels() %>%
        group_by(Gender) %>%
        count()
      
      age_group_1 <- dem_group_1 %>%
        select(Age) %>%
        group_by(Age) %>%
        count()
      
      age_group_2 <- dem_group_2 %>%
        select(Age) %>%
        group_by(Age) %>%
        count()
      
      occupation_group_1 <- dem_group_1 %>%
        mutate(Occupation = fct_recode(f = Occupation, 
          "Long-term sick leave"  = "Long-term sick leave (more than 3 months)",
          "Sickness benefit"      = "Sickness or activity benefit"
        )) %>%
        select(Occupation) %>%
        group_by(Occupation) %>%
        count()
      
      occupation_group_2 <- dem_group_2 %>%
        mutate(Occupation = fct_recode(f = Occupation, 
          "Long-term sick leave"  = "Long-term sick leave (more than 3 months)",
          "Sickness benefit"      = "Sickness or activity benefit"
        )) %>%
        select(Occupation) %>%
        group_by(Occupation) %>%
        count()
      
      education_group_1 <- dem_group_1 %>%
        mutate(Education.level = fct_recode(f = Education.level, 
          "University"            = "College/University",
          "Elem. school"          = "Elementary school or equivalent compulsory school",
          "High school"           = "High school, Nordic folk high school, or equivalent",
          "No elem. school"       = "No elementary or equivalent compulsary school",
          "Other"                 = "Other post-secondary education"
        )) %>%
        select(Education.level) %>%
        group_by(Education.level) %>%
        count()
      
      education_group_2 <- dem_group_2 %>%
        mutate(Education.level = fct_recode(f = Education.level, 
          "University"            = "College/University",
          "Elem. school"          = "Elementary school or equivalent compulsory school",
          "High school"           = "High school, Nordic folk high school, or equivalent",
          "No elem. school"       = "No elementary or equivalent compulsary school",
          "Other"                 = "Other post-secondary education"
        )) %>%
        select(Education.level) %>%
        group_by(Education.level) %>%
        count()
      
      year_group_1 <- dem_group_1 %>%
        select(Year) %>%
        group_by(Year) %>%
        count()
      
      year_group_2 <- dem_group_2 %>%
        select(Year) %>%
        group_by(Year) %>%
        count()
      
      gender <- bind_rows(gender_group_1, gender_group_2, .id = "Group") %>%
        mutate(Gender = reorder(Gender, n)) %>%
        top_n(n = 4, wt = n) %>%
        droplevels()
      
      age <- bind_rows(age_group_1, age_group_2, .id = "Group")
      
      occupation <- bind_rows(occupation_group_1, occupation_group_2, .id = "Group") %>% 
        mutate(Occupation = reorder(Occupation, n)) %>%
        top_n(n = 10, wt = n) %>%
        droplevels()
      
      education <- bind_rows(education_group_1, education_group_2, .id = "Group") %>%
        mutate(Education = reorder(Education.level, n)) %>%
        select(Group, Education, n) %>%
        top_n(n = 10, wt = n) %>%
        droplevels()
      
      year <- bind_rows(year_group_1, year_group_2, .id = "Group") %>%
        mutate(Year = fct_recode(f = Year, "10+ years" = "10 or more years")) %>%
        mutate(Year = fct_relevel(f = Year, "0-4 years", "5-9 years", "10+ years")) %>%
        droplevels()
      
      # Plot Gender
      gender %>%
        ggvis(y = ~Gender, x = ~n, fill = ~Group, stroke := "") %>%
        scale_nominal(property = "fill", range = c("steelblue", "firebrick")) %>%
        add_axis(type = "x", title = "Count", ticks = 5, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Gender", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 60) %>%
        compute_stack(stack_var = ~n, group_var = ~Gender) %>%
        layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
        hide_legend(scales = "fill") %>%
        set_options(width = "auto", height = "200") %>%
        bind_shiny(plot_id = "gender")
      incProgress(amount = 1/16, detail = "Plot 13")
      
      # Plot Age
      age %>%
        ggvis(y = ~Age, x = ~n, fill = ~Group, stroke := "") %>%
        scale_ordinal(property = "fill", range = c("steelblue", "firebrick")) %>%
        scale_ordinal(property = "y", reverse = TRUE) %>%
        add_axis(type = "x", title = "Count", ticks = 8, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Age", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 45) %>%
        compute_stack(stack_var = ~n, group_var = ~Age) %>%
        layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
        hide_legend(scales = "fill") %>%
        set_options(width = "auto", height = "200") %>%
        bind_shiny(plot_id = "age")
      incProgress(amount = 1/16, detail = "Plot 14")
      
      # Plot Occupation
      occupation %>%
        ggvis(y = ~Occupation, x = ~n, fill = ~Group, stroke := "") %>%
        scale_nominal(property = "fill", range = c("steelblue", "firebrick")) %>%
        scale_nominal(property = "y", reverse = TRUE) %>%
        add_axis(type = "x", title = "Count", ticks = 5, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Occupation", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 90) %>%
        compute_stack(stack_var = ~n, group_var = ~Occupation) %>%
        layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
        hide_legend(scales = "fill") %>%
        set_options(width = "auto", height = "200") %>%
        bind_shiny(plot_id = "occupation")
      incProgress(amount = 1/16, detail = "Plot 15")
      
      # Plot Education
      education %>%
        ggvis(y = ~Education, x = ~n, fill = ~Group, stroke := "") %>%
        scale_nominal(property = "fill", range = c("steelblue", "firebrick")) %>%
        scale_nominal(property = "y", reverse = TRUE) %>%
        add_axis(type = "x", title = "Count", ticks = 5, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Education", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 80) %>%
        compute_stack(stack_var = ~n, group_var = ~Education) %>%
        layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
        hide_legend(scales = "fill") %>%
        set_options(width = "auto", height = "200") %>%
        bind_shiny(plot_id = "education")
      incProgress(amount = 1/16, detail = "Plot 16")
      
      # Plot Years
      year %>%
        ggvis(y = ~Year, x = ~n, fill = ~Group, stroke := "") %>%
        scale_nominal(property = "fill", range = c("steelblue", "firebrick")) %>%
        scale_nominal(property = "y", reverse = TRUE) %>%
        add_axis(type = "x", title = "Count", ticks = 5, grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8))) %>%
        add_axis(type = "y", title = "Length of residency", grid = FALSE, properties = axis_props(title = list(fontSize = 8), labels = list(fontSize = 8)), title_offset = 60) %>%
        compute_stack(stack_var = ~n, group_var = ~Year) %>%
        layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
        hide_legend(scales = "fill") %>%
        set_options(width = "auto", height = "200") %>%
        bind_shiny(plot_id = "year")
      incProgress(amount = 1/16, detail = "Plot 17")
      
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
  
  # color <- colorRampPalette(brewer.pal(n = 11, name = "RdYlGn"))(15)
  # barplot(height = 0:14, beside = TRUE, names = "RdYlGn", col = color)
  # display.brewer.pal(11, "RdYlGn")
  
  output$table <- DT::renderDataTable({
    table_filter() %>%
      datatable(filter = "top", options = list(pageLength = 10)) %>%
      formatStyle(columns = c(7:56, 67:69), backgroundColor = styleInterval(cuts = c(0:13), 
        values = colorRampPalette(brewer.pal(n = 11, name = tdata$colorpal()))(15))) %>%
      formatStyle(columns = 57:66, background = styleColorBar(data = 0:15, color = "lightblue", angle = -90))
  })
  
})