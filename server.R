shinyServer(function(input, output, session) {
  
  tdata <- callModule(module = menu, id = "one")
  
  # BOOKMARKING ###############################################################
  
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c("bookmark1", "bookmark2"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  
  # INTRO JS ##################################################################
  
  observeEvent(input$help,
    introjs(
      session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Previous",
        "skipLabel" = "Close"
      ),
      events = list("oncomplete" = I('alert("End")'))))
  
  # MAP #######################################################################
  
  ## Create static map and polygon wireframes
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 17.91128,
        lat = 59.51839,
        zoom = 12) %>%
      addPolygons(
        data = nyko,
        fill = TRUE,
        fillOpacity = 0.1,
        fillColor = "blue",
        stroke = TRUE,
        weight = 0.7,
        color = "black",
        group = "nyko"
      ) %>%
      addMeasure() %>%
      # addGraticule(interval = 1, group = "Graticule") %>%
      # addTerminator(group = "Daylight") %>%
      # addLayersControl(overlayGroups = c("Graticule", "Daylight"), options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap(toggleDisplay = TRUE,
        width = 200,
        height = 200)
  })
  
  ## Define color palette
  colorpal <- reactive({
    colorNumeric(
      palette = tdata$colorpal(),
      domain = c(0, 14),
      na.color = "gray"
    )
  })
  
  ## Add color legend to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearControls() %>%
      addLegend(
        position = "bottomleft",
        pal = colorpal(),
        values = c(0:14),
        labels = c("Min", "Mean", "Max")
      )
  })
  
  ## Add polygons to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearGroup(group = "group1Polygons") %>%
      clearGroup(group = "group2Polygons")
    
    ### Group 1
    leafletProxy(mapId = "map") %>%
      addPolygons(
        data = tdata$group_1_filter_1(),
        fill = TRUE,
        fillColor = ~ colorpal()(tdata$group_1_mean()),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        color = "steelblue",
        layerId = tdata$group_1_filter_1()$Area,
        group = "group1Polygons",
        highlightOptions = highlightOptions(
          color = "blue",
          weight = 3,
          bringToFront = TRUE
        )
      )
    
    ### Group 2
    leafletProxy(mapId = "map") %>%
      addPolygons(
        data = tdata$group_2_filter_1(),
        fill = TRUE,
        fillColor = ~ colorpal()(tdata$group_2_mean()),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        color = "firebrick",
        layerId = tdata$group_2_filter_1()$Area,
        group = "group2Polygons",
        highlightOptions = highlightOptions(
          color = "red",
          weight = 3,
          bringToFront = TRUE
        )
      )
  })
  
  ## Add markers to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearMarkers()
    
    ### Group 1
    if (tdata$markers1() == TRUE) {
      leafletProxy(mapId = "map") %>%
        addMarkers(
          data = tdata$group_1_filter_1(),
          lng = ~ long,
          lat = ~ lat,
          popup = tdata$group_1_filter_1()$Area,
          layerId = tdata$group_1_filter_1()$Area,
          options = markerOptions(
            title = paste(
              tdata$group_1_filter_1()$Area,
              tdata$group_1_mean(),
              sep = ": "
            )
          )
        )
    }
    
    ### Group 2
    if (tdata$markers2() == TRUE) {
      leafletProxy(mapId = "map") %>%
        addMarkers(
          data = tdata$group_2_filter_1(),
          lng = ~ long,
          lat = ~ lat,
          popup = tdata$group_2_filter_1()$Area,
          layerId = tdata$group_2_filter_1()$Area,
          options = markerOptions(
            title = paste(
              tdata$group_2_filter_1()$Area,
              tdata$group_2_mean(),
              sep = ": "
            )
          ),
          icon = list(
            iconUrl = "images/marker-icon-red.png",
            iconWidth = 25,
            iconHeight = 41,
            iconAnchorX = 0,
            iconAnchorY = 0,
            shadowUrl = "images/marker-shadow.png",
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
  
  ## Add popups to map
  observe({
    leafletProxy(mapId = "map") %>%
      clearPopups()
    
    ### Group 1
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
    
    ### Group 2
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
  
  # PLOTS #####################################################################
  
  observe({
        
        # F V D P PANEL #######################################################
        
        ## F TAB ##############################################################
        
        ### Get data for F panel
        des_group_1 <- tdata$group_1_filter_2() %>% as.vector()
        des_group_2 <- tdata$group_2_filter_2() %>% as.vector()
        
        ### Concatenate and coerce into data frame
        des_total <- c(des_group_1, des_group_2) %>% data.frame()
        
        ### Coerce into data frame
        des_group_1 <- data.frame(des_group_1)
        des_group_2 <- data.frame(des_group_2)
        
        ### Plot values group 1
        des_group_1 %>%
          ggvis(x = ~ des_group_1, fill := "steelblue", stroke := "") %>%
          layer_histograms(width = 1) %>%
          scale_numeric(property = "x", domain = c(0, 14)) %>%
          scale_numeric(property = "y", domain = c(0, NA)) %>%
          add_axis(
            type = "x",
            title = "Value",
            ticks = 7,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Count",
            ticks = 6,
            format = "d",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_1")
        
        ### Plot values group 2
        des_group_2 %>%
          ggvis(x = ~ des_group_2, fill := "firebrick", stroke := "") %>%
          layer_histograms(width = 1) %>%
          scale_numeric(property = "x", domain = c(0, 14)) %>%
          scale_numeric(property = "y", domain = c(0, NA)) %>%
          add_axis(
            type = "x",
            title = "Value",
            ticks = 7,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Count",
            ticks = 6,
            format = "d",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_2")
        
        ### Plot total values
        des_total %>%
          ggvis(~ ., fill := "darkslateblue", stroke := "") %>%
          layer_histograms(width = 1) %>%
          scale_numeric(property = "x", domain = c(0, 14)) %>%
          scale_numeric(property = "y", domain = c(0, NA)) %>%
          add_axis(
            type = "x",
            title = "Value",
            ticks = 7,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Count",
            ticks = 6,
            format = "d",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_3")
        
        ## V TAB ##############################################################
        
        ### Get data for V panel
        data.vec1 <-
          distance(tdata$theme(), tdata$group_1_filter_1())
        data.vec2 <-
          distance(tdata$theme(), tdata$group_2_filter_1())
        data.vec12 <- distance(tdata$theme(), rbind(tdata$group_1_filter_1(), tdata$group_2_filter_1()))
        # data.vec1.sd <-
        #   calculateSD(tdata$theme(), tdata$group_1_filter_1())
        # data.vec2.sd <-
        #   calculateSD(tdata$theme(), tdata$group_2_filter_1())
        
        ### NEW Calculate pro-index for group 1
        # pro_group_1 <- lapply(seq(1, 40, by = 8), function(x) {
        #   proIdx = data.vec1[x + 4]
        #   return(proIdx)
        # })
        
        ### NEW Calculate con-index for group 1
        # con_group_1 <- lapply(seq(1, 40, by = 8), function(x) {
        #   conIdx = data.vec1[x+3]
        #   return(conIdx)
        # })
        
        ### NEW Calculate pro-index for group 2
        # pro_group_2 <- lapply(seq(1, 40, by = 8), function(x) {
        #   proIdx = data.vec2[x + 4]
        #   return(proIdx)
        # })
        
        ### NEW Calculate con-index for group 2
        # con_group_2 <- lapply(seq(1, 40, by = 8), function(x) {
        #   conIdx = data.vec2[x+3]
        #   return(conIdx)
        # })
        
        ### NEW Calculate total pro-index for group 1 and group 2
        # pro_group_1_2 <- lapply(seq(1, 40, by = 8), function(x) {
        #   proIdx1 <- data.vec1[x + 4]
        #   proIdx2 <- data.vec2[x + 4]
        #   proIdx_1_2 = proIdx1 + proIdx2
        #   return(proIdx_1_2)
        # })
        
        ### NEW Calculate total con-index for group 1 and group 2
        # con_group_1_2 <- lapply(seq(1, 40, by = 8), function(x) {
        #   conIdx1 <- data.vec1[x+3]
        #   conIdx2 <- data.vec2[x+3]
        #   conIdx_1_2 = conIdx1 + conIdx2
        #   return(conIdx_1_2)
        # })
        
        ### Calculate group 1 mean weighted values
        val_group_1 <- lapply(seq(1, 45, by = 9), function(x) {
          lambda = 1 / (data.vec1[x + 6] + data.vec1[x + 7])
          return(data.vec1[x + 8] * lambda) 
        })
        
        ### Calculate group 2 mean weighted values
        val_group_2 <- lapply(seq(1, 45, by = 9), function(x) {
          lambda = 1 / (data.vec2[x + 6] + data.vec2[x + 7])
          return(data.vec2[x + 8] * lambda)
        })
        
        ### Calculate total mean weighted values
        val_group_1_2 <- lapply(seq(1, 45, by = 9), function(x) {
          # n_grp1 <- (data.vec1[x + 6] + data.vec1[x + 5])
          # n_grp2 <- (data.vec2[x + 6] + data.vec2[x + 5])
          # v_grp1 <- data.vec1[x + 5]
          # v_grp2 <- data.vec2[x + 5]
          # org_v_grp_1 <- v_grp1 / (1 / n_grp1)
          # org_v_grp_2 <- v_grp2 / (1 / n_grp2)
          # m_grp_1_2 <-
          #   (org_v_grp_1 + org_v_grp_2) / (n_grp1 + n_grp2)
          # return(m_grp_1_2)
          lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7])
          return(data.vec12[x + 8] * lambda)
        })
        
        ### Flatten lists and transform into data frames
        val_group_1_df <-
          val_group_1 %>% flatten_dbl() %>% data.frame()
        val_group_2_df <-
          val_group_2 %>% flatten_dbl() %>% data.frame()
        val_group_1_2_df <-
          val_group_1_2 %>% flatten_dbl() %>% data.frame()
        
        ### Create row names
        alternatives <-
          c("a", "b", "c", "d", "e") %>% data.frame()
        
        ### Add column names to data frames
        colnames(alternatives) <- "x"
        colnames(val_group_1_df) <- "y1"
        colnames(val_group_2_df) <- "y2"
        colnames(val_group_1_2_df) <- "y3"
        
        ### Bind data frames together
        val_data <-
          bind_cols(alternatives,
            val_group_1_df,
            val_group_2_df,
            val_group_1_2_df)
        
        ### Plot group 1 mean weighted values
        val_data %>%
          ggvis(x = ~ x,
            y = ~ y1,
            fill := "steelblue",
            stroke := "") %>%
          layer_bars() %>%
          scale_numeric(property = "y", domain = c(ifelse(min(val_data$y1) < 0, min(val_data$y1) * 1.1, 0), max(val_data$y1))) %>%
          add_axis(
            type = "x",
            title = "Alternative",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Value",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_4")
        
        ### Plot group 2 mean weighted values
        val_data %>%
          ggvis(x = ~ x,
            y = ~ y2,
            fill := "firebrick",
            stroke := "") %>%
          layer_bars() %>%
          scale_numeric(property = "y", domain = c(ifelse(min(val_data$y2) < 0, min(val_data$y2) * 1.1, 0), max(val_data$y2))) %>%
          add_axis(
            type = "x",
            title = "Alternative",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Value",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_5")
        
        ### Plot total mean weighted values
        val_data %>%
          ggvis(x = ~ x,
            y = ~ y3,
            fill := "darkslateblue",
            stroke := "") %>%
          layer_bars() %>%
          scale_numeric(property = "y", domain = c(ifelse(min(val_data$y3) < 0, min(val_data$y3) * 1.1, 0), max(val_data$y3))) %>%
          add_axis(
            type = "x",
            title = "Alternative",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Value",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_6")
        
        ## D TAB ##############################################################
        
        ### Calculate distance within group 1
        dis_group_1 <- lapply(seq(1, 45, by = 9), function(x) {
          lambda = 1 / (data.vec1[x + 6] + data.vec1[x + 7]) ^ 2
          beta <- 1 / (lambda * (data.vec1[x + 6] + data.vec1[x + 7]))
          sqrt(beta * (data.vec1[x] * lambda - (data.vec1[x + 1] * lambda + data.vec1[x + 2] * lambda)))
        })
        
        # print("group 1")
        # tdv1 <- data.vec1*(1/(data.vec1[7]+data.vec1[8])^2)
        # print(data.vec1)
        # print("cvar")
        # cat(tdv1[2],tdv1[11],tdv1[20],tdv1[29],tdv1[38], sep=",")
        # print("pvar")
        # cat(tdv1[3],tdv1[12],tdv1[21],tdv1[30],tdv1[39], sep=",")
        # print("var")
        # cat(tdv1[1],tdv1[10],tdv1[19],tdv1[28],tdv1[37],sep=",")
        # print(dis_group_1)
        
        ### Calculate distance within group 2
        dis_group_2 <- lapply(seq(1, 45, by = 9), function(x) {
          lambda = 1 / (data.vec2[x + 6] + data.vec2[x + 7]) ^ 2
          beta <- 1 / (lambda * (data.vec2[x + 6] + data.vec2[x + 7]))
          sqrt(beta * (data.vec2[x] * lambda - (data.vec2[x + 1] * lambda + data.vec2[x + 2] * lambda)))
        })
        
        ### Calculate total distance
        dis_total <- lapply(seq(1, 45, by = 9), function(x) {
          lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7]) ^ 2
          beta <- 1 / (lambda * (data.vec12[x + 6] + data.vec12[x + 7]))
          T1 <- data.vec1[x] * lambda
          T2 <- data.vec2[x] * lambda
          T12 <- data.vec12[x] * lambda
          C1 <- data.vec1[x + 1] * lambda
          C2 <- data.vec2[x + 1] * lambda
          C12 <- data.vec12[x + 1] * lambda
          P1 <- data.vec1[x + 2] * lambda
          P2 <- data.vec2[x + 2] * lambda
          P12 <- data.vec12[x + 2] * lambda
          sqrt(beta * abs((T12 - (T1 + T2)) - ((C12 - (C1 + C2)) + (P12 - (P1 + P2)))))
        })
        
        # print("group 1")
        # tdv1_2g <- data.vec1*(1/(data.vec12[7]+data.vec12[8])^2)
        # print(tdv1_2g)
        # print("cvar")
        # cat(tdv1_2g[2],tdv1_2g[11],tdv1_2g[20],tdv1_2g[29],tdv1_2g[38], sep=",")
        # print("pvar")
        # cat(tdv1_2g[3],tdv1_2g[12],tdv1_2g[21],tdv1_2g[30],tdv1_2g[39], sep=",")
        # print("var")
        # cat(tdv1_2g[1],tdv1_2g[10],tdv1_2g[19],tdv1_2g[28],tdv1_2g[37],sep=",")
        # 
        # print("group 2")
        # tdv2_2g <- data.vec2*(1/(data.vec12[7]+data.vec12[8])^2)
        # print(data.vec2)
        # print("cvar")
        # cat(tdv2_2g[2],tdv2_2g[11],tdv2_2g[20],tdv2_2g[29],tdv2_2g[38], sep=",")
        # print("pvar")
        # cat(tdv2_2g[3],tdv2_2g[12],tdv2_2g[21],tdv2_2g[30],tdv2_2g[39], sep=",")
        # print("var")
        # cat(tdv2_2g[1],tdv2_2g[10],tdv2_2g[19],tdv2_2g[28],tdv2_2g[37],sep=",")
        # 
        # print("group total")
        # tdv12 <- data.vec12*(1/(data.vec12[7]+data.vec12[8])^2)
        # print(data.vec12)
        # print("cvar")
        # cat(tdv12[2],tdv12[11],tdv12[20],tdv12[29],tdv12[38], sep=",")
        # print("pvar")
        # cat(tdv12[3],tdv12[12],tdv12[21],tdv12[30],tdv12[39], sep=",")
        # print("var")
        # cat(tdv12[1],tdv12[10],tdv12[19],tdv12[28],tdv12[37],sep=",")
        # print(dis_total)

        ### Flatten list and coerce to data frame
        dis_group_1 <- flatten_dbl(dis_group_1) %>% data.frame()
        dis_group_2 <- flatten_dbl(dis_group_2) %>% data.frame()
        dis_total <- flatten_dbl(dis_total) %>% data.frame()
        
        ### Create row names
        alternatives <-
          c("a", "b", "c", "d", "e") %>% data.frame()
        
        # Add column names to data frames
        colnames(alternatives) <- "x"
        colnames(dis_group_1) <- "y1"
        colnames(dis_group_2) <- "y2"
        colnames(dis_total) <- "y3"
        
        ### Bind data frames together
        dis_data <-
          bind_cols(alternatives, dis_group_1, dis_group_2, dis_total)
        
        ### Plot group 1 distance
        dis_data %>%
          ggvis(x = ~ x,
            y = ~ y1,
            fill := "steelblue",
            stroke := "") %>%
          layer_bars() %>%
          add_axis(
            type = "x",
            title = "Alternative",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Distance",
            format = "d",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_7")
        
        ### Plot group 2 distance
        dis_data %>%
          ggvis(x = ~ x,
            y = ~ y2,
            fill := "firebrick",
            stroke := "") %>%
          layer_bars() %>%
          add_axis(
            type = "x",
            title = "Alternative",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Distance",
            format = "d",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_8")
        
        ### Plot total distance
        dis_data %>%
          ggvis(x = ~ x,
            y = ~ y3,
            fill := "darkslateblue",
            stroke := "") %>%
          layer_bars() %>%
          add_axis(
            type = "x",
            title = "Alternative",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Distance",
            format = "####",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 40
          ) %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_9")
        
        
        ## P TAB ##############################################################
        
        ### NEW Con-index constrained portfolios (maximise pro-index)
        # Unlist lists
        # pro_group_1 <- unlist(pro_group_1)
        # pro_group_2 <- unlist(pro_group_2)
        # pro_group_1_2 <- unlist(pro_group_1_2)
        # con_group_1 <- unlist(con_group_1)
        # con_group_2 <- unlist(con_group_2)
        # con_group_1_2 <- unlist(con_group_1_2)
        
        # NEW hack
        # val_group_1 <- unlist(pro_group_1)
        # val_group_2 <- unlist(pro_group_2)
        # val_group_1_2 <- unlist(pro_group_1_2)
        # dis_group_1 <- unlist(con_group_1)
        # dis_group_2 <- unlist(con_group_2)
        # dis_total <- unlist(con_group_1_2)

        ### Unlist lists
        val_group_1 <- unlist(val_group_1)
        val_group_2 <- unlist(val_group_2) 
        val_group_1_2 <- unlist(val_group_1_2)
        dis_group_1 <- unlist(dis_group_1)
        dis_group_2 <- unlist(dis_group_2)
        dis_total <- unlist(dis_total)
        
        ### Set initial budget constraints
        budget_group_1 <- sum(dis_group_1)
        budget_group_2 <- sum(dis_group_2) 
        budget_total <- sum(dis_total)
        
        ### Portfolios group 1 for table
        portfolios_group_1_pos <- get_all_portfolios(
          actions = actions,
          values = val_group_1,
          distance = dis_group_1,
          initial_budget_constraint = budget_group_1,
          direction = "max"
        )
        portfolios_group_1_neg <- get_all_portfolios(
          actions = actions,
          values = val_group_1,
          distance = dis_group_1,
          initial_budget_constraint = budget_group_1,
          direction = "min"
        )
        portfolios_group_1_pos_rev <-
          portfolios_group_1_pos[rev(rownames(portfolios_group_1_pos)), ]
        portfolios_group_1 <-
          rbind(portfolios_group_1_pos_rev, portfolios_group_1_neg[-1, ])
        portfolios_group_1$id <- 1:nrow(portfolios_group_1)
        portfolios_group_1$value <- portfolios_group_1$value
        portfolios_group_1$distance <-
          portfolios_group_1$distance
        
        ### Portfolios group 1 for plotting
        all_portfolios_group_1 <-
          expand.grid(0:1, 0:1, 0:1, 0:1, 0:1)
        names(all_portfolios_group_1) <- actions
        all_portfolio_val_group_1 <-
          apply(all_portfolios_group_1, 1, function(row) {
            sum(val_group_1[which(row %in% 1)])
          })
        all_portfolio_dis_group_1 <-
          apply(all_portfolios_group_1, 1, function(row) {
            sum(dis_group_1[which(row %in% 1)])
          })
        all_portfolios_group_1$value <-
          all_portfolio_val_group_1
        all_portfolios_group_1$distance <-
          all_portfolio_dis_group_1
        all_portfolios_group_1 <-
          full_join(portfolios_group_1, all_portfolios_group_1, by = actions)
        all_portfolios_group_1$dx <-
          rep_len(c(-10, 5), length.out = nrow(all_portfolios_group_1))
        all_portfolios_group_1$dy <-
          rep_len(c(-5, 10), length.out = nrow(all_portfolios_group_1))
        all_portfolios_group_1$id <-
          1:nrow(all_portfolios_group_1)
        
        ### Portfolios group 2 for table
        portfolios_group_2_pos <- get_all_portfolios(
          actions = actions,
          values = val_group_2,
          distance = dis_group_2,
          initial_budget_constraint = budget_group_2,
          direction = "max"
        )
        portfolios_group_2_neg <- get_all_portfolios(
          actions = actions,
          values = val_group_2,
          distance = dis_group_2,
          initial_budget_constraint = budget_group_2,
          direction = "min"
        )
        portfolios_group_2_pos_rev <-
          portfolios_group_2_pos[rev(rownames(portfolios_group_2_pos)),]
        portfolios_group_2 <-
          rbind(portfolios_group_2_pos_rev, portfolios_group_2_neg[-1, ])
        portfolios_group_2$id <- 1:nrow(portfolios_group_2)
        portfolios_group_2$value <- portfolios_group_2$value
        portfolios_group_2$distance <-
          portfolios_group_2$distance
        
        ### Portfolios group 2 for plotting
        all_portfolios_group_2 <-
          expand.grid(0:1, 0:1, 0:1, 0:1, 0:1)
        names(all_portfolios_group_2) <- actions
        all_portfolios_val_group_2 <-
          apply(all_portfolios_group_2, 1, function(row) {
            val <- sum(val_group_2[which(row %in% 1)])
          })
        all_portfolios_dis_group_2 <-
          apply(all_portfolios_group_2, 1, function(row) {
            dis <- sum(dis_group_2[which(row %in% 1)])
          })
        all_portfolios_group_2$value <-
          all_portfolios_val_group_2
        all_portfolios_group_2$distance <-
          all_portfolios_dis_group_2
        all_portfolios_group_2 <-
          full_join(portfolios_group_2, all_portfolios_group_2, by = actions)
        all_portfolios_group_2$dx <-
          rep_len(c(-10, 5), length.out = nrow(all_portfolios_group_2))
        all_portfolios_group_2$dy <-
          rep_len(c(-5, 10), length.out = nrow(all_portfolios_group_2))
        all_portfolios_group_2$id <-
          1:nrow(all_portfolios_group_2)
        
        ### Portfolios total for table
        portfolios_total_pos <- get_all_portfolios(
          actions = actions,
          values = val_group_1_2,
          distance = dis_total,
          initial_budget_constraint = budget_total,
          direction = "max"
        )
        
        portfolios_total_neg <- get_all_portfolios(
          actions = actions,
          values = val_group_1_2,
          distance = dis_total,
          initial_budget_constraint = budget_total,
          direction = "min"
        )

        portfolios_total_pos_rev <-
          portfolios_total_pos[rev(rownames(portfolios_total_pos)),]
        portfolios_total <-
          rbind(portfolios_total_pos_rev, portfolios_total_neg[-1, ])
        portfolios_total$id <- 1:nrow(portfolios_total)
        portfolios_total$value <- portfolios_total$value
        portfolios_total$distance <-
          portfolios_total$distance
        
        ### Portfolios total for plotting
        all_portfolios_total <-
          expand.grid(0:1, 0:1, 0:1, 0:1, 0:1)
        names(all_portfolios_total) <- actions
        all_portfolios_val_total <-
          apply(all_portfolios_total, 1, function(row) {
            val <- sum(val_group_1_2[which(row %in% 1)])
          })
        all_portfolios_dis_total <-
          apply(all_portfolios_total, 1, function(row) {
            dis <- sum(dis_total[which(row %in% 1)])
          })
        all_portfolios_total$value <-
          all_portfolios_val_total
        all_portfolios_total$distance <-
          all_portfolios_dis_total
        all_portfolios_total <-
          full_join(portfolios_total, all_portfolios_total, by = actions)
        all_portfolios_total$dx <-
          rep_len(c(-10, 5), length.out = nrow(all_portfolios_total))
        all_portfolios_total$dy <-
          rep_len(c(-5, 10), length.out = nrow(all_portfolios_total))
        all_portfolios_total$id <- 1:nrow(all_portfolios_total)
        
        ### Tooltips
        tooltip_1 <- function(x) {
          if (is.null(x))
            return(NULL)
          row <-
            all_portfolios_group_1[all_portfolios_group_1$id == x$id, ]
          row$dx <- row$dy <- NULL
          paste0(names(row), ": ", format(x = row, digits = 2), collapse = "<br />")
        }
        tooltip_2 <- function(x) {
          if (is.null(x))
            return(NULL)
          row <-
            all_portfolios_group_2[all_portfolios_group_2$id == x$id, ]
          row$dx <- row$dy <- NULL
          paste0(names(row), ": ", format(x = row, digits = 2), collapse = "<br />")
        }
        tooltip_total <- function(x) {
          if (is.null(x))
            return(NULL)
          row <-
            all_portfolios_total[all_portfolios_total$id == x$id, ]
          row$dx <- row$dy <- NULL
          paste0(names(row), ": ", format(x = row, digits = 2), collapse = "<br />")
        }
        
        ### Plot group 1 portfolios
        all_portfolios_group_1 %>%
          ggvis() %>%
          add_axis(
            type = "x",
            title = "Distance",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Value",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          layer_points(
            x = ~ distance.y,
            y = ~ value.y,
            key := ~ id,
            fillOpacity := .5,
            fill := "grey"
          ) %>%
          layer_points(
            x = ~ distance.x,
            y = ~ value.x,
            key := ~ id,
            fillOpacity := 1,
            fill := "steelblue"
          ) %>%
          layer_paths(
            x = ~ distance.x,
            y = ~ value.x,
            key := ~ id,
            stroke := "steelblue"
          ) %>%
          layer_text(
            x = ~ distance.x,
            y = ~ value.x,
            data = na.omit(all_portfolios_group_1),
            text := ~ id,
            fill := "steelblue",
            fontSize := 8,
            dx := ~ dx,
            dy := ~ dy
          ) %>%
          add_tooltip(html = tooltip_1, on = "hover") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_13")
        
        ### Plot group 2 portfolios
        all_portfolios_group_2 %>%
          ggvis() %>%
          add_axis(
            type = "x",
            title = "Distance",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Value",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          layer_points(
            x = ~ distance.y,
            y = ~ value.y,
            key := ~ id,
            fillOpacity := .5,
            fill := "grey"
          ) %>%
          layer_points(
            x = ~ distance.x,
            y = ~ value.x,
            key := ~ id,
            fillOpacity := 1,
            fill := "firebrick"
          ) %>%
          layer_paths(
            x = ~ distance.x,
            y = ~ value.x,
            key := ~ id,
            stroke := "firebrick"
          ) %>%
          layer_text(
            x = ~ distance.x,
            y = ~ value.x,
            data = na.omit(all_portfolios_group_2),
            text := ~ id,
            fill := "firebrick",
            fontSize := 8,
            dx := ~ dx,
            dy := ~ dy
          ) %>%
          add_tooltip(html = tooltip_2, on = "hover") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_14")
        
        ### Plot total portfolios
        all_portfolios_total %>%
          ggvis() %>%
          add_axis(
            type = "x",
            title = "Distance",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis(
            type = "y",
            title = "Value",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          layer_points(
            x = ~ distance.y,
            y = ~ value.y,
            key := ~ id,
            fillOpacity := .5,
            fill := "grey"
          ) %>%
          layer_points(
            x = ~ distance.x,
            y = ~ value.x,
            key := ~ id,
            fillOpacity := 1,
            fill := "darkslateblue"
          ) %>%
          layer_paths(
            x = ~ distance.x,
            y = ~ value.x,
            key := ~ id,
            stroke := "darkslateblue"
          ) %>%
          layer_text(
            x = ~ distance.x,
            y = ~ value.x,
            data = na.omit(all_portfolios_total),
            text := ~ id,
            fill := "darkslateblue",
            fontSize := 8,
            dx := ~ dx,
            dy := ~ dy
          ) %>%
          add_tooltip(html = tooltip_total, on = "hover") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_15")
        
        # ## VtD TAB
        # 
        # val_dis_data <- val_data[2:4] / dis_data[2:4]
        # val_dis_data <-
        #   add_column(val_dis_data, x = c(as.character(letters[1:5]))) %>% select(x, y1, y2, y3)
        # 
        # ### Plot group 1 value / distance
        # val_dis_data %>%
        #   ggvis(x = ~ x,
        #     y = ~ y1,
        #     fill := "steelblue",
        #     stroke := "") %>%
        #   layer_bars() %>%
        #   scale_numeric(property = "y", domain = c(ifelse(min(val_dis_data$y1) < 0, min(val_dis_data$y1) * 1.1, 0), max(val_dis_data$y1))) %>%
        #   add_axis(
        #     type = "x",
        #     title = "Alternative",
        #     grid = FALSE,
        #     properties = axis_props(
        #       title = list(fontSize = 8),
        #       labels = list(fontSize = 8)
        #     )
        #   ) %>%
        #   add_axis(
        #     type = "y",
        #     title = "VtD",
        #     grid = FALSE,
        #     properties = axis_props(
        #       title = list(fontSize = 8),
        #       labels = list(fontSize = 8)
        #     ),
        #     title_offset = 40
        #   ) %>%
        #   set_options(width = "auto",
        #     height = 180,
        #     renderer = "canvas") %>%
        #   bind_shiny(plot_id = "ggvis_10")
        # 
        # ### Plot group 2 value / distance
        # val_dis_data %>%
        #   ggvis(x = ~ x,
        #     y = ~ y2,
        #     fill := "firebrick",
        #     stroke := "") %>%
        #   layer_bars() %>%
        #   scale_numeric(property = "y", domain = c(ifelse(min(val_dis_data$y2) < 0, min(val_dis_data$y2) * 1.1, 0), max(val_dis_data$y2))) %>%
        #   add_axis(
        #     type = "x",
        #     title = "Alternative",
        #     grid = FALSE,
        #     properties = axis_props(
        #       title = list(fontSize = 8),
        #       labels = list(fontSize = 8)
        #     )
        #   ) %>%
        #   add_axis(
        #     type = "y",
        #     title = "VtD",
        #     grid = FALSE,
        #     properties = axis_props(
        #       title = list(fontSize = 8),
        #       labels = list(fontSize = 8)
        #     ),
        #     title_offset = 40
        #   ) %>%
        #   set_options(width = "auto",
        #     height = 180,
        #     renderer = "canvas") %>%
        #   bind_shiny(plot_id = "ggvis_11")
        # 
        # ### Plot total value / distance
        # val_dis_data %>%
        #   ggvis(x = ~ x,
        #     y = ~ y3,
        #     fill := "darkslateblue",
        #     stroke := "") %>%
        #   layer_bars() %>%
        #   scale_numeric(property = "y", domain = c(ifelse(min(val_dis_data$y3) < 0, min(val_dis_data$y3) * 1.1, 0), max(val_dis_data$y3))) %>%
        #   add_axis(
        #     type = "x",
        #     title = "Alternative",
        #     grid = FALSE,
        #     properties = axis_props(
        #       title = list(fontSize = 8),
        #       labels = list(fontSize = 8)
        #     )
        #   ) %>%
        #   add_axis(
        #     type = "y",
        #     title = "VtD",
        #     grid = FALSE,
        #     properties = axis_props(
        #       title = list(fontSize = 8),
        #       labels = list(fontSize = 8)
        #     ),
        #     title_offset = 40
        #   ) %>%
        #   set_options(width = "auto",
        #     height = 180,
        #     renderer = "canvas") %>%
        #   bind_shiny(plot_id = "ggvis_12")
        
        # PORTFOLIO DETAILS PANEL #############################################
        
        ## G1 TAB #############################################################
        
        ### G1 table
        output$portfolios_group_1_table <- DT::renderDataTable({
          a <-
            round(sum(portfolios_group_1$Alt.a) / nrow(portfolios_group_1) * 100,
              digits = 0)
          b <-
            round(sum(portfolios_group_1$Alt.b) / nrow(portfolios_group_1) * 100,
              digits = 0)
          c <-
            round(sum(portfolios_group_1$Alt.c) / nrow(portfolios_group_1) * 100,
              digits = 0)
          d <-
            round(sum(portfolios_group_1$Alt.d) / nrow(portfolios_group_1) * 100,
              digits = 0)
          e <-
            round(sum(portfolios_group_1$Alt.e) / nrow(portfolios_group_1) * 100,
              digits = 0)
          portfolios_group_1 %>%
            rename(
              a = Alt.a,
              b = Alt.b,
              c = Alt.c,
              d = Alt.d,
              e = Alt.e
            ) %>%
            mutate(V = value, D = distance) %>%
            # mutate(VtD = V / D) %>%
            # select(id, a, b, c, d, e, V, D, VtD) %>%
            select(id, a, b, c, d, e, V, D) %>%
            add_row(
              a = a,
              b = b,
              c = c,
              d = d,
              e = e
            ) %>%
            round(digits = 1) %>%
            datatable(
              rownames = FALSE,
              options =
                list(
                  dom = "t",
                  pageLength = 100,
                  rowCallback = JS(
                    "function(row, data) {",
                    "if(data[0] == null){",
                    "console.log(data)",
                    "var num = data[1].toString() + '%';",
                    "$('td:eq(1)', row).html(num);",
                    "var num2 = data[2].toString() + '%';",
                    "$('td:eq(2)', row).html(num2);",
                    "var num3 = data[3].toString() + '%';",
                    "$('td:eq(3)', row).html(num3);",
                    "var num4 = data[4].toString() + '%';",
                    "$('td:eq(4)', row).html(num4);",
                    "var num5 = data[5].toString() + '%';",
                    "$('td:eq(5)', row).html(num5);",
                    "}}"
                  )
                )
            )
        })
        
        ## G2 TAB #############################################################
        
        ### G2 table
        output$portfolios_group_2_table <- DT::renderDataTable({
          a <-
            round(sum(portfolios_group_2$Alt.a) / nrow(portfolios_group_2) * 100,
              digits = 0)
          b <-
            round(sum(portfolios_group_2$Alt.b) / nrow(portfolios_group_2) * 100,
              digits = 0)
          c <-
            round(sum(portfolios_group_2$Alt.c) / nrow(portfolios_group_2) * 100,
              digits = 0)
          d <-
            round(sum(portfolios_group_2$Alt.d) / nrow(portfolios_group_2) * 100,
              digits = 0)
          e <-
            round(sum(portfolios_group_2$Alt.e) / nrow(portfolios_group_2) * 100,
              digits = 0)
          portfolios_group_2 %>%
            rename(
              a = Alt.a,
              b = Alt.b,
              c = Alt.c,
              d = Alt.d,
              e = Alt.e
            ) %>%
            mutate(V = value, D = distance) %>%
            # mutate(VtD = V / D) %>%
            # select(id, a, b, c, d, e, V, D, VtD) %>%
            select(id, a, b, c, d, e, V, D) %>%
            add_row(
              a = a,
              b = b,
              c = c,
              d = d,
              e = e
            ) %>%
            round(digits = 1) %>%
            datatable(
              rownames = FALSE,
              options =
                list(
                  dom = "t",
                  pageLength = 100,
                  rowCallback = JS(
                    "function(row, data) {",
                    "if(data[0] == null){",
                    "console.log(data)",
                    "var num = data[1].toString() + '%';",
                    "$('td:eq(1)', row).html(num);",
                    "var num2 = data[2].toString() + '%';",
                    "$('td:eq(2)', row).html(num2);",
                    "var num3 = data[3].toString() + '%';",
                    "$('td:eq(3)', row).html(num3);",
                    "var num4 = data[4].toString() + '%';",
                    "$('td:eq(4)', row).html(num4);",
                    "var num5 = data[5].toString() + '%';",
                    "$('td:eq(5)', row).html(num5);",
                    "}}"
                  )
                )
            )
        })
        
        ## T TAB
        
        ## T table
        output$portfolios_total_table <- DT::renderDataTable({
          a <-
            round(sum(portfolios_total$Alt.a) / nrow(portfolios_total) * 100,
              digits = 0)
          b <-
            round(sum(portfolios_total$Alt.b) / nrow(portfolios_total) * 100,
              digits = 0)
          c <-
            round(sum(portfolios_total$Alt.c) / nrow(portfolios_total) * 100,
              digits = 0)
          d <-
            round(sum(portfolios_total$Alt.d) / nrow(portfolios_total) * 100,
              digits = 0)
          e <-
            round(sum(portfolios_total$Alt.e) / nrow(portfolios_total) * 100,
              digits = 0)
          portfolios_total %>%
            rename(
              a = Alt.a,
              b = Alt.b,
              c = Alt.c,
              d = Alt.d,
              e = Alt.e
            ) %>%
            mutate(V = value, D = distance) %>%
            # mutate(VtD = V / D) %>%
            # select(id, a, b, c, d, e, V, D, VtD) %>%
            select(id, a, b, c, d, e, V, D) %>%
            add_row(
              a = a,
              b = b,
              c = c,
              d = d,
              e = e
            ) %>%
            round(digits = 1) %>%
            datatable(
              rownames = FALSE,
              options =
                list(
                  dom = "t",
                  pageLength = 100,
                  rowCallback = JS(
                    "function(row, data) {",
                    "if(data[0] == null){",
                    "console.log(data)",
                    "var num = data[1].toString() + '%';",
                    "$('td:eq(1)', row).html(num);",
                    "var num2 = data[2].toString() + '%';",
                    "$('td:eq(2)', row).html(num2);",
                    "var num3 = data[3].toString() + '%';",
                    "$('td:eq(3)', row).html(num3);",
                    "var num4 = data[4].toString() + '%';",
                    "$('td:eq(4)', row).html(num4);",
                    "var num5 = data[5].toString() + '%';",
                    "$('td:eq(5)', row).html(num5);",
                    "}}"
                  )
                )
            )
        })
        
        # DEMOGRAPHICS PANEL ##################################################
        
        ## DEMOGRAPHICS TAB
        
        dem_group_1 <- tdata$group_1_filter_1()@data %>% tbl_df()
        dem_group_2 <- tdata$group_2_filter_1()@data %>% tbl_df()
        
        gender_group_1 <- dem_group_1 %>%
          select(Gender) %>%
          filter(Gender == "Woman" | Gender == "Man") %>%
          # droplevels() %>%
          group_by(Gender) %>%
          count()
        
        gender_group_2 <- dem_group_2 %>%
          select(Gender) %>%
          filter(Gender == "Woman" | Gender == "Man") %>%
          # droplevels() %>%
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
          mutate(
            Occupation = fct_recode(
              .f = Occupation,
              "Long-term sick leave"  = "Long-term sick leave (more than 3 months)",
              "Sickness benefit"      = "Sickness or activity benefit"
            )
          ) %>%
          select(Occupation) %>%
          group_by(Occupation) %>%
          count()
        
        occupation_group_2 <- dem_group_2 %>%
          mutate(
            Occupation = fct_recode(
              .f = Occupation,
              "Long-term sick leave"  = "Long-term sick leave (more than 3 months)",
              "Sickness benefit"      = "Sickness or activity benefit"
            )
          ) %>%
          select(Occupation) %>%
          group_by(Occupation) %>%
          count()
        
        education_group_1 <- dem_group_1 %>%
          mutate(
            Education.level = fct_recode(
              .f = Education.level,
              "University"            = "College/University",
              "Elem. school"          = "Elementary school or equivalent compulsory school",
              "High school"           = "High school, Nordic folk high school, or equivalent",
              "No elem. school"       = "No elementary or equivalent compulsary school",
              "Other"                 = "Other post-secondary education"
            )
          ) %>%
          select(Education.level) %>%
          group_by(Education.level) %>%
          count()
        
        education_group_2 <- dem_group_2 %>%
          mutate(
            Education.level = fct_recode(
              .f = Education.level,
              "University"            = "College/University",
              "Elem. school"          = "Elementary school or equivalent compulsory school",
              "High school"           = "High school, Nordic folk high school, or equivalent",
              "No elem. school"       = "No elementary or equivalent compulsary school",
              "Other"                 = "Other post-secondary education"
            )
          ) %>%
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
        
        gender <-
          bind_rows(gender_group_1, gender_group_2, .id = "Group") %>%
          ungroup() %>%
          mutate(Gender = fct_reorder(.f = Gender, n)) %>%
          top_n(n = 4, wt = n) %>%
          droplevels()
        
        age <- bind_rows(age_group_1, age_group_2, .id = "Group")
        
        occupation <-
          bind_rows(occupation_group_1, occupation_group_2, .id = "Group") %>%
          ungroup() %>%
          mutate(Occupation = fct_reorder(.f = Occupation, n)) %>%
          top_n(n = 8, wt = n) %>%
          droplevels()
        
        education <-
          bind_rows(education_group_1, education_group_2, .id = "Group") %>%
          ungroup() %>%
          mutate(Education = fct_reorder(.f = Education.level, n)) %>%
          select(Group, Education, n) %>%
          top_n(n = 8, wt = n) %>%
          droplevels()
        
        year <-
          bind_rows(year_group_1, year_group_2, .id = "Group") %>%
          ungroup() %>%
          mutate(Year = fct_recode(.f = Year, "10+ years" = "10 or more years")) %>%
          mutate(Year = fct_relevel(.f = Year, "0-4 years", "5-9 years", "10+ years")) %>%
          droplevels()
        
        ### Plot Gender
        gender %>%
          ggvis(
            y = ~ Gender,
            x = ~ n,
            fill = ~ Group,
            stroke := ""
          ) %>%
          scale_nominal(property = "fill",
            range = c("steelblue", "firebrick")) %>%
          add_axis(
            type = "x",
            title = "Count",
            ticks = 5,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis("x", orient = "top", ticks = 0, title = "Gender",
                   properties = axis_props(
                     axis = list(stroke = "white"),
                     labels = list(fontSize = 0))
                   ) %>%
          add_axis(
            type = "y",
            title = "",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 60
          ) %>%
          compute_stack(stack_var = ~ n, group_var = ~ Gender) %>%
          layer_rects(
            x = ~ stack_lwr_,
            x2 = ~ stack_upr_,
            height = band()
          ) %>%
          hide_legend(scales = "fill") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_16")
        
        ### Plot Age
        age %>%
          ggvis(y = ~ Age,
            x = ~ n,
            fill = ~ Group,
            stroke := "") %>%
          scale_ordinal(property = "fill",
            range = c("steelblue", "firebrick")) %>%
          scale_ordinal(property = "y", reverse = TRUE) %>%
          add_axis(
            type = "x",
            title = "Count",
            ticks = 8,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis("x", orient = "top", ticks = 0, title = "Age",
                   properties = axis_props(
                     axis = list(stroke = "white"),
                     labels = list(fontSize = 0))
          ) %>%
          add_axis(
            type = "y",
            title = "",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 45
          ) %>%
          compute_stack(stack_var = ~ n, group_var = ~ Age) %>%
          layer_rects(
            x = ~ stack_lwr_,
            x2 = ~ stack_upr_,
            height = band()
          ) %>%
          hide_legend(scales = "fill") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_17")
        
        ### Plot Occupation
        occupation %>%
          ggvis(
            y = ~ Occupation,
            x = ~ n,
            fill = ~ Group,
            stroke := ""
          ) %>%
          scale_nominal(property = "fill",
            range = c("steelblue", "firebrick")) %>%
          scale_nominal(property = "y", reverse = TRUE) %>%
          add_axis(
            type = "x",
            title = "Count",
            ticks = 5,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis("x", orient = "top", ticks = 0, title = "Occupation",
                   properties = axis_props(
                     axis = list(stroke = "white"),
                     labels = list(fontSize = 0))
          ) %>%
          add_axis(
            type = "y",
            title = "",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 90
          ) %>%
          compute_stack(stack_var = ~ n, group_var = ~ Occupation) %>%
          layer_rects(
            x = ~ stack_lwr_,
            x2 = ~ stack_upr_,
            height = band()
          ) %>%
          hide_legend(scales = "fill") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_18")
        
        ### Plot Education
        education %>%
          ggvis(
            y = ~ Education,
            x = ~ n,
            fill = ~ Group,
            stroke := ""
          ) %>%
          scale_nominal(property = "fill",
            range = c("steelblue", "firebrick")) %>%
          scale_nominal(property = "y", reverse = TRUE) %>%
          add_axis(
            type = "x",
            title = "Count",
            ticks = 5,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis("x", orient = "top", ticks = 0, title = "Education level",
                   properties = axis_props(
                     axis = list(stroke = "white"),
                     labels = list(fontSize = 0))
          ) %>%
          add_axis(
            type = "y",
            title = "",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 80
          ) %>%
          compute_stack(stack_var = ~ n, group_var = ~ Education) %>%
          layer_rects(
            x = ~ stack_lwr_,
            x2 = ~ stack_upr_,
            height = band()
          ) %>%
          hide_legend(scales = "fill") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_19")
        
        ### Plot Years
        year %>%
          ggvis(y = ~ Year,
            x = ~ n,
            fill = ~ Group,
            stroke := "") %>%
          scale_nominal(property = "fill",
            range = c("steelblue", "firebrick")) %>%
          scale_nominal(property = "y", reverse = TRUE) %>%
          add_axis(
            type = "x",
            title = "Count",
            ticks = 5,
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            )
          ) %>%
          add_axis("x", orient = "top", ticks = 0, title = "Length of residency",
                   properties = axis_props(
                     axis = list(stroke = "white"),
                     labels = list(fontSize = 0))
          ) %>%
          add_axis(
            type = "y",
            title = "",
            grid = FALSE,
            properties = axis_props(
              title = list(fontSize = 8),
              labels = list(fontSize = 8)
            ),
            title_offset = 60
          ) %>%
          compute_stack(stack_var = ~ n, group_var = ~ Year) %>%
          layer_rects(
            x = ~ stack_lwr_,
            x2 = ~ stack_upr_,
            height = band()
          ) %>%
          hide_legend(scales = "fill") %>%
          set_options(width = "auto",
            height = 180,
            renderer = "canvas") %>%
          bind_shiny(plot_id = "ggvis_20")
  })
  
  # TABLE #####################################################################
  
  data_df <- data_df %>%
    mutate(
      Education.level =
        fct_recode(
          .f = Education.level,
          "University"      = "College/University",
          "Elem. school"    = "Elementary school or equivalent compulsory school",
          "High school"     = "High school, Nordic folk high school, or equivalent",
          "No elem. school" = "No elementary or equivalent compulsary school",
          "Other"           = "Other post-secondary education"
        )
    ) %>%
    mutate(
      Year =
        fct_recode(
          .f = Year,
          "10+ years"       = "10 or more years",
          "5-9 years"       = "5-9 years",
          "0-4 years"       = "0-4 years"
        )
    ) %>%
    mutate(
      Occupation =
        fct_recode(
          .f = Occupation,
          "LOA"             = "Leave of absence",
          "LTSL"            = "Long-term sick leave (more than 3 months)",
          "SB"              = "Sickness or activity benefit",
          "Senior"          = "Senior citizen"
        )
    ) %>%
    mutate(
      Gender =
        fct_recode(
          .f = Gender,
          "Other"           = "Other/No gender",
          "-"               = "Prefer not to disclose"
        )
    ) %>%
    rename(
      "1a" = Alt.1a,
      "1b" = Alt.1b,
      "1c" = Alt.1c,
      "1d" = Alt.1d,
      "1e" = Alt.1e,
      "2a" = Alt.2a,
      "2b" = Alt.2b,
      "2c" = Alt.2c,
      "2d" = Alt.2d,
      "2e" = Alt.2e,
      "3a" = Alt.3a,
      "3b" = Alt.3b,
      "3c" = Alt.3c,
      "3d" = Alt.3d,
      "3e" = Alt.3e,
      "4a" = Alt.4a,
      "4b" = Alt.4b,
      "4c" = Alt.4c,
      "4d" = Alt.4d,
      "4e" = Alt.4e,
      "5a" = Alt.5a,
      "5b" = Alt.5b,
      "5c" = Alt.5c,
      "5d" = Alt.5d,
      "5e" = Alt.5e,
      "6a" = Alt.6a,
      "6b" = Alt.6b,
      "6c" = Alt.6c,
      "6d" = Alt.6d,
      "6e" = Alt.6e,
      "7a" = Alt.7a,
      "7b" = Alt.7b,
      "7c" = Alt.7c,
      "7d" = Alt.7d,
      "7e" = Alt.7e,
      "8a" = Alt.8a,
      "8b" = Alt.8b,
      "8c" = Alt.8c,
      "8d" = Alt.8d,
      "8e" = Alt.8e,
      "9a" = Alt.9a,
      "9b" = Alt.9b,
      "9c" = Alt.9c,
      "9d" = Alt.9d,
      "9e" = Alt.9e,
      "10a" = Alt.10a,
      "10b" = Alt.10b,
      "10c" = Alt.10c,
      "10d" = Alt.10d,
      "10e" = Alt.10e
    )
  
  table_filter <- reactive({
    if (!identical(input$area3, "All")) {
      for (o in seq_along(input$area3)) {
        data_df <-
          head(data_df[data_df$Area %in% input$area3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$gender3, "All")) {
      for (p in seq_along(input$gender3)) {
        data_df <-
          head(data_df[data_df$Gender %in% input$gender3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$age3, "All")) {
      for (q in seq_along(input$age3)) {
        data_df <-
          head(data_df[data_df$Age %in% input$age3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$occupation3, "All")) {
      for (r in seq_along(input$occupation3)) {
        data_df <-
          head(data_df[data_df$Occupation %in% input$occupation3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$education3, "All")) {
      for (s in seq_along(input$education3)) {
        data_df <-
          head(data_df[data_df$Education.level %in% input$education3, ], n = 1040, drop = FALSE)
      }
    }
    if (!identical(input$years3, "All")) {
      for (t in seq_along(input$years3)) {
        data_df <-
          head(data_df[data_df$Year %in% input$years3, ], n = 1040, drop = FALSE)
      }
    }
    data_df
  })
  
  # color <- colorRampPalette(brewer.pal(n = 11, name = "RdYlGn"))(15)
  # barplot(height = 0:14, beside = TRUE, names = "RdYlGn", col = color)
  # display.brewer.pal(11, "RdYlGn")
  
  output$table <- DT::renderDataTable({
    table_filter() %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(width = '200px', targets = 0),
            list(width = '50px', targets = 5)
          )
        )
      ) %>%
      formatStyle(
        columns = c(7:56, 67:69),
        backgroundColor = styleInterval(
          cuts = c(0:13),
          values = colorRampPalette(brewer.pal(
            n = 11, name = tdata$colorpal()
          ))(15)
        )
      ) %>%
      formatStyle(
        columns = 57:66,
        background = styleColorBar(
          data = 0:15,
          color = "lightblue",
          angle = -90
        )
      )
  })
})
