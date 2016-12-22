# Module UI
tabset_UI <- function(id) {
  ns <- NS(id)
  rnd <- sample(x = vars_area[3:45], size = 10) %>% split(f = c(1, 2))
  tagList(
    sidebarPanel(width = 4,
      tabsetPanel(
        type = "tabs",
        tabPanel("Theme",
          selectInput(ns("theme"), label = "Theme", choices = theme, selected = theme[1]),
          uiOutput(ns("alternatives"))
        ),
        tabPanel("Group 1",
          lapply(seq_along(background_variables), function(j) {
            if (j == 1) {
              to_select <- rnd[[1]]
            } else {
              to_select <- background_variables[[1]][2]
            }
            selectInput(
              ns(paste(ui_names_bg[j], 1, sep = "")),
              label = paste(dropdown_names_bg[j]),
              choices = background_variables[[j]],
              selected = to_select,
              multiple = TRUE
            )
          }),
          checkboxInput(ns("pop1"), label = "Add popups", value = TRUE),
          checkboxInput(ns("markers1"), label = "Add markers", value = TRUE)
        ),
        tabPanel("Group 2",
          lapply(seq_along(background_variables), function(j) {
            if (j == 1) {
              to_select <- rnd[[2]]
            } else {
              to_select <- background_variables[[1]][2]
            }
            selectInput(
              ns(paste(ui_names_bg[j], 2, sep = "")),
              label = paste(dropdown_names_bg[j]),
              choices = background_variables[[j]],
              selected = to_select,
              multiple = TRUE
            )
          }),
          checkboxInput(ns("pop2"), label = "Add popups", value = TRUE),
          checkboxInput(ns("markers2"), label = "Add markers", value = TRUE)
        )
      )
    )
  )
}

# Module server
tabset <- function(input, output, session) {
  
  # Render the alternatives dropdown menu
  output$alternatives <- renderUI({
    ns <- session$ns
    switch(
        input$theme,
        "1. Parks and green areas" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[1]]),
        "2. Diversity in housing supply" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[2]]),
        "3. Invest in public areas" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[3]]),
        "4. Communications" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[4]]),
        "5. Culture and leasure" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[5]]),
        "6. Education" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[6]]),
        "7. Care" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[7]]),
        "8. School" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[8]]),
        "9. Safety" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[9]]),
        "10. Ecological sustainability" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[10]])
      )
  })
  
  ####################################################################################################################
  
  ### FILTERS ########################################################################################################
  
  ####################################################################################################################
  
  # return corresponding dataframe column name of an input column name
  get_input_category <- function(ui_col_name) {
    if (!is.element(ui_col_name, names(df_names_bg))) {
      return("404")
    }
    if (identical(df_names_bg[[ui_col_name]], character(0))) {
      return("404")
    }
    return(df_names_bg[[ui_col_name]])
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
  
  # Return list of reactive expression
  return(
    list(
      area1 = reactive(input$area1),
      gender1 = reactive(input$gender1), # 2
      age1 = reactive(input$age1),
      occupation1 = reactive(input$occupation1), # 4
      education1 = reactive(input$education1),
      years1 = reactive(input$years1), # 6
      pop1 = reactive(input$pop1),
      markers1 = reactive(input$markers1), # 8
      
      area2 = reactive(input$area2),
      gender2 = reactive(input$gender2), # 10
      age2 = reactive(input$age2), 
      occupation2 = reactive(input$occupation2), # 12
      education2 = reactive(input$education2),
      years2 = reactive(input$years2), # 14
      pop2 = reactive(input$pop2),
      markers2 = reactive(input$markers2), # 16
      
      theme = reactive(input$theme), 
      alt = reactive(input$alt), # 18
      
      group_1_filter_1 = group_1_filter_1,
      group_1_filter_2 = group_1_filter_2, # 20
      group_1_mean = group_1_mean,
      
      group_2_filter_1 = group_2_filter_1, # 22
      group_2_filter_2 = group_2_filter_2,
      group_2_mean = group_2_mean # 24
      
    )
  )
  
}