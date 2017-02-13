# Module UI
tabset_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(width = 0,
      tabsetPanel(
        type = "tabs",
        tabPanel(title = "Theme",
          selectInput(ns("theme"), label = "Theme", choices = theme, selected = theme[2]), 
          uiOutput(ns("alternatives")),
          selectInput(ns("colorpal"), label = "Palette", choices = rownames(brewer.pal.info[1:9, ]), selected = "RdYlGn")
        ),
        tabPanel(title = "Grp 1",
          lapply(seq_along(b_variables), function(j) {
            if (j == 1) {
              to_select <- rnd[[1]]
            } else {
              to_select <- b_variables[[1]][2]
            }
            selectInput(
              ns(paste(b_names[j], 1, sep = "")),
              label = paste(b_labels[j]),
              choices = b_variables[[j]],
              selected = to_select,
              multiple = TRUE
            )
          }),
          checkboxInput(ns("markers1"), label = "Add markers", value = TRUE),
          checkboxInput(ns("pop1"), label = "Add popups", value = FALSE)
          
        ),
        tabPanel(title = "Grp 2",
          lapply(seq_along(b_variables), function(j) {
            if (j == 1) {
              to_select <- rnd[[2]]
            } else {
              to_select <- b_variables[[1]][2]
            }
            selectInput(
              ns(paste(b_names[j], 2, sep = "")),
              label = paste(b_labels[j]),
              choices = b_variables[[j]],
              selected = to_select,
              multiple = TRUE
            )
          }),
          checkboxInput(ns("markers2"), label = "Add markers", value = TRUE),
          checkboxInput(ns("pop2"), label = "Add popups", value = FALSE)
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
        "1. Parks and green areas" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[1]], selectize = FALSE, size = 5),
        "2. Diversity in housing supply" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[2]], selectize = FALSE, size = 5),
        "3. Invest in public areas" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[3]], selectize = FALSE, size = 5),
        "4. Communications" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[4]], selectize = FALSE, size = 5),
        "5. Culture and leasure" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[5]], selectize = FALSE, size = 5),
        "6. Education" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[6]], selectize = FALSE, size = 5),
        "7. Care" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[7]], selectize = FALSE, size = 5),
        "8. School" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[8]], selectize = FALSE, size = 5),
        "9. Safety" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[9]], selectize = FALSE, size = 5),
        "10. Ecological sustainability" = selectInput(ns("alt"), label = "Alternatives", choices = alt_list[[10]], selectize = FALSE, size = 5)
      )
  })
  
  ### GROUP 1 FILTERS ################################################
  
  # Subset themes for group 1
  group_1_filter_1 <- reactive({
    req(input$area1, input$gender1, input$age1, input$occupation1, input$education1, input$years1)
    
    if (input$area1 != "All") {
      for (i in seq_along(input$area1)) { 
        results_spdf1 <- results_spdf1[results_spdf1$Area %in% input$area1, ]
      }
    }
    if (input$gender1 != "All") {
      for (i in seq_along(input$gender1)) { 
        results_spdf1 <- results_spdf1[results_spdf1$Gender %in% input$gender1, ]
      }
    }
    if (input$age1 != "All") {
      for (i in seq_along(input$age1)) { 
        results_spdf1 <- results_spdf1[results_spdf1$Age %in% input$age1, ]
      }
    }
    if (input$occupation1 != "All") {
      for (i in seq_along(input$occupation1)) { 
        results_spdf1 <- results_spdf1[results_spdf1$Occupation %in% input$occupation1, ]
      }
    }
    if (input$education1 != "All") {
      for (i in seq_along(input$education1)) { 
        results_spdf1 <- results_spdf1[results_spdf1$Education.level %in% input$education1, ]
      }
    }
    if (input$years1 != "All") {
      for (i in seq_along(input$years1)) { 
        results_spdf1 <- results_spdf1[results_spdf1$Year %in% input$years1, ]
      }
    }
    results_spdf1
  })
  
  # Subset alternatives for group 1
  group_1_filter_2 <- reactive({
    
    req(input$alt)
    
    # Theme 1
    if (input$alt == alt_list[[1]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 10]))
    }
    if (input$alt == alt_list[[1]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 11]))
    }
    if (input$alt == alt_list[[1]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 12]))
    }
    if (input$alt == alt_list[[1]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 13]))
    }
    if (input$alt == alt_list[[1]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 14]))
    }
    # Theme 2
    if (input$alt == alt_list[[2]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 15]))
    }
    if (input$alt == alt_list[[2]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 16]))
    }
    if (input$alt == alt_list[[2]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 17]))
    }
    if (input$alt == alt_list[[2]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 18]))
    }
    if (input$alt == alt_list[[2]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 19]))
    }
    # Theme 3
    if (input$alt == alt_list[[3]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 20]))
    }
    if (input$alt == alt_list[[3]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 21]))
    }
    if (input$alt == alt_list[[3]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 22]))
    }
    if (input$alt == alt_list[[3]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 23]))
    }
    if (input$alt == alt_list[[3]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 24]))
    }
    # Theme 4
    if (input$alt == alt_list[[4]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 25]))
    }
    if (input$alt == alt_list[[4]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 26]))
    }
    if (input$alt == alt_list[[4]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 27]))
    }
    if (input$alt == alt_list[[4]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 28]))
    }
    if (input$alt == alt_list[[4]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 29]))
    }
    # Theme 5
    if (input$alt == alt_list[[5]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 30]))
    }
    if (input$alt == alt_list[[5]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 31]))
    }
    if (input$alt == alt_list[[5]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 32]))
    }
    if (input$alt == alt_list[[5]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 33]))
    }
    if (input$alt == alt_list[[5]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 34]))
    }
    # Theme 6
    if (input$alt == alt_list[[6]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 35]))
    }
    if (input$alt == alt_list[[6]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 36]))
    }
    if (input$alt == alt_list[[6]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 37]))
    }
    if (input$alt == alt_list[[6]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 38]))
    }
    if (input$alt == alt_list[[6]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 39]))
    }
    # Theme 7
    if (input$alt == alt_list[[7]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 40]))
    }
    if (input$alt == alt_list[[7]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 41]))
    }
    if (input$alt == alt_list[[7]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 42]))
    }
    if (input$alt == alt_list[[7]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 43]))
    }
    if (input$alt == alt_list[[7]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 44]))
    }
    # Theme 8
    if (input$alt == alt_list[[8]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 45]))
    }
    if (input$alt == alt_list[[8]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 46]))
    }
    if (input$alt == alt_list[[8]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 47]))
    }
    if (input$alt == alt_list[[8]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 48]))
    }
    if (input$alt == alt_list[[8]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 49]))
    }
    # Theme 9
    if (input$alt == alt_list[[9]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 50]))
    }
    if (input$alt == alt_list[[9]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 51]))
    }
    if (input$alt == alt_list[[9]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 52]))
    }
    if (input$alt == alt_list[[9]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 53]))
    }
    if (input$alt == alt_list[[9]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 54]))
    }
    # Theme 10
    if (input$alt == alt_list[[10]][1]) {
      return(as.matrix(group_1_filter_1()@data[, 55]))
    }
    if (input$alt == alt_list[[10]][2]) {
      return(as.matrix(group_1_filter_1()@data[, 56]))
    }
    if (input$alt == alt_list[[10]][3]) {
      return(as.matrix(group_1_filter_1()@data[, 57]))
    }
    if (input$alt == alt_list[[10]][4]) {
      return(as.matrix(group_1_filter_1()@data[, 58]))
    }
    if (input$alt == alt_list[[10]][5]) {
      return(as.matrix(group_1_filter_1()@data[, 59]))
    }
  })
  
  # # Calculate mean for group 1
  group_1_mean <- reactive({
    round(mean(group_1_filter_2()), digits = 2)
  })
  
  ### GROUP 2 FILTERS ################################################
  
  # Subset background variables for group 2
  group_2_filter_1 <- reactive({
    req(input$area2, input$gender2, input$age2, input$occupation2, input$education2, input$years2)
    
    if (input$area2 != "All") {
      for (i in seq_along(input$area2)) { 
        results_spdf2 <- results_spdf2[results_spdf2$Area %in% input$area2, ]
      }
    }
    if (input$gender2 != "All") {
      for (i in seq_along(input$gender2)) { 
        results_spdf2 <- results_spdf2[results_spdf2$Gender %in% input$gender2, ]
      }
    }
    if (input$age2 != "All") {
      for (i in seq_along(input$age2)) { 
        results_spdf2 <- results_spdf2[results_spdf2$Age %in% input$age2, ]
      }
    }
    if (input$occupation2 != "All") {
      for (i in seq_along(input$occupation2)) { 
        results_spdf2 <- results_spdf2[results_spdf2$Occupation %in% input$occupation2, ]
      }
    }
    if (input$education2 != "All") {
      for (i in seq_along(input$education2)) { 
        results_spdf2 <- results_spdf2[results_spdf2$Education.level %in% input$education2, ]
      }
    }
    if (input$years2 != "All") {
      for (i in seq_along(input$years2)) { 
        results_spdf2 <- results_spdf2[results_spdf2$Year %in% input$years2, ]
      }
    }
    results_spdf2
  })
  
  # # Subset alternatives for group 2
  group_2_filter_2 <- reactive({
    
    req(input$alt)
    
    # theme 1
    if (input$alt == alt_list[[1]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 10]))
    }
    if (input$alt == alt_list[[1]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 11]))
    }
    if (input$alt == alt_list[[1]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 12]))
    }
    if (input$alt == alt_list[[1]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 13]))
    }
    if (input$alt == alt_list[[1]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 14]))
    }
    # theme 2
    if (input$alt == alt_list[[2]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 15]))
    }
    if (input$alt == alt_list[[2]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 16]))
    }
    if (input$alt == alt_list[[2]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 17]))
    }
    if (input$alt == alt_list[[2]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 18]))
    }
    if (input$alt == alt_list[[2]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 19]))
    }
    # theme 3
    if (input$alt == alt_list[[3]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 20]))
    }
    if (input$alt == alt_list[[3]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 21]))
    }
    if (input$alt == alt_list[[3]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 22]))
    }
    if (input$alt == alt_list[[3]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 23]))
    }
    if (input$alt == alt_list[[3]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 24]))
    }
    # theme 4
    if (input$alt == alt_list[[4]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 25]))
    }
    if (input$alt == alt_list[[4]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 26]))
    }
    if (input$alt == alt_list[[4]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 27]))
    }
    if (input$alt == alt_list[[4]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 28]))
    }
    if (input$alt == alt_list[[4]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 29]))
    }
    # theme 5
    if (input$alt == alt_list[[5]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 30]))
    }
    if (input$alt == alt_list[[5]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 31]))
    }
    if (input$alt == alt_list[[5]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 32]))
    }
    if (input$alt == alt_list[[5]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 33]))
    }
    if (input$alt == alt_list[[5]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 34]))
    }
    # theme 6
    if (input$alt == alt_list[[6]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 35]))
    }
    if (input$alt == alt_list[[6]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 36]))
    }
    if (input$alt == alt_list[[6]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 37]))
    }
    if (input$alt == alt_list[[6]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 38]))
    }
    if (input$alt == alt_list[[6]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 39]))
    }
    # theme 7
    if (input$alt == alt_list[[7]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 40]))
    }
    if (input$alt == alt_list[[7]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 41]))
    }
    if (input$alt == alt_list[[7]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 42]))
    }
    if (input$alt == alt_list[[7]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 43]))
    }
    if (input$alt == alt_list[[7]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 44]))
    }
    # theme 8
    if (input$alt == alt_list[[8]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 45]))
    }
    if (input$alt == alt_list[[8]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 46]))
    }
    if (input$alt == alt_list[[8]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 47]))
    }
    if (input$alt == alt_list[[8]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 48]))
    }
    if (input$alt == alt_list[[8]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 49]))
    }
    # theme 9
    if (input$alt == alt_list[[9]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 50]))
    }
    if (input$alt == alt_list[[9]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 51]))
    }
    if (input$alt == alt_list[[9]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 52]))
    }
    if (input$alt == alt_list[[9]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 53]))
    }
    if (input$alt == alt_list[[9]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 54]))
    }
    # theme 10
    if (input$alt == alt_list[[10]][1]) {
      return(as.matrix(group_2_filter_1()@data[, 55]))
    }
    if (input$alt == alt_list[[10]][2]) {
      return(as.matrix(group_2_filter_1()@data[, 56]))
    }
    if (input$alt == alt_list[[10]][3]) {
      return(as.matrix(group_2_filter_1()@data[, 57]))
    }
    if (input$alt == alt_list[[10]][4]) {
      return(as.matrix(group_2_filter_1()@data[, 58]))
    }
    if (input$alt == alt_list[[10]][5]) {
      return(as.matrix(group_2_filter_1()@data[, 59]))
    }
  })
  
  # calculate mean
  group_2_mean <- reactive({
    round(mean(group_2_filter_2()), digits = 2)
  })
  
  # Return list of reactive expressions
  return(
    list(
      area1 = reactive(input$area1),
      gender1 = reactive(input$gender1),
      age1 = reactive(input$age1),
      occupation1 = reactive(input$occupation1),
      education1 = reactive(input$education1),
      years1 = reactive(input$years1),
      pop1 = reactive(input$pop1),
      markers1 = reactive(input$markers1),
      
      area2 = reactive(input$area2),
      gender2 = reactive(input$gender2),
      age2 = reactive(input$age2), 
      occupation2 = reactive(input$occupation2),
      education2 = reactive(input$education2),
      years2 = reactive(input$years2),
      pop2 = reactive(input$pop2),
      markers2 = reactive(input$markers2),
      
      colorpal = reactive(input$colorpal),
      
      alt = reactive(input$alt),
      
      group_1_filter_1 = group_1_filter_1,
      group_1_filter_2 = group_1_filter_2,
      group_1_mean = group_1_mean,

      group_2_filter_1 = group_2_filter_1,
      group_2_filter_2 = group_2_filter_2, 
      group_2_mean = group_2_mean,
      
      theme = reactive(input$theme)
    )
  )
  
}