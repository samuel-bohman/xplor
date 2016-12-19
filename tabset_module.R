# Module UI
tabset_module_UI <- function(id) {
  ns <- NS(id)
  rnd <- sample(x = vars_area[3:45], size = 10) %>% split(f = c(1, 2))
  tagList(
    tabBox(id = "tabset", width = NULL, 
      # Tab 1
      tabPanel(h4("Theme"),
        selectInput(ns("theme"), label = "Theme", choices = theme, selected = theme[1]),
        uiOutput(ns("alternatives"))
      ),
      # Tab 2
      tabPanel(h4("Group 1"), 
        lapply(seq_along(background_choices), function(j) {
          if (j == 1) {
            to_select <- rnd[[1]]
          } else {
            to_select <- background_choices[[1]][2]
          }
          selectInput(
            ns(paste(ui_names_bg[j], 1, sep = "")),
            label = paste(dropdown_names_bg[j]),
            choices = background_choices[[j]],
            selected = to_select,
            multiple = TRUE
          )
        }),
        checkboxInput(ns("pop1"), label = "Add popups", value = FALSE),
        checkboxInput(ns("markers1"), label = "Add markers", value = TRUE)
      ),
      # Tab 3
      tabPanel(h4("Group 2"), 
        lapply(seq_along(background_choices), function(j) {
          if (j == 1) {
            to_select <- rnd[[2]]
          } else {
            to_select <- background_choices[[1]][2]
          }
          selectInput(
            ns(paste(ui_names_bg[j], 2, sep = "")),
            label = paste(dropdown_names_bg[j]),
            choices = background_choices[[j]],
            selected = to_select,
            multiple = TRUE
          )
        }),
        checkboxInput(ns("pop2"), label = "Add popups", value = FALSE),
        checkboxInput(ns("markers2"), label = "Add markers", value = TRUE)
      )
    )
  )
}

# Module server
tabset_module <- function(input, output, session) {
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
}