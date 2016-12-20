shinyUI(
  navbarPage("xplor beta", id = "nav", position = "static-top", collapsible = TRUE, fluid = FALSE,
    tabPanel(title = "Map", icon = icon("map-o"),
      sidebarLayout(fluid = FALSE,
        tabset_module_UI(id = "one"),
        # sidebarPanel(width = 4,
        #   tabsetPanel(
        #     type = "tabs",
        #     tabPanel("Theme",
        #       selectInput(inputId = "theme", label = "Theme", choices = theme, selected = theme[1]),
        #       uiOutput("alternatives"),
        #       selectInput(inputId = "colors", label = "Palette", choices = rownames(subset(brewer.pal.info, category %in% "div")), selected = "RdYlGn")
        #     ),
        #     tabPanel("Group 1",
        #       lapply(seq_along(background_choices), function(j) {
        #         if (j == 1) {
        #           to_select <- rnd[[1]]
        #         } else {
        #           to_select <- background_choices[[1]][2]
        #         }
        #         selectInput(
        #           inputId = paste(ui_names_bg[j], 1, sep = ""),
        #           label = paste(dropdown_names_bg[j]),
        #           choices = background_choices[[j]],
        #           selected = to_select,
        #           multiple = TRUE
        #         )
        #       }),
        #       checkboxInput(inputId = "pop1", label = "Add popups", value = FALSE),
        #       checkboxInput(inputId = "markers1", label = "Add markers", value = TRUE)
        #     ),
        #     tabPanel("Group 2",
        #       lapply(seq_along(background_choices), function(j) {
        #         if (j == 1) {
        #           to_select <- rnd[[2]]
        #         } else {
        #           to_select <- background_choices[[1]][2]
        #         }
        #         selectInput(
        #           inputId = paste(ui_names_bg[j], 2, sep = ""),
        #           label = paste(dropdown_names_bg[j]),
        #           choices = background_choices[[j]],
        #           selected = to_select,
        #           multiple = TRUE
        #         )
        #       }),
        #       checkboxInput(inputId = "pop2", label = "Add popups", value = FALSE),
        #       checkboxInput(inputId = "markers2", label = "Add markers", value = TRUE)
        #     )
        #   )
        # ),
        mainPanel(width = 8,
          leafletOutput(outputId = "map", height = 615)
        )
      )
    ),
    tabPanel(title = "Plots", icon = icon("bar-chart")),
    tabPanel(title = "Table", icon = icon("table"),
      fluidRow(
        column(width = 3, selectInput(inputId = "area3", label = "Area", choices = vars_area, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "gender3", label = "Gender", choices = vars_gender, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "age3", label = "Age", choices = vars_age, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "occupation3", label = "Occupation", choices = vars_occupation, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "education3", label = "Education level", choices = vars_education, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "years3", label = "Length of residency", choices = vars_years, selected = "All", multiple = TRUE))
      ),
      fluidRow(
        DT::dataTableOutput(outputId = "table")
      )
    ),
    tabPanel(title = "Report", icon = icon("file-o")),
    tabPanel(title = "Help", icon = icon("question")),
    tabPanel(title = "Code", icon = icon("github"))
  )
)