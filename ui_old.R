shinyUI(
  navbarPage("Xplor Beta", id = "nav", position = "static-top", collapsible = TRUE, fluid = FALSE, 
    tabPanel(title = "Map", icon = icon("map-o"),
      fixedRow(
        div(
          class = "outer",
          tags[["head"]](includeCSS("styles.css")),
          leafletOutput("map", width = "100%", height = "100%"),
          
          # panel 1
          absolutePanel(
            id = "panel1", class = "panel panel-default", draggable = TRUE, top = 20, left = 420, right = "auto", bottom = "auto", width = 350, height = "auto", h3("Themes"),
            selectInput(inputId = "theme", label = "Theme", choices = theme, selected = theme[1]),
            uiOutput("alternatives"),
            selectInput(inputId = "colors", label = "Palette", choices = rownames(subset(brewer.pal.info, category %in% "div")), selected = "RdYlGn")
          ),
          
          # panel 2  
          absolutePanel(
            id = "panel2", class = "panel panel-default", draggable = TRUE, top = 20, left = 60, right = "auto", bottom = "auto", width = 350, height = "auto", h3("Group 1"),
            lapply(seq_along(background_choices), function(j) {
              if (j == 1) {
                to_select <- rnd[[1]]
              } else {
                to_select <- background_choices[[1]][2]
              }
              selectInput(
                inputId = paste(ui_names_bg[j], 1, sep = ""),
                label = paste(dropdown_names_bg[j]),
                choices = background_choices[[j]],
                selected = to_select,
                multiple = TRUE
              )
            }),
            checkboxInput(inputId = "pop1", label = "Add popups", value = FALSE),
            checkboxInput(inputId = "markers1", label = "Add markers", value = TRUE)
          ),
          
          # panel 3
          absolutePanel(
            id = "panel3", class = "panel panel-default", draggable = TRUE, top = 20, left = "auto", right = 20, bottom = "auto", width = 350, height = "auto", h3("Group 2"),
            lapply(seq_along(background_choices), function(j) {
              if (j == 1) {
                to_select <- rnd[[2]]
              } else {
                to_select <- background_choices[[1]][2]
              }
              selectInput(
                inputId = paste(ui_names_bg[j], 2, sep = ""),
                label = paste(dropdown_names_bg[j]),
                choices = background_choices[[j]],
                selected = to_select,
                multiple = TRUE
              )
            }),
            checkboxInput(inputId = "pop2", label = "Add popups", value = FALSE),
            checkboxInput(inputId = "markers2", label = "Add markers", value = TRUE)
          )
          
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