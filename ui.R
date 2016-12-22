shinyUI(
  navbarPage("xplor", id = "nav", position = "static-top", collapsible = TRUE, fluid = FALSE,
    tabPanel(title = "Map", icon = icon("map-o"),
      sidebarLayout(fluid = FALSE,
        tabset_UI(id = "one"),
        mainPanel(width = 8,
          leafletOutput(outputId = "map", height = 615)
        )
      )
    ),
    tabPanel(title = "Plots", icon = icon("bar-chart")),
    tabPanel(title = "Table", icon = icon("table"),
      fluidRow(
        column(width = 3, selectInput(inputId = "area3", label = "Area", choices = b_area, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "gender3", label = "Gender", choices = b_gender, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "age3", label = "Age", choices = b_age, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "occupation3", label = "Occupation", choices = b_occupation, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "education3", label = "Education level", choices = b_education, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "years3", label = "Length of residency", choices = b_years, selected = "All", multiple = TRUE))
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