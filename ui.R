shinyUI(
  navbarPage(title = "xplor", id = "nav", position = "static-top", collapsible = TRUE, fluid = TRUE, 
    
    tabPanel(title = "Map", icon = icon(name = "map-o"),
      fluidPage(
        fluidRow(
          column(width = 3, tabset_UI(id = "one")),
          column(width = 6, leafletOutput(outputId = "map", height = 740)),
          column(width = 3, 
            sidebarPanel(width = 0,
              tabsetPanel(
                tabPanel(title = "Desc.",
                  "Group 1",
                  ggvisOutput("ggvis_1"),
                  "Group 2",
                  ggvisOutput("ggvis_2"),
                  "Group 1 and 2",
                  ggvisOutput("ggvis_3")
                ),
                tabPanel(title = "Value",
                  "Group 1",
                  ggvisOutput("ggvis_4"),
                  "Group 2",
                  ggvisOutput("ggvis_5"),
                  "Group 1 and 2",
                  ggvisOutput("ggvis_6")
                ),
                tabPanel(title = "Dis.",
                  "Group 1",
                  ggvisOutput("ggvis_7"),
                  "Group 2",
                  ggvisOutput("ggvis_8"),
                  "Between group 1 and 2",
                  ggvisOutput("ggvis_9")
                ),
                tabPanel(title = "Port.",
                  "Group 1",
                  ggvisOutput("ggvis_10"),
                  "Group 2",
                  ggvisOutput("ggvis_11"),
                  "Group 1 and 2",
                  ggvisOutput("ggvis_12")
                )
              )
            )
          )
        )
      )
    ),
    
    tabPanel(title = "Table", icon = icon(name = "table"),
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
    tabPanel(title = "Report", icon = icon(name = "file-o")),
    tabPanel(title = "Help", icon = icon(name = "question")),
    tabPanel(title = "Code", icon = icon(name = "github"))
  )
)