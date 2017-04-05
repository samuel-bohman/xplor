shinyUI(
  navbarPage(title = "Upplands Väsby Data Explorer", id = "nav", position = "static-top", collapsible = TRUE, fluid = TRUE, 
    
    tabPanel(title = "Map", icon = icon(name = "map-o"),
      fluidPage(
        fluidRow(
          column(width = 2, tabset_UI(id = "one")),
          column(width = 6, leafletOutput(outputId = "map", height = 680)),
          column(width = 2,
            sidebarPanel(width = 0,
              tabsetPanel(
                tabPanel(title = "Des.",
                  "Group 1",
                  ggvisOutput(plot_id = "ggvis_1"),
                  "Group 2",
                  ggvisOutput(plot_id = "ggvis_2"),
                  "Total",
                  ggvisOutput(plot_id = "ggvis_3")
                ),
                tabPanel(title = "V",
                  "Group 1",
                  ggvisOutput(plot_id = "ggvis_4"),
                  "Group 2",
                  ggvisOutput(plot_id = "ggvis_5"),
                  "Total",
                  ggvisOutput(plot_id = "ggvis_6")
                ),
                tabPanel(title = "D",
                  "Group 1",
                  ggvisOutput(plot_id = "ggvis_7"),
                  "Group 2",
                  ggvisOutput(plot_id = "ggvis_8"),
                  "Between group 1 and 2",
                  ggvisOutput(plot_id = "ggvis_9")
                ),
                tabPanel(title = "VtD",
                  "Group 1",
                  ggvisOutput(plot_id = "ggvis_10"),
                  "Group 2",
                  ggvisOutput(plot_id = "ggvis_11"),
                  "Total",
                  ggvisOutput(plot_id = "ggvis_12")
                )
              )
            )
          ),
          column(width = 2,
            sidebarPanel(width = 0,
              tabsetPanel(
                tabPanel(title = "Portfolios",
                  "Group 1",
                  ggvisOutput(plot_id = "ggvis_13"),
                  "Group 2",
                  ggvisOutput(plot_id = "ggvis_14"),
                  "Total",
                  ggvisOutput(plot_id = "ggvis_15")
                )
              )
            )
          )
        ),
        fluidRow(
          column(width = 2),
          column(width = 6,
            sidebarPanel(width = 0,
              tabsetPanel(
                tabPanel(title = "Demographics",
                  htmlOutput(outputId = "grid_ggvis")
                )
              )
            )
          ),
          column(width = 4,
            sidebarPanel(width = 0,
              tabsetPanel(
                tabPanel(title = "Group 1",
                  DT::dataTableOutput(outputId = "portfolios_group_1_table")
                ),
                tabPanel(title = "Group 2",
                  DT::dataTableOutput(outputId = "portfolios_group_2_table")
                ),
                tabPanel(title = "Total",
                  DT::dataTableOutput(outputId = "portfolios_total_table")
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
        column(width = 3, selectInput(inputId = "gender3", label = "Gender", choices = b_gender_t, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "age3", label = "Age", choices = b_age, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "occupation3", label = "Occupation", choices = b_occupation_t, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "education3", label = "Education level", choices = b_education_t, selected = "All", multiple = TRUE)),
        column(width = 3, selectInput(inputId = "years3", label = "Length of residency", choices = b_years_t, selected = "All", multiple = TRUE))
      ),
      fluidRow(
        DT::dataTableOutput(outputId = "table")
      )
    ),
    tabPanel(title = "About", icon = icon(name = "question"),
      includeMarkdown("about.md"))
  )
)