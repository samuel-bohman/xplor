shinyUI(
  navbarPage(title = "Upplands Väsby Data Explorer", id = "nav", position = "static-top", collapsible = TRUE, fluid = TRUE, 
    tabPanel(title = "Map", icon = icon(name = "map-o"),
      fluidPage(
        fluidRow(
          column(width = 2,
            introjsUI(),  # Call introjsUI() to use rintrojs 
            introBox(
              menu_UI(id = "one"),
              data.step = 1,
              data.intro = "This is the main menu. It has three tabs: 'Start' is where you select a theme (1-10). Under each theme, there are five alternatives (a-e). With the palette drop-down you can select a color scheme for the map. Under the 'G1' and 'G2' tabs you select area(s) of interest for each respective group as well as five demographic variables: gender, age, occupation, education, and length of residency. Finally, you can decide if you want the map to display markers and/or popups for the selected area(s)."
            )
          ),
          column(width = 6,
            introBox(
              leafletOutput(outputId = "map", height = 680),
              data.step = 2,
              data.intro = "This is the map. Each area (polygon) is filled with a color according to a specified color scheme shown in the legend in the bottom left corner of the map. You can change color scheme in the main menu to the left."
              )
            ),
          column(width = 2,
            introBox(
              sidebarPanel(width = 0,
                tabsetPanel(
                  tabPanel(title = "H",
                    "G1",
                    ggvisOutput(plot_id = "ggvis_1"),
                    "G2",
                    ggvisOutput(plot_id = "ggvis_2"),
                    "T",
                    ggvisOutput(plot_id = "ggvis_3")
                  ),
                  tabPanel(title = "V",
                    "G1",
                    ggvisOutput(plot_id = "ggvis_4"),
                    "G2",
                    ggvisOutput(plot_id = "ggvis_5"),
                    "T",
                    ggvisOutput(plot_id = "ggvis_6")
                  ),
                  tabPanel(title = "D",
                    "G1",
                    ggvisOutput(plot_id = "ggvis_7"),
                    "G2",
                    ggvisOutput(plot_id = "ggvis_8"),
                    "Diff",
                    ggvisOutput(plot_id = "ggvis_9")
                  )
                )
              ),
              data.step = 3,
              data.intro = "This is the descriptive panel. It has 3 tabs: 'H' displays histograms over the value distributions, 'V' displays bar plots over mean values for each alternative, and 'D' displays bar plots over mean disagreements for each alternative."
            )
          ),
          column(width = 2,
            introBox(
              sidebarPanel(width = 0,
                tabsetPanel(
                  tabPanel(title = "P",
                    "G1",
                    ggvisOutput(plot_id = "ggvis_13"),
                    "G2",
                    ggvisOutput(plot_id = "ggvis_14"),
                    "T",
                    ggvisOutput(plot_id = "ggvis_15")
                  ),
                  tabPanel(title = "VtD",
                    "G1",
                    ggvisOutput(plot_id = "ggvis_10"),
                    "G2",
                    ggvisOutput(plot_id = "ggvis_11"),
                    "T",
                    ggvisOutput(plot_id = "ggvis_12")
                  )
                )
              ),
              data.step = 4,
              data.intro = "This is the portfolio panel. It has two tabs: 'P' displays optimized portfolios. 'VtD' displays Value-to-Disagreement bar plots for each alternative."
            )
          )
        ),
        fluidRow(
          column(width = 2),
          column(width = 6,
            introBox(
              sidebarPanel(width = 0,
                tabsetPanel(
                  tabPanel(title = "Demographics",
                    htmlOutput(outputId = "grid_ggvis")
                  )
                )
              ),
              data.step = 5,
              data.intro = "This is the demographics panel. It displays five plots: gender, age, occupation, education level, and length of residency."
            )
          ),
          column(width = 4,
            
            introBox(
              sidebarPanel(width = 0,
                tabsetPanel(
                  tabPanel(title = "G1",
                    DT::dataTableOutput(outputId = "portfolios_group_1_table")
                  ),
                  tabPanel(title = "G2",
                    DT::dataTableOutput(outputId = "portfolios_group_2_table")
                  ),
                  tabPanel(title = "T",
                    DT::dataTableOutput(outputId = "portfolios_total_table")
                  )
                )
              ),
              data.step = 6,
              data.intro = "This is the portfolio table. It has three tabs: 'G1' displays porfolio details for group 1, 'G2' displays portfolio details for group 2, and 'T' displays total portfolio details. All three tables display essentially the same information as the portfolio (P) plots."
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