library(rintrojs)

G1 <- "<font color='steelblue'><b>G1</b></font>"
G2 <- "<font color='firebrick'><b>G2</b></font>"
T <- "<font color='darkslateblue'><b>T</b></font>"
Diff <- "<font color='darkslateblue'><b>Diff</b></font>"

shinyUI(
  navbarPage(title = HTML("<font color='#4080bf'><b>Upplands Väsby Data Explorer</b></font>"), id = "nav", position = "static-top", collapsible = TRUE, fluid = TRUE, 
    tabPanel(title = "Map", icon = icon(name = "map-o"),
      fluidPage(
        fluidRow(
          column(width = 2,
            introjsUI(), 
            introBox(
              menu_UI(id = "one"),
              data.step = 1,
              data.intro = paste0("This is the menu. It has three tabs: 'Start', ", G1, " (Group 1) and ", G2, " (Group 2).<br><br> 'Start' is where you begin your analysis by selecting a 'Theme' (1-10) and then one 'Alternative' (a-e) associated with that theme. From the 'Start' tab you can also customize the map color scheme. A click on the 'Help' button displays a brief introduction (this one!). <br><br> From the ", G1, " and ", G2, " tabs you can select geographical areas from the dropdown list 'Area'. By adjusting the five demographic variables (Gender, Age, Occupation, Eduaction, and Length of residency) you can 'slice and dice' the data to see it from different viewpoints. Finally, you can enable or disable map markers and popups.")
            )
          ),
          column(width = 6,
            introBox(
              leafletOutput(outputId = "map", height = 680),
              data.step = 2,
              data.intro = paste0("This is the map. Each selected area (polygon) is filled with a color representing the mean value of the selected alternative for ", G1, " and ", G2, ", respectively. <br><br> A color legend is available in the bottom left corner of the map. The '+' and '-' signs in the top left corner enables you to zoom in or zoom out of the map. In the top right corner there is a tool for measuring distances and areas. The minimap in the bottom right corner provides an overview.")
              )
            ),
          column(width = 2,
            introBox(
              sidebarPanel(width = 0,
                tabsetPanel(
                  tabPanel(title = "F",
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
              data.intro = paste0("This is the descriptive panel. It has three tabs: 'F', 'V', and 'D'. <br><br> 'F' displays histograms of the frequency distribution for the selected alternative for ", G1, ", ", G2, ", and ", T, " (total). <br><br> 'V' displays bar plots of the relative mean value for each alternative (a-e) for ", G1, ", ", G2, ", and ", T, " (total). <br><br> 'D' displays bar plots of relative mean disagreement for each alternative (a-e) for ", G1, ", ", G2, ", and ", Diff, " (difference). <br><br> All plots can be downloaded in SVG or PNG format by clicking on the gear icon <i class='fa fa-gear'></i>.")
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
              data.intro = paste0("This is the portfolio panel. It has two tabs: 'P' and 'VtD'. <br><br> 'P' displays line graphs of efficient portfolios of alternatives for ", G1, ", ", G2, ", and ", T, " (total). <br><br> 'VtD' displays value-to-disagreement bar plots for each alternative (a-e). <br><br> All plots can be downloaded in SVG or PNG format by clicking on the gear icon <i class='fa fa-gear'></i>.")
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
              data.intro = paste0("This is the demographics panel. It displays bar plots of five background variables for ", G1, " and ", G2, ": Gender, Age, Occupation, Education level, and Length of residency. <br><br> All plots can be downloaded in SVG or PNG format by clicking on the gear icon <i class='fa fa-gear'></i>.")
            )
          ),
          column(width = 4,
            
            introBox(
              sidebarPanel(width = 0,
                tabsetPanel(
                  tabPanel(title = HTML(G1),
                    DT::dataTableOutput(outputId = "portfolios_group_1_table")
                  ),
                  tabPanel(title = HTML(G2),
                    DT::dataTableOutput(outputId = "portfolios_group_2_table")
                  ),
                  tabPanel(title = HTML(T),
                    DT::dataTableOutput(outputId = "portfolios_total_table")
                  )
                )
              ),
              data.step = 6,
              data.intro = paste0("This is the portfolio table panel. It has three tabs: ", G1, ", ", G2, ", and ", T, " (total). <br><br> Column 'id' displays the portfolio identification number. Columns 'a' through 'e' indicate if the alternative is present (1) or not present (0) in the portfolio. Columns 'V' and 'D' display the value of and the disagreement in the portfolio, respectively. Column 'VtD' displays the value-to-disagreement ratio of the portfolio.")
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