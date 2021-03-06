library(rintrojs)

G1   <- HTML("<font color='steelblue'><b>G1</b></font>")
G2   <- HTML("<font color='firebrick'><b>G2</b></font>")
To    <- HTML("<font color='darkslateblue'><b>T</b></font>")

shinyUI(
  function(request) {
    navbarPage(
      title = HTML("Upplands Väsby Data Explorer"),
      id = "nav",
      position = "static-top",
      collapsible = TRUE,
      fluid = TRUE,
      tabPanel(
        title = "Map",
        icon = icon(name = "map-o"),
        fluidPage(
          fluidRow(
            column(
              width = 3,
              introjsUI(),
              introBox(
                menu_UI(id = "one"),
                data.step = 1,
                data.intro = paste0(
                  "This is the menu. It has three tabs: 'Start', ",
                  G1,
                  " (Group 1) and ",
                  G2,
                  " (Group 2).<br><br> 'Start' is where you begin your analysis. Select a focus area (1-10) and then one action (a-e) associated with that particular focus area. From the 'Start' tab you can also select a new color scheme for the map. A click on the <i class='fa fa-question'></i> Help button displays a brief introduction (this one!). <br><br> From the ",
                  G1,
                  " and ",
                  G2,
                  " tabs you can select geographical areas. Adjust the demographic variables (Gender, Age, Occupation, Education, and Length of Residency) to see the data from different viewpoints. At the bottom, two checkboxes let you enable or disable map markers and map popups."
                )
              )
            ),
            column(
              width = 6,
              introBox(
                leafletOutput(outputId = "map", height = 680),
                data.step = 2,
                data.intro = paste0(
                  "This is the map. Each selected area (polygon) is filled with a color representing the mean value of the selected action for ",
                  G1,
                  " and ",
                  G2,
                  ", respectively. <br><br> A color legend is available in the bottom left corner. The '+' and '-' sign in the top left corner enables you to zoom in or zoom out, respectively. <br><br> In the top right corner there is a tool for measuring distances and areas. In the bottom right corner there is a minimap that provides a geographical context relative to the map."
                )
              )
            ),
            column(
              width = 3,
              introBox(
                sidebarPanel(
                  width = 0,
                  tabsetPanel(
                    id = "plots",
                    tabPanel(
                      title = "F",
                      G1,
                      ggvisOutput(plot_id = "ggvis_1"),
                      G2,
                      ggvisOutput(plot_id = "ggvis_2"),
                      To,
                      ggvisOutput(plot_id = "ggvis_3")
                    ),
                    tabPanel(
                      title = "V",
                      G1,
                      ggvisOutput(plot_id = "ggvis_4"),
                      G2,
                      ggvisOutput(plot_id = "ggvis_5"),
                      To,
                      ggvisOutput(plot_id = "ggvis_6")
                    ),
                    tabPanel(
                      title = "D",
                      G1,
                      ggvisOutput(plot_id = "ggvis_7"),
                      G2,
                      ggvisOutput(plot_id = "ggvis_8"),
                      To,
                      ggvisOutput(plot_id = "ggvis_9")
                    ),
                    tabPanel(
                      title = "P",
                      G1,
                      ggvisOutput(plot_id = "ggvis_13"),
                      G2,
                      ggvisOutput(plot_id = "ggvis_14"),
                      To,
                      ggvisOutput(plot_id = "ggvis_15")
                    )
                  )
                ),
                data.step = 3,
                data.intro = paste0(
                  "This is the value plots panel. It has four tabs: 'F', 'V', 'D' and 'P'. <br><br> 'F' displays histograms of the frequency distribution for the selected action for ", G1, ", ", G2, ", and ", To, " (total). <br><br> 'V' displays bar plots of the relative mean value for each action (a-e) for ", G1, ", ", G2, ", and ", To, ". <br><br> 'D' displays bar plots of relative mean distance for each action (a-e) for ", G1, ", ", G2, ", and ", To, " (difference). <br><br> 'P' displays Pareto-efficient frontiers for ", G1, ", ", G2, ", and ", To, " (total). A Pareto frontier is the set of all optimal combinations of actions. A mouse roll-over gives detailed information about that particular portfolio. <br><br> To download a plot, click the <i class='fa fa-gear'></i> icon."
                )
              )
            )
          ),
          fluidRow(
            column(width = 3),
            column(
              width = 3,
              introBox(
                sidebarPanel(
                  width = 0,
                  id = "demographics1",
                  ggvisOutput(plot_id = "ggvis_16"),
                  ggvisOutput(plot_id = "ggvis_17"),
                  ggvisOutput(plot_id = "ggvis_20")
                ),
                
                data.step = 4,
                data.intro = paste0(
                  "This is the demographics panel (1/2). It displays bar plots of three background variables for ", G1, " and ", G2, ": Gender, Age, and Length of Residency. <br><br> To download a plot, click the <i class='fa fa-gear'></i> icon."
                )
              )
            ),
            column(
              width = 3,
              introBox(
                sidebarPanel(
                  width = 0,
                  id = "demographics2",
                  ggvisOutput(plot_id = "ggvis_18"),
                  ggvisOutput(plot_id = "ggvis_19")
                ),
                data.step = 5,
                
                data.intro = paste0(
                  "This is the demographics panel (2/2). It displays bar plots of two background variables for ", G1, " and ", G2, ": Occupation and Education. <br><br> To download a plot, click the <i class='fa fa-gear'></i> icon."
                )
              )
            ),
            
            column(
              width = 3,
              
              introBox(
                sidebarPanel(
                  width = 0,
                  tabsetPanel(
                    id = "portfolio-table",
                    tabPanel(
                      title = HTML(G1),
                      DT::dataTableOutput(outputId = "portfolios_group_1_table")
                    ),
                    tabPanel(
                      title = HTML(G2),
                      DT::dataTableOutput(outputId = "portfolios_group_2_table")
                    ),
                    tabPanel(
                      title = HTML(To),
                      DT::dataTableOutput(outputId = "portfolios_total_table")
                    )
                  )
                ),
                data.step = 6,
                data.intro = paste0(
                  "This is the portfolio details panel. It has three tabs: ",
                  G1,
                  ", ",
                  G2,
                  ", and ",
                  To,
                  " (total). <br><br> Column 'id' displays the portfolio identification number. Columns 'a' through 'e' indicate if the action is present (1) or not present (0) in the portfolio. Columns 'V' and 'D' display the value of and the distance within the portfolio, respectively."
                )
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Table",
        icon = icon(name = "table"),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = "area3",
              label = "Geographic Subarea",
              choices = b_area,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "gender3",
              label = "Gender",
              choices = b_gender_t,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "age3",
              label = "Age",
              choices = b_age,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "occupation3",
              label = "Occupation",
              choices = b_occupation_t,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "education3",
              label = "Education",
              choices = b_education_t,
              selected = "All",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "years3",
              label = "Length of Residency",
              choices = b_years_t,
              selected = "All",
              multiple = TRUE
            )
          ),
          bookmarkButton(id = "bookmark2")
        ),
        fluidRow(DT::dataTableOutput(outputId = "table"))
      ),
      tabPanel(
        title = "About",
        icon = icon(name = "info-circle"),
        includeMarkdown("about.md")
      )
    )
  }
)