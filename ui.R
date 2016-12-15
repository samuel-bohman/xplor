# header ----
header <- dashboardHeader(title = "xplor beta", titleWidth = 150)

# sidebar ----
sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(id = "tabs",
    menuItem(text = "Map", tabName = "map", icon = icon("map-o"), selected = TRUE),
    menuItem(text = "Plots", tabName = "plots", icon = icon("bar-chart")),
    menuItem(text = "Table", tabName = "table", icon = icon("table")),
    menuItem(text = "Report", tabName = "report", icon = icon("file-o")),
    menuItem(text = "About", tabName = "about", icon = icon("question")),
    menuItem(text = "Code", href = "https://github.com/samuel-bohman/xplor/", icon = icon("github"))
  )
)

results_df <- read.table("data/results.csv", header = TRUE, sep = ";", fileEncoding = "UTF-8")

to_select_rnd <- function(group) {
  rnd <- NULL
rnd <- sample(x = vars_area[3:45], size = 10) %>% split(f = c(1, 2))
  if (group == 1) {
  rnd[[1]]
  } else {
    rnd[[2]]
    }
}

# body ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
      fluidRow(
        
        # column 1
        column(width = 3,
          tabBox(id = "tabset", width = NULL,
            tabPanel(h4("Theme"),
              selectInput(inputId = "themes", label = "Theme", choices = themes, selected = themes[1]),
              uiOutput("alternatives"),
              selectInput(inputId = "colors", label = "Palette", choices = rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "RdYlGn")
            ),
            tabPanel(h4("Group 1"), 
              lapply(seq_along(background_choices), function(j) {
                if (j == 1) {
                  to_select <- to_select_rnd(1)
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
              checkboxInput(inputId = "pop1", label = "Add popups", value = TRUE),
              checkboxInput(inputId = "markers1", label = "Add markers", value = FALSE)
            ),
            tabPanel(h4("Group 2"), 
              lapply(seq_along(background_choices), function(j) {
                if (j == 1) {
                  to_select <- to_select_rnd(2)
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
        ),
        
        # column 2
        column(width = 8,
          fluidRow(
            infoBoxOutput(outputId = "group_1_mean"),
            valueBoxOutput(outputId = "group_2_mean"),
            valueBoxOutput(outputId = "overall_mean")
          ),
          box(width = NULL, leafletOutput(outputId = "map", height = 800))
        )
      )
    ),
    tabItem(tabName = "plots", box(title = "Plots", width = NULL)),
    tabItem(tabName = "table", 
      box(title = NULL, width = NULL, 
        fluidRow(
          column(width = 3, selectInput(inputId = "area3", label = "Area", choices = vars_area, selected = "All", multiple = TRUE)),
          column(width = 3, selectInput(inputId = "gender3", label = "Gender", choices = vars_gender, selected = "All", multiple = TRUE)),
          column(width = 3, selectInput(inputId = "age3", label = "Age", choices = vars_age, selected = "All", multiple = TRUE)),
          column(width = 3, selectInput(inputId = "occupation3", label = "Occupation", choices = vars_occupation, selected = "All", multiple = TRUE)),
          column(width = 3, selectInput(inputId = "education3", label = "Education level", choices = vars_education, selected = "All", multiple = TRUE)),
          column( width = 3, selectInput(inputId = "years3", label = "Length of residency", choices = vars_years, selected = "All", multiple = TRUE))
        )
      ),
      box(title = NULL, width = NULL, DT::dataTableOutput(outputId = "table"))),
    tabItem(tabName = "report", box(title = "Report", width = NULL)),
    tabItem(tabName = "about", box(title = "About", width = NULL))
  )
)

# dashboardPage
ui <- dashboardPage(header,
  sidebar,
  body)
