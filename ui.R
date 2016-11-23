shinyUI(
  navbarPage(
    "xploR beta",
    id = "nav",
    position = "static-top",
    collapsible = TRUE,
    tabPanel(
      "Karta",
      div(
        class = "outer",
        tags[["head"]](includeCSS("styles.css")),
        leafletOutput("map", width = "100%", height = "100%"),
        # panel 1

        absolutePanel(
          id = "panel1",
          class = "panel panel-default",
          draggable = TRUE,
          top = 30,
          left = 60,
          right = "auto",
          bottom = "auto",
          width = 320,
          heigh = "auto",
          h3("Dynamic: Group 1"),
          lapply(seq_along(backgroundChoices), function(i) {
            selectInput(
              inputId = paste(uiNamesBg1[i], 1, sep=""),
              #label = names(backgroundChoices)[i], #Taken from results_df for now
              label = paste(uiNamesBg1[i], 1, sep=""),
              choices = backgroundChoices[[i]]
              )
            #selectInput(paste0('a', i), paste0('SelectA', i),
            #choices = sample(LETTERS, 5))
          }),
          checkboxInput(
            inputId = "pop1",
            label = "Visa namn"
          ),
          checkboxInput(
            inputId = "markers1",
            label = "Visa markörer"
          )
        ),
        # panel 2
        absolutePanel(
          id = "panel2",
          class = "panel panel-default",
          draggable = TRUE,
          top = 30,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 320,
          height = "auto",
          h3("Grupp 2"),
          selectInput(
            inputId = "area2",
            label = "Område:",
            choices = vars_area,
            selected = c("Fresta glesbygd")
          ),
          selectInput(
            inputId = "sex2",
            label = "Kön:",
            choices = vars_sex,
            selected = "Man"
          ),
          selectInput(
            inputId = "age2",
            label = "Ålder:",
            choices = vars_age,
            selected = c("Alla")
          ),
          selectInput(
            inputId = "occupation2",
            label = "Sysselsättning:",
            choices = vars_occupation,
            selected = "Alla"
          ),
          selectInput(
            inputId = "education2",
            label = "Utbildningsnivå:",
            choices = vars_education,
            selected = "Alla"
          ),
          selectInput(
            inputId = "years2",
            label = "Hur länge bott i kommunen:",
            choices = vars_years,
            selected = "Alla"
          ),
          checkboxInput(
            inputId = "pop2",
            label = "Visa namn"
          ),
          checkboxInput(
            inputId = "markers2",
            label = "Visa markörer"
          )
        ),
        # panel 3
        absolutePanel(
          id = "panel3",
          class = "panel panel-default",
          draggable = TRUE,
          top = 30,
          left = 400,
          right = "auto",
          bottom = "auto",
          width = "auto",
          height = "auto",
          h3("Tema & Alternativ"),
          selectInput(
            inputId = "themes",
            label = h5("Tema"),
            choices = themes,
            selected = "1. Parker & grönområden"
          ),
          uiOutput("alternatives"),
          selectInput(
            inputId = "colors",
            label = "Färgschema",
            choices = rownames(subset(
              brewer.pal.info, category %in% c("seq", "div")
            )),
            selected = "RdYlGn"
          )
        )
      )
    ),
    tabPanel(
      "Tabell",
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "area",
            label = "Område:",
            choices = vars_area,
            selected = "Alla"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "sex",
            label = "Kön:",
            choices = vars_sex,
            selected = "Alla"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "age",
            label = "Ålder:",
            choices = vars_age,
            selected = "Alla"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "occupation",
            label = "Sysselsättning:",
            choices = vars_occupation,
            selected = "Alla"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "education",
            label = "Utbildningsnivå:",
            choices = vars_education,
            selected = "Alla"
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "years",
            label = "Hur länge bott i kommunen:",
            choices = vars_years,
            selected = "Alla"
          )
        )
      ),
      fluidRow(DT::dataTableOutput(outputId = "table"))
    ),
    tabPanel("Hjälp",
             fluidRow(includeMarkdown("help.Rmd")))
  )
)
