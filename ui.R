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

        # panel 2
        lapply(1:2, function(i){
          absolutePanel(
            id = paste("panel", i, sep=""),
            class = "panel panel-default",
            draggable = TRUE,
            top = 30,
            left = 60 * i^2,
            right = "auto",
            bottom = "auto",
            width = 320,
            heigh = "auto",
            h3(paste("Dynamic: Group ", i, sep="")),

            lapply(seq_along(backgroundChoices), function(j) {
              selectInput(
                inputId = paste(uiNamesBg1[j], i, sep=""),
                #label = names(backgroundChoices)[j], #Taken from results_df for now
                label = paste(uiNamesBg1[j], i, sep=""),
                choices = backgroundChoices[[j]]
                )
            }),
            checkboxInput(
              inputId = paste("pop", i, sep=""),
              label = "Visa namn"
            ),
            checkboxInput(
              inputId = paste("markers", i, sep=""),
              label = "Visa markörer"
            )
          )
        }),
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
