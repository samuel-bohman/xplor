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
        # Panel 1 & 2 dynamically
        lapply(1:group_amount, function(i) {
          absolutePanel(
            id = paste("panel", i, sep = ""),
            class = "panel panel-default",
            draggable = TRUE,
            top = 30,
            left = 60 * (i ^ 2.7),
            right = "auto",
            bottom = "auto",
            width = 320,
            heigh = "auto",
            h3(paste("Grupp ", i, sep = "")),
            lapply(seq_along(background_choices), function(j) {
              # Set selection of first dropdown to blank and the rest to "Alla"
              if (j == 1){
                to_select <- background_choices[[1]][1]
              }else{
                to_select <- background_choices[[1]][2]
              }
              selectInput(
                inputId = paste(ui_names_bg[j], i, sep = ""),
                label = paste(dropdown_names_bg[j], ":", sep = ""),
                choices = background_choices[[j]],
                selected = to_select
                )
            }),
            checkboxInput(
              inputId = paste("pop", i, sep = ""),
              label = "Visa namn"
            ),
            checkboxInput(
              inputId = paste("markers", i, sep = ""),
              label = "Visa markörer"
            )
          )
        }),
        # Panel 3
        absolutePanel(
          id = "panel3",
          class = "panel panel-default",
          draggable = TRUE,
          top = 30,
          left = 750,
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