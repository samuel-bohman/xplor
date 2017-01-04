library(shiny)
library(RMySQL)

options(mysql = list(
  "host" = "mysql.dsv.su.se",
  "port" = 3306,
  "user" = "ersa8789",
  "password" = "xo2eeD9Ooche"
))

databaseName <- "ersa8789"
table <- "questionnaires"

saveData <- function(message) {
    if (!is.null(message)) {
		db <- dbConnect(RMySQL::MySQL(),
			  dbname = databaseName,
			  host = options()$mysql$host, 
			  port = options()$mysql$port,
			  user = options()$mysql$user, 
			  password = options()$mysql$password)
		
		column_name <- "questionnaire_raw_html"
		
		message$data <- dbEscapeStrings(db, message$data)
		
		query <- sprintf("UPDATE %s SET %s='%s' WHERE userId=%i",
				  table, column_name, message$data, message$userId)
		
		response <- dbExecute(db, query)
		
		if (response == 0L) {
		  # Do an insert instead
		  query <- sprintf("INSERT INTO %s VALUES (%i, '%s')",
		      table, message$userId, message$data)
		  
		  response <- dbExecute(db, query)
		}
		
		dbDisconnect(db)
		response
	}
}

loadData <- function(message) {
	if (!is.null(message)) {
		db <- dbConnect(RMySQL::MySQL(),
				dbname = databaseName,
				host = options()$mysql$host,
				port = options()$mysql$port,
				user = options()$mysql$user,
				password = options()$mysql$password)
		
		column_name <- "questionnaire_raw_html"
		query <- sprintf("SELECT * FROM %s WHERE userId=%i", table, message$userId)
		
		response <- dbGetQuery(db, query)
		dbDisconnect(db)
		mydata <- response[1, column_name]
		Encoding(mydata) <- "UTF-8"
		mydata
	}
}

function(input, output, session) {
	obs1 <- observe({
		input$load_questionnaire
		raw_html <- loadData(input$load_questionnaire)
		session$sendCustomMessage(type = "loadQuestionnaireHandler", raw_html)
	})
  
	obs2 <- observe({
		input$save_questionnaire
		response <- saveData(input$save_questionnaire)
		session$sendCustomMessage(type = "saveQuestionnaireHandler", response)
	})

  # Generate a summary of the data
  #output$summary <- renderPrint({
  #  loadData()
  #})
 
  # When the client ends the session, suspend the observer.
  # Otherwise, the observer could keep running after the client
  # ends the session.
  #session$onSessionEnded(function() {
  #  obs$suspend()

    # Also clean up the log file for this example
  #  unlink(logfilename)
  #})
}
