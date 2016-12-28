## Returns a vector with the alternatives names
#
get_alternatives <- function(criterion){
  number <- unlist(criterion_number[[criterion]])
  alternatives <- c(paste("Alt.", number, letters[1:5], sep=""), paste("Alt.", number, "p", sep=""))
}

get_criterion <- function(criterion){
  number <- unlist(criterion_number[[criterion]])
  criterion <- paste("wc", number, ".car", sep="")
}

disagreement_data <- function(criterion, resultat) {
  
  alternatives <- get_alternatives(criterion)
  alternatives_prop_scores <- c(paste(alternatives, ".value", sep=""))
  alternatives_cop <- c(paste(alternatives, ".cop", sep=""))
  alternatives_nr <- c(paste(alternatives, ".nr", sep=""))
  alternatives_nrcon <- c(paste(alternatives, ".nrcon", sep=""))
  alternatives_nrpro <- c(paste(alternatives, ".nrpro", sep=""))
  alternatives_cval <- c(paste(alternatives, ".cval", sep=""))
  alternatives_pval <- c(paste(alternatives, ".pval", sep=""))
  alternatives_val <- c(paste(alternatives, ".val", sep=""))
  
  criterion.name <- get_criterion(criterion)
  
  resultat <- resultat@data
  pseudo.name <- tail(alternatives, n=1)

  resultNames.vec <- c()
  resultValues.vec <- c()
  
  tmpresults.vec <- c()
  tmpnames.vec <- c()
  
  # Add new columns for the proportinal scores in the bipolar cardinal ranking of alternatives
  resultat[pseudo.name] <- 7

  # Iterates over the alternatives-column # a6 is the pseudo alternative!
  for (i in 1:length(alternatives)) {
    
    # Adds columns with proportial scores
    resultat[alternatives_prop_scores[i]] <- 
      apply(resultat, 1, function(x) {
        # Find the min and max values for question 1 and action 1-5 and the pseudo action (6)
        v.max <- max(as.numeric(x[c(alternatives)]))
        v.min <- min(as.numeric(x[c(alternatives)]))
        # Calculate the proportinal score for question 1 and action i
        (as.numeric(x[alternatives[i]]) - v.min) / (v.max - v.min)
      })
    
    # Remove all NaN produced by the proportinal score
    resultat <- resultat[complete.cases(resultat), ]
    
    # Adds a con/pro column for each alternative
    resultat[alternatives_cop[i]] <-
      apply(resultat, 1, function(x) {
        if (as.numeric(x[alternatives[i]]) < as.numeric(x[pseudo.name])) {
          res <- 0
        } else {res <- 1}
      })
    
    # Sums nr, con, pro 
    tmpnames.vec <- c(tmpnames.vec, c(alternatives_nrcon[i]))
    tmpresults.vec <- c(tmpresults.vec, c(sum(resultat[, alternatives_cop[i]] == 0)))
    tmpnames.vec <- c(tmpnames.vec, c(alternatives_nrpro[i]))
    tmpresults.vec <- c(tmpresults.vec, c(sum(resultat[, alternatives_cop[i]] == 1)))
    tmpnames.vec <- c(tmpnames.vec, c(alternatives_nr[i]))
    tmpresults.vec <- c(tmpresults.vec, c(sum(sum(resultat[, alternatives_cop[i]] == 0), sum(resultat[, alternatives_cop[i]] == 1))))
    names(tmpresults.vec) <- tmpnames.vec
    
    # Calculate the con and pro index for q1a1
    # Lambda = stakeholder weight, 1/qXaY.nr
    # Create the total nr and the nr of cons and pros for each alternative
    
    # Add a column with weighted con values
    totNum <- tmpresults.vec[alternatives_nr[i]]
    lambda <- 1/totNum
      
    resultat[alternatives_cval[i]] <-
      apply(resultat, 1, function(x) {
        if (as.numeric(x[alternatives_cop[i]]) == 0) {
          qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseudo.name]))
          qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[alternatives_prop_scores[i]]))
          lambda * abs(qShAlpha - qShPartWorth)
        } else {0}
      })

    # Add a column with weighted pro values
    resultat[alternatives_pval[i]] <-
      apply(resultat, 1, function(x) {
        if (as.numeric(x[alternatives_cop[i]]) == 1) {
          qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseudo.name]))
          qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[alternatives_prop_scores[i]]))
          lambda * abs(qShAlpha - qShPartWorth)
        } else {0}
      })

    # Add a column with weighted values
    resultat[alternatives_val[i]] <-
      apply(resultat, 1, function(x) {
        qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseudo.name]))
        qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[alternatives_prop_scores[i]]))
        lambda * (qShPartWorth - qShAlpha)
      })
    
    # Preparing return values
    # Con index
    resultNames.vec <- c(resultNames.vec, c(alternatives_cval[i]))
    resultValues.vec <- c(resultValues.vec, c(sum(resultat[, alternatives_cval[i]])))
    
    # Pro index
    resultNames.vec <- c(resultNames.vec, c(alternatives_pval[i]))
    resultValues.vec <- c(resultValues.vec, c(sum(resultat[, alternatives_pval[i]])))
    
    # Avg value
    resultNames.vec <- c(resultNames.vec, c(alternatives_val[i]))
    resultValues.vec <- c(resultValues.vec, c(sum(resultat[, alternatives_val[i]])))
    
    # Number of members of the con group
    resultNames.vec <- c(resultNames.vec, c(alternatives_nrcon[i]))
    resultValues.vec <- c(resultValues.vec, c(tmpresults.vec[alternatives_nrcon[i]]))
    
    # Number of members of the pro group
    resultNames.vec <- c(resultNames.vec, c(alternatives_nrpro[i]))
    resultValues.vec <- c(resultValues.vec, c(tmpresults.vec[alternatives_nrpro[i]]))
 }

  names(resultValues.vec) <- resultNames.vec 

  return(resultValues.vec)
}

