number <- function(pseudo.name, colNames, aNames, question.name, criterion.name, resultat) {
  results.vec <- c()
  rNames.vec <- c()
  resultNames.vec <- c()
  resultValues.vec <- c()
  
  # add new columns used for the proportinal scores used in the bipolar cardinal ranking of the alternatives
  resultat[pseudo.name] <- 7
  nr = 1
  v = c()
  for (i in colNames) {
    collName = paste0(question.name, nr, collapse = NULL) # a6 is the pseudo alternative!
    resultat[collName] <- 
      apply(resultat, 1, function(x) {
        # find the min and max values for question 1 and action 1-5 and the pseudo action (6)
        v.max <- max(as.numeric(x[c(colNames)]))
        v.min <- min(as.numeric(x[c(colNames)]))
        # calculate the proportinal score for question 1 and action i
        ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
      })
    nr = nr + 1
  }
  
  # remove all NaN produced by the proportinal score
  resultat <- resultat[complete.cases(resultat), ]
  
  # add new columns for the BCAR values of each alternative
  # cop = con or pro, 0 or 1
  columnNamesList <- c()
  nr = 1
  for (i in aNames) {
    cName = paste0(question.name, nr, ".cop", collapse = NULL) # cname = collName = alternativeName
    columnNamesList <- append(columnNamesList, cName)
    resultat[cName] <- apply(resultat, 1, function(x) {
      if (as.numeric(x[i]) < as.numeric(x[paste(question.name, "6", sep = "")])) {
        res <- 0
      } else {res <- 1}
    })
    nr = nr + 1
  }
  
  tmpresults.vec <- c()
  tmpnames.vec <- c()
  
  # calculate the con and pro index for q1a1
  # lambda = stakeholder weight, 1/qXaY.nr
  # create the total nr and the nr of cons and pros for each alternative
  for (i in columnNamesList) {
    tmpnames.vec <- c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrcon", sep = '')))
    tmpresults.vec <- c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 0)))
    tmpnames.vec <- c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrpro", sep = '')))
    tmpresults.vec <- c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 1)))
    tmpnames.vec <- c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nr", sep = '')))
    tmpresults.vec <- c(tmpresults.vec, c(sum(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 0), sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 1))))
  }
  
  names(tmpresults.vec) <- tmpnames.vec
  
  # preparing return values
  for (i in columnNamesList) {
    altName = substr(i, 1, 4)
    
    # number of members of the con group
    resultNames.vec <- c(resultNames.vec, c(paste(altName, ".nrcon", sep = '')))
    resultValues.vec <- c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrcon", sep = "")]))
    
    # number of members of the pro group     
    resultNames.vec <- c(resultNames.vec, c(paste(altName, ".nrpro", sep = '')))
    resultValues.vec <- c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrpro", sep = "")]))
  }
  names(resultValues.vec) <- resultNames.vec
  return(resultValues.vec)
}