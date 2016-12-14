## FUNCTIONS

# Bipolar Cardinal Ranking.
calculateBCAR <-
  function(pseudo.name,
    colNames,
    aNames,
    question.name,
    criterion.name,
    resultat) {
    #tmpresults.vec <- c()
    results.vec <- c()
    rNames.vec <- c()
    resultNames.vec <- c()
    resultValues.vec <- c()
    
    # Adds new columns used for the proportinal scores used in the bipolar cardinal ranking of the alternatives.
    resultat[pseudo.name] <- 7
    nr = 1
    v = c()
    for (i in colNames) {
      collName = paste0(question.name, nr, collapse = NULL) # a6 is the pseudo alternative!
      resultat[collName] <- apply(resultat, 1, function(x) {
        # finds the min and max values for question 1 and action 1-5 and the pseudo action (6)
        v.max <- max(as.numeric(x[c(colNames)]))
        v.min <- min(as.numeric(x[c(colNames)]))
        
        # calculates the proportinal score for question 1 and action i
        ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
      })
      nr = nr + 1
    }
    
    # Removes all NaN produced by the proportinal score
    resultat <- resultat[complete.cases(resultat), ]
    
    # Adds new columns for the BCAR values of each alternative.
    # cop = con or pro, 0 or 1
    columnNamesList <- c()
    nr = 1
    for (i in aNames) {
      cName = paste0(question.name, nr, ".cop", collapse = NULL) #cname = collName = alternativeName
      columnNamesList <- append(columnNamesList, cName)
      resultat[cName] <- apply(resultat, 1, function(x) {
        if (as.numeric(x[i]) < as.numeric(x[paste(question.name, "6", sep = "")])) {
          res <- 0
        }
        else {
          res <- 1
        }
      })
      nr = nr + 1
    }
    
    
    tmpresults.vec <- c()
    tmpnames.vec <- c()
    
    ## Calculates the con- and pro-index for q1a1.
    # lambda = stakeholder weight, 1/qXaY.nr
    
    # Creates the total nr and the nr of cons and pros for each alternative
    for (i in columnNamesList) {
      tmpnames.vec <-
        c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrcon", sep = '')))
      tmpresults.vec <-
        c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
            "")] == 0)))
      tmpnames.vec <-
        c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrpro", sep = '')))
      tmpresults.vec <-
        c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
            "")] == 1)))
      tmpnames.vec <-
        c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nr", sep = '')))
      tmpresults.vec <-
        c(tmpresults.vec, c(sum(
          sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 0), sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
              "")] == 1)
        )))
    }
    
    names(tmpresults.vec) <- tmpnames.vec
    
    
    # Adds a column with weighted con-values
    for (i in columnNamesList) {
      altName <- substr(i, 1, 4)
      pseuName <- c(paste(substr(i, 1, 3), "6", sep = ''))
      totNum <- tmpresults.vec[paste(substr(i, 1, 4), ".nr", sep ="")]
      lambda <- 1/totNum
      
      resultat[paste(altName, ".cval", sep = '')] <-
        apply(resultat, 1, function(x) {
          if (as.numeric(x[paste(altName, ".cop", sep = '')]) == 0) {
            qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseuName]))
            qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
            res <- lambda * abs(qShAlpha - qShPartWorth)
            
          } else
            res <- 0
        })
    }
    
    # Adds a column with weighted pro-values
    for (i in columnNamesList) {
      altName <- substr(i, 1, 4)
      pseuName <- c(paste(substr(i, 1, 3), "6", sep = ''))
      totNum <- tmpresults.vec[paste(substr(i, 1, 4), ".nr", sep ="")]
      lambda <- 1/totNum
      # print(paste("totalNumber:", totalNumber))
      #print(paste("totNum:", totNum))
      resultat[paste(altName, ".pval", sep = '')] <-
        apply(resultat, 1, function(x) {
          if (as.numeric(x[paste(altName, ".cop", sep = '')]) == 1) {
            qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseuName]))
            qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
            #print(paste("lambda: ", lambda))
            #print(paste("alpha:", qShAlpha))
            #print(paste("pw:", qShPartWorth))
            #print(paste("res", lambda * abs(qShAlpha - qShPartWorth)))
            res <- lambda * abs(qShAlpha - qShPartWorth)
            
          } else
            res <- 0
        })
    }
    
    # Adds a column with weighted values
    for (i in columnNamesList) {
      altName <- substr(i, 1, 4)
      totNum <- tmpresults.vec[paste(substr(i, 1, 4), ".nr", sep ="")]
      lambda <- 1/totNum
      resultat[paste(altName, ".val", sep = '')] <-
        apply(resultat, 1, function(x) {
          qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseuName]))
          qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
          res <- lambda * (qShPartWorth - qShAlpha)
          #qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
          #res <- lambda * qShPartWorth
        })
    }
    
    # Preparing return values
    for (i in columnNamesList) {
      altName = substr(i, 1, 4)
      
      #con-index
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".conidx", sep = '')))
      resultValues.vec <-
        c(resultValues.vec, c(sum(resultat[, paste(altName, ".cval", sep = "")])))
      
      #pro-index
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".proidx", sep = '')))
      resultValues.vec <-## FUNCTIONS
        
        # Bipolar Cardinal Ranking.
        calculateBCAR <-
        function(pseudo.name,
          colNames,
          aNames,
          question.name,
          criterion.name,
          resultat) {
          #tmpresults.vec <- c()
          results.vec <- c()
          rNames.vec <- c()
          resultNames.vec <- c()
          resultValues.vec <- c()
          
          # Adds new columns used for the proportinal scores used in the bipolar cardinal ranking of the alternatives.
          resultat[pseudo.name] <- 7
          nr = 1
          v = c()
          for (i in colNames) {
            collName = paste0(question.name, nr, collapse = NULL) # a6 is the pseudo alternative!
            resultat[collName] <- apply(resultat, 1, function(x) {
              # finds the min and max values for question 1 and action 1-5 and the pseudo action (6)
              v.max <- max(as.numeric(x[c(colNames)]))
              v.min <- min(as.numeric(x[c(colNames)]))
              
              # calculates the proportinal score for question 1 and action i
              ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
            })
            nr = nr + 1
          }
          
          # Removes all NaN produced by the proportinal score
          resultat <- resultat[complete.cases(resultat), ]
          
          # Adds new columns for the BCAR values of each alternative.
          # cop = con or pro, 0 or 1
          columnNamesList <- c()
          nr = 1
          for (i in aNames) {
            cName = paste0(question.name, nr, ".cop", collapse = NULL) #cname = collName = alternativeName
            columnNamesList <- append(columnNamesList, cName)
            resultat[cName] <- apply(resultat, 1, function(x) {
              if (as.numeric(x[i]) < as.numeric(x[paste(question.name, "6", sep = "")])) {
                res <- 0
              }
              else {
                res <- 1
              }
            })
            nr = nr + 1
          }
          
          
          tmpresults.vec <- c()
          tmpnames.vec <- c()
          
          ## Calculates the con- and pro-index for q1a1.
          # lambda = stakeholder weight, 1/qXaY.nr
          
          # Creates the total nr and the nr of cons and pros for each alternative
          for (i in columnNamesList) {
            tmpnames.vec <-
              c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrcon", sep = '')))
            tmpresults.vec <-
              c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
                  "")] == 0)))
            tmpnames.vec <-
              c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrpro", sep = '')))
            tmpresults.vec <-
              c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
                  "")] == 1)))
            tmpnames.vec <-
              c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nr", sep = '')))
            tmpresults.vec <-
              c(tmpresults.vec, c(sum(
                sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 0), sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
                    "")] == 1)
              )))
          }
          
          names(tmpresults.vec) <- tmpnames.vec
          
          
          # Adds a column with weighted con-values
          for (i in columnNamesList) {
            altName <- substr(i, 1, 4)
            pseuName <- c(paste(substr(i, 1, 3), "6", sep = ''))
            totNum <- tmpresults.vec[paste(substr(i, 1, 4), ".nr", sep ="")]
            lambda <- 1/totNum
            
            resultat[paste(altName, ".cval", sep = '')] <-
              apply(resultat, 1, function(x) {
                if (as.numeric(x[paste(altName, ".cop", sep = '')]) == 0) {
                  qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseuName]))
                  qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
                  res <- lambda * abs(qShAlpha - qShPartWorth)
                  
                } else
                  res <- 0
              })
          }
          
          # Adds a column with weighted pro-values
          for (i in columnNamesList) {
            altName <- substr(i, 1, 4)
            pseuName <- c(paste(substr(i, 1, 3), "6", sep = ''))
            totNum <- tmpresults.vec[paste(substr(i, 1, 4), ".nr", sep ="")]
            lambda <- 1/totNum
            # print(paste("totalNumber:", totalNumber))
            #print(paste("totNum:", totNum))
            resultat[paste(altName, ".pval", sep = '')] <-
              apply(resultat, 1, function(x) {
                if (as.numeric(x[paste(altName, ".cop", sep = '')]) == 1) {
                  qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseuName]))
                  qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
                  #print(paste("lambda: ", lambda))
                  #print(paste("alpha:", qShAlpha))
                  #print(paste("pw:", qShPartWorth))
                  #print(paste("res", lambda * abs(qShAlpha - qShPartWorth)))
                  res <- lambda * abs(qShAlpha - qShPartWorth)
                  
                } else
                  res <- 0
              })
          }
          
          # Adds a column with weighted values
          for (i in columnNamesList) {
            altName <- substr(i, 1, 4)
            totNum <- tmpresults.vec[paste(substr(i, 1, 4), ".nr", sep ="")]
            lambda <- 1/totNum
            resultat[paste(altName, ".val", sep = '')] <-
              apply(resultat, 1, function(x) {
                qShAlpha = (as.numeric(x[criterion.name]) * as.numeric(x[pseuName]))
                qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
                res <- lambda * (qShPartWorth - qShAlpha)
                #qShPartWorth = (as.numeric(x[criterion.name]) * as.numeric(x[altName]))
                #res <- lambda * qShPartWorth
              })
          }
          
          # Preparing return values
          for (i in columnNamesList) {
            altName = substr(i, 1, 4)
            
            #con-index
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".conidx", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(sum(resultat[, paste(altName, ".cval", sep = "")])))
            
            #pro-index
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".proidx", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(sum(resultat[, paste(altName, ".pval", sep = "")])))
            
            #avg value   
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".val", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(sum(resultat[, paste(altName, ".val", sep = "")])))
            
            # number of members of the con-group
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".nrcon", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrcon", sep ="")]))
            
            # number of members of the pro-group     
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".nrpro", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrpro", sep ="")]))
          }
          
          names(resultValues.vec) <- resultNames.vec
          #write.xlsx(resultat, paste(sample(1:1000, 1), "resultat.xlsx", sep=""))
          return(resultValues.vec)
        }
      
      calculateCAR <- function(resultat) {
        ## Cardinal ranking
        # q1a1 = qustion 1, action 1
        # Adds new columns of the cardinal ranking of the criteria.
        wColNames = c(
          "X1..Parker.och.grönområden",
          "X2..Mångfald.i.bostadsutbudet",
          "X3..Levandegöra.gemensamma.platser",
          "X4..Kommunikationer",
          "X5..Kultur.och.fritid",
          "X6..Utbildning",
          "X7..Omsorg",
          "X8..Skolan",
          "X9..Trygghet",
          "X10..Ekologisk.hållbarhet"
        )
        
        # Adds a column with reformatted criterion ranks. The most important is set to 1, e.g., if c_1 = 14, c_2 = 10, c_1 is formatted to, 14-14+1 = 1, and c_2 to, 14-10+1 = 5.
        # rc = ranked criterion
        nr <- 1
        v <- c()
        for (i in wColNames) {
          collName = paste0("rc", nr, collapse = NULL)
          resultat[collName] <- apply(resultat, 1, function(x) {
            v <-
              max(as.numeric(c(x["X1..Parker.och.grönområden"], x["X2..Mångfald.i.bostadsutbudet"], x["X3..Levandegöra.gemensamma.platser"], x["X4..Kommunikationer"],
                x["X5..Kultur.och.fritid"], x["X6..Utbildning"], x["X7..Omsorg"], x["X8..Skolan"], x["X9..Trygghet"], x["X10..Ekologisk.hållbarhet"])))
            
            ret <- v - as.numeric(x[i]) + 1
          })
          nr = nr + 1
        }
        
        #Finds Q
        nr <- 1
        for (i in wColNames) {
          collName = paste0("max", nr, collapse = NULL)
          resultat[collName] <- apply(resultat, 1, function(x) {
            v <- max(as.numeric(c(x["rc1"], x["rc2"], x["rc3"], x["rc4"], x["rc5"], x["rc6"], x["rc7"], x["rc8"], x["rc9"], x["rc10"])))
          })
          nr = nr + 1
        }
        
        # Adds new columns for the CAR weight of each criterion.
        # wcX.car = weight criterion X .car, (cardinal weight of criteron X)
        #q = 15
        v = c()
        for (k in 1:10) {
          collName = paste0("wc", k, ".car", collapse = NULL)
          resultat[collName] <- apply(resultat, 1, function(x) {
            pos = 1
            for (j in 1:10) {
              q <- as.numeric(x[paste0("max", j, collapse = NULL)])
              v[j] <-
                (1 / as.numeric(x[paste0("rc", j, collapse = NULL)])) + (q + 1 - as.numeric(x[paste0("rc", j, collapse =
                    NULL)])) / q
            }
            v[k] / sum(v)
          })
        }
        return(resultat)
      }
      
      
      # Bipolar Cardinal Ranking.
      calculateNumber <-
        function(pseudo.name,
          colNames,
          aNames,
          question.name,
          criterion.name,
          resultat) {
          #tmpresults.vec <- c()
          results.vec <- c()
          rNames.vec <- c()
          resultNames.vec <- c()
          resultValues.vec <- c()
          
          # Adds new columns used for the proportinal scores used in the bipolar cardinal ranking of the alternatives.
          resultat[pseudo.name] <- 7
          nr = 1
          v = c()
          for (i in colNames) {
            collName = paste0(question.name, nr, collapse = NULL) # a6 is the pseudo alternative!
            resultat[collName] <- apply(resultat, 1, function(x) {
              # finds the min and max values for question 1 and action 1-5 and the pseudo action (6)
              v.max <- max(as.numeric(x[c(colNames)]))
              v.min <- min(as.numeric(x[c(colNames)]))
              
              # calculates the proportinal score for question 1 and action i
              #ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
              ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
            })
            nr = nr + 1
          }
          #print(resultat)    
          # Removes all NaN produced by the proportinal score
          resultat <- resultat[complete.cases(resultat), ]
          
          # Adds new columns for the BCAR values of each alternative.
          # cop = con or pro, 0 or 1
          columnNamesList <- c()
          nr = 1
          for (i in aNames) {
            cName = paste0(question.name, nr, ".cop", collapse = NULL) #cname = collName = alternativeName
            columnNamesList <- append(columnNamesList, cName)
            
            resultat[cName] <- apply(resultat, 1, function(x) {
              if (as.numeric(x[i]) < as.numeric(x[paste(question.name, "6", sep = "")])) {
                res <- 0
              }
              else {
                res <- 1
              }
            })
            nr = nr + 1
          }
          
          tmpresults.vec <- c()
          tmpnames.vec <- c()
          
          ## Calculates the con- and pro-index for q1a1.
          # lambda = stakeholder weight, 1/qXaY.nr
          
          # Creates the total nr and the nr of cons and pros for each alternative
          for (i in columnNamesList) {
            tmpnames.vec <-
              c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrcon", sep = '')))
            tmpresults.vec <-
              c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
                  "")] == 0)))
            tmpnames.vec <-
              c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrpro", sep = '')))
            tmpresults.vec <-
              c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
                  "")] == 1)))
            tmpnames.vec <-
              c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nr", sep = '')))
            tmpresults.vec <-
              c(tmpresults.vec, c(sum(
                sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 0), sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
                    "")] == 1)
              )))
          }
          
          names(tmpresults.vec) <- tmpnames.vec
          
          
          
          # Preparing return values
          for (i in columnNamesList) {
            altName = substr(i, 1, 4)
            
            # number of members of the con-group
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".nrcon", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrcon", sep ="")]))
            
            # number of members of the pro-group     
            resultNames.vec <-
              c(resultNames.vec, c(paste(altName, ".nrpro", sep = '')))
            resultValues.vec <-
              c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrpro", sep ="")]))
          }
          
          names(resultValues.vec) <- resultNames.vec
          
          return(resultValues.vec)
        }
        c(resultValues.vec, c(sum(resultat[, paste(altName, ".pval", sep = "")])))
      
      #avg value   
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".val", sep = '')))
      resultValues.vec <-
        c(resultValues.vec, c(sum(resultat[, paste(altName, ".val", sep = "")])))
      
      # number of members of the con-group
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".nrcon", sep = '')))
      resultValues.vec <-
        c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrcon", sep ="")]))
      
      # number of members of the pro-group     
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".nrpro", sep = '')))
      resultValues.vec <-
        c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrpro", sep ="")]))
    }
    
    names(resultValues.vec) <- resultNames.vec
    #write.xlsx(resultat, paste(sample(1:1000, 1), "resultat.xlsx", sep=""))
    return(resultValues.vec)
  }




# Bipolar Cardinal Ranking.
calculateNumber <-
  function(pseudo.name,
    colNames,
    aNames,
    question.name,
    criterion.name,
    resultat) {
    #tmpresults.vec <- c()
    results.vec <- c()
    rNames.vec <- c()
    resultNames.vec <- c()
    resultValues.vec <- c()
    
    # Adds new columns used for the proportinal scores used in the bipolar cardinal ranking of the alternatives.
    resultat[pseudo.name] <- 7
    nr = 1
    v = c()
    for (i in colNames) {
      collName = paste0(question.name, nr, collapse = NULL) # a6 is the pseudo alternative!
      resultat[collName] <- apply(resultat, 1, function(x) {
        # finds the min and max values for question 1 and action 1-5 and the pseudo action (6)
        v.max <- max(as.numeric(x[c(colNames)]))
        v.min <- min(as.numeric(x[c(colNames)]))
        
        # calculates the proportinal score for question 1 and action i
        #ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
        ret <- (as.numeric(x[i]) - v.min) / (v.max - v.min)
      })
      nr = nr + 1
    }
    #print(resultat)    
    # Removes all NaN produced by the proportinal score
    resultat <- resultat[complete.cases(resultat), ]
    
    # Adds new columns for the BCAR values of each alternative.
    # cop = con or pro, 0 or 1
    columnNamesList <- c()
    nr = 1
    for (i in aNames) {
      cName = paste0(question.name, nr, ".cop", collapse = NULL) #cname = collName = alternativeName
      columnNamesList <- append(columnNamesList, cName)
      
      resultat[cName] <- apply(resultat, 1, function(x) {
        if (as.numeric(x[i]) < as.numeric(x[paste(question.name, "6", sep = "")])) {
          res <- 0
        }
        else {
          res <- 1
        }
      })
      nr = nr + 1
    }
    
    tmpresults.vec <- c()
    tmpnames.vec <- c()
    
    ## Calculates the con- and pro-index for q1a1.
    # lambda = stakeholder weight, 1/qXaY.nr
    
    # Creates the total nr and the nr of cons and pros for each alternative
    for (i in columnNamesList) {
      tmpnames.vec <-
        c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrcon", sep = '')))
      tmpresults.vec <-
        c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
            "")] == 0)))
      tmpnames.vec <-
        c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nrpro", sep = '')))
      tmpresults.vec <-
        c(tmpresults.vec, c(sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
            "")] == 1)))
      tmpnames.vec <-
        c(tmpnames.vec, c(paste(substr(i, 1, 4), ".nr", sep = '')))
      tmpresults.vec <-
        c(tmpresults.vec, c(sum(
          sum(resultat[, paste(substr(i, 1, 4), ".cop", sep = "")] == 0), sum(resultat[, paste(substr(i, 1, 4), ".cop", sep =
              "")] == 1)
        )))
    }
    
    names(tmpresults.vec) <- tmpnames.vec
    
    
    
    # Preparing return values
    for (i in columnNamesList) {
      altName = substr(i, 1, 4)
      
      # number of members of the con-group
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".nrcon", sep = '')))
      resultValues.vec <-
        c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrcon", sep ="")]))
      
      # number of members of the pro-group     
      resultNames.vec <-
        c(resultNames.vec, c(paste(altName, ".nrpro", sep = '')))
      resultValues.vec <-
        c(resultValues.vec, c(tmpresults.vec[paste(altName, ".nrpro", sep ="")]))
    }
    
    names(resultValues.vec) <- resultNames.vec
    
    return(resultValues.vec)
  }