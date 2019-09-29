source("r/distance.R")
debugSource("r/optimization.R")
source("global.R")
source("r/car.R")
source("r/car_ce.R")
library(xtable)

## Create alternative strings
get_portfolio_alternatives <- function(criterion) {
  number <- unlist(criterion_number[[criterion]])
  c(paste("Alt.", number, letters[1:5], sep = ""))
}

## Function to calculate portfolio core index.
calculateCoreIndex <- function(portfolio_df_pos, portfolio_df_neg, alt){
  if(sum(portfolio_df_pos[alt]) == 0){
    alt.ci <- paste("(-)",round(sum(portfolio_df_neg[alt]) / (nrow(portfolio_df_neg)-1) * 100, digits = 0),"%",sep = "")
  } else {
    alt.ci <- paste("(+)",round(sum(portfolio_df_pos[alt]) / (nrow(portfolio_df_pos)-1) * 100, digits = 0),"%",sep = "")
  }
  return(alt.ci)
}

# Load the data
data <- readRDS("data-derived/data_spdf.rds")
data_g1 <- data[data$Area %in% c("Sigma/Apoteksskogen", "Hasselgatan", "Ekebo", "Dragonvägen", "Kavallerigatan/Vilundaparken", "Smedby 2", "Smedby 3", "Stallgatan"), ]
data_g2 <- data[data$Area %in% c("Sjukyrkoberget", "Norra Bollstanäs", "Södra Bollstanäs", "Grimstaby", "Norra Nordanvägen"), ]
data_t <- rbind(data_g1, data_g2)
df <- NULL

createPortfolio50LatexTables <- function(values, conflicts, grupp){
  values <- unlist(values)
  conflicts <- unlist(conflicts)
  
  budget <- sum(conflicts)
  bugdet <- unlist(budget)
  
  ### Portfolios total for table
  portfolios_total_pos <- get_all_portfolios(
    actions = p_actions,
    values = values,
    distance = conflicts,
    initial_budget_constraint = bugdet,
    direction = "max"
  )
  
  portfolios_total_neg <- get_all_portfolios(
    actions = p_actions,
    values = values,
    distance = conflicts,
    initial_budget_constraint = bugdet,
    direction = "min"
  )
  
  portfolios_total_pos_rev <- portfolios_total_pos[rev(rownames(portfolios_total_pos)),]
  portfolios_total <- rbind(portfolios_total_pos_rev, portfolios_total_neg[-1, ])
  portfolios_total$id <- 1:nrow(portfolios_total)
  portfolios_total$value <- portfolios_total$value
  portfolios_total$distance <- portfolios_total$distance
  portfolios_total <- portfolios_total[, c(length(portfolios_total), 1:length(portfolios_total)-1)]
  write.csv2(portfolios_total, file = paste("analysis/results/portfolios50",grupp,".csv", sep=""), row.names=FALSE)
  
  # grp_ci <- NULL
  # for(x in 1:50){
  #   grp_ci <- c(grp_ci, calculateCoreIndex(portfolios_total_pos_rev, portfolios_total_neg, p_actions[x]))
  # }
  # 
  # is.num <- sapply(portfolios_total, is.numeric)
  # portfolios_total[is.num] <- lapply(portfolios_total[is.num], round, 4)
  # portfolios_total <- format(portfolios_total, digits=4)
  # 
  # portfolios_total <- rbind(portfolios_total, c("CI", grp_ci, "", ""))
  # 
  # xtab <- xtable(portfolios_total[,1:length(portfolios_total)], caption = paste("Group ",grupp,"'s portfolios.", sep = ""))
  # 
  # options(xtable.comment = FALSE)
  # filename <- paste('analysis/results/portfolios50','.tex', sep="")
  # sink(filename, append = TRUE)
  # print.xtable(xtab, include.rownames=FALSE, hline.after = nrow(portfolios_total)-1, caption.placement = "top")
  # sink()
}

createValueConflictDataFrame <- function(focusarea, data_g1, data_g2, data_t){
  ### Distances
  data.vec1 <- distance(focusarea, data_g1)
  data.vec2 <- distance(focusarea, data_g2)
  data.vec12 <- distance(focusarea, data_t)
  
  ### Calculate total mean weighted values
  val_total <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7])
    return(data.vec12[x + 5] * lambda)
  })
  
  ### Calculate total distance
  dis_total <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7]) ^ 2
    beta <- 1 / (lambda * (data.vec12[x + 6] + data.vec12[x + 7]))
    T1 <- data.vec1[x] * lambda
    T2 <- data.vec2[x] * lambda
    T12 <- data.vec12[x] * lambda
    C1 <- data.vec1[x + 1] * lambda
    C2 <- data.vec2[x + 1] * lambda
    C12 <- data.vec12[x + 1] * lambda
    P1 <- data.vec1[x + 2] * lambda
    P2 <- data.vec2[x + 2] * lambda
    P12 <- data.vec12[x + 2] * lambda
    sqrt(beta * abs((T12 - (T1 + T2)) - ((C12 - (C1 + C2)) + (P12 - (P1 + P2)))))
  })
  
  ### Calculate distance within the group
  dis_group_1 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec1[x + 6] + data.vec1[x + 7]) ^ 2
    beta <- 1 / (lambda * (data.vec1[x + 6] + data.vec1[x + 7]))
    sqrt(beta * (data.vec1[x] * lambda - (data.vec1[x + 1] * lambda + data.vec1[x + 2] * lambda)))
  })
  
  ### Calculate total mean weighted values
  val_group_1 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec1[x + 6] + data.vec1[x + 7])
    return(data.vec1[x + 5] * lambda)
  })
  
  ### Calculate distance within the group
  dis_group_2 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec2[x + 6] + data.vec2[x + 7]) ^ 2
    beta <- 1 / (lambda * (data.vec2[x + 6] + data.vec2[x + 7]))
    sqrt(beta * (data.vec2[x] * lambda - (data.vec2[x + 1] * lambda + data.vec2[x + 2] * lambda)))
  })
  
  ### Calculate total mean weighted values 
  val_group_2 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec2[x + 6] + data.vec2[x + 7])
    return(data.vec2[x + 5] * lambda)
  })
  
  df <- data.frame(unlist(val_group_1), unlist(val_group_2), unlist(val_total), unlist(dis_group_1), unlist(dis_group_2), unlist(dis_total))
  rownames(df) <- get_portfolio_alternatives(focusarea)
  return(df)
}

p_actions <- NULL
for(i in 1:length(theme)){
  focusarea <- theme[i]
  df <- rbind(df, createValueConflictDataFrame(focusarea, data_g1, data_g2, data_t))
  p_actions <- c(p_actions, get_portfolio_alternatives(focusarea))
  
}  

df <- data.frame(names = row.names(df), df)
rownames(df) = 1:nrow(df)
colnames(df) <- c("Action", "Value G1", "Value G2", "Value T", "Conflict G1", "Conflict G2", "Conflict T")

createPortfolio50LatexTables(df[,2], df[,5], "G1")
createPortfolio50LatexTables(df[,3], df[,6], "G2")
createPortfolio50LatexTables(df[,4], df[,7], "T")
