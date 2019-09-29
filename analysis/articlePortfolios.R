debugSource("r/distance.R")
source("r/optimization.R")
source("global.R")
source("r/car.R")
source("r/car_ce.R")
library(xtable)

# Load the data
data <- readRDS("data-derived/data_spdf.rds")
data_g1 <- data[data$Area %in% c("Sigma/Apoteksskogen", "Hasselgatan", "Ekebo", "Dragonvägen", "Kavallerigatan/Vilundaparken", "Smedby 2", "Smedby 3", "Stallgatan"), ]
data_g2 <- data[data$Area %in% c("Sjukyrkoberget", "Norra Bollstanäs", "Södra Bollstanäs", "Grimstaby", "Norra Nordanvägen"), ]
data_t <- rbind(data_g1, data_g2)

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

createPortfolioLatexTables <- function(focusarea, values, conflicts, grupp){
  values <- unlist(values)
  conflicts <- unlist(conflicts)
  
  p_actions <- get_portfolio_alternatives(focusarea)
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
  portfolios_total <- portfolios_total[, c(8,1:7)]
  write.csv2(portfolios_total, file = paste("analysis/results/portfolios ", focusarea, " ", grupp, ".csv", sep = ""), row.names=FALSE)
  
  # grp_ci_a <- calculateCoreIndex(portfolios_total_pos_rev, portfolios_total_neg, p_actions[1])
  # grp_ci_b <- calculateCoreIndex(portfolios_total_pos_rev, portfolios_total_neg, p_actions[2])
  # grp_ci_c <- calculateCoreIndex(portfolios_total_pos_rev, portfolios_total_neg, p_actions[3])
  # grp_ci_d <- calculateCoreIndex(portfolios_total_pos_rev, portfolios_total_neg, p_actions[4])
  # grp_ci_e <- calculateCoreIndex(portfolios_total_pos_rev, portfolios_total_neg, p_actions[5])
  # 
  # is.num <- sapply(portfolios_total, is.numeric)
  # portfolios_total[is.num] <- lapply(portfolios_total[is.num], round, 4)
  # portfolios_total <- format(portfolios_total, digits=4)
  # 
  # portfolios_total <- rbind(portfolios_total, c("CI", grp_ci_a,grp_ci_b,grp_ci_c,grp_ci_d,grp_ci_e,"",""))
  # 
  # xtab <- xtable(portfolios_total[,1:8], digits=c(0,0,0,0,0,0,0,4,4), caption = paste("Group ",grupp,"'s portfolios for focus area ", focusarea, ".",sep = ""))
  # 
  # options(xtable.comment = FALSE)
  # filename <- paste('analysis/results/portfolios','.tex', sep="")
  # sink(filename, append = TRUE)
  # print.xtable(xtab, include.rownames=FALSE, hline.after = nrow(portfolios_total)-1, caption.placement = "top")
  # sink()
}

for(i in 1:length(theme)){
  ### Distances
  focusarea <- theme[i]
  data.vec1 <- distance(focusarea, data_g1)
  data.vec2 <- distance(focusarea, data_g2)
  data.vec12 <- distance(focusarea, data_t)
  
  ### Calculate total mean weighted values
  val_total <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7])
    return(data.vec12[x + 8] * lambda)
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
    return(data.vec1[x + 8] * lambda)
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
    return(data.vec2[x + 8] * lambda)
  })
  
  createPortfolioLatexTables(focusarea, val_group_1, dis_group_1, "G1")
  createPortfolioLatexTables(focusarea, val_group_2, dis_group_2, "G2")
  createPortfolioLatexTables(focusarea, val_total, dis_total, "T")
}

