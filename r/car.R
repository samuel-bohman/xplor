# Calculate cardinal ranking
calculateCAR <- function(results) {
  
  # Add new columns of the cardinal ranking of the criteria
  wColNames = c(
    "Parks.and.green.spaces",
    "Diversity.in.housing.supply",
    "Invest.in.public.spaces",
    "Communications",
    "Culture.and.leisure",
    "Education",
    "Care",
    "School",
    "Safety",
    "Ecological.sustainability"
  )
  
  # Add a column with reformatted criterion ranks
  # The most important is set to 1, e.g., if c_1 = 14, c_2 = 10,
  # c_1 is formatted to, 14-14+1 = 1, and c_2 to, 14-10+1 = 5
  # rc = ranked criterion
  nr <- 1
  v <- c()
  for (i in wColNames) {
    collName = paste0("rc", nr, collapse = NULL)
    results[collName] <-
      apply(results, 1, function(x) {
        v <- max(as.numeric(c(x["Parks.and.green.spaces"],
          x["Diversity.in.housing.supply"],
          x["Invest.in.public.spaces"],
          x["Communications"],
          x["Culture.and.leisure"],
          x["Education"], 
          x["Care"],
          x["School"],
          x["Safety"],
          x["Ecological.sustainability"])))
        ret <- v - as.numeric(x[i]) + 1
      })
    nr = nr + 1
  }
  
  results["max"] <- apply(results, 1, function(x) {
    v <-
      max(as.numeric(c(x["rc1"], x["rc2"], x["rc3"], x["rc4"], x["rc5"], x["rc6"], x["rc7"], x["rc8"], x["rc9"], x["rc10"])))
  })
  
  v = c()
  for (k in 1:10) {
    collName = paste0("wc", k, ".car", collapse = NULL)
    results[collName] <- apply(results, 1, function(x) {
      pos = 1
      for (j in 1:10) {
        q <- as.numeric(x["max"])
        v[j] <-
          (1 / as.numeric(x[paste0("rc", j, collapse = NULL)])) + (q + 1 - as.numeric(x[paste0("rc", j, collapse = NULL)])) / q
      }
      v[k] / sum(v)
    })
  }
  results[70:80] <- NULL
  return(results)
}
