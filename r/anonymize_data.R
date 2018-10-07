# Load the data
data <-
  read.table(
    "data-raw/data-raw.csv",
    header = TRUE,
    sep = ";",
    skip = 2,
    na.strings = " ",
    fileEncoding = "latin1",
    stringsAsFactors = FALSE
  )

# Remove IP adresses
data$IP.Address <- 0

# Save data
write.csv2(
  x = data,
  file = "data-raw/data.csv",
  row.names = FALSE,
  fileEncoding = "latin1"
)

remove(data)
