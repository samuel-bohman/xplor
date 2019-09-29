# Load the data
# data <-
#   read.table(
#     "data-raw/data.csv",
#     header = TRUE,
#     sep = ";",
#     skip = 0,
#     na.strings = " ",
#     fileEncoding = "latin1",
#     stringsAsFactors = FALSE)

# Load the data
if (!("data.table" %in% installed.packages()[,"Package"])) {
  message("Package data.table not installed: installing...") 
  install.packages("data.table")
}

data <- data.table::fread("data-raw/data.csv", data.table = FALSE)

# Delete 10 unnecessary/empty columns
data[1:7] <- list(NULL)
data[c(
  "X12b..Bevara.natur.och.strandlinjer.intakta",
  "X13b..Bevara.grönområden.intakta",
  "X14b..Satsa.på.mindre.tätorter.i.kommunen"
)] <- list(NULL)

# Rename columns with shorter names
names(data)[1]  <- "Alt.1a"
names(data)[2]  <- "Alt.1b"
names(data)[3]  <- "Alt.1c"
names(data)[4]  <- "Alt.1d"
names(data)[5]  <- "Alt.1e"

names(data)[6]  <- "Alt.2a"
names(data)[7]  <- "Alt.2b"
names(data)[8]  <- "Alt.2c"
names(data)[9]  <- "Alt.2d"
names(data)[10] <- "Alt.2e"

names(data)[11] <- "Alt.3a"
names(data)[12] <- "Alt.3b"
names(data)[13] <- "Alt.3c"
names(data)[14] <- "Alt.3d"
names(data)[15] <- "Alt.3e"

names(data)[16] <- "Alt.4a"
names(data)[17] <- "Alt.4b"
names(data)[18] <- "Alt.4c"
names(data)[19] <- "Alt.4d"
names(data)[20] <- "Alt.4e"

names(data)[21] <- "Alt.5a"
names(data)[22] <- "Alt.5b"
names(data)[23] <- "Alt.5c"
names(data)[24] <- "Alt.5d"
names(data)[25] <- "Alt.5e"

names(data)[26] <- "Alt.6a"
names(data)[27] <- "Alt.6b"
names(data)[28] <- "Alt.6c"
names(data)[29] <- "Alt.6d"
names(data)[30] <- "Alt.6e"

names(data)[31] <- "Alt.7a"
names(data)[32] <- "Alt.7b"
names(data)[33] <- "Alt.7c"
names(data)[34] <- "Alt.7d"
names(data)[35] <- "Alt.7e"

names(data)[36] <- "Alt.8a"
names(data)[37] <- "Alt.8b"
names(data)[38] <- "Alt.8c"
names(data)[39] <- "Alt.8d"
names(data)[40] <- "Alt.8e"

names(data)[41] <- "Alt.9a"
names(data)[42] <- "Alt.9b"
names(data)[43] <- "Alt.9c"
names(data)[44] <- "Alt.9d"
names(data)[45] <- "Alt.9e"

names(data)[46] <- "Alt.10a"
names(data)[47] <- "Alt.10b"
names(data)[48] <- "Alt.10c"
names(data)[49] <- "Alt.10d"
names(data)[50] <- "Alt.10e"

names(data)[51] <- "Parks and green spaces"
names(data)[52] <- "Diversity in housing supply"
names(data)[53] <- "Invest in public spaces"
names(data)[54] <- "Communications"
names(data)[55] <- "Culture and leisure"
names(data)[56] <- "Education"
names(data)[57] <- "Care"
names(data)[58] <- "School"
names(data)[59] <- "Safety"
names(data)[60] <- "Ecological sustainability"

names(data)[61] <- "Disagreement 1"
names(data)[62] <- "Disagreement 2"
names(data)[63] <- "Disagreement 3"

names(data)[64] <- "Area"
names(data)[65] <- "Education level"
names(data)[66] <- "Occupation"
names(data)[67] <- "Year"
names(data)[68] <- "Age"
names(data)[69] <- "Gender"

# Find incomplete cases
# data[!complete.cases(data), ] # 438, 503, 813

# Delete NAs
data <- na.omit(data) # 438, 503, 813

# Delete 1 row with empty cell manually
data <- data[-1,] # row 1, column "Area"

# Find duplicate rows
# c(which(duplicated(data, fromLast = F) == T), which(duplicated(data, fromLast = T) == T))

# Remove duplicate rows
data <- unique(data)

# Type convert
data$Alt.7e <- as.integer(data$Alt.7e)
data$Alt.8a <- as.integer(data$Alt.8a)
data$Alt.8d <- as.integer(data$Alt.8d)
data$Alt.9c <- as.integer(data$Alt.9c)
data$"Disagreement 1" <- as.integer(data$"Disagreement 1")
data$"Disagreement 2" <- as.integer(data$"Disagreement 2")
data$"Disagreement 3" <- as.integer(data$"Disagreement 3")
data$Area <- factor(data$Area)
data$"Education level" <- factor(data$"Education level")
data$Occupation <- factor(data$Occupation)
data$Year <- factor(data$Year)
data$Age <- factor(data$Age)
data$Gender <- factor(data$Gender)

# Fix levels attributes of factors
levels(data$"Education level") <- c("Other post-secondary education",
                                    "Doctorate",
                                    "Elementary school or equivalent compulsory school",
                                    "High school, Nordic folk high school, or equivalent",
                                    "College/University",
                                    "No elementary or equivalent compulsary school")

levels(data$Occupation) <- c("Job-seeker",
                             "Self-employed",
                             "Employee",
                             "Sickness or activity benefit",
                             "Long-term sick leave (more than 3 months)",
                             "Other",
                             "Senior citizen",
                             "Student",
                             "Leave of absence")

levels(data$Year) <- c("0-4 years", "5-9 years", "10 or more years")

levels(data$Gender) <- c("Other/No gender", "Woman", "Man", "Prefer not to disclose")

# Reorder columns
data <- data[, c(64, 69, 68, 66, 65, 67, 1:50, 51:60, 61:63)]

# Save data
write.csv2(
  x = data,
  file = "data-derived/data.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)