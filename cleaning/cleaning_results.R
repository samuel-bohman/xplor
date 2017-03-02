# Load the data
results <- read.table("cleaning/data/results/uv_original.csv", header = TRUE, sep = ";", skip = 2, fileEncoding = "latin1")

# Delete 10 unnecessary/empty columns
results[1:7] <- list(NULL)
results[c(
    "X12b..Bevara.natur.och.strandlinjer.intakta",
    "X13b..Bevara.grönområden.intakta",
    "X14b..Satsa.på.mindre.tätorter.i.kommunen"
)] <- list(NULL)

# Rename columns with shorter names
names(results)[1]  <- "Alt.1a"
names(results)[2]  <- "Alt.1b"
names(results)[3]  <- "Alt.1c"
names(results)[4]  <- "Alt.1d"
names(results)[5]  <- "Alt.1e"

names(results)[6]  <- "Alt.2a"
names(results)[7]  <- "Alt.2b"
names(results)[8]  <- "Alt.2c"
names(results)[9]  <- "Alt.2d"
names(results)[10] <- "Alt.2e"

names(results)[11] <- "Alt.3a"
names(results)[12] <- "Alt.3b"
names(results)[13] <- "Alt.3c"
names(results)[14] <- "Alt.3d"
names(results)[15] <- "Alt.3e"

names(results)[16] <- "Alt.4a"
names(results)[17] <- "Alt.4b"
names(results)[18] <- "Alt.4c"
names(results)[19] <- "Alt.4d"
names(results)[20] <- "Alt.4e"

names(results)[21] <- "Alt.5a"
names(results)[22] <- "Alt.5b"
names(results)[23] <- "Alt.5c"
names(results)[24] <- "Alt.5d"
names(results)[25] <- "Alt.5e"

names(results)[26] <- "Alt.6a"
names(results)[27] <- "Alt.6b"
names(results)[28] <- "Alt.6c"
names(results)[29] <- "Alt.6d"
names(results)[30] <- "Alt.6e"

names(results)[31] <- "Alt.7a"
names(results)[32] <- "Alt.7b"
names(results)[33] <- "Alt.7c"
names(results)[34] <- "Alt.7d"
names(results)[35] <- "Alt.7e"

names(results)[36] <- "Alt.8a"
names(results)[37] <- "Alt.8b"
names(results)[38] <- "Alt.8c"
names(results)[39] <- "Alt.8d"
names(results)[40] <- "Alt.8e"

names(results)[41] <- "Alt.9a"
names(results)[42] <- "Alt.9b"
names(results)[43] <- "Alt.9c"
names(results)[44] <- "Alt.9d"
names(results)[45] <- "Alt.9e"

names(results)[46] <- "Alt.10a"
names(results)[47] <- "Alt.10b"
names(results)[48] <- "Alt.10c"
names(results)[49] <- "Alt.10d"
names(results)[50] <- "Alt.10e"

names(results)[51] <- "Parks and green areas"
names(results)[52] <- "Diversity in housing supply"
names(results)[53] <- "Invest in public areas"
names(results)[54] <- "Communications"
names(results)[55] <- "Culture and leasure"
names(results)[56] <- "Education"
names(results)[57] <- "Care"
names(results)[58] <- "School"
names(results)[59] <- "Safety"
names(results)[60] <- "Ecological sustainability"

names(results)[61] <- "Disagreement 1"
names(results)[62] <- "Disagreement 2"
names(results)[63] <- "Disagreement 3"

names(results)[64] <- "Area"
names(results)[65] <- "Education level"
names(results)[66] <- "Occupation"
names(results)[67] <- "Year"
names(results)[68] <- "Age"
names(results)[69] <- "Gender"

# Change variable type from factor to integer
results$Alt.7e <- as.integer(results$Alt.7e)
results$Alt.8a <- as.integer(results$Alt.8a)
results$Alt.8d <- as.integer(results$Alt.8d)
results$Alt.9c <- as.integer(results$Alt.9c)
results$"Disagreement 1" <- as.integer(results$"Disagreement 1")
results$"Disagreement 2" <- as.integer(results$"Disagreement 2")
results$"Disagreement 3" <- as.integer(results$"Disagreement 3")

# Lists rows that have missing values 
# missing_values <- results[!complete.cases(results), ] # 438, 503, 813

# Delete rows with missing values (NA)
# results <- na.omit(results)

# Delete 4 rows
results <- results[-c(1, 438, 503, 813), ]

# Cleaning Area
results$Area <- factor(results$Area)

# Cleaning Education level
results$"Education level" <- factor(results$"Education level")
levels(results$"Education level") <- c("Other post-secondary education", "Doctorate", "Elementary school or equivalent compulsory school", "High school, Nordic folk high school, or equivalent", "College/University", "No elementary or equivalent compulsary school")

# Cleaning Occupation
results$Occupation <- factor(results$Occupation)
levels(results$Occupation) <- c("Job-seeker", "Self-employed", "Employee", "Sickness or activity benefit", "Long-term sick leave (more than 3 months)", "Other", "Senior citizen", "Student", "Leave of absence")

# Cleaning Year
results$Year <- factor(results$Year)
levels(results$Year) <- c("0-4 years", "5-9 years", "10 or more years")

# Cleaning Age
results$Age <- factor(results$Age)

# Cleaning Gender
results$Gender <- factor(results$Gender)
levels(results$Gender) <- c("Other/No gender", "Woman", "Man", "Prefer not to disclose")

# Reorder columns
results <- results[, c(64, 69, 68, 66, 65, 67, 1:50, 51:60, 61:63)]

# Save data
write.csv2(x = results, file = "data/results/results.csv", row.names = FALSE, fileEncoding = "UTF-8")
remove(results)