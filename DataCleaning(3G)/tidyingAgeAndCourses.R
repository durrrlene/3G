library(readxl)
library(dplyr)
library(tidyr)



orig <- read_excel("C:/Users/steve/Documents/3G/DataCleaning(3G)/questionnaire3g.xlsx")
deets <- read_excel("C:/Users/steve/Documents/3G/DataCleaning(3G)/questionnaire3g.xlsx")


#getting the age average

age <- c(deets$Age)

#converting age to numeric form
age <- as.numeric(gsub("[^0-9]", "", as.character(age)))

#removes zeros
age <- as.numeric(gsub("0$", "", as.character(age)))

age
#neglecting NAs in solving average
ave_age <- mean(age, na.rm = TRUE)
ave_age


# removing decimals on ave_age
(ave_age <- floor(ave_age))

#fills in the NAs with the ave_age
age[is.na(age)] <- ave_age
age

deets$Age <- age
deets$Age


# Applying the revisions
View(deets)



# ----------------------------------- Changing Courses to its acronym ----------
