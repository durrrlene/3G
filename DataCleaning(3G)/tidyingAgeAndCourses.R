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

# replacing long terms to acronyms

# show courses
print(deets$Course)

# to BSIT
to_bsit <- c(1, 14, 46, 56, 59, 61, 68)
deets$Course[to_bsit] <- "BSIT"

# to BSCE
to_bsce <- c(2)
deets$Course[to_bsce] <- "BSCE"

#to BS Agri
to_bsagri <- c(17, 40)
deets$Course[to_bsagri] <- "BS Agri"

# to BS Chem
to_bschem <- c(16, 80)
deets$Course[to_bschem] <- "BS Chem"

# to bsa
to_bsa <- c(25, 28)
deets$Course[to_bsa] <- "BSA"

# to bshm
to_bshm <- c(31, 98) 
deets$Course[to_bshm] <- "BSHM"

# to bs phar
to_bsphar <- c(44)
deets$Course[to_bsphar] <- "BS Phar"

deets$Course

# HAY FINALLY KAPOY BA HAHAHA



