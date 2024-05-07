library(readxl)
library(dplyr)
library(knitr)


# Read the Excel file
questionnaire <- read_excel("questionnaire3g.xlsx", sheet = "Form Responses 1")

conversion <- function(response) {
  if (response == "Strongly Disagree") {
    return(1)
  } else if (response == "Disagree") {
    return(2)
  } else if (response == "Neutral") {
    return(3)
  } else if (response == "Agree") {
    return(4)
  } else {
    return(5)
  }
}

# Apply conversion function to each column
columns_to_convert <- c(10, 11, 12, 14, 17, 19, 20, 22, 23, 25, 26, 27, 28, 29, 30, 31, 32)

for (column_index in columns_to_convert) {
  questionnaire[[column_index]] <- sapply(questionnaire[[column_index]], conversion)
}


conversion1 <- function(response) {
  if (is.na(response)) {
    return(NA)
  } else if (response == "Yes") {
    return(1)
  } else if (response == "No") {
    return(2)
  } else if (response == "Possibly") {
    return(3)
  } else {
    return(NA)
  }
}

columns_to_convert1 <- c(9,15,16,18,21,24,33,34,35)

for (column in columns_to_convert1) {
  questionnaire[[column]] <- sapply(questionnaire[[column]], conversion1)
}

View(questionnaire)


#Mean and Standard Deviation
# Calculate the mean and sd for Performance Expectancy
performanceExpectancy <- questionnaire[, c(9:11)]

means1 <- colMeans(performanceExpectancy, na.rm = TRUE)
sd_pe <- sapply(performanceExpectancy, sd, na.rm = TRUE)


#calculate the mean and sd for Effort Expectancy
effortExpectancy <- questionnaire[, c(12:14)]

means2 <- colMeans(effortExpectancy, na.rm = TRUE)
means2

sd_ee <- sapply(effortExpectancy, sd, na.rm = TRUE)
sd_ee

#calculate the mean and sd for Social Influence
socialInfluence<- questionnaire[, c(19:21)]

means3 <- colMeans(socialInfluence, na.rm = TRUE)
means3

sd_si <- sapply(socialInfluence, sd, na.rm = TRUE)
sd_si

#calculate the mean for Facilitating Conditions
facilitatingConditions <- questionnaire[, c(22:25)]

means4 <- colMeans(facilitatingConditions, na.rm = TRUE)
means4

sd_fc <- sapply(facilitatingConditions, sd, na.rm = TRUE)
sd_fc

#Calculate the mean for Behavioral Intention to use the system
behavioralIntention <- questionnaire[, c(33:35)]

means5 <- colMeans(behavioralIntention, na.rm = TRUE)
means5

sd_bi <- sapply(behavioralIntention, sd, na.rm = TRUE)
sd_bi

#####Make a data frame of all factors######
item_labels <- colnames(questionnaire[, c(9:11)])

pe <- data.frame(
  Items = item_labels ,
  Description = "Performance Expectancy",
  Mean = means1,
  SD = sd_pe
)

View(pe)

ee <- data.frame(
  Description = "Effort Expectancy",
  Mean = means2,
  SD = sd_ee
)


si <- data.frame(
  Description = "Social Influence",
  Mean = means3,
  SD = sd_si
)

fc <- data.frame(
  Description = "Facilitating Conditions",
  Mean = means4,
  SD = sd_fc
)

bi <- data.frame(
  Description = "Behavioral Intention",
  Mean = means5,
  SD = sd_bi
)

#####Combine all factors using kable() function######
summary <- rbind(pe, ee, si, fc, bi)
summary_table<-  kable(summary)
View(summary_table)

View(summary)






