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

avg_mean1 <- round(mean(means1), 2)
avg_sd1 <- round(mean(sd_pe), 2)


# Calculate the mean and sd for Effort Expectancy
effortExpectancy <- questionnaire[, c(12:14)]

means2 <- colMeans(effortExpectancy, na.rm = TRUE)
sd_ee <- sapply(effortExpectancy, sd, na.rm = TRUE)

avg_mean2 <- round(mean(means2), 2)
avg_sd2 <- round(mean(sd_ee), 2)

# Calculate the mean and sd for Social Influence
socialInfluence <- questionnaire[, c(19:21)]

means3 <- colMeans(socialInfluence, na.rm = TRUE)
sd_si <- sapply(socialInfluence, sd, na.rm = TRUE)

avg_mean3 <- round(mean(means3), 2)
avg_sd3 <- round(mean(sd_si), 2)

# Calculate the mean and sd for Facilitating Conditions
facilitatingConditions <- questionnaire[, c(22:25)]

means4 <- colMeans(facilitatingConditions, na.rm = TRUE)
sd_fc <- sapply(facilitatingConditions, sd, na.rm = TRUE)

avg_mean4 <- round(mean(means4), 2)
avg_sd4 <- round(mean(sd_fc), 2)

# Calculate the mean and sd for Behavioral Intention to use the system
behavioralIntention <- questionnaire[, c(33:35)]

means5 <- colMeans(behavioralIntention, na.rm = TRUE)
sd_bi <- sapply(behavioralIntention, sd, na.rm = TRUE)

avg_mean5 <- round(mean(means5), 2)
avg_sd5 <- round(mean(sd_bi), 2)

#####Make a data frame of all factors######
#item_labels <- colnames(questionnaire[, c(9:11)])

pe <- data.frame(
  #Items = item_labels,
  Description = "Performance Expectancy",
  Mean = means1,
  SD = sd_pe,
  Average_Mean = avg_mean1,
  Average_SD = avg_sd1
)

ee <- data.frame(
  Description = "Effort Expectancy",
  Mean = means2,
  SD = sd_ee,
  Average_Mean = total_mean2,
  Average_SD = total_sd2
)


si <- data.frame(
  Description = "Social Influence",
  Mean = means3,
  SD = sd_si,
  Average_Mean = total_mean3,
  Average_SD = total_sd3
)

fc <- data.frame(
  Description = "Facilitating Conditions",
  Mean = means4,
  SD = sd_fc,
  Average_Mean = total_mean4,
  Average_SD = total_sd4
)

bi <- data.frame(
  Description = "Behavioral Intention",
  Mean = means5,
  SD = sd_bi,
  Average_Mean = total_mean5,
  Average_SD = total_sd5
)

#####Combine all factors using kable() function######
summary <- rbind(pe, ee, si, fc, bi)
summary_table<-  kable(summary)
View(summary_table)







