library(readxl)
library(dplyr)
library(knitr)


# Read the Excel file
questionnaire <- read_excel("dataCleaning_questionnaire/questionnaire3g.xlsx")

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
columns_to_convert <- c(
  "By using the Adobe application enables you to accomplish tasks more quickly.",
  "By using the Adobe application it increases my productivity.",
  "The Adobe application makes work more interesting.",
  "My peers influence necessary to use the Adobe application.",
  "People who are important to me think that I should use the Adobe application.",
  "I have the resources necessary to use the Adobe application.",
  "My peers is available for assistance with Adobe application's difficulties.",
  "I could complete a job or task using the Adobe application if  there was no one around to tell me what to do as I go.",
  "I could complete a job or task using the Adobe application if I had a lot of time to complete the job for which the software was provided.",
  "I could complete a job or task using the Adobe application if I had just the built-in help facility for assistance."
)

for (column in columns_to_convert) {
  questionnaire[[column]] <- sapply(questionnaire[[column]], conversion)
}


conversion1 <- function(response) {
  if (response == "Yes") {
    return(1)
  } else {
    return(2)
  }
}

columns_to_convert1<- c(
  "In general, the school has supported the use of the Adobe application",
  "I know the knowledge necessary to use the Adobe application.",
  "I intend to use the Adobe application in the next 2 months.",
  "I predict I will use the Adobe application in the next 2 months.",
  "I plan to use the system in the next 2 months."
)

for (column in columns_to_convert1) {
  questionnaire[[column]] <- sapply(questionnaire[[column]], conversion1)
}

View(questionnaire)

#Mean and Standard Deviation
# Calculate the mean and sd for Performance Expectancy
performanceExpectancy <- c(
  "By using the Adobe application enables you to accomplish tasks more quickly.",
  "By using the Adobe application it increases my productivity."
)

means1 <- colMeans(questionnaire[performanceExpectancy], na.rm = TRUE)
means1

sd_pe <- sapply(questionnaire[performanceExpectancy], sd, na.rm = TRUE)
sd_pe

#calculate the mean and sd for Effort Expectancy
effortExpectancy <- c(
  "My interaction with the Adobe application would be clear and understandable.",
  "It would be easy for me to become more skillful at using the Adobe application.",
  "Learning to operate the Adobe application is easy for me."
)

means2 <- colMeans(questionnaire[effortExpectancy], na.rm = TRUE)
means2

sd_ee <- sapply(questionnaire[effortExpectancy], sd, na.rm = TRUE)
sd_ee

#calculate the mean and sd for Social Influence
socialInfluence<- c(
  "My peers influence necessary to use the Adobe application.",
  "People who are important to me think that I should use the Adobe application."
)

means3 <- colMeans(questionnaire[socialInfluence], na.rm = TRUE)
means3

sd_si <- sapply(questionnaire[socialInfluence], sd, na.rm = TRUE)
sd_si

#calculate the mean for Facilitating Conditions
facilitatingConditions <- c(
  "I have the resources necessary to use the Adobe application.",
  "My peers is available for assistance with Adobe application's difficulties.",
  "I know the knowledge necessary to use the Adobe application."
)

means4 <- colMeans(questionnaire[facilitatingConditions], na.rm = TRUE)
means4

sd_fc <- sapply(questionnaire[facilitatingConditions], sd, na.rm = TRUE)
sd_fc

#Calculate the mean for Behavioral Intention to use the system
behavioralIntention <- c(
  "I intend to use the Adobe application in the next 2 months.",
  "I predict I will use the Adobe application in the next 2 months.",
  "I plan to use the system in the next 2 months."
)

means5 <- colMeans(questionnaire[behavioralIntention], na.rm = TRUE)
means5

sd_bi <- sapply(questionnaire[behavioralIntention], sd, na.rm = TRUE)
sd_bi

#Combine all factors using kable() function
pe <- data.frame(
  Factor = "Performance Expectancy",
  Mean = colMeans(questionnaire[performanceExpectancy], na.rm = TRUE),
  SD = sapply(questionnaire[performanceExpectancy], sd, na.rm = TRUE)
)

ee <- data.frame(
  Factor = "Effort Expectancy",
  Mean = colMeans(questionnaire[effortExpectancy], na.rm = TRUE),
  SD = sapply(questionnaire[effortExpectancy], sd, na.rm = TRUE)
)

si <- data.frame(
  Factor = "Social Influence",
  Mean = colMeans(questionnaire[socialInfluence], na.rm = TRUE),
  SD = sapply(questionnaire[socialInfluence], sd, na.rm = TRUE)
)

fc <- data.frame(
  Factor = "Facilitating Conditions",
  Mean = colMeans(questionnaire[facilitatingConditions], na.rm = TRUE),
  SD = sapply(questionnaire[facilitatingConditions], sd, na.rm = TRUE)
)

bi <- data.frame(
  Factor = "Behavioral Intention",
  Mean = colMeans(questionnaire[behavioralIntention], na.rm = TRUE),
  SD = sapply(questionnaire[behavioralIntention], sd, na.rm = TRUE)
)

summary_table <- rbind(pe, ee, si, fc, bi)
table<-  kable(summary.table)
View(table)






