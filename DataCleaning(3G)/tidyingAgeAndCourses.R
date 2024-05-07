library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(openxlsx)
library(ggplot2)




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

# graphs for age ( PIE CHART )

age_counts <- table(age)
percentages <- round(prop.table(age_counts) * 100, 1)
pie(age_counts, main = "Ages", labels = paste(names(age_counts), "\n", percentages, "%", sep = ""), col = rainbow(length(age_counts)), cex = 0.7)
legend("topright", legend = paste(names(age_counts), "-", percentages, "%", sep = ""), cex = 0.6, fill = rainbow(length(age_counts)))



# Applying the revisions
View(deets)


# ---------------------------------- Sex 

males <- subset(deets, Sex == "Male")
females <- subset(deets, Sex == "Female")

sex_counts <- data.frame(
  Sex = c("Male", "Female"),
  Count = c(nrow(males), nrow(females))
)

ggplot(sex_counts, aes(x = Sex, y = Count, fill = Sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Males and Females",
       x = "Sex",
       y = "Count") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink"))





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
to_bshm <- c(29, 98) 
deets$Course[to_bshm] <- "BSHM"

# to bs phar
to_bsphar <- c(44)
deets$Course[to_bsphar] <- "BS Phar"

deets$Course

View(deets)

# HAY FINALLY KAPOY BA HAHAHA


# -------------------------------------- course -------------------------

course <- deets$Course

categorize_course <- function(course) {
  if (grepl("IT|CS|BSIT", course)) {
    return("Computer Literature")
  }
  else if (grepl("ED|BSED", course)) {
    return("Education")
  }
  else if (grepl("BSA|BSBA", course)) {
    return("Business and Finance")
  }
  else if (grepl("CE", course)) {
    return("Engineering")
  }
  else if (grepl("BSHM", course)) {
   return("Hospitality Management") 
  }
  else {
    return("Other")
  }
}
  
course_categories <- sapply(course, categorize_course)

# count the data of each category
category_counts <- table(course_categories)


# convert the table to a data frame
category_counts_df <- as.data.frame(category_counts)
names(category_counts_df) <- c("Category", "Count")


# create the bar plot
ggplot(category_counts_df, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Course Distribution by Category",
       x = "Category",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Computer Literature" = "skyblue",
                               "Education" = "pink",
                               "Business and Finance" = "orange",
                               "Engineering" = "green",
                               "Hospitality Management" = "yellow",
                               "Other" = "grey"))



# ----------------------------------- School --------------------------

school <- deets$School

school_factor <- factor(School)

school_counts <- table(school_factor)

school_counts_df <- as.data.frame(school_counts)
names(school_counts_df) <- c("School", "Count")

# Sort the data frame by Count in descending order
school_counts_df <- school_counts_df[order(-school_counts_df$Count), ]

# Create the bar plot
ggplot(school_counts_df, aes(x = reorder(School, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "School Distribution",
       x = "School",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()



  # ------------------------------------ UTAUT ---------------------------

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
  deets[[column]] <- sapply(deets[[column]], conversion)
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
  deets[[column]] <- sapply(deets[[column]], conversion1)
}

View(deets)


performanceExpectancy <- c(
  "By using the Adobe application enables you to accomplish tasks more quickly.",
  "By using the Adobe application it increases my productivity."
)

# getting the average
means1 <- colMeans(deets[performanceExpectancy], na.rm = TRUE)
means1

# getting the standard deviation
sd_pe <- sapply(deets[performanceExpectancy], sd, na.rm = TRUE)
sd_pe


# calculate the mean and sd for Effort Expectancy
effortExpectancy <- c(
  "My interaction with the Adobe application would be clear and understandable.",
  "It would be easy for me to become more skillful at using the Adobe application.",
  "Learning to operate the Adobe application is easy for me."
)

# average
means2 <- colMeans(deets[effortExpectancy], na.rm = TRUE)
means2

# standard deviation
sd_ee <- sapply(deets[effortExpectancy], sd, na.rm = TRUE)
sd_ee


#calculate the mean and sd for Social Influence
socialInfluence<- c(
  "My peers influence necessary to use the Adobe application.",
  "People who are important to me think that I should use the Adobe application."
)

# average
means3 <- colMeans(deets[socialInfluence], na.rm = TRUE)
means3

# standard deviation
sd_si <- sapply(deets[socialInfluence], sd, na.rm = TRUE)
sd_si


#calculate the mean for Facilitating Conditions
facilitatingConditions <- c(
  "I have the resources necessary to use the Adobe application.",
  "My peers is available for assistance with Adobe application's difficulties.",
  "I know the knowledge necessary to use the Adobe application."
)

# average
means4 <- colMeans(deets[facilitatingConditions], na.rm = TRUE)
means4

# standard deviation
sd_fc <- sapply(deets[facilitatingConditions], sd, na.rm = TRUE)
sd_fc


#Calculate the mean for Behavioral Intention to use the system
behavioralIntention <- c(
  "I intend to use the Adobe application in the next 2 months.",
  "I predict I will use the Adobe application in the next 2 months.",
  "I plan to use the system in the next 2 months."
)

# average
means5 <- colMeans(deets[behavioralIntention], na.rm = TRUE)
means5

# standard deviation
sd_bi <- sapply(deets[behavioralIntention], sd, na.rm = TRUE)
sd_bi


# combine all factors using kable() function
pe <- data.frame(
  Factor = "Performance Expectancy",
  Mean = colMeans(deets[performanceExpectancy], na.rm = TRUE),
  SD = sapply(deets[performanceExpectancy], sd, na.rm = TRUE)
)

ee <- data.frame(
  Factor = "Effort Expectancy",
  Mean = colMeans(deets[effortExpectancy], na.rm = TRUE),
  SD = sapply(deets[effortExpectancy], sd, na.rm = TRUE)
)

si <- data.frame(
  Factor = "Social Influence",
  Mean = colMeans(deets[socialInfluence], na.rm = TRUE),
  SD = sapply(deets[socialInfluence], sd, na.rm = TRUE)
)

fc <- data.frame(
  Factor = "Facilitating Conditions",
  Mean = colMeans(deets[facilitatingConditions], na.rm = TRUE),
  SD = sapply(deets[facilitatingConditions], sd, na.rm = TRUE)
)

bi <- data.frame(
  Factor = "Behavioral Intention",
  Mean = colMeans(deets[behavioralIntention], na.rm = TRUE),
  SD = sapply(deets[behavioralIntention], sd, na.rm = TRUE)
)


data_summary <- rbind(pe, ee, si, fc, bi)

table <-  kable(data_summary)
View(table)


write.xlsx(deets, "C:/Users/steve/Documents/3G/DataCleaning(3G)/manipulated_data.xlsx", row.names = FALSE)


