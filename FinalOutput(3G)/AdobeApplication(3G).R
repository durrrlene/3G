library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(openxlsx)
library(ggplot2)
library(plotly)



orig <- read_excel("DataCleaning(3G)/questionnaire3g.xlsx")
deets <- read_excel("DataCleaning(3G)/questionnaire3g.xlsx")


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



# seeking the revisions
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



# -------------------------------------- course -------------------------

# categorizing to departments
course <- deets$Course

categorize_course <- function(course) {
  if (grepl("IT|CS|BSIT|IS", course)) {
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

school_factor <- factor(school)

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


# -------------------------------------- YEAR LEVEL -------------------------
yrlvl <- deets$`Year-level`
yrlvl_factor <- factor(yrlvl)


yrlvl_counts <- table(yrlvl_factor)

yrlvl_df <- as.data.frame(yrlvl_counts)
yrlvl_df$Year_Level <- as.factor(rownames(yrlvl_df))

plot_ly(yrlvl_df, x = ~Year_Level, y = ~Freq, type = 'scatter', mode = 'lines+markers', 
        marker = list(color = '', size = 10), line = list(color = 'yellow')) %>%
  layout(title = "Year Level Counts",
         xaxis = list(title = "Year Level", tickmode = "array", tickvals = ~Year_Level,
                      ticktext = c("1st", "2nd", "3rd", "4th")),
         yaxis = list(title = "Count"))



#   -------------------------------------- APPLICATION ----------------------

app <- deets$`Here are some list of Adobe Cloud which of these applications do you use?`

app_factor <- factor(app)
app_factor

app_counts <- table(app_factor)
app_counts_df <- as.data.frame(app_counts)
names(app_counts_df) <- c("Application", "Count")

app_counts_df <- app_counts_df[order(-app_counts_df$Count), ]

ggplot(data = app_counts_df, aes(x = "", y = Count, fill = Application)) +
  geom_bar(stat = "identity") +
  labs(title = "Application Usage Counts",
       x = NULL,
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "right") +
  guides(fill=guide_legend(title="Application"))





# ------------------------------------ UTAUT ---------------------------

# Read the Excel file
#questionnaire <- read_excel("questionnaire3g.xlsx", sheet = "Form Responses 1")

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
  deets[[column_index]] <- sapply(deets[[column_index]], conversion)
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
  deets[[column]] <- sapply(deets[[column]], conversion1)
}

View(deets)


#Mean and Standard Deviation
# Calculate the mean and sd for Performance Expectancy
performanceExpectancy <- deets[, c(9:11)]

means1 <- colMeans(performanceExpectancy, na.rm = TRUE)
sd_pe <- sapply(performanceExpectancy, sd, na.rm = TRUE)

avg_mean1 <- round(mean(means1), 2)
avg_sd1 <- round(mean(sd_pe), 2)


# Calculate the mean and sd for Effort Expectancy
effortExpectancy <- deets[, c(12:14)]

means2 <- colMeans(effortExpectancy, na.rm = TRUE)
sd_ee <- sapply(effortExpectancy, sd, na.rm = TRUE)

avg_mean2 <- round(mean(means2), 2)
avg_sd2 <- round(mean(sd_ee), 2)

# Calculate the mean and sd for Social Influence
socialInfluence <- deets[, c(19:21)]

means3 <- colMeans(socialInfluence, na.rm = TRUE)
sd_si <- sapply(socialInfluence, sd, na.rm = TRUE)

avg_mean3 <- round(mean(means3), 2)
avg_sd3 <- round(mean(sd_si), 2)

# Calculate the mean and sd for Facilitating Conditions
facilitatingConditions <- deets[, c(22:25)]

means4 <- colMeans(facilitatingConditions, na.rm = TRUE)
sd_fc <- sapply(facilitatingConditions, sd, na.rm = TRUE)

avg_mean4 <- round(mean(means4), 2)
avg_sd4 <- round(mean(sd_fc), 2)

# Calculate the mean and sd for Behavioral Intention to use the system
behavioralIntention <- deets[, c(33:35)]

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
  Average_Mean = avg_mean2,
  Average_SD = avg_sd2
)


si <- data.frame(
  Description = "Social Influence",
  Mean = means3,
  SD = sd_si,
  Average_Mean = avg_mean3,
  Average_SD = avg_sd3
)

fc <- data.frame(
  Description = "Facilitating Conditions",
  Mean = means4,
  SD = sd_fc,
  Average_Mean = avg_mean4,
  Average_SD = avg_sd4
)

bi <- data.frame(
  Description = "Behavioral Intention",
  Mean = means5,
  SD = sd_bi,
  Average_Mean = avg_mean5,
  Average_SD = avg_sd5
)

#####Combine all factors using kable() function######
summary <- rbind(pe, ee, si, fc, bi)
summary_table<-  kable(summary)
View(summary_table)


write.xlsx(deets, "FinalOutput(3G)/updated_data.xlsx", rowNames = FALSE)


