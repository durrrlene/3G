library(readxl)
library(dplyr)

questionnaire <- read_excel("3g/questionnaire3g.xlsx")
questionnaire

factor("Age")
factor("Sex")
factor("School")
factor("Course")
factor("'Year-level'")

#getting the age average
age <- c(questionnaire$Age)

#converting age to numeric form
age <- as.numeric(gsub("[^0-9]", "", as.character(age)))
#removes zeros
age <- as.numeric(gsub("0$", "", as.character(age)))
#neglecting NAs in solving average
ave_age <- mean(age, na.rm = TRUE)
ave_age
#removing decimals
(ave_age <- floor(ave_age))
#fills in the NAs with the ave_age
age[is.na(age)] <- ave_age
age
(barplot(table(age)))
(pie(table(questionnaire$Age)))


(Sex<-factor(questionnaire$Sex))
(School <- factor(questionnaire$School))
(Course <- factor(questionnaire$Course))
(Yrlvl<- factor(questionnaire$`Year-level`))

Age
Sex
School
Course
Yrlvl

converted_Year<-as.numeric(gsub("[^0-9]", "", Yrlvl))
converted_Year

barplot(table(questionnaire$Sex))
pie(table(questionnaire$Sex))

barplot(table(questionnaire$School))
pie(table(questionnaire$School))

barplot(table(questionnaire$Course))
pie(table(questionnaire$Course))

barplot(table(Yrlvl))
pie(table(Yrlvl))
hist(converted_Year)

Age <- factor(questionnaire$Age, levels = unique(questionnaire$Age)[order(as.numeric(gsub("[^0-9]", "", unique(questionnaire$Age))))])
barplot(table(Age))

inorder<-order(Yrlvl)
Yrlvl <- factor(questionnaire$`Year-level`, levels = unique(questionnaire$`Year-level`)[inorder])
barplot(table(Yrlvl))


(App <- factor(questionnaire$`Here are some list of Adobe Cloud which of these applications do you use?`))
barplot(t(table(App)), xlab = "Frequency", ylab = "", horiz = TRUE, las = 2, col = myColors)
myColors <- "green"

