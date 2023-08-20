# Group 6
# Project Data Cleaning
# 4/20/23

library(ggplot2)

# importing and summarizing
studentdata <- read.csv(file = "StudentsPerformance.csv", stringsAsFactors = T)

names(studentdata)
summary(studentdata)
View(studentdata)

studentdata <- studentdata[,-2]
# ethnicity groups are not defined, so we cannot draw conclusions from them
View(studentdata)

# checking for entry errors
unique(studentdata$test.preparation.course)
unique(studentdata$lunch)
unique(studentdata$gender)
unique(studentdata$parental.level.of.education)
unique(studentdata$math.score)
unique(studentdata$reading.score)
unique(studentdata$writing.score)

#### creating new variable for cumulative test scores ####
studentdata$cumulative.score <- rowSums(studentdata[, c(5, 6, 7)]/300)
View(studentdata)

ggplot(studentdata, aes(x = cumulative.score)) + geom_density()

#### binning test scores to create predictable categorical variables: ####
# bin 1 = very low, 2 = low, 3 = average, 4 = high, 5 = very high

# binning math scores
sort(unique(studentdata$math.score))
studentdata$mathbin <- as.factor(cut(studentdata$math.score, 5, labels = F))
ggplot(studentdata, aes(x = mathbin)) + 
  geom_bar()

# binning reading scores
sort(unique(studentdata$reading.score))
studentdata$readingbin <- as.factor(cut(studentdata$reading.score, 5, labels = F))
ggplot(studentdata, aes(x = readingbin)) + 
  geom_bar()

# binning writing scores
sort(unique(studentdata$writing.score))
studentdata$writingbin <- as.factor(cut(studentdata$writing.score, 5, labels = F))
ggplot(studentdata, aes(x = writingbin)) + 
  geom_bar()

View(studentdata)

#### exporting clean file with new columns ####
write.csv(studentdata, "StudentPerformance_clean.csv",
          row.names = F)
