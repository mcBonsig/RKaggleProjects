library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(caret)

StudentPerformance <- read.csv("StudentPerformance_clean_Final.csv")



lm.fit = lm(cumulative.score~parental.level.of.education.numeric, data = StudentPerformance)
lm.fit

summary(lm.fit)

#scatter plot and trend line
plot(StudentPerformance$parental.level.of.education.numeric, StudentPerformance$cumulative.score)
abline(lm.fit, lwd = 4, col="red")

# diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))



##trying to get a boxplot
StudentPerformance %>%
  group_by(test.preparation.course) %>%
  get_summary_stats(cumulative.score, type="mean_sd")

ggboxplot(StudentPerformance,x="test prep",y="cumulative.score")


##test prep course (sample independent t-test)
IToutput <- StudentPerformance %>%
  t_test(cumulative.score ~ test.preparation.course) %>%
  add_significance()
IToutput

##test gender (sample independent t-test)
IToutput <- StudentPerformance %>%
  t_test(cumulative.score ~ gender) %>%
  add_significance()
IToutput


##test lunch type (sample independent t-test)
IToutput <- StudentPerformance %>%
  t_test(cumulative.score ~ lunch) %>%
  add_significance()
IToutput

##test parental level of education (one was ANOVA)
IToutput <- StudentPerformance %>%
  t_test(cumulative.score ~ parental.level.of.education) %>%
  add_significance()
IToutput
StudentPerformance.aov <- StudentPerformance %>% anova_test(cumulative.score ~ parental.level.of.education)

StudentPerformanceMath.aov <- StudentPerformance %>% anova_test(math.score ~ parental.level.of.education)
StudentPerformanceMath.aov
StudentPerformanceWriting.aov <- StudentPerformance %>% anova_test(writing.score ~ parental.level.of.education)
StudentPerformanceWriting.aov




#regression model
lm.fit=lm(cumulative.score~lunch+test.preparation.course+gender+parental.level.of.education.numeric,data=StudentPerformance)
summary(lm.fit)
varImp(lm.fit, scale = FALSE)

lm.fit=lm(math.score~writing.score+reading.score, data=StudentPerformance)
summary(lm.fit)

lm.fit=lm(reading.score~writing.score, data=StudentPerformance)
summary(lm.fit)
