# Mike Bonsignore
# CIS 468
# Assignment 1
# 2/12/23

###### DATA READ AND SUMMARY #####

game_data <- read.csv(file = "games.csv", stringsAsFactors = T)

names(game_data)
game_data[1:10,]

summary(game_data)
summary(game_data$price_final)
summary(game_data$mac)
summary(game_data$rating)
min(game_data$price_original, na.rm = T)
max(game_data$price_original, na.rm = T)
mean(game_data$price_original, na.rm = T)
median(game_data$price_original, na.rm = T)
length(game_data$price_original)
var(game_data$price_original, na.rm = T)
sd(game_data$price_original, na.rm = T)


##### MATH AND DESCRIPTIVE STATS #####

# skew
price_original_skew <- (3*(mean(game_data$price_original, na.rm = T)
                           - median(game_data$price_original, na.rm = T)
                           ))/sd(game_data$price_original, na.rm = T)

price_discounted_skew <- (3*(mean(game_data$price_discounted, na.rm = T)
                             - median(game_data$price_discounted, na.rm = T)
                            ))/sd(game_data$price_discounted, na.rm = T)

price_final_skew <- (3*(mean(game_data$price_final, na.rm = T)
                        - median(game_data$price_final, na.rm = T)
                        ))/sd(game_data$price_final, na.rm = T)

price_original_skew
price_discounted_skew
price_final_skew

# outliers
sd_price_original <- sd(game_data$price_original, na.rm = T)
sd_upper <- mean(game_data$price_original, na.rm = T)+(3*sd_price_original)
sd_lower <- mean(game_data$price_original, na.rm = T)-(3*sd_price_original)
outlier_price_original_sd <- (game_data$price_original > sd_upper
                              | game_data$price_original < sd_lower)
outlier_price_original_sd

zscore_price_original <- (game_data$price_origina - mean(game_data$price_original, na.rm = T))/sd(game_data$price_original, na.rm = T)
outlier_price_original_zscore <- zscore_price_original > 3 | zscore_price_original < (-3)
outlier_price_original_zscore

# iqr
quartile_price_original <- quantile(game_data$price_original, names=FALSE, na.rm = T)
iqr_price_original <- quartile_price_original[4]-quartile_price_original[2]
iqr_upper <- quartile_price_original[4]+(1.5*iqr_price_original)
iqr_lower <- quartile_price_original[2]-(1.5*iqr_price_original)
outlier_income_iqr <- game_data$price_original>iqr_upper|game_data$price_original<iqr_lower
outlier_income_iqr


##### VISUALIZATIONS #####

library(ggplot2)
library(scales)
library(WVPlots)

# ratings distribution
ggplot(game_data, aes(x = rating)) + geom_bar(fill = "blue") + coord_flip() +
                  scale_x_discrete(limits = 
                  c("Mostly Negative", "Mixed", "Positive", "Mostly Positive", "Very Positive", "Overwhelmingly Positive"))

# price distributions

install.packages("gridExtra")
library(gridExtra)

price_original_graph <- ggplot(game_data, aes(x = price_original)) + geom_density()
price_discounted_graph <- ggplot(game_data, aes(x = price_discounted)) + geom_density()
price_final_graph <- ggplot(game_data, aes(x = price_final)) + geom_density()

grid.arrange(price_original_graph, price_discounted_graph, price_final_graph, ncol = 1)

# prices vs rating
ggplot(game_data, aes(x = rating, y = price_original)) + geom_boxplot() + 
                  scale_x_discrete(limits = 
                  c("Mostly Negative", "Mixed", "Positive", "Mostly Positive", "Very Positive", "Overwhelmingly Positive"))

# OS
OS_Win_chart <- ggplot(game_data, aes(x = win)) + geom_bar()
OS_Mac_chart <- ggplot(game_data, aes(x = mac)) + geom_bar()
OS_Linux_chart <- ggplot(game_data, aes(x = linux)) + geom_bar()

grid.arrange(OS_Win_chart, OS_Mac_chart, OS_Linux_chart, ncol = 3)

# OS vs rating
win_vs_rating <- ggplot(game_data, aes(x= rating, fill=win)) + 
  geom_bar(position = "fill") + 
  scale_x_discrete(limits = 
  c("Mostly Negative", "Mixed", "Positive", "Mostly Positive", "Very Positive", "Overwhelmingly Positive")) +
  scale_fill_brewer(palette="Set1") +
  labs(fill="Windows Compatible") +
  xlab("Rating") +
  ylab("Proportion")

mac_vs_rating <- ggplot(game_data, aes(x= rating, fill = mac)) + 
  geom_bar(position = "fill") + 
  scale_x_discrete(limits = 
                     c("Mostly Negative", "Mixed", "Positive", "Mostly Positive", "Very Positive", "Overwhelmingly Positive")) +
  scale_fill_brewer(palette="Set1") +
  labs(fill="Mac Compatible") +
  xlab("Rating") +
  ylab("Proportion")

linux_vs_rating <- ggplot(game_data, aes(x= rating, fill=linux)) + 
  geom_bar(position = "fill") + 
  scale_x_discrete(limits = 
                     c("Mostly Negative", "Mixed", "Positive", "Mostly Positive", "Very Positive", "Overwhelmingly Positive")) +
  scale_fill_brewer(palette="Set1") +
  labs(fill="Linux Compatible") +
  xlab("Rating") +
  ylab("Proportion")

grid.arrange(win_vs_rating, mac_vs_rating, linux_vs_rating, ncol = 1)
