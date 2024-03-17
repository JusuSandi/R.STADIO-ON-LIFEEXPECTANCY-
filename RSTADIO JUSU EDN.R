library(DataExplorer)
names(lifeexpectancy_dataset)
str(lifeexpectancy_dataset)
summary(lifeexpectancy_dataset)
library(ggplot2)
library(plotly)
plot1 <- ggplot(lifeexpectancy_dataset, aes(x = Adult_Mortality)) +  
  geom_histogram(fill = "skyblue", color = "black", bins = 20) + 
  labs(title = "Histogram Of Adult_Mortality", x = "Deaths", y = "Frequency") 
plot1
plot20 <- ggplot(lifeexpectancy_dataset,aes(x=Adult_Mortality, y=Life_expectancy)) + geom_point() + geom_smooth(color="tomato")
plot20
plot21 <- ggplot(lifeexpectancy_dataset, aes(x=Adult_Mortality, y= Life_expectancy)) + geom_smooth(color = "tomato") + theme_dark() + facet_wrap(~Status)
plot21
plot21 <- ggplot(lifeexpectancy_dataset, aes(x=infant_deaths, y= Life_expectancy)) + geom_smooth(color = "tomato") + theme_dark() + facet_wrap(~Status) + ggtitle("raster") + theme_dark()
plot21
Life_exp_by_country <- aggregate(lifeexpectancy_dataset ~ Country, data = lifeexpectancy_dataset, FUN = mean)
library(dplyr)
top_5 <- lifeexpectancy_dataset %>%
  arrange(desc(lifeexpectancy_dataset)) %>%
  head(5)
bottom_5 <- lifeexpectancy_dataset %>%
  arrange(lifeexpectancy_dataset) %>%
  head(5)
print("Top 5 countries with highest life expectancy:")
print(top_5)
print("Bottom 5 countries with lowest life expectancy:")
print(bottom_5)
library(ggplot2)
ggplot(lifeexpectancy_dataset, aes(x = GDP, y = Life_expectancy)) +
  geom_point() +
  labs(x = "GDP", y = "Life Expectancy") +
  ggtitle("Relationship between GDP and Life Expectancy")
ggplot(lifeexpectancy_dataset, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(x = "BMI", y = "Frequency") +
  ggtitle("Distribution of BMI in the Population")
ggplot(lifeexpectancy_dataset, aes(x = thinness1_9_years, y = thinness_5_9years)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.8) +
  labs(x = "Age Group", y = "Prevalence of Thinness") +
  ggtitle("Prevalence of Thinness in Different Age Groups")
ggplot(lifeexpectancy_dataset, aes(x = Schooling, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Schooling (Years)", y = "Life Expectancy") +
  ggtitle("Relationship between Schooling and Life Expectancy")
correlation <- cor(lifeexpectancy_dataset$Income_composition, lifeexpectancy_dataset$Diphtheria, use = "complete.obs")
print(paste("Correlation coefficient:", correlation))
top_countries <- lifeexpectancy_dataset[order(-lifeexpectancy_dataset$under_five_deaths),]
top_countries <- head(top_countries, 10)
ggplot(top_countries, aes(x = reorder(Country, -under_five_deaths), y = under_five_deaths)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Country", y = "Under 5 Deaths", title = "Top Countries with Under 5 Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

