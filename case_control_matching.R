install.packages("MatchIt")

library(dplyr)
library(MatchIt)

data <- read.csv("C:/Users/svenm/Documents/Radboud/Bayesian Networks/BNCI/data/diabetes_binary_health_indicators_BRFSS2015.csv")

str(data)
summary(data)

data <- data %>%
  mutate(
    Smoker = as.factor(Smoker),
    Diabetes_binary = as.factor(Diabetes_binary)
  )

#exact_matched_data <- matchit(Smoker ~ Diabetes_binary, data = data, method = "exact", ratio = 1)
propensity_matched_data <- matchit(Smoker ~ Diabetes_binary, data = data, method = "nearest", ratio = 1)

#summary(exact_matched_data)
summary(propensity_matched_data)

matched_df <- match.data(matched_data)

write.csv(matched_df, "matched_data.csv", row.names = FALSE)
