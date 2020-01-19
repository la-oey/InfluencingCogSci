setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")

library(tidyverse)
library(lme4)

model_data <- read_csv("model_data_run2.csv")

df <- model_data %>%
  filter(authorA != authorB)
glimpse(df)
m <- glm(new_publication ~ prior_publication + topic_similarity, data=df, family=binomial())
summary(m)

m.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
summary(m.base)

anova(m.base, m,test='Chisq')
