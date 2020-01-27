setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(tidyverse)
library(lme4)
cogsci_byAuthor = read_csv("../cogsci_vss_nips/cogsci_byAuthor.csv")
centrality2018 = read_csv("networkByYear/centrality_2018.csv")
centrality_all = read_csv("networkByYear/centrality_all.csv")

model_data <- data.frame()
for(i in 2001:2019){
  subset_data <- read_csv(paste0("topicCoauthMatr/topicCoauth",i,".csv")) %>%
    dplyr::select(-X1)
  model_data <- bind_rows(model_data, subset_data)
}
write.csv(model_data, "full_model_data.csv")

glimpse(model_data)
model_data %>%
  filter((authorA == "J Fan" | authorB == "J Fan") & prior_publication == 1)

nrow(model_data)
model_data %>%
  filter(is.na(prior_publication)) %>%
  group_by(authorA) %>%
  summarise(n=n()) %>%
  dplyr::arrange(desc(n)) # some authors missing from coauthorship matrix



# model with 1 previous year
df <- model_data %>%
  filter(year < 2019)
glimpse(df)

m.quad <- glm(new_publication ~ prior_publication + poly(topicSim,2), data=df, family=binomial())
summary(m.quad)

m <- glm(new_publication ~ prior_publication + poly(topicSim,1), data=df, family=binomial())
summary(m)

m.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
summary(m.base)

anova(m.base, m,test='Chisq')

## predict 2019
pred2019 <- model_data %>%
  filter(year == 2019)
predict.m <- predict.glm(m.quad, newdata=dplyr::select(pred2019.df,c(prior_publication,topicSim)), family="binomial")

logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}
pred2019.df$new_publication <- logOdds.to.Prob(predict.m)

write.csv(pred2019.df, "model_prediction.csv")

