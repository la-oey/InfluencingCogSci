setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(lme4)
cogsci_byAuthor = read_csv("../cogsci_vss_nips/cogsci_byAuthor.csv")
centrality2018 = read_csv("networkByYear/centrality_2018.csv")
centrality_all = read_csv("networkByYear/centrality_all.csv")

model_data <- data.frame()
for(i in 2000:2019){
  subset_data <- read_csv(paste0("topicCoauthMatr/topicCoauth",i,".csv")) %>%
    mutate(year=i)
  model_data <- bind_rows(model_data, subset_data)
}

glimpse(model_data)


model_data <- model_data %>%
  dplyr::select(-X1) %>%
  distinct()


nrow(model_data)
model_data %>%
  filter(is.na(prior_publication)) %>%
  group_by(authorA) %>%
  summarise(n=n()) %>%
  dplyr::arrange(desc(n)) # some authors missing from coauthorship matrix

model_data %>%
  filter(!is.na(prior_publication) & !is.na(new_publication)) %>%
  ggplot(aes(x=as.factor(new_publication), y=topicSim)) +
  geom_violin() +
  coord_flip() +
  facet_grid(prior_publication~.)
ggsave("img/coauthor_violin.png")

# model with 1 previous year
df <- model_data %>%
  filter(year < 2018) %>%
  dplyr::select(-X1)
glimpse(df)
m <- glm(new_publication ~ prior_publication + topicSim, data=df, family=binomial())
summary(m)

m.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
summary(m.base)

anova(m.base, m,test='Chisq')

## predict 2019
pred2018.df <- model_data %>%
  filter(year == 2018)
predict.m <- predict.glm(m, newdata=dplyr::select(pred2018.df,c(prior_publication,topicSim)), family="binomial")
logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}
pred2018.df$new_publication <- logOdds.to.Prob(predict.m)

write.csv(pred2019.df, "model_prediction.csv")
pred2018.df <- read_csv("model_prediction.csv")
head(pred2018.df)





# subset of 2018 authors
multAuthors <- cogsci_byAuthor %>%
  filter(year >= 2015) %>%
  group_by(authorAbbr) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  .$authorAbbr
centrality2018 <- centrality2018 %>%
  mutate(CM = as.factor(CM))
central <- centrality2018 %>%
  filter(CM=="eigen" & label %in% multAuthors) %>%
  dplyr::arrange(desc(measure)) %>%
  top_n(100, measure) %>%
  dplyr::arrange(label) %>%
  mutate(index=1:nrow(.))
centralA <- central %>%
  mutate(authorA = label,
         indexA = index) %>%
  dplyr::select(c(authorA, indexA))
centralB <- central %>%
  mutate(authorB = label,
         indexB = index) %>%
  dplyr::select(c(authorB, indexB))
head(central)
head(pred2018.df)

pred2018.df %>%
  filter(authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
  left_join(centralA, by=c("authorA")) %>%
  left_join(centralB, by=c("authorB")) %>%
  ggplot(aes(x=authorB, y=authorA, fill=prior_publication)) +
  geom_raster() +
  ggtitle("Observed 2018 Co-authorships") +
  scale_x_discrete("") +
  scale_y_discrete("") +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")
ggsave("img/coauthorship.matr/observed2018.png", height=9, width=8.9)
  
# 2019 prediction from model
pred2018.df %>%
  filter(authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
  left_join(centralA, by=c("authorA")) %>%
  left_join(centralB, by=c("authorB")) %>%
  mutate(likely = ifelse(new_publication > 1/2003, "likely","unlikely")) %>%
  ggplot(aes(x=authorB, y=authorA, fill=likely)) +
  geom_raster() +
  ggtitle("Predicted 2019 Co-authorships") +
  scale_x_discrete("") +
  scale_y_discrete("") +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=45, hjust=1))
ggsave("img/coauthorship.matr/predicted2019_bin.png", height=9, width=8.9)

# actual 2019 co-authorship
publishedAgain <- model_data %>%
  filter(year == 2019 & authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
  .$authorA %>%
  unique() %>%
  length()

model_data %>%
  filter(year == 2019 & authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
  left_join(centralA, by=c("authorA")) %>%
  left_join(centralB, by=c("authorB")) %>%
  ggplot(aes(x=authorB, y=authorA, fill=prior_publication)) +
  geom_raster() +
  ggtitle(paste0("Actual 2019 Co-authorships (", publishedAgain, " of 100 authors)")) +
  scale_x_discrete("") +
  scale_y_discrete("") +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")
ggsave("img/coauthorship.matr/actual2019.png", height=9, width=8.9)



pred2019.df %>%
  filter(!is.na(prior_publication)) %>%
  group_by(prior_publication) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(proportion = n/sum(n))
ggplot(pred2019.df, aes(x=new_publication, fill=prior_publication)) +
  geom_density() 
(quants <- quantile(pred2019.df$new_publication, c(.025,.25,.5,.75,.95,.80,.975,.998,.999096), na.rm=TRUE))
topPred <- pred2019.df %>%
  filter(new_publication > quants[['80%']])

