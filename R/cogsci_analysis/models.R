setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(lme4)
cogsci_byAuthor = read_csv("cogsci_byAuthor.csv")
centrality2018 = read_csv("networkByYear/centrality_2018.csv")
centrality_all = read_csv("networkByYear/centrality_all.csv")

model_data <- data.frame()
for(i in 2000:2019){
  subset_data <- read_csv(paste0("topicCoauthMatr/topicCoauth",i,".csv")) %>%
    mutate(topicSim = authorsSim) %>%
    dplyr::select(-c(X1, authorsSim))
  model_data <- bind_rows(model_data, subset_data)
}
write_csv(model_data, "full_model_data.csv")




glimpse(model_data)
model_data %>%
  filter((authorA == "J Fan" | authorB == "J Fan") & prior_publication == 1)

nrow(model_data)
model_data %>%
  filter(is.na(prior_publication)) %>%
  group_by(authorA) %>%
  summarise(n=n()) %>%
  dplyr::arrange(desc(n)) # some authors missing from coauthorship matrix

data.pre2019 <- model_data %>%
  filter(year < 2019) %>%
  mutate(logTopicSim = -log(pi/2-topicSim))


# model we'll be using?
m.log <- glm(new_publication ~ prior_publication + logTopicSim, data=data.pre2019, family=binomial())
summary(m.log)

m <- glm(new_publication ~ prior_publication + topicSim, data=df, family=binomial())
summary(m)

m.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
summary(m.base)

anova(m.quad, m.base,test='Chisq')

## predict 2019
pred2019.df <- model_data %>%
  filter(year == 2019)
predict.m <- predict.glm(m, newdata=dplyr::select(pred2019.df,c(prior_publication,topicSim)), family="binomial")
logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}
pred2018.df$new_publication <- logOdds.to.Prob(predict.m)

write.csv(pred2019.df, "model_prediction.csv")





# # subset of 2018 authors
# multAuthors <- cogsci_byAuthor %>%
#   filter(year >= 2015) %>%
#   group_by(authorAbbr) %>%
#   summarise(n = n()) %>%
#   filter(n > 1) %>%
#   .$authorAbbr
# centrality2018 <- centrality2018 %>%
#   mutate(CM = as.factor(CM))
# central <- centrality2018 %>%
#   filter(CM=="eigen" & label %in% multAuthors) %>%
#   dplyr::arrange(desc(measure)) %>%
#   top_n(100, measure) %>%
#   dplyr::arrange(label) %>%
#   mutate(index=1:nrow(.))
# centralA <- central %>%
#   mutate(authorA = label,
#          indexA = index) %>%
#   dplyr::select(c(authorA, indexA))
# centralB <- central %>%
#   mutate(authorB = label,
#          indexB = index) %>%
#   dplyr::select(c(authorB, indexB))
# head(central)
# head(pred2018.df)
# 
# pred2018.df %>%
#   filter(authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
#   left_join(centralA, by=c("authorA")) %>%
#   left_join(centralB, by=c("authorB")) %>%
#   ggplot(aes(x=authorB, y=authorA, fill=prior_publication)) +
#   geom_raster() +
#   ggtitle("Observed 2018 Co-authorships") +
#   scale_x_discrete("") +
#   scale_y_discrete("") +
#   theme(text = element_text(size=7),
#         axis.text.x = element_text(angle=45, hjust=1),
#         legend.position="none")
# ggsave("img/coauthorship.matr/observed2018.png", height=9, width=8.9)
#   
# # 2019 prediction from model
# pred2018.df %>%
#   filter(authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
#   left_join(centralA, by=c("authorA")) %>%
#   left_join(centralB, by=c("authorB")) %>%
#   mutate(likely = ifelse(new_publication > 1/2003, "likely","unlikely")) %>%
#   ggplot(aes(x=authorB, y=authorA, fill=likely)) +
#   geom_raster() +
#   ggtitle("Predicted 2019 Co-authorships") +
#   scale_x_discrete("") +
#   scale_y_discrete("") +
#   theme(text = element_text(size=7),
#         axis.text.x = element_text(angle=45, hjust=1))
# ggsave("img/coauthorship.matr/predicted2019_bin.png", height=9, width=8.9)
# 
# # actual 2019 co-authorship
# publishedAgain <- model_data %>%
#   filter(year == 2019 & authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
#   .$authorA %>%
#   unique() %>%
#   length()
# 
# model_data %>%
#   filter(year == 2019 & authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
#   left_join(centralA, by=c("authorA")) %>%
#   left_join(centralB, by=c("authorB")) %>%
#   ggplot(aes(x=authorB, y=authorA, fill=prior_publication)) +
#   geom_raster() +
#   ggtitle(paste0("Actual 2019 Co-authorships (", publishedAgain, " of 100 authors)")) +
#   scale_x_discrete("") +
#   scale_y_discrete("") +
#   theme(text = element_text(size=7),
#         axis.text.x = element_text(angle=45, hjust=1),
#         legend.position="none")
# ggsave("img/coauthorship.matr/actual2019.png", height=9, width=8.9)






#### Model data with 5 years ####################


model_data.5 <- data.frame()
for(i in 2004:2019){
  subset_data <- read_csv(paste0("topicCoauthMatr.5/topicCoauth",i,".5.csv"), 
                          col_types = cols(prior_publication_minus1=col_number(),
                                           prior_publication_minus2=col_number(),
                                           prior_publication_minus3=col_number(),
                                           prior_publication_minus4=col_number(),
                                           prior_publication_minus5=col_number())) %>%
    mutate(prior_publication_minus2=ifelse(is.na(prior_publication_minus2), 0, prior_publication_minus2),
           prior_publication_minus3=ifelse(is.na(prior_publication_minus3), 0, prior_publication_minus3),
           prior_publication_minus4=ifelse(is.na(prior_publication_minus4), 0, prior_publication_minus4),
           prior_publication_minus5=ifelse(is.na(prior_publication_minus5), 0, prior_publication_minus5)) %>%
    dplyr::select(-c(X1))
  model_data.5 <- bind_rows(model_data.5, subset_data)
}
write_csv(model_data.5, "full_model_data.5.csv")

model_data.5 %>%
  filter((authorA == "N Goodman" | authorB == "N Goodman") & prior_publication_minus1 == 1)


data.pre2019.5 <- model_data.5 %>%
  filter(year < 2019 & !is.na(topicSim) & 
           !is.na(prior_publication_minus1) & !is.na(prior_publication_minus2) &
           !is.na(prior_publication_minus3) & !is.na(prior_publication_minus4) &
           !is.na(prior_publication_minus5))
head(data.pre2019.5)


data.pre2019.5 %>%
  filter(prior_publication_minus1 == 1) %>%
  head(10)


round(cor(dplyr::select(data.pre2019.5, c(prior_publication_minus1, prior_publication_minus2, 
                                    prior_publication_minus3, prior_publication_minus4, 
                                    prior_publication_minus5, topicSim, new_publication))),4)

m5.quad <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                 prior_publication_minus3 + prior_publication_minus4 +
                 prior_publication_minus5 + poly(topicSim,2), data=data.pre2019.5, family=binomial())
summary(m5.quad)


m5.line <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                 prior_publication_minus3 + prior_publication_minus4 +
                 prior_publication_minus5 + poly(topicSim,1), data=data.pre2019.5, family=binomial())
summary(m5.line)

anova(m5.line, m5.quad, test='Chisq')

data.frame(prior_pub_year = paste0("year = n-",1:5),
           estimate = coef(summary(m5.quad))[2:6,'Estimate'],
           std.err = coef(summary(m5.quad))[2:6,'Std. Error']) %>%
  mutate(lower=estimate-std.err,
         upper=estimate+std.err) %>%
  ggplot(aes(x=prior_pub_year, y=estimate)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper))
ggsave("img/betaParameter_year.png")
