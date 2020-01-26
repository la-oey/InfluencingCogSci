setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(lme4)
library(car)

model_data <- read_csv("full_model_data.csv") %>%
  select(-X1)


# sanity check
model_data %>%
  filter((authorA == "J Fan" | authorB == "J Fan") & prior_publication == 1)


model_data %>%
  group_by(prior_publication, new_publication) %>%
  summarise(n=n())

model_data %>%
  filter(is.na(prior_publication)) %>%
  group_by(authorA) %>%
  summarise(n = n()) %>%
  dplyr::arrange(desc(n))

data.pre2019 <- model_data %>%
  filter(year < 2019 & !is.na(prior_publication))

cor(dplyr::select(data.pre2019, c(authorsSim, prior_publication, new_publication)))

train.m <- glm(new_publication ~ prior_publication + poly(authorsSim,1), data=data.pre2019, family=binomial())
summary(train.m)

train.m.quad <- glm(new_publication ~ prior_publication + poly(authorsSim,2), data=data.pre2019, family=binomial())
summary(train.m.quad)

vif(train.m.quad)

anova(train.m, train.m.quad, test='Chisq')






# split data 90% = training, 10% = verification for each year
verification_set <- data.pre2019 %>%
  group_by(year) %>%
  sample_n(floor(.1*n()))
training_set <- data.pre2019 %>%
  anti_join(verification_set)

train.m <- glm(new_publication ~ prior_publication + authorsSim, data=training_set, family=binomial())
summary(train.m)




predict.verif.m <- predict.glm(train.m, newdata=dplyr::select(verification_set,c(prior_publication,authorsSim)), family="binomial")
logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}
verification_set$predicted_new <- logOdds.to.Prob(predict.verif.m)

(quants <- quantile(verification_set$predicted_new, c(.25,.5,.75,.8,.85,.9,.95,.975,.998,.999096), na.rm=TRUE))



all.accuracy <- data.frame()
for(i in 1:length(quants)){
  THRESHOLD <- quants[[i]]
  temp.accuracy <- verification_set %>%
    mutate(predictAcc = case_when(
      predicted_new >= THRESHOLD & new_publication==1 ~ "HIT",
      predicted_new >= THRESHOLD & new_publication==0 ~ "FALSE ALARM",
      predicted_new < THRESHOLD & new_publication==1 ~ "MISS",
      predicted_new < THRESHOLD & new_publication==0 ~ "CORRECT REJECTION"
    )) %>%
    group_by(predictAcc) %>%
    summarise(n=n())
  all.accuracy <- bind_rows(all.accuracy, data.frame(threshold=THRESHOLD,
                                                     hit=temp.accuracy$n[3],
                                                     false.alarm=temp.accuracy$n[2],
                                                     miss=temp.accuracy$n[4],
                                                     correct.rejection=temp.accuracy$n[1]))
}

all.accuracy

maxAcc <- verification_set %>%
  group_by(new_publication) %>%
  summarise(n=n())

all_accuracy <- all.accuracy %>%
  mutate(false.alarm.perc = false.alarm / filter(maxAcc,new_publication==0)$n,
         hit.perc = hit / filter(maxAcc,new_publication==1)$n,
         dprime = qnorm(hit.perc) - qnorm(false.alarm.perc))
all_accuracy

ggplot(all_accuracy, aes(x=false.alarm.perc, y=hit.perc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour="forestgreen", linetype=2) +
  ggtitle("ROC Curve") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1))
ggsave("img/ROCcurve.png")
