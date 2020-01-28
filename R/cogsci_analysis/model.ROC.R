setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(lme4)
library(car) #VIF
library(arm) #binned residual plot

model_data <- read_csv("full_model_data.csv")


#### FUNCTIONS ####

logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}

###################


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
  filter(year < 2019 & !is.na(prior_publication)) %>%
  #mutate(logTopicSim = log(topicSim+0.0000001))
  mutate(logTopicSim = -log(pi/2-topicSim))

cor(dplyr::select(data.pre2019, c(topicSim, prior_publication, new_publication)))

m.base <- glm(new_publication ~ prior_publication, data=data.pre2019, family=binomial())
summary(m.base)

m <- glm(new_publication ~ prior_publication + topicSim, data=data.pre2019, family=binomial())
summary(m)

m.log <- glm(new_publication ~ prior_publication + logTopicSim, data=data.pre2019, family=binomial())
summary(m.log)

anova(m.base, m.log, test='Chisq')






# split data 90% = training, 10% = verification for each year
verification_set <- data.pre2019 %>%
  group_by(year) %>%
  sample_n(floor(.1*n()))
training_set <- data.pre2019 %>%
  anti_join(verification_set)


train.m.line.log <- glm(new_publication ~ prior_publication + logTopicSim, data=training_set, family=binomial())
summary(train.m.line.log)

# train.m.line <- glm(new_publication ~ prior_publication + topicSim, data=training_set, family=binomial())
# summary(train.m.line)
# 
# train.m.base <- glm(new_publication ~ prior_publication, data=training_set, family=binomial())
# summary(train.m.base)
# 
# predict.verif.m.line <- predict.glm(train.m.line, newdata=dplyr::select(training_set,c(prior_publication,topicSim)), family="binomial")
# training_set$predicted_new.line <- logOdds.to.Prob(predict.verif.m.line)

# predict.verif.m.line.log <- predict.glm(train.m.line.log, newdata=dplyr::select(training_set,c(prior_publication,logTopicSim)), family="binomial")
# training_set$predicted_new.line.log <- logOdds.to.Prob(predict.verif.m.line.log)


### Visualizing Error in Linear Topic Similarity Predictors ###

# training_set <- training_set %>%
#   mutate(resid.line = new_publication - predicted_new.line.log,
#          neglogTopicSim = -log10(pi/2-topicSim),
#          bin = ntile(logTopicSim,200))
# 
# bin.training.resid <- training_set %>%
#   filter(bin != 1) %>%
#   group_by(bin) %>%
#   summarise(mean.bin.logTopicSim=mean(logTopicSim),
#             mean.resid=mean(resid.line))
# ggplot(bin.training.resid, aes(x=mean.bin.logTopicSim, y=mean.resid)) +
#   geom_point() +
#   scale_x_continuous("binned topic similarity")
# ggsave("img/binnedResidPlot.loglinearPredictor.png")

###




predict.verif.m <- predict.glm(train.m.line.log, newdata=dplyr::select(verification_set,c(prior_publication,logTopicSim)), family="binomial")
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




#### Model data w/ 5 years of coauthorships ####

model_data.5 <- read_csv("full_model_data.5.csv")

data.pre2019.5 <- model_data.5 %>%
  filter(year < 2019 & !is.na(topicSim) & 
           !is.na(prior_publication_minus1) & !is.na(prior_publication_minus2) &
           !is.na(prior_publication_minus3) & !is.na(prior_publication_minus4) &
           !is.na(prior_publication_minus5)) %>%
  mutate(neglogTopicSim = log(pi/2-topicSim),
         logTopicSim = log(topicSim+0.0001))

# split data 80% = training, 20% = verification for each year
verification_set <- data.pre2019.5 %>%
  group_by(year) %>%
  sample_n(floor(.2*n()))
training_set <- data.pre2019.5 %>%
  anti_join(verification_set)

# m5.train.log <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
#                  prior_publication_minus3 + prior_publication_minus4 +
#                  prior_publication_minus5 + logTopicSim, data=training_set, family=binomial())
# summary(m5.train.log)

m5.train.neglog <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                      prior_publication_minus3 + prior_publication_minus4 +
                      prior_publication_minus5 + neglogTopicSim, data=training_set, family=binomial())
summary(m5.train.neglog)

m5.train <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                      prior_publication_minus3 + prior_publication_minus4 +
                      prior_publication_minus5 + topicSim, data=training_set, family=binomial())
summary(m5.train)

m5.train.noSim <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                      prior_publication_minus3 + prior_publication_minus4 +
                      prior_publication_minus5, data=training_set, family=binomial())
summary(m5.train.noSim)

m4.train.noSim <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                        prior_publication_minus3 + prior_publication_minus4, data=training_set, family=binomial())
summary(m4.train.noSim)

m3.train.noSim <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                        prior_publication_minus3, data=training_set, family=binomial())
summary(m3.train.noSim)

m2.train.noSim <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2, data=training_set, family=binomial())
summary(m2.train.noSim)

m1.train.noSim <- glm(new_publication ~ prior_publication_minus1, data=training_set, family=binomial())
summary(m1.train.noSim)

anova(m1.train.noSim, m2.train.noSim, m3.train.noSim, m4.train.noSim, m5.train.noSim, m5.train.neglog, test='Chisq')

# plot residuals from model
# predict.train.m5 <- predict.glm(m5.train.neglog,
#                                newdata=dplyr::select(training_set,
#                                                      c(prior_publication_minus1,
#                                                        prior_publication_minus2,
#                                                        prior_publication_minus3,
#                                                        prior_publication_minus4,
#                                                        prior_publication_minus5,
#                                                        neglogTopicSim)),
#                                family="binomial")
# training_set$predicted_new <- logOdds.to.Prob(predict.train.m5)
# 
# training_set <- training_set %>%
#   mutate(resid = new_publication - predicted_new,
#          bin = ntile(neglogTopicSim,200))
# 
# bin.training.resid <- training_set %>%
#   group_by(bin) %>%
#   summarise(mean.bin.logTopicSim=mean(neglogTopicSim),
#             mean.resid=mean(resid))
# ggplot(bin.training.resid, aes(x=mean.bin.logTopicSim, y=mean.resid)) +
#   geom_point() +
#   scale_x_continuous("binned topic similarity")








predict.verif.m5 <- predict.glm(m5.train.neglog,
                                newdata=dplyr::select(verification_set,
                                                      c(prior_publication_minus1,
                                                        prior_publication_minus2,
                                                        prior_publication_minus3,
                                                        prior_publication_minus4,
                                                        prior_publication_minus5,
                                                        neglogTopicSim)),
                                family="binomial")
verification_set$predicted_new <- logOdds.to.Prob(predict.verif.m5)

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
