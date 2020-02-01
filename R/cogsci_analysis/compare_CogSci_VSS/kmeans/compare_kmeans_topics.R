#################################################
##### Compare k means across VSS and CogSci #####
#################################################

setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/compare_CogSci_VSS/kmeans/")
library(tidyverse)
library(factoextra)


#### load k means by year ####
mds.year = list()
cogsci_years=2000:2019
for(i in 1:length(cogsci_years)){
  temp <- read_csv(paste0("cogsci_mdsfits/mdsfit_",cogsci_years[i],".csv")) %>%
    mutate(conference="cogsci",
           year=cogsci_years[i])
  mds.year[[i]] = temp
}
vss_years=2001:2019
for(i in 1:length(vss_years)){
  temp <- read_csv(paste0("vss_mdsfits/mdsfit_",vss_years[i],".csv")) %>%
    mutate(conference="vss",
           year=vss_years[i])
  mds.year[[i+length(cogsci_years)]] = temp
}

for(n in 1:length(mds.year)){
  print(nrow(mds.year[[n]]))
}

#### compare k=5 across years ####
kmean.5 = list()
kmean.ss = data.frame()
for(i in 1:length(mds.year)){
  fitdf.final <- mds.year[[i]][,1:2]
  rownames(fitdf.final) <- mds.year[[i]]$authors
  
  for(j in 5:10){
    kmean.5[[i]] <- kmeans(fitdf.final, j, nstart = 25)
    kmean.ss <- bind_rows(kmean.ss,
                          data.frame(conference=unique(mds.year[[i]]$conference),
                                     year=unique(mds.year[[i]]$year),
                                     k=j,
                                     tot.withinss=kmean.5[[i]]$tot.withinss,
                                     betweenss=kmean.5[[i]]$betweenss,
                                     totss=kmean.5[[i]]$totss))
  }
}


# analyzing fit
## aim to minimize total within sum of squares (within group similarity)
## aim to maximize between sum of squares (between group dissimilarity)
kmean.ss %>%
  mutate(within_bw = tot.withinss/betweenss) %>%
  ggplot(aes(x=year, y=within_bw, colour=conference)) +
  geom_line() +
  #scale_x_continuous(limits=c(2000,2020)) +
  #scale_y_continuous("total within / between SS for k=5", limits=c(0,0.5)) +
  facet_wrap(~k) +
  theme_minimal()


kmean.ss %>%
  filter(k == 5) %>%
  mutate(within_bw = tot.withinss/betweenss) %>%
  ggplot(aes(x=year, y=within_bw, colour=conference)) +
  geom_line() +
  scale_x_continuous(limits=c(2000,2020)) +
  scale_y_continuous("total within / between SS for k=5", limits=c(0,0.5)) +
  facet_wrap(~k) +
  theme_minimal()
ggsave("img/kmeans_fit.png")

kmean.model <- kmean.ss %>%
  filter(k == 5) %>%
  mutate(within_bw = tot.withinss/betweenss)

model <- lm(within_bw ~ year * conference, data=kmean.model)
summary(model)











