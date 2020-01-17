library(tidyverse)
library(lmer)
i=1
coauthor_mats = list()
topicSims = list()

for(year in 2000:2019){
  load(paste0("authorMatrix/cogsci_binary/author_mat_year_cogsci_", year,".RData"))
  coauthor_mats[[i]] = author_mat_year
  topicSims[[i]] = read_csv(paste0("topicSimYear/cogsci_topicSim_", year,".csv"))
  i = i+1
}

cogsci_byAuthor = read_csv("cogsci_vss_nips/cogsci_byAuthor.csv")

author_key = unique(cogsci_byAuthor$authorAbbr)

author_key

model_data = data.frame(new_publication = c(),
                        prior_publication = c(),
                        topic_similarity = c())

years = 2000:2019

for(i in 1:(length(topicSims)-1)){
  for(j in 1:nrow(topicSims[[i]])){
    row = which(topicSims[[i]]$authorA[j] == author_key)
    col = which(topicSims[[i]]$authorB[j] == author_key)
    new_pub = coauthor_mats[[i+1]][row,col]
    prior_pub = coauthor_mats[[i]][row,col]
    topic_sim = topicSims[[i]]$authorsSim[j]
    model_data = rbind(model_data, c(new_pub,prior_pub,topic_sim))
  }
}

colnames(model_data) = c("new_publication",
                         "prior_publication",
                         "topic_similarity")

write.csv(model_data, file = "model_data.csv")
