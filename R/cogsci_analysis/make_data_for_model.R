setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
cogsci_byAuthor = read_csv("../cogsci_vss_nips/cogsci_byAuthor.csv")
author_key = unique(cogsci_byAuthor$authorAbbr)
author_key

library(tidyverse)
library(lme4)
library(futile.matrix)
i=1
coauthor_mats = list()
topicSims = list()

for(year in 2000:2019){
  load(paste0("../authorMatrix/cogsci_binary/author_mat_year_cogsci_", year,".RData"))
  coauthor_mats[[i]] = author_mat_year
  topicSims[[i]] = read_csv(paste0("topicSimYear/cogsci_topicSim_", year,".csv"))
  i = i+1
}

length(topicSims) #should be 20


peek(coauthor_mats[[1]], 5)
head(topicSims[[1]],5)



# sanity check
cogsci_byAuthor %>%
  filter(year==2000) %>%
  .$authorAbbr %>%
  unique() %>%
  length()
topicSims[[1]] %>%
  filter(authorB == "V Sloutsky") %>%
  nrow()
# should be equal for each year




# Lauren's attempt to combine topic similarity and publication. 
# after running this, still need to full_bind() with next year's coauthorship network

topic.coauthor.matrices <- list()

start <- Sys.time()
for(i in 1:20){
  print(paste0("year: ",i+1999))
  tempTop <- topicSims[[i]]
  tempTop <- tempTop %>%
    spread(authorB, authorsSim) %>%
    dplyr::select(-authorA) %>%
    as.matrix()
  tempTop[upper.tri(tempTop, diag = TRUE)] <- NA #ignore upper triangle
  peek(round(tempTop,4),15)
  
  tempAuth <- coauthor_mats[[i]]
  colnames(tempAuth) <- author_key
  tempAuth[upper.tri(tempAuth, diag = TRUE)] <- t(tempAuth)[upper.tri(tempAuth, diag = TRUE)] #make coauthorship network symmetric
  #peek(tempAuth, 15)
  
  tempTop.df <- tempTop %>%
    as.data.frame() %>%
    gather("authorA","topicSim",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop)))) %>%
    filter(!is.na(topicSim))
  tempAuth.df <- tempAuth %>%
    as.data.frame() %>%
    gather("authorA","prior_publication",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth)))) %>%
    filter(!is.na(prior_publication))
  tempBoth <- tempTop.df %>%
    left_join(tempAuth.df, by=c("authorA","authorB")) #this takes the longest
  
  topic.coauthor.matrices[[i]] <- tempBoth
  print(Sys.time()-start)
}








#Isabella's code to combine topic similarity and publication

model_data = data.frame(authorA = c(),
                        authorB = c(),
                        new_publication = c(),
                        prior_publication = c(),
                        topic_similarity = c())

for(i in 1:(length(topicSims)-1)){
  for(j in 1:nrow(topicSims[[i]])){
    rowAuth = topicSims[[i]]$authorA[j]
    colAuth = topicSims[[i]]$authorB[j]
    row = which(topicSims[[i]]$authorA[j] == author_key)
    col = which(topicSims[[i]]$authorB[j] == author_key)
    new_pub = coauthor_mats[[i+1]][row,col]
    prior_pub = coauthor_mats[[i]][row,col]
    topic_sim = topicSims[[i]]$authorsSim[j]
    model_data = bind_rows(model_data, data.frame(authorA=rowAuth, #keep track of authors, critical for removing identity "co-authors"
                                                  authorB=colAuth,
                                                  new_publication=new_pub,
                                                  prior_publication=prior_pub,
                                                  topic_similarity=topic_sim))
  }
}


# colnames(model_data) = c("new_publication",
#                          "prior_publication",
#                          "topic_similarity")
model_data
write.csv(model_data, file = "model_data.csv")


nrow(model_data)
model_data_diffAuthors <- model_data %>%
  filter(authorA != authorB)

head(model_data_diffAuthors)
glm(model_data_diffAuthors, aes(new_publication ~ prior_publication + topic_similarity, family="binomial"))
