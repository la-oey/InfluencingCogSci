
library(tidyverse)
library(lme4)
library(futile.matrix)
i=1
coauthor_mats = list()
topicSims = list()

for(year in 1981:2019){
  load(paste0("authorMatrix/fullcogsci_binary/author_mat_year_fullcogsci_", year,".RData"))
  coauthor_mats[[i]] <- author_mat_year
  names(coauthor_mats[[i]]) <-NULL 
  topicSims[[i]] <- read_csv(paste0("topicSimYear/cogsci_topicSim_", year,".csv"))
  i = i+1
}

length(topicSims) == length(coauthor_mats)

isSymmetric(coauthor_mats[[39]])

# this makes is the coauthorship network is symmetric
makeSym = function(mat){
  mat[upper.tri(mat, diag = TRUE)] <- t(mat)[upper.tri(mat, diag = TRUE)]
  return(mat)
}


# Lauren's attempt to combine topic similarity and publication. 
# after running this, still need to full_bind() with next year's coauthorship network

topic.coauthor.matrices <- list()
years = 1981:2019

for(i in 1:length(topicSims)){
  author_key = fullcogsci_byAuthor %>% filter(year  == years[i]) %>% pull(authorAbbr) %>% unique()
  print(i)
  tempTop <- topicSims[[i]]
  tempTop <- tempTop %>%
    spread(authorB, authorsSim) %>%
    dplyr::select(-authorA) %>%
    as.matrix()
  tempTop[upper.tri(tempTop, diag = TRUE)] <- NA #ignore upper triangle
  
  tempAuth <- coauthor_mats[[i]]
  colnames(tempAuth) <- author_key
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
}

write.csv( topic.coauthor.matrices[[i]], file = "topic.coauthor.matrices.fullcogsci.csv")
