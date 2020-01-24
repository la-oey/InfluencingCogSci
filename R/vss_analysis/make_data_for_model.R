setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(tidyverse)
library(lme4)
library(futile.matrix)
vss_byAuthor = read_csv("vss_byAuthor.csv")

fullauthor_key = unique(vss_byAuthor$authorAbbr)
fullauthor_key
length(fullauthor_key)


i=1
coauthor_mats = list()
topicSims = list()

for(year in 2001:2019){
  load(paste0("vss_binary/author_mat_year_vss_", year,".RData"))
  coauthor_mats[[i]] = author_mat_year
  names(coauthor_mats[[i]]) <-NULL 
  topicSims[[i]] = read_csv(paste0("topicSimYear/vss_topicSim_", year,".csv"))
  i = i+1
}

length(topicSims) == length(coauthor_mats)

isSymmetric(coauthor_mats[[1]])
peek(coauthor_mats[[39]],15)

### Old Functions ###
makeSym = function(mat){
  mat[upper.tri(mat, diag = TRUE)] <- t(mat)[upper.tri(mat, diag = TRUE)]
  return(mat)
}

pmax.matr <- function(matr){ #function to make matrix symmetrical (pmax)
  tri <- pmax(matr[lower.tri(matr, diag=FALSE)], t(matr)[lower.tri(matr, diag=FALSE)]) #pmax of triangels
  matr[lower.tri(matr, diag = FALSE)] <- tri
  matr[upper.tri(matr, diag=FALSE)] <- t(matr)[upper.tri(matr, diag = FALSE)]
  return(matr)
}

#################


length(topicSims) #should be 20


peek(coauthor_mats[[1]], 15)
head(topicSims[[1]],15)



# sanity check
vss_byAuthor %>%
  filter(year==2001) %>%
  .$authorAbbr %>%
  unique() %>%
  length()
topicSims[[1]] %>%
  nrow() %>%
  sqrt()
# should be equal for each year
# sanity check coauthorship matrix

test <- matrix(floor(runif(16, 0, 10)), nrow=4, ncol=4)
test
pmax.matr(test)

dim(coauthor_mats[[1]])
length(author_key)



# Lauren's attempt to combine topic similarity and publication. 
# after running this, still need to left_join() with next year's coauthorship network

topic.coauthor.matrices <- list()
years = 2001:2019

start <- Sys.time()
for(i in 1:length(topicSims)){
  print(paste0("year: ",years[i]))
  author_key = vss_byAuthor %>% filter(year  == years[i]) %>% pull(authorAbbr) %>% unique()
  
  tempTop <- topicSims[[i]]
  tempTop <- tempTop %>%
    spread(authorB, authorsSim) %>%
    dplyr::select(-authorA) %>%
    as.matrix()
  tempTop[upper.tri(tempTop, diag = TRUE)] <- NA #ignore upper triangle
  #peek(round(tempTop,4),15)

  tempAuth <- coauthor_mats[[i]]
  #tempAuth <- pmax.matr(tempAuth)
  colnames(tempAuth) <- author_key
  #peek(tempAuth, 15)

  tempTop.df <- tempTop %>%
    as.data.frame() %>%
    gather("authorA","topicSim",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop))),
           tempA = ifelse(authorA < authorB, authorA, authorB), #alphabetically first author => authorA
           tempB = ifelse(authorA > authorB, authorA, authorB), #alphabetically last author => authorB
           authorA = tempA, 
           authorB = tempB) %>%
    dplyr::select(-c(tempA, tempB)) %>%
    filter(!is.na(topicSim))
  tempAuth.df <- tempAuth %>%
    as.data.frame() %>%
    gather("authorA","prior_publication",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth))),
           tempA = ifelse(authorA < authorB, authorA, authorB), #alphabetically first author => authorA
           tempB = ifelse(authorA > authorB, authorA, authorB), #alphabetically last author => authorB
           authorA = tempA, 
           authorB = tempB) %>%
    dplyr::select(-c(tempA, tempB)) %>%
    filter(!is.na(prior_publication))
  tempBoth <- tempTop.df %>%
    left_join(tempAuth.df, by=c("authorA","authorB")) #this takes the longest

  topic.coauthor.matrices[[i]] <- tempBoth %>%
    mutate(year=years[i]) %>%
    distinct()
  print(Sys.time()-start)
}

head(topic.coauthor.matrices[[1]],15)

# 
# # bind dependent measure (whether authors publish together the following year)
start <- Sys.time()
for(i in 1:(length(topicSims)-1)){
  print(paste0("year: ",years[i]))
  temp <- topic.coauthor.matrices[[i]]
  temp.next <- topic.coauthor.matrices[[i+1]] %>%
    mutate(new_publication = prior_publication) %>%
    dplyr::select(-c(prior_publication, topicSim, year))
  temp.tot <- temp %>%
    left_join(temp.next, by=c("authorA","authorB")) %>%
    mutate(new_publication = ifelse(is.na(new_publication), 0, new_publication))
  topic.coauthor.matrices[[i]] <- temp.tot
  print(Sys.time()-start)
}

for(i in 1:length(topicSims)){
  write.csv(topic.coauthor.matrices[[i]], paste0("topicCoauthMatr/topicCoauth",years[i],".csv"))
}



# topic.coauthor.matrices <- list()
# for(i in 1:20){
#   printname = paste0("topicCoauthMatr/topicCoauth",(i+1999),".csv")
#   topic.coauthor.matrices[[i]] <- read_csv(printname) %>%
#     mutate(year = i+1999,
#            tempA = ifelse(authorA < authorB, authorA, authorB), #alphabetically first author => authorA
#            tempB = ifelse(authorA > authorB, authorA, authorB), #alphabetically last author => authorB
#            authorA = tempA, 
#            authorB = tempB) %>%
#     dplyr::select(-c(X1, tempA, tempB))
# }
# length(topic.coauthor.matrices)
# head(topic.coauthor.matrices[[1]], 20)
# head(topic.coauthor.matrices[[2]], 20)
# 
# temp1 <- topic.coauthor.matrices[[1]] %>%
#   select(-c(new_publication, year, topicSim))
# temp2 <- topic.coauthor.matrices[[2]]
# temp.final <- full_join(temp2, temp1, by=c('authorA','authorB'), suffix=c(".2001",".2000"))
# 
# head(temp.final,20)

topic.coauthor.matrices.span <- list()
# data for model with multiple years in independent measure
N.YEARS = 5
YEAR.SPAN = N.YEARS:19
for(i in YEAR.SPAN){
  full_temp <- topic.coauthor.matrices[[i]]
  for(j in 1:(N.YEARS-1)){
    year.temp = (i+1999)-j
    temp <- topic.coauthor.matrices[[i-j]] %>%
      dplyr::select(-c(new_publication, year, topicSim))
    full_temp <- full_join(full_temp, temp, by=c('authorA','authorB'), suffix=c("",paste0(".",toString(year.temp))))
  }
  topic.coauthor.matrices.span[[i-(N.YEARS-1)]] <- full_temp
}

for(i in 1:length(topic.coauthor.matrices.span)){
  printname = paste0("topicCoauthMatrSpan/topicCoauthSpan",(i+2003),".csv")
  write.csv(topic.coauthor.matrices.span[[i]], printname)
}



#Isabella's code to combine topic similarity and publication

# model_data = data.frame(authorA = c(),
#                         authorB = c(),
#                         new_publication = c(),
#                         prior_publication = c(),
#                         topic_similarity = c())
# 
# for(i in 1:(length(topicSims)-1)){
#   for(j in 1:nrow(topicSims[[i]])){
#     rowAuth = topicSims[[i]]$authorA[j]
#     colAuth = topicSims[[i]]$authorB[j]
#     row = which(topicSims[[i]]$authorA[j] == author_key)
#     col = which(topicSims[[i]]$authorB[j] == author_key)
#     new_pub = coauthor_mats[[i+1]][row,col]
#     prior_pub = coauthor_mats[[i]][row,col]
#     topic_sim = topicSims[[i]]$authorsSim[j]
#     model_data = bind_rows(model_data, data.frame(authorA=rowAuth, #keep track of authors, critical for removing identity "co-authors"
#                                                   authorB=colAuth,
#                                                   new_publication=new_pub,
#                                                   prior_publication=prior_pub,
#                                                   topic_similarity=topic_sim))
#   }
# }
# 
# 
# # colnames(model_data) = c("new_publication",
# #                          "prior_publication",
# #                          "topic_similarity")
# model_data
# write.csv(model_data, file = "model_data.csv")
# 
# 
# nrow(model_data)
# model_data_diffAuthors <- model_data %>%
#   filter(authorA != authorB)
# 
# head(model_data_diffAuthors)
# glm(model_data_diffAuthors, aes(new_publication ~ prior_publication + topic_similarity, family="binomial"))
