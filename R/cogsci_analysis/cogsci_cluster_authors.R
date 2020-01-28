setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(pbapply)
library(stringr)
library(cluster)
topic.df <- read_csv("cogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors)) %>% #manually fixing incorrect author naming
  dplyr::select(-X1)

allAuthors <- topic.df %>%
  dplyr::select(authors) %>%
  distinct() %>%
  dplyr::arrange(authors) #data frame of authors in alphabetical order
(len.authors = nrow(allAuthors))
### FUNCTIONS ###

# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    dplyr::select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}

selectTopicByAuthor <- function(author){
  topic.df %>%
    filter(authors == author) %>%
    get_avg_topic_dist()
}

#################

selectTopicByAuthor("E Vul")

authorTopics.df <- allAuthors %>%
  mutate(authorTopics = pbmapply(selectTopicByAuthor, authors)) %>%
  unnest(authorTopics) %>%
  mutate(topic = paste0("topic",str_pad(rep(1:100, len.authors), 3, pad = "0"))) %>%
  spread(topic, authorTopics) %>%
  na.omit()

start <- Sys.time()
set.seed(1000)
authorClusters = list()
for(i in 1:30){
  temp.authorCluster <- kmeans(authorTopics.df[, 2:101], i, nstart = 100)
  authorClusters[[i]] = temp.authorCluster
  print(Sys.time()-start)
}

authorClusters[[30]]
evalK = data.frame()
for(i in 1:30){
  evalK <- bind_rows(evalK,
                        data.frame(k=i,
                                   betweenss=authorClusters[[i]]$betweenss,
                                   tot.withinss=authorClusters[[i]]$tot.withinss,
                                   totss=authorClusters[[i]]$totss))
}
evalK %>%
  mutate(b_totss = betweenss/totss) %>%
  dplyr::arrange(desc(b_totss))



