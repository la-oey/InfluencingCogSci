library(tidyverse)
source('networkCentralityFunctions.R')
source('topicSpaceVectorProjectionFunctions.R')
source('NullHypothesisFunctions.R')

topic.df.50 = read_csv('topic_dist_fulltext_50.csv') %>% transform_authorAbbr()
topic.df.100 = read_csv('topic_dist_fulltext_100.csv') %>% transform_authorAbbr()
centrality_subset = read_csv('centrality_subset.csv') %>% select(-X1)
load('yearMatrix.Rdata')
load('AuthorMatrix.Rdata')


global_topic.50 =  topic.df.50 %>% 
  globalTopicDistByYear(., 50) %>% # long format with topic, year, probability (global average of each topic by year)
  group_by(topic) %>% 
  mutate(diff = lead(prob,3) - prob)

global_topic.100 =  topic.df.100 %>% 
  globalTopicDistByYear(., 100) %>% # long format with topic, year, probability (global average of each topic by year)
  group_by(topic) %>% 
  mutate(diff = lead(prob,3) - prob)

while(T){
  null.output = replicate(15, NullShuffleBundle(topic.df.50,centrality_subset,50, author.mat, year.mat, global_topic.50))
  write.csv(null.output, file = paste0('H0_50/NullShuffleBundle50.', sample(1:1000000000, 1),'.csv'))
  
  null.output = replicate(15, NullShuffleBundle(topic.df.100,centrality_subset,100, year.mat,author.mat, global_topic.100))
  write.csv(null.output, file = paste0('H0_100/NullShuffleBundle100.', sample(1:1000000000, 1),'.csv'))
  
}