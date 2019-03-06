library(tidyverse)
source('networkCentralityFunctions.R')
source('topicSpaceVectorProjectionFunctions.R')
source('NullHypothesisFunctions.R')

topic.df.50 = read_csv('topic_dist_fulltext_50.csv') %>% transform_authorAbbr()
topic.df.100 = read_csv('topic_dist_fulltext_100.csv') %>% transform_authorAbbr()
centrality_subset = read_csv('centrality_subset.csv') %>% select(-X1)

while(T){
  null.output = replicate(5, NullShuffleBundle(topic.df.50,centrality_subset,50))
  write.csv(null.output, file = paste0('H0_50/NullShuffleBundle50.', sample(1:1000000000, 1),'.csv'))
  
  null.output = replicate(5, NullShuffleBundle(topic.df.100,centrality_subset,100))
  write.csv(null.output, file = paste0('H0_100/NullShuffleBundle100.', sample(1:1000000000, 1),'.csv'))
  
}