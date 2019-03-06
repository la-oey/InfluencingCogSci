library(tidyverse)
source('networkCentralityFunctions.R')
source('topicSpaceVectorProjectionFunctions.R')

#getting topic distributions
DATA_50 = 'topic_dist_fulltext_50.csv'
DATA_100 = 'topic_dist_fulltext_100.csv'

topic.df.50 = read_csv(DATA_50) %>% transform_authorAbbr()
topic.df.100 = read_csv(DATA_100) %>% transform_authorAbbr()

#getting network and centrality
author_net = getAuthorList(topic.df)
centrality = writeAuthorNet(author_net)

centrality_subset = centralityQuantile(centrality, q= c(0,1))

#get author influence for 50 and 10 topic df
author_influence.50 = centrality_subset%>%
  pull(label)%>%
  unique()%>%
  mapply(authorsInfluence,.,MoreArgs = list(topic.df = topic.df.50, N=50)) 

author_influence.50 = data.frame(t(author_influence.50))%>%
  inner_join(centrality_subset, by = c("author" = "label"))%>%
  filter(global_influence_author != 'NaN', author_influence_global != 'NaN')%>%
  mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
         global_influence_author = as.numeric(as.character(global_influence_author)))%>%
  do(cor.global.author = cor.test(.$global_influence_author,.$author_influence_global),
            cor.global.influence.author.centrality = cor.test(.$global_influence_author,log(.$measure)),
            cor.author.influence.global.centrality = cor.test(.$author_influence_global,log(.$measure)))

author_influence.100 =  centrality_subset%>%
  pull(label)%>%
  unique()%>%
  mapply(authorsInfluence,.,MoreArgs = list(topic.df = topic.df.100, N=100)) 

author_influence.100 = data.frame(t(author_influence.100))%>%
  inner_join(centrality_subset, by = c("author" = "label"))%>%
  filter(global_influence_author != 'NaN', author_influence_global != 'NaN')%>%
  mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
         global_influence_author = as.numeric(as.character(global_influence_author))) %>%
  do(cor.global.author = cor.test(.$global_influence_author,.$author_influence_global),
     cor.global.influence.author.centrality = cor.test(.$global_influence_author,log(.$measure)),
     cor.author.influence.global.centrality = cor.test(.$author_influence_global,log(.$measure)))
