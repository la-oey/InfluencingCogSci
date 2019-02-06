### Script / functions for processing topic distributions over a set of papers
library(tidyverse)
source('networkCentralityFunctions.R')

# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}

# returns a vector X with differences [x1, ..., xn] for a given paper
# s.t. the paper's topic dist + X = global topic distribution over all papers
get_paper_global_comparison <- function(sample.paper.topics, global.topic.dist) {
  paper.topic.dist = sample.paper.topics %>%
    select(-title, -authors, -year) %>%
    gather(topic, topicMean, `1`:`50`) # NB: this upper range needs to be set every time
  return(data.frame(topicDiff = global.topic.dist$topic.means - paper.topic.dist$topicMean))
}


get_author_rows <- function(author.lookup, df) {
  author.grep = as.vector(unlist(sapply(author.lookup, function(author.lookup){return(grep(author.lookup, df$authors, value = TRUE))})))
  author.rows = df %>%
    filter(authors %in% author.grep)
  all.other.rows = anti_join(df, author.rows, by = c('authors'))
  sanity.check = dim(author.rows)[1] + dim(all.other.rows)[1] == dim(df)[1]
  print(paste("Sanity check for author extraction passed: ", sanity.check, sep = " "))
  return(list(author = author.rows, global = all.other.rows))
}




DATA_YEAR = 'topic_dist_year.csv'
DATA_50 = 'topic_dist_fulltext_50.csv'
DATA_100 = 'topic_dist_fulltext_100.csv'

# topic.df = read_csv(DATA_50)
topic.df = read_csv(DATA_100)
glimpse(topic.df)

global.means = get_avg_topic_dist(topic.df)

# Compare to one paper
sample.paper.topics = topic.df[1,]
sample.paper.difference.vector = get_paper_global_comparison(sample.paper.topics, global.means)

# Select an author's papers, all papers minus that author
tenenbaum.comparison = get_author_rows(c('Tenenbaum','Griffiths'), topic.df)
tenenbaum = tenenbaum.comparison$author
all.minus.tenenbaum = tenenbaum.comparison$global

get_author_rows('Michael', topic.df)

#plotting author topic dist by year

authorTopicDistByYear = function(thisAuthor, topic.df, ntopics){
  year_topics  = matrix(nrow = ntopics,ncol = length(unique(topic.df$year)) )
  for(i in 1:length(unique(topic.df$year))){
    comparison = topic.df %>% filter(year == unique(topic.df$year)[i]) %>% 
      get_author_rows(thisAuthor, .)
    year_topics[,i] = t(get_avg_topic_dist(comparison$author))  
  }
  year_topics = data.frame(year_topics)
  names(year_topics) = unique(topic.df$year)
  year_topics = year_topics %>% mutate(topic = factor(1:ntopics)) %>% gather( key = year, value = prob, -topic)
  return(year_topics)
}

thisAuthor = c('Tenenbaum', 'Griffiths')

topic.df %>% authorTopicDistByYear(thisAuthor,.,100) %>%
  ggplot(aes(x=year,y=prob,group = topic))+
  geom_smooth(se=F, aes(col = topic),size = .5, alpha = .5)


#plotting global topic dist by year
globalTopicDistByYear = function(topic.df,ntopics){
  year_topics  = matrix(nrow = ntopics,ncol = length(unique(topic.df$year)) )
  for(i in 1:length(unique(topic.df$year))){
    comparison = topic.df %>% filter(year == unique(topic.df$year)[i])
    year_topics[,i] = t(get_avg_topic_dist(comparison))  
  }
  year_topics = data.frame(year_topics)
  names(year_topics) = unique(topic.df$year)
  year_topics = year_topics %>% mutate(topic = factor(1:ntopics)) %>% gather( key = year, value = prob, -topic)
  return(year_topics)
}

topic.df %>% globalTopicDistByYear(.,100) %>%
  ggplot(aes(x=year,y=prob,group = topic))+
  geom_smooth(se=F, aes(col = topic), size = .5, alpha = .5)


#getting top 5 authors
author_net = getAuthorList(topic.df)

author_net %>%
  group_by(authorAbbr) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n),
         rank = dense_rank(desc(n))) %>%
  arrange(desc(prop)) %>%
  mutate(cumsum = cumsum(prop)) %>%
  filter(cumsum < .05) %>%
  ggplot(aes(x=reorder(authorAbbr, rank), y=prop)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#getting top 5 authors by centrality

centrality = writeAuthorNet(author_net)

num_papers = author_net %>%
  group_by(authorAbbr) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n),
         rank = dense_rank(desc(n)))

centrality %>% 
  filter(CM == 'degree') %>% 
  inner_join(num_papers,., by = c('authorAbbr' = 'label')) %>%
  mutate(normalized_measure = measure/n) %>%
  arrange(desc(measure)) %>%
  select(-id,-CM) %>%
  gather(key = quantity, value = value, -authorAbbr,-rank)%>%
  ggplot(aes(x=reorder(factor(authorAbbr),rank),y=value))+
    geom_point(size=.1)+
    facet_wrap(~quantity)


data.final = data.frame()
for (year in levels(as.factor(tenenbaum$year))) {
  data = tenenbaum[tenenbaum$year == year,]
  avg.year = get_avg_topic_dist(data)
  # glimpse(avg.year)
  rbind(data.final, avg.year)
}
glimpse(data.final)

