### Script / functions for processing topic distributions over a set of papers
library(tidyverse)
source('networkCentralityFunctions.R')

# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
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
  author.grep = grep(author.lookup, df$authors, value = TRUE)
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
tenenbaum.comparison = get_author_rows('Tenenbaum', topic.df)
tenenbaum = tenenbaum.comparison$author
all.minus.tenenbaum = tenenbaum.comparison$global


#getting top 5 authors
author_net = getAuthorList(topic.df)



