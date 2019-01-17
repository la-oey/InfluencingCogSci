### Script / functions for processing topic distributions over a set of papers


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

# TODO similar comparisons for all papers with a particular author, all papers for a particular year (minus those from an author)





