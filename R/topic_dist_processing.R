### Script / functions for processing topic distributions over a set of papers


# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
}

# returns a vector X with multipliers [x1, ..., xn] for a given paper
# s.t. the paper's topic dist * X = global topic distribution over all papers
get_paper_global_comparison <- function(sample.paper.topics, global.topic.dist) {
  paper.topic.dist = sample.paper.topics %>%
    select(-title, -authors, -year) %>%
    gather(topic, topicMean, `1`:`20`)
  return(data.frame(topicMult = global.topic.dist$topic.means / paper.topic.dist$topicMean))
}




DATA = 'topic_dist_year.csv'
topic.df = read_csv(DATA)

global.means = get_avg_topic_dist(topic.df)

sample.paper.topics = topic.df[1,]
sample.paper.multiplier = get_paper_global_comparison(sample.paper.topics, global.means)
