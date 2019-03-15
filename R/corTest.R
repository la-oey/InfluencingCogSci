library(tidyverse)
source('networkCentralityFunctions.R')
source('topicSpaceVectorProjectionFunctions.R')

#getting topic distributions
DATA_50 = 'topic_dist_fulltext_50.csv'
DATA_100 = 'topic_dist_fulltext_100.csv'

topic.df.50 = read_csv(DATA_50) %>% transform_authorAbbr()
topic.df.100 = read_csv(DATA_100) %>% transform_authorAbbr()

#getting network and centrality
author_net = getAuthorList(topic.df.50)
# remove single author variables

#centrality = writeAuthorNet(author_net_filtered)
centrality = writeAuthorNet(author_net)

centrality_subset = centralityQuantile(centrality, q = c(0,1))

author_net_filtered = author_net %>%
  group_by(authorAbbr) %>% # NB: authorAbbr gets populated elsewhere
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup()

centrality_subset = centrality_subset %>% filter(label %in% author_net_filtered$authorAbbr)
# process topic.df.* for more efficient handling in code below

years = topic.df.50 %>%
  pull(year) %>%
  unique() %>%
  sort()

authors = centrality_subset %>%
  pull(label) %>%
  unique()

papers = topic.df.50$title

library(Matrix)

# Sparse matrix of papers x authors
author.mat = Matrix(0, 
                    nrow = length(papers),
                    ncol = length(authors),
                    dimnames = list(papers, as.character(authors)))

for (col in seq(1:dim(author.mat)[2])) { # This takes 3 mins
  author.lookup = colnames(author.mat)[col]
  author.grep = as.vector(unlist(sapply(author.lookup, function(author.lookup){return(grep(author.lookup, topic.df.50$authors, value = TRUE))})))
  author.rows = topic.df.50 %>%
    filter(authors %in% author.grep) %>%
    select(title)
  if (sum(rownames(author.mat) %in% author.rows$title) > 0) {
    author.mat[rownames(author.mat) %in% author.rows$title, colnames(author.mat)[col]] = 1
  }
}

save(author.mat, file = 'AuthorMatrix.Rdata')

# validating
rownames(author.mat)[1]
colnames(author.mat)[1]

# Sparse matrix of papers x years
year.mat = Matrix(0,
                  nrow = length(papers),
                  ncol = length(years),
                  dimnames = list(papers, as.character(years)))

for (col in seq(1:dim(year.mat)[2])) { # This takes ~1s
  year.lookup = colnames(year.mat)[col]
  year.rows = topic.df.50 %>%
    filter(as.character(year) == year.lookup) %>%
    select(title)
  year.mat[rownames(year.mat) %in% year.rows$title, colnames(year.mat)[col]] = 1
}

save(year.mat, file = 'yearMatrix.Rdata')
# Testing
thisAuthor = colnames(author.mat)[1]
year.sample = year.mat[,19]
auth.sample = author.mat[,colnames(author.mat) == thisAuthor]
tmp_years_auth = topic.df.50[year.sample & auth.sample,]

# topic distribution for each paper for an author in a given year
tmp_years_auth = topic.df.50[year.mat[,19] & author.mat[,colnames(author.mat) == thisAuthor],]



#get author influence for 50 and 100 topic df

# Testing
# author_influence.50 = centrality_subset$label[1:300] %>% # takes < 1min to run but might not be fast enough
#   mapply(authorsInfluence, ., MoreArgs = list(topic.df = topic.df.50, N = 50, author.matrix = author.mat, year.matrix = year.mat))

global_topic.50 =  topic.df.50 %>% 
  globalTopicDistByYear(., 50) %>% # long format with topic, year, probability (global average of each topic by year)
  group_by(topic) %>% 
  mutate(diff = lead(prob,3) - prob)

author_influence.50 = centrality_subset %>%
  pull(label) %>%
  unique() %>%
  # mapply(authorsInfluence, ., MoreArgs = list(topic.df = topic.df.50, N = 50)) #take 23 min to run with full centrality vector
  mapply(authorsInfluence, ., MoreArgs = list(topic.df = topic.df.50, N = 50,
                                              author.matrix = author.mat, year.matrix = year.mat,
                                              global_topic = global_topic.50))


author_influence.50 = data.frame(t(author_influence.50))%>%
  inner_join(centrality_subset, by = c("author" = "label"))%>%
  filter(global_influence_author != 'NaN', author_influence_global != 'NaN')%>%
  mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
         global_influence_author = as.numeric(as.character(global_influence_author)))%>%
  do(cor.global.author = cor.test(.$global_influence_author,.$author_influence_global),
            cor.global.influence.author.centrality = cor.test(.$global_influence_author,log(.$measure)),
            cor.author.influence.global.centrality = cor.test(.$author_influence_global,log(.$measure)))

global_topic.100 =  topic.df.100 %>% 
  globalTopicDistByYear(., 100) %>% # long format with topic, year, probability (global average of each topic by year)
  group_by(topic) %>% 
  mutate(diff = lead(prob,3) - prob)


author_influence.100 =  centrality_subset%>%
  pull(label)%>%
  unique()%>%
  mapply(authorsInfluence,.,MoreArgs = list(topic.df = topic.df.100, N=100, author.matrix = author.mat, year.matrix = year.mat,
                                            global_topic = global_topic.100)) 

author_influence.100 = data.frame(t(author_influence.100))%>%
  inner_join(centrality_subset, by = c("author" = "label"))%>%
  filter(global_influence_author != 'NaN', author_influence_global != 'NaN')%>%
  mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
         global_influence_author = as.numeric(as.character(global_influence_author))) %>%
  do(cor.global.author = cor.test(.$global_influence_author,.$author_influence_global),
     cor.global.influence.author.centrality = cor.test(.$global_influence_author,log(.$measure)),
     cor.author.influence.global.centrality = cor.test(.$author_influence_global,log(.$measure)))
