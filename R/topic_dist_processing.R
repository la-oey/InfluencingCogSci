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
  #print(paste("Sanity check for author extraction passed: ", sanity.check, sep = " "))
  return(list(author = author.rows, global = all.other.rows))
}




DATA_YEAR = 'topic_dist_year.csv'
DATA_50 = 'topic_dist_fulltext_50.csv'
DATA_100 = 'topic_dist_fulltext_100.csv'

topic.df = read_csv(DATA_50)
topic.df = read_csv(DATA_100)
glimpse(topic.df)

topic.df = transform_authorAbbr(topic.df)


global.means = get_avg_topic_dist(topic.df)

# Compare to one paper
sample.paper.topics = topic.df[1,]
sample.paper.difference.vector = get_paper_global_comparison(sample.paper.topics, global.means)

# Select an author's papers, all papers minus that author
tenenbaum.comparison = get_author_rows(c("J Tenenbaum", "T Griffiths"), topic.df)
tenenbaum = tenenbaum.comparison$author
all.minus.tenenbaum = tenenbaum.comparison$global


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

thisAuthor = c('Tenenbaum')

topic.df %>% authorTopicDistByYear(thisAuthor,.,50) %>%
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

topic.df %>% globalTopicDistByYear(.,50) %>%
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
  inner_join(num_papers, ., by = c('authorAbbr' = 'label')) %>%
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



##############################
### EXPERIMENTING WITH DVs ###
##############################
#thisAuthor = c('Tenenbaum')

get_projection_angle = function(vec.a, vec.b) {
  # get projection angles
  theta = acos( sum(vec.a * vec.b) / ( sqrt(sum(vec.a * vec.a)) * sqrt(sum(vec.b * vec.b)) ) )
  return(theta)
}


global_topic = topic.df %>% 
  globalTopicDistByYear(., 50) %>% # long format with topic, year, probability
  group_by(topic) %>% 
  mutate(diff = lead(prob) - prob) # subtract previous year's value from each value (NA when no value)


tb_topic = topic.df %>% 
  authorTopicDistByYear(thisAuthor, ., 50) %>% 
  group_by(topic) %>% 
  mutate(diff = lead(prob) - prob)

gr_topic = topic.df %>% 
  authorTopicDistByYear(c('Griffiths'), ., 50) %>% 
  group_by(topic) %>% 
  mutate(diff = lead(prob) - prob)

# get the vector we project onto
combined_vec = cbind(global_topic, gr_topic) ;glimpse(combined_vec)
combined_vec = combined_vec %>%
  mutate(target_vec = prob - prob1)

# clean up
glimpse(combined_vec)
combined_vec = combined_vec %>%
  filter(!is.na(diff) & !is.nan(diff1) & !is.nan(target_vec))


projection_angles = combined_vec %>%
  group_by(year) %>%
  summarize(proj_angle_gr = get_projection_angle(diff1, target_vec),
            proj_angle_gl = get_projection_angle(diff, target_vec))


glimpse(projection_angles)
projection_angles %>%
  ggplot(aes(x = year)) +
  geom_point(aes(y = proj_angle_gr), color = "blue") +
  geom_point(aes(y = proj_angle_gl), color = "red") +
  ylim(1.2, 2.2)


authorsInfluence = function(topic.df, N= 50, thisAuthor){
      global_topic =  topic.df %>% 
                    globalTopicDistByYear(., N) %>% # long format with topic, year, probability
                    group_by(topic) %>% 
                    mutate(diff = lead(prob) - prob)
      
      author_topic = topic.df %>% 
        authorTopicDistByYear(thisAuthor, ., N) %>% 
        group_by(topic) %>% 
        mutate(diff = lead(prob) - prob)
      
      combined_vec = cbind(global_topic, author_topic) %>%
        mutate(target_vec = prob - prob1)%>%
        filter(!is.na(diff) & !is.nan(diff1) & !is.nan(target_vec))
      
      projection_angles = combined_vec %>%
        group_by(year) %>%
        summarize(proj_author = get_projection_angle(diff1, target_vec)*sqrt(sum(diff1^2)),
                  proj_global = get_projection_angle(diff, target_vec)*sqrt(sum(diff^2)))%>%
        ungroup()%>%
        summarise(global_influence_author = mean(proj_author), author_influence_global = mean(proj_global))
     print(thisAuthor)
       return(c(author = thisAuthor, 
                        global_influence_author = projection_angles$global_influence_author, 
                        author_influence_global = projection_angles$author_influence_global))
}


author_influence = centralityQuantile(centrality, q= c(.90,1)) %>% 
  mutate(authorAbbr = as.character(label))%>%
  mutate(authorLast = strsplit(authorAbbr, ' ')) %>% 
  .$authorLast %>% 
  sapply(function(x){return(x[2])}) %>% 
  unique()%>%
  mapply(authorsInfluence,.,MoreArgs = list(topic.df = topic.df, N=100))


author_influence = data.frame(t(author_influence))

names(author_influence) = c("author", "global_influence_author", "author_influence_global")

ggplot(author_influence_centrality)+
  geom_histogram(aes(x=author_influence_global), bins = 50, fill = 'red', alpha = .5)+
  geom_histogram(aes(x=global_influence_author), bins = 50,fill = 'blue', alpha = .5)

ggplot(author_influence_centrality)+
  geom_histogram(aes(x=measure), bins = 50, fill = 'red', alpha = .5)


ggplot(author_influence_centrality)+
  geom_point(aes(x=log10(measure), y = author_influence_global))



author_influence_centrality = author_influence

cent = centralityQuantile(centrality,q= c(.90,1))
cent$authorLast =  centralityQuantile(centrality,q= c(.90,1)) %>% 
  mutate(authorAbbr = as.character(label))%>%
  mutate(authorLast = strsplit(authorAbbr, ' ')) %>%  
  .$authorLast %>% 
  sapply(function(x){return(x[2])})

author_influence %>%
  filter(global_influence_author != 'NaN', author_influence_global != 'NaN')
  
author_influence_centrality = author_influence_centrality%>%
  inner_join(cent, by = c("author" = "authorLast"))%>%
  filter(global_influence_author != 'NaN', author_influence_global != 'NaN') 

author_influence_centrality = author_influence_centrality %>%
   mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
          global_influence_author = as.numeric(as.character(global_influence_author)))

 
cor.test(author_influence_centrality$global_influence_author,author_influence_centrality$author_influence_global)

cor.test(author_influence_centrality$global_influence_author,log(author_influence_centrality$measure))
 
cor.test(author_influence_centrality$author_influence_global,log(author_influence_centrality$measure)) 
          
<<<<<<< HEAD



 
=======
 
>>>>>>> 16504dc9ffec128d179cc1c4d07e6b1b2e06cc10
