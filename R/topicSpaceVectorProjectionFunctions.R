### Script / functions for processing topic distributions over a set of papers

# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
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

get_projection_angle = function(vec.a, vec.b) {
  # get projection angles
  theta = acos( sum(vec.a * vec.b) / ( sqrt(sum(vec.a * vec.a)) * sqrt(sum(vec.b * vec.b)) ) )
  return(theta)
}


authorsInfluence = function(topic.df, N= 50, thisAuthor){
  thisAuthor=as.character(thisAuthor)
  
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
  #print(thisAuthor)
  return(c(author = thisAuthor, 
           global_influence_author = projection_angles$global_influence_author, 
           author_influence_global = projection_angles$author_influence_global))
}
