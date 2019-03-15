

NullShuffleBundle = function(topic.df, centrality_subset, N ,
                                  author.matrix , year.matrix,
                                  global_topic){
  null.topic.df = topic.df %>%
    mutate(authors = sample(authors,length(authors), F))
  
  author_influence.null = centrality_subset%>%
    pull(label)%>%
    unique()%>%
    mapply(authorsInfluence,.,MoreArgs = list(topic.df = null.topic.df, N = N,
                                              author.matrix = author.matrix, year.matrix = year.matrix,
                                              global_topic = global_topic)) 
  
  author_influence.null = data.frame(t(author_influence.null))%>%
    inner_join(centrality_subset, by = c("author" = "label"))%>%
    filter(global_influence_author != 'NaN', author_influence_global != 'NaN')%>%
    mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
           global_influence_author = as.numeric(as.character(global_influence_author)))%>%
    do(cor.global.author = cor.test(.$global_influence_author,.$author_influence_global),
       cor.global.influence.author.centrality = cor.test(.$global_influence_author,log(.$measure)),
       cor.author.influence.global.centrality = cor.test(.$author_influence_global,log(.$measure)))
  
  cor.GA = author_influence.null$cor.global.author[[1]]$estimate
  cor.GIAC = author_influence.null$cor.global.influence.author.centrality[[1]]$estimate
  cor.AIGC = author_influence.null$cor.author.influence.global.centrality[[1]]$estimate
  
  return(c(cor.GA = cor.GA, cor.GIAC = cor.GIAC, cor.AIGC=cor.AIGC))
}

concatAuthors = function(author){
  authors = character()
  for(i in 1:length(author)){
    if(length(author)>1 && i == 1){
      authors = paste0(authors, author[i], ', ')
    }
    else if(i == length(author)){
      authors = paste0(authors, author[i]) 
    }
    else{
    authors = paste0(authors, author[i], ', ')
    }
  }
  return(authors)
}

NullShuffleAuthors = function(topic.df, N.topics){
  author_net = getAuthorList(topic.df)
  
  author_net.null = author_net %>% 
    mutate(authorAbbr = sample(authorAbbr,length(authorAbbr),F))%>%
    group_by(title,year)%>%
    mutate(authors = rep(concatAuthors(authorAbbr),length(authorAbbr)))

  centrality = writeAuthorNet(author_net)
  centrality_subset = centralityQuantile(centrality,q=c(0,1))
  
  author_net.null = author_net.null %>% select(title,year,authors)
  
  null.topic.df = topic.df %>% 
    select(-authors)%>%
    inner_join(author_net.null)
  
  author_influence.null = centrality_subset %>%
    pull(label)%>%
    unique()%>%
    mapply(authorsInfluence,.,MoreArgs = list(topic.df = null.topic.df, N=N.topics)) 
  
  author_influence.null = data.frame(t(author_influence.50))%>%
    inner_join(centrality_subset, by = c("author" = "label"))%>%
    filter(global_influence_author != 'NaN', author_influence_global != 'NaN')%>%
    mutate(author_influence_global = as.numeric(as.character(author_influence_global)),
           global_influence_author = as.numeric(as.character(global_influence_author)))%>%
    do(cor.global.author = cor.test(.$global_influence_author,.$author_influence_global),
       cor.global.influence.author.centrality = cor.test(.$global_influence_author,log(.$measure)),
       cor.author.influence.global.centrality = cor.test(.$author_influence_global,log(.$measure)))
  
  cor.GA = author_influence.null$cor.global.author[[1]]$estimate
  cor.GIAC = author_influence.null$cor.global.influence.author.centrality[[1]]$estimate
  cor.AIGC = author_influence.null$cor.author.influence.global.centrality[[1]]$estimate
  
  return(c(cor.GA = cor.GA, cor.GIAC = cor.GIAC, cor.AIGC=cor.AIGC))
}