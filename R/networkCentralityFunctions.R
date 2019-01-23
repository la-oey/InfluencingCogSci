getEdges = function(x){
  from = c()
  to = c()
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      if(i!=j && i>j){
        from=c(from, x[i])
        to = c(to,x[j])
      }
    }
  }
  return(data.frame(from=from,to=to))
}

edgeList = function(author_net,uniqueEdges = T){
  edges = data.frame(from = c(),to =c())
  for(k in unique(author_net$title)){
    temp = author_net %>% filter(title == k)
    if(length(temp$authorAbbr) == 1){
      edges_itt = data.frame(from = temp$authorAbbr,to = temp$authorAbbr)
    }
    else{
      edges_itt = getEdges(temp$authorAbbr)
    }
    edges = rbind(edges,edges_itt)
    print(paste0(which(unique(author_net$title) == k), ' out of ', length(unique(author_net$title))))
  }
  if(uniqueEdges && length(edges!=0)){
    edges = edges[!duplicated(apply(edges,1,function(x) paste(sort(x),collapse=''))),]
  }
  return(edges)
}


library(igraph)

writeAuthorNet = function(author_net){
  all_nodes = unique(author_net$authorAbbr) 
  all_edges = edgeList(author_net%>%select(title,authorAbbr)) #do not run this line with full author_net set it will take HOURS load csv 'all_edges.csv' instead
  
  all_nodes=data.frame(id = 1:length(all_nodes), label = all_nodes)
  
  all_edges = apply(all_edges, MARGIN = c(1,2), function(x){return(which(all_nodes[2] == x))})
  all_edges = data.frame(all_edges)
  
  
  #igraph
  #igraph has some functions for calculating centrality on network objects(?). Object for igraph created below.  
  
  graph_edges = c()
  
  for(i in 1:dim(all_edges)[1]){
    graph_edges = c(graph_edges, all_edges[i,])
  }
  graph_edges = as.numeric(graph_edges)
  graph = make_graph(graph_edges, directed = FALSE)
  
  
  #We can certainly use degree centrality, but need to carefully consider other measures because of the weird structure of the graph. igraph has other useful network analysis functions that might help us systematically trim the graph to a simple and fully connected one.
  #centrality (degree, close, between)
  degree_centrality = centr_degree(graph)
  closeness_centrality = centr_clo(graph)
  betweenness_centrality = centr_betw(graph)
  
  all_centrality = data.frame(id = rep(all_nodes$id,3), 
                              label = rep(all_nodes$label,3), 
                              measure = c(degree_centrality$res, closeness_centrality$res, betweenness_centrality$res), 
                              CM = c(rep('degree', length(all_nodes$id)), 
                                     rep('close', length(all_nodes$id)), 
                                     rep('between', length(all_nodes$id))))
  
  return(all_centrality)
}

centralityQuantile = function(dat, q = c(.95,1), cent='degree'){
  q1=q[1]
  q2=q[2]
  
  dat = dat %>% 
    filter(CM == cent) %>%
    arrange(desc(measure)) 
  
  nAuthors = length(dat$label)
  
  dat %>% 
    mutate(index = 1:nAuthors, pct = (1 - index/nAuthors)) %>%
    filter(q1<=pct, pct<q2)%>%
    select(label,CM, measure,pct) 
}
