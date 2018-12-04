
#Need to run lauren's script before running this one.

#helper functions
getEdges = function(x){
  from = c()
  to = c()
  for(i in 1: length(x)){
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
    
    if(uniqueEdges && length(edges)!=0){
      edges_itt = removeDuplicates(edges_itt,edges) #can add this line to get unique edges ignoring ordering of pair
    }
    edges = rbind(edges,edges_itt)
  }
  return(edges)
}

#function that might be useful if we want unique edges
removeDuplicates = function(x,y){
  del = c()
  for(j in 1:dim(y)[1]){
    for(i in 1:dim(x)[1]){
      if(x$from[i] == y$to[j] && x$to[i] == y$from[j]){
        del = c(del, i)
      }
      if(x$from[i] == y$from[j] && x$to[i] == y$to[j]){
        del = c(del, i)
      }
    }
  }
  if(length(del!=0)){
    x = x[-del,]
  }
  return(x)
} 

##Author network data
#Some authors are not connected to the main part of the network. This happens when they have one publication/other reasons. This is bad for measuring certain kinds of centrality because they depend on paths and loops through the network. Taking the most published authors is expedient, but a more sophisticated means should be considered.  Additionally our graph is not simple because there are multiple connections between authors, if we select unique connections the network will be more amenable (including the removeDuplicates function in getEdges will do this). 

#includes all authors
author_net = byAuthor %>% select(title,authorAbbr) %>% unique()

#removes authors with <n publications
n.pubs=10
author_net = author_net %>% 
  group_by(authorAbbr) %>% 
  summarise(count = n()) %>% 
  filter(count<=n.pubs) %>% 
  anti_join(author_net,.)

#histogram of number of publications
author_net %>% 
  group_by(authorAbbr) %>% 
  summarise(count = n()) %>% ungroup() %>% 
  ggplot(aes(x=reorder(authorAbbr,count),y=count)) + geom_boxplot(stat = 'identity')

##Author Network
#nodes and edges

#Creating an edge list to be used with igraph (for calculating centrality) and visNetwork (fun visualization compatable with shiny for writing interactive app).

#visNetwork version of nodes and edges
nodes = unique(author_net$authorAbbr) 
edges = edgeList(author_net)

write.csv(nodes, 'nodes.csv')
write.csv(edges, 'edges.csv')


#visNetwork interactive graph
#This can be used with shiny. Interactive properties are cool. 
#For example can add entry of author and have the app focus on that node. 
#Can also add coloring to the graph based on centrality measures. 

nodes=data.frame(id = 1:length(nodes), label = nodes)

edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
edges = data.frame(edges)

library(visNetwork)
visNetwork(nodes, edges, width = "100%")



#igraph
#igraph has some functions for calculating centrality on network objects(?). Object for igraph created below.  

library(igraph)
graph_edges = c()

for(i in 1:dim(edges)[1]){
  graph_edges = c(graph_edges, edges[i,])
}
graph_edges = as.numeric(graph_edges)
graph = make_graph(graph_edges, directed = FALSE)


#We can certainly use degree centrality, but need to carefully consider other measures because of the weird structure of the graph. igraph has other useful network analysis functions that might help us systematically trim the graph to a simple and fully connected one.
#centrality (degree, close, between)
degree_centrality = centr_degree(graph)
closeness_centrality = centr_clo(graph)
betweenness_centrality = centr_betw(graph)

centrality = data.frame(nodes = rep(nodes$id,3), 
                        label = rep(nodes$label,3), 
                        measure = c(degree_centrality$res, closeness_centrality$res, betweenness_centrality$res), 
                        CM = c(rep('degree', length(nodes$id)), 
                               rep('close', length(nodes$id)), 
                               rep('between', length(nodes$id))))

#Space I was using to explore centrality.

top5 = centrality %>% 
  group_by(CM) %>% 
  arrange(desc(measure)) %>% 
  top_n(10,measure) %>% 
  select(label,CM) %>% 
  mutate(position = 1:10)# %>% ungroup()%>% spread(CM,label)


wide_centrality = centrality%>% spread(CM,measure)

#order nodes by centrality measure
CM_test = c('degree','close','between')
nodes = data.frame(label = centrality %>% filter(CM == CM_test[1]) %>% arrange(measure) %>% pull(label))

