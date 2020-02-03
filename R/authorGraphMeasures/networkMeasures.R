library(tidyverse)
library(igraph)



#expected edges/papers by year
cogsci_byAuthor = read_csv("cogsci_vss_nips/cogsci_byAuthor.csv")
vss_byAuthor = read_csv("cogsci_vss_nips/vss_byAuthor.csv")

cogsci_paper_edges_byYear = cogsci_byAuthor %>%
  group_by(year, title)%>%
  summarise(N=n()) %>%
  mutate(num_paper_edges = N*(N-1)/2) %>%
  group_by(year)%>%
  summarise(num_paper_edges = sum(num_paper_edges))

vss_paper_edges_byYear = vss_byAuthor %>%
  group_by(year, title)%>%
  summarise(N=n()) %>%
  mutate(num_paper_edges = N*(N-1)/2) %>%
  group_by(year)%>%
  summarise(num_paper_edges = sum(num_paper_edges))


getNetworkMeasures = function(all_nodes, all_edges, edge_expect, y){
  #allAuthorsGlobal <- inner_join(all_nodes, Topics, by = c("label" = "author", "year" ="year"))  %>% filter(year == y)
  year_nodes <- all_nodes %>% filter( year == y) %>% select(id, label) %>% data.frame()
  year_edges <- all_edges %>% filter( year == y) %>% select(from,to) %>% data.frame()
  year_edge_expect <- edge_expect %>% filter(year == y) %>% pull(num_paper_edges)
  
  print(y)
  graph_edges = c()
  
  for(i in 1:nrow(year_edges)){
    graph_edges = c(graph_edges, year_edges[i,])
  }
  graph_edges = as.numeric(graph_edges)
  graph = make_graph(graph_edges, directed = FALSE)
  #save(graph, file = paste0("cogsci_graphs/graph_cogsci_",y,".RData"))
  
  #topic_assortative = assortativity(graph, topicSim, directed = FALSE)
  
  full_trans = transitivity(graph)
  full_edge_density = edge_density(graph)
  full_edge_density_adjusted = nrow(year_edges)/year_edge_expect
  
  full_deg = centr_degree(graph)
  full_eigen = centr_eigen(graph)
  
  full_deg_assortative = assortativity(graph, full_deg$res, directed = FALSE)
  full_eigen_assortative = assortativity(graph, full_eigen$vector, directed = FALSE)
  
  #full_topic_assortative = assortativity(graph,as.numeric(allAuthorsGlobal$authorsSim), directed = FALSE )
  
  full_deg_centralization = full_deg$centralization
  full_eigen_centralization = full_eigen$centralization
  
  full_num_edge = length(E(graph))
  full_num_vertex = length(V(graph))
  
  
  list_of_subgraphs <- decompose.graph(graph)
  subgraph_size = list_of_subgraphs %>% lapply( function(x){length(V(x))}) %>% unlist() 
  max_subgraph = list_of_subgraphs[[which(subgraph_size == max(subgraph_size))]]
  max_subgraph_size= length(V(max_subgraph))
  
  
  subgraph_nodes <- year_nodes %>% filter(id %in% V(max_subgraph))
  subgraph_edges <- E(max_subgraph)
  #subgraphSim <- allAuthorsGlobal %>% filter(label %in% subgraph_nodes$label)
  num_islands = length(subgraph_size)
  
  trans = transitivity(max_subgraph)
  edge_density = edge_density(max_subgraph)
  num_edge = length(E(max_subgraph))
  num_vertex = length(V(max_subgraph))
  
  deg = centr_degree(max_subgraph)
  eigen = centr_eigen(max_subgraph)
  betw = centr_betw(max_subgraph)
  close = centr_clo(max_subgraph)
  
  #topic_assortative = assortativity(max_subgraph,as.numeric(subgraphSim$authorsSim), directed = FALSE )
  
  deg_assortative = assortativity(max_subgraph, deg$res, directed = FALSE)
  eigen_assortative = assortativity(max_subgraph, eigen$vector, directed = FALSE)
  betw_assortative = assortativity(max_subgraph, betw$res, directed = FALSE)
  close_assortative = assortativity(max_subgraph, close$res, directed = FALSE)
  
  deg_centralization = deg$centralization
  eigen_centralization = eigen$centralization
  betw_centralization = betw$centralization
  close_centralization = close$centralization
  
  return(c(
    full_trans = full_trans,
    full_edge_density = full_edge_density,
    full_edge_density_adjusted =full_edge_density_adjusted,
    full_num_edge = full_num_edge,
    full_num_vertex = full_num_vertex,
    full_deg_centralization = full_deg_centralization,
    full_deg_assortative = full_deg_assortative,
    full_eigen_centralization = full_eigen_centralization,
    full_eigen_assortative = full_eigen_assortative,
    #full_topic_assortative = full_topic_assortative, 
    num_islands = num_islands,
    trans = trans, 
    edge_density = edge_density,
    num_edge = num_edge,
    num_vertex = num_vertex,
    deg_centralization = deg_centralization,
    eigen_centralization = eigen_centralization,
    betw_centralization = betw_centralization,
    close_centralization = close_centralization,
    deg_assortative = deg_assortative,
    eigen_assortative = eigen_assortative,
    betw_assortative = betw_assortative,
    close_assortative = close_assortative,
    #topic_assortative = topic_assortative,
    max_subgraph_size=max_subgraph_size,
    year =y
  ))
}

all_nodes_vss = read_csv("all_nodes_vss.csv") %>% select(-X1)
all_edges_vss = read_csv("all_edges_vss.csv") %>% select(-X1)

all_nodes_cogsci = read_csv("all_nodes_cogsci.csv")
all_edges_cogsci = read_csv("all_edges_cogsci.csv")


vss_years = all_nodes_vss %>% arrange(year) %>% pull(year) %>% unique()
vss_measures = mapply(getNetworkMeasures, vss_years, MoreArgs = list(all_nodes = all_nodes_vss, all_edges = all_edges_vss, edge_expect = vss_paper_edges_byYear))
vss_measures = data.frame(t(vss_measures)) %>% mutate(year = as.integer(year)) %>% mutate(name = "VSS")


cogsci_years = all_nodes_cogsci%>% arrange(year) %>% pull(year) %>% unique()
cogsci_measures = mapply(getNetworkMeasures, cogsci_years, MoreArgs = list( all_nodes = all_nodes_cogsci, all_edges = all_edges_cogsci,edge_expect = cogsci_paper_edges_byYear))
cogsci_measures =  data.frame(t(cogsci_measures)) %>% mutate(year = as.integer(year)) %>% mutate(name = "CogSci")

vss_cogsci_graph_measures = rbind(vss_measures, cogsci_measures) 

#edge_density number of edges over total possible number of edges
vss_cogsci_graph_measures %>% select(full_edge_density_adjusted , year, name) %>%
  ggplot(aes(x=year, y= full_edge_density_adjusted , color = name))+
  geom_point()+
  geom_line(size = 2)+
  scale_color_manual(values = c("#027880", "#E0772F"))+  theme_minimal()+
  scale_x_continuous(labels = 2000:2019, breaks = 2000:2019)+
  labs(x = "Year", y="Adjusted Edge Density", col = "")+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .4))



#transitivity -- overall probability for the network to have adjacent nodes interconnected, thus revealing the existence of tightly connected communities (or clusters, subgroups, cliques)
vss_cogsci_graph_measures%>% select(full_trans,trans, year,name) %>%
  ggplot(aes(linetype = name))+
  geom_point(aes(x=year, y= full_trans, col = name))+
  geom_line(aes(x=year, y= full_trans, col = name), linetype = 1, size = 2)+
  #geom_point(aes(x=year, y= trans,col =name))+
  #geom_line(aes(x=year, y= trans, col= name), linetype = 2)+
  scale_x_continuous(labels = 2000:2019, breaks = 2000:2019)+
  scale_color_manual(values = c("#027880", "#E0772F"))+  theme_minimal()+
  #ylim(0,1)+
  labs(x = "Year", y="Transitivity", col = "")+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .4))


#max_subgraph - the ratio of the number of verticies in the maximum subgraph to the total size of the graph
## I think this plot shows that the community at cogsci is becoming more of an individual unit. 
vss_cogsci_graph_measures %>% mutate(num_vertex = num_vertex/full_num_vertex)%>%
  ggplot(aes(x=year, y= num_vertex, color = name))+
  geom_point()+
  geom_line(size = 2)+
  scale_x_continuous(labels = 2000:2019, breaks = 2000:2019)+
  scale_color_manual(values = c("#027880", "#E0772F"))+  theme_minimal()+
  #ylim(0,1)+
  labs(x = "Year", y="Maximum Subgraph", col = "")+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .4))

