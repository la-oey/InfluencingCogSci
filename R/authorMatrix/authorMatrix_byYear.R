library(dplyr)
library(tidyr)

getAuthorMatrixByYear = function(byAuthor, journal){
  author_net = byAuthor %>% select(title,authorAbbr,year) %>% unique()
  for(j in unique(author_net$year)){
    author_net_year = author_net %>% filter(year == j) %>% select(title, authorAbbr)
    nodes= unique(author_net$authorAbbr)
    author_mat_year = matrix(rep(0,length(nodes)*length(nodes)),nrow = length(nodes), ncol = length(nodes))
    year_edges = edgeList(author_net_year)
    nodes=data.frame(id = 1:length(nodes), label = nodes)
    year_edges = apply(year_edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
    year_edges = data.frame(year_edges)
    print(year_edges)
    mapply(function(x,y){author_mat_year[x,y]<<-1}, year_edges$from, year_edges$to)
    names(author_mat_year) = nodes[2]
    filename = paste0("authorMatrix/",journal,"_binary/author_mat_year_",journal,"_",j,".RData" )
    print(filename)
    save(author_mat_year, file = filename)
  }
}

getAuthorMatrixByYear(cogsci_byAuthor, "cogsci")
getAuthorMatrixByYear(nips_byAuthor, "nips")
getAuthorMatrixByYear(vss_byAuthor, "vss")

getAuthorMatrixByYearFullPubCount = function(byAuthor, journal){
  author_net = byAuthor %>% select(title,authorAbbr,year) %>% unique()
  for(j in unique(author_net$year)){
    author_net_year = author_net %>% filter(year == j) %>% select(title, authorAbbr)
    nodes= unique(author_net$authorAbbr)
    author_mat_year = matrix(rep(0,length(nodes)*length(nodes)),nrow = length(nodes), ncol = length(nodes))
    year_edges = edgeList(author_net_year)
    nodes=data.frame(id = 1:length(nodes), label = nodes)
    year_edges = apply(year_edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
    year_edges = data.frame(year_edges)

    mapply(function(x,y,nodes,author_net){author_mat_year[x,y]<<-countPubs(x,y,node,author_net)}, year_edges$from, year_edges$to, MoreArgs = list(nodes= nodes, author_net = author_net))
    filename = paste0("authorMatrix/",journal,"_counts/author_mat_year_",journal,"_",j,".RData" )
    names(author_mat_year) = nodes[2]
    print(filename)
    save(author_mat_year, file = filename)
  }
}

countPubs = function(a1,a2,nodes, author_net){
  a1_name = as.character(nodes[which(nodes[1] == a1),2])
  a2_name = as.character(nodes[which(nodes[1] == a2),2])
  papers = author_net %>% filter(authorAbbr %in% c(a1_name, a2_name)) 
  sum(duplicated(papers$title))
}


getAuthorMatrixByYearFullPubCount(cogsci_byAuthor, "cogsci")
getAuthorMatrixByYearFullPubCount(nips_byAuthor, "nips")
getAuthorMatrixByYearFullPubCount(vss_byAuthor, "vss")

