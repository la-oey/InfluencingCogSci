setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(shiny)
library(tidyr)
library(visNetwork)
library(colorspace)
library(igraph)

whichYear = "2019" #all for all years

ui <- fluidPage(
  textInput('textname', label = 'Author:', placeholder = 'E Vul'),
  selectInput('CM', label = 'Centrality Measure:', choices = c('degree','close','between','eigen'), selected = c('degree')),
  visNetworkOutput("author_network", height = "1000px")
  
)
input = ui
output = server

server<- function(input,output){

  nodes<- read.csv(paste0('centrality_byYear/nodes_',whichYear,'.csv')) %>%
    dplyr::select(-X)
  edges<- read.csv(paste0('centrality_byYear/edges_',whichYear,'.csv')) %>%
    dplyr::select(-X)
  
  edges = data.frame(edges)
  edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
  nodes = data.frame(nodes) 
  names(nodes) = c('id','label')
  edges = data.frame(edges)
  
  centrality  = read.csv(paste0('centrality_byYear/centrality_',whichYear,'.csv')) %>%
    dplyr::select(-X)
  

  output$author_network <- renderVisNetwork({
    nodes = data.frame(centrality) %>% filter(label %in% nodes[,2], CM == input$CM) %>% dplyr::select(label,measure)%>% inner_join(nodes) %>% dplyr::arrange(desc(measure)) %>% dplyr::select(id,label)
    
    nodes$color = sequential_hcl(length(nodes$id))
    
    visNetwork(nodes, edges, width = "150%")
    
  })
  
  
  observe({
    if(input$textname %in% nodes$label){
    focus = which(nodes$label == as.character(input$textname))
    
    visNetworkProxy("author_network") %>%
      visFocus(id = focus , scale = 4, locked = F, offset = list(x=0,y=10))
    }
  })
}

shinyApp(ui = ui, server = server)


