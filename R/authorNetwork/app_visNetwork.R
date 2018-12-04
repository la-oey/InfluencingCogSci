library(shiny)
library(tidyr)
library(visNetwork)
library(colorspace)
library(igraph)

ui <- fluidPage(
  visNetworkOutput("author_network", height = "2000px")
  
)

server<- function(input,output){
  output$author_network <- renderVisNetwork({
    #calculate authors to include
    
    nodes<- read.csv('nodes.csv')
    edges<- read.csv('edges.csv')
    edges$X = NULL
    
    edges = data.frame(edges)
    edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
    nodes = data.frame(nodes) 
    names(nodes) = c('id','label')
    #nodes$label = NULL
    
    edges = data.frame(edges)

    visNetwork(nodes, edges, width = "150%")
    
  })
 
}

shinyApp(ui = ui, server = server)
