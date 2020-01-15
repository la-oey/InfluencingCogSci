library(shiny)
library(tidyr)
library(visNetwork)
library(colorspace)
library(igraph)

ui <- fluidPage(
  textInput('textname', label = 'Author:', placeholder = 'E Vul'),
  selectInput('CM', label = 'Centrality Measure:', choices = c('degree','close','between'), selected = c('degree')),
  visNetworkOutput("author_network", height = "1000px")
  
)

server<- function(input,output){

  nodes<- read.csv('all_nodes.csv')
  edges<- read.csv('all_edges.csv')
  edges$X = NULL
  
  edges = data.frame(edges)
  edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
  nodes = data.frame(nodes) 
  names(nodes) = c('id','label')
  edges = data.frame(edges)
  
  centrality  = read.csv('all_centrality.csv') 
  

  output$author_network <- renderVisNetwork({
    nodes = data.frame(centrality) %>% filter(label %in% nodes[,2], CM == input$CM) %>% select(label,measure)%>% inner_join(nodes) %>% arrange(desc(measure)) %>% select(id,label)
    
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


