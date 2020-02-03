##################################
##################################
########## Ideal Graphs ##########
##################################
##################################

setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/compare_CogSci_VSS/general/")
library(tidyverse)
library(pbapply)

##############################
##### Read in Graph Info #####
##############################

cogsci_edges <- data.frame()
cogsci_nodes <- data.frame()
cogsci_centr <- data.frame()
for(i in 2000:2019){
  temp_edges <- read_csv(paste0("cogsci_network/edges_",i,".csv")) %>%
    dplyr::select(-X1) %>%
    mutate(year = i, conference="CogSci")
  cogsci_edges <- bind_rows(cogsci_edges, temp_edges)
  temp_nodes <- read_csv(paste0("cogsci_network/nodes_",i,".csv")) %>%
    dplyr::select(-X1) %>%
    mutate(year = i, conference="CogSci")
  cogsci_nodes <- bind_rows(cogsci_nodes, temp_nodes)
  temp_centr <- read_csv(paste0("cogsci_network/centrality_",i,".csv")) %>%
    dplyr::select(-X1) %>%
    mutate(year = i, conference="CogSci")
  cogsci_centr <- bind_rows(cogsci_centr, temp_centr)
}

vss_edges <- data.frame()
vss_nodes <- data.frame()
vss_centr <- data.frame()
for(i in 2001:2019){
  temp_edges <- read_csv(paste0("vss_network/edges_",i,".csv")) %>%
    dplyr::select(-X1) %>%
    mutate(year = i, conference="VSS")
  vss_edges <- bind_rows(vss_edges, temp_edges)
  temp_nodes <- read_csv(paste0("vss_network/nodes_",i,".csv")) %>%
    dplyr::select(-X1) %>%
    mutate(year = i, conference="VSS")
  vss_nodes <- bind_rows(vss_nodes, temp_nodes)
  temp_centr <- read_csv(paste0("vss_network/centrality_",i,".csv")) %>%
    dplyr::select(-X1) %>%
    mutate(year = i, conference="VSS")
  vss_centr <- bind_rows(vss_centr, temp_centr)
}

all_edges <- bind_rows(cogsci_edges, vss_edges)
all_nodes <- bind_rows(cogsci_nodes, vss_nodes)
all_centr <- bind_rows(cogsci_centr, vss_centr)

##############################

cogsci <- read_csv("cogsci_byAuthor.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(authorAbbr=="J Tenenbaums", "J Tenenbaum", authors)) 
vss <- read_csv("vss_byAuthor.csv") %>%
  distinct()

full <- bind_rows(mutate(cogsci, conference="CogSci"), 
                  mutate(vss, conference="VSS"))
head(full)

##############################

cogsci_topics <- read_csv("cogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors),
         conference="CogSci") %>%
  dplyr::select(-X1)

vss_topics <- read_csv("vss_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         conference="VSS") %>%
  dplyr::select(-X1)

all_topics <- bind_rows(cogsci_topics, vss_topics)
head(all_topics)

##############################

cogsci_similar <- data.frame()
for(i in 2000:2019){
  temp_similar <- read_csv(paste0("cogsci_topicSimYear/cogsci_topicSim_",i,".csv")) %>%
    mutate(year = i, conference="CogSci")
  cogsci_similar <- bind_rows(cogsci_similar, temp_similar)
}

vss_similar <- data.frame()
for(i in 2001:2019){
  temp_similar <- read_csv(paste0("vss_topicSimYear/vss_topicSim_",i,".csv")) %>%
    mutate(year = i, conference="VSS")
  vss_similar <- bind_rows(vss_similar, temp_similar)
}

all_similar <- bind_rows(cogsci_similar, vss_similar)

##############################

cogsci_data <- read_csv("cogsci_full_model_data.csv") %>%
  mutate(conference="CogSci") %>%
  dplyr::select(-X1)
vss_data <- read_csv("vss_full_model_data.csv") %>%
  mutate(conference="VSS") %>%
  dplyr::select(-X1)

full_data <- bind_rows(cogsci_data, vss_data)

head(full_data)

##############################

docsByYear <- full %>%
  dplyr::select(conference, year, authors, title) %>%
  distinct() %>%
  ungroup() %>%
  group_by(conference, year) %>%
  summarise(documents=n())


# get number of edges for every year
edgesByYear <- all_edges %>% 
  filter(from != to) %>%
  group_by(conference, year) %>%
  summarise(edges=n())
edgesByYear

# get number of nodes for every year
nodesByYear <- full %>%
  dplyr::select(conference,year,authorAbbr) %>%
  distinct() %>%
  group_by(conference, year) %>%
  summarise(nodes=n())
nodesByYear

totalPossibleEdges <- full %>%
  group_by(conference, year, title, authors) %>%
  summarise(numAuthors = n()) %>%
  mutate(maxEdges = numAuthors*(numAuthors-1)/2) %>%
  ungroup() %>%
  group_by(conference, year) %>%
  summarise(totEdges = sum(maxEdges))

authorsPerPaperByYear <- full %>%
  dplyr::select(conference,year,title,authorAbbr) %>%
  distinct() %>%
  group_by(conference, year, title) %>%
  summarise(authorsPerPaper=n()) %>%
  ungroup() %>%
  group_by(conference, year) %>%
  summarise(meanAuthorsPerPaper=mean(authorsPerPaper))

fullSummary <- left_join(docsByYear, nodesByYear, by=c("conference","year")) %>%
  left_join(authorsPerPaperByYear, by=c("conference","year")) %>%
  left_join(edgesByYear) %>%
  left_join(totalPossibleEdges)
fullSummary

# Proportion of Edges / Total Possible Edges #
edgesByYear %>%
  left_join(totalPossibleEdges) %>%
  mutate(propEdges = edges/totEdges) %>%
  ggplot(aes(x=year, y=propEdges, colour=conference)) +
  geom_line()



#### IDEAL GRAPHS ####

## Random Shuffle Authors to Papers
set.seed(1234)
shuffled <- full %>% 
  dplyr::select(-c(html_link, pdf_link, author)) %>%
  group_by(conference, year) %>% 
  mutate(shuffAuthor=sample(authorAbbr))

## Shuffle Authors within Similarity Bin
flipped_cogsci <- all_similar %>%
  filter(conference == "CogSci") %>%
  mutate(authorTemp = authorA,
         authorA = authorB,
         authorB = authorTemp) %>%
  dplyr::select(-authorTemp)

all_similar <- bind_rows(flipped_cogsci, all_similar)

binnedSimilar <- all_similar %>%
  filter(authorA != authorB) %>%
  mutate(authorAbbr = authorA) %>%
  group_by(conference,year,authorAbbr) %>%
  mutate(bin = ntile(authorsSim,20))


countAuthors <- full %>%
  group_by(conference, year, authors, title) %>%
  summarise(numAuthors = n())
head(countAuthors,10)

sampleBin <- binnedSimilar %>%
  filter(bin==1)


sample_author <- function(conference.select, year.select, num.assignments){
  selectConfYear = sampleBin %>%
    filter(conference==conference.select, year==year.select)
  samp = sample(unique(selectConfYear$authorAbbr),num.assignments)
  return(list(samp))
}

sample_batch <- function(conference.select, year.select, author1, num.authors){
  selectConfYearAuth <- sampleBin %>%
    filter(conference==conference.select, year==year.select, authorAbbr==author1)
  samp = sample(unique(selectConfYearAuth$authorB), num.authors-1)
  return(list(c(author1,samp)))
}



listAuthors <- docsByYear %>%
  mutate(sampled_author1 = pbmapply(sample_author, conference, year, documents))
unnestAuthors <- listAuthors %>%
  unnest()
unnestAuthors$numAuthors <- countAuthors$numAuthors
selectAllAuthors <- unnestAuthors %>%
  mutate(sampled_batch = pbmapply(sample_batch, conference, year, sampled_author1, numAuthors))
unnestAllAuthors <- selectAllAuthors %>%
  unnest() %>%
  dplyr::select(-c(documents, numAuthors, sampled_author1))

shuffledSimilar <- full
shuffledSimilar$shuffAuthor=unnestAllAuthors$sampled_batch
nrow(shuffledSimilar)
nrow(unnestAllAuthors)
