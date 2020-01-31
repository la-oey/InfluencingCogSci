setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/influenceNeighbors")
library(tidyverse)
library(futile.matrix)
library(pbapply)
topic.df <- read_csv("../cogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors)) %>%
  dplyr::select(-X1)

full_author_neighbor.df = c()
neighbor_mats_final = list()
years = 2000:2019
for(i in 1:20){
  temp = readRDS(paste0("neighbors/neighbors_",years[i],".rds"))
  diag(temp) <- NA # removing self co-authorship binary info too
  neighbor_mats_final[[i]] <- temp 
  neighbor.names = rownames(temp)
  full_author_neighbor.df <- c(full_author_neighbor.df, paste0(neighbor.names,"_",years[i]))
}

unique_authors <- unique(full_author_neighbor.df)
length(unique_authors)
peek(neighbor_mats_final[[1]],15)
peek(neighbor_mats_final[[20]],15)

#### FUNCTIONS ####

get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    dplyr::select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}

get_neighbor_topic_dist_byYear <- function(year.select, author, neighbor.dist=2:4){
  neighbor.matr = neighbor_mats_final[[year.select-1999]][author,]
  neighbors = neighbor.matr[which(neighbor.matr %in% neighbor.dist)]
  neighbor.topics <- topic.df %>%
    filter(year==year.select & length(neighbors > 0) & authors %in% names(neighbors)) %>%
    get_avg_topic_dist() %>%
    as.list()
  return(neighbor.topics)
}

get_projection_angle = function(vec.a, vec.b) {
  cos.theta = sum(vec.a * vec.b) / ( sqrt(sum(vec.a * vec.a)) * sqrt(sum(vec.b * vec.b)) )
  return(cos.theta)
}

prob_to_logit = function(p){
  log(p/(1-p))
}

####################

# Each author's topic distribution for a given year
allTopicDist <- topic.df %>%
  mutate(author_year = paste0(authors,"_",year)) %>%
  filter(author_year %in% unique_authors) %>%
  group_by(year, authors) %>%
  dplyr::select(-c(title, author_year)) %>%
  summarise_all(mean)
allAuthorTopicDist <- allTopicDist %>%
  ungroup() %>%
  gather("topic","author_mean",3:102) %>%
  dplyr::arrange(year,authors) %>%
  mutate(topic = paste0("topic",str_pad(topic, 3, pad = "0")))
#write_csv(allAuthorTopicDist, "avg_topic_dist_author_year.csv")

# Author's topic distribution for previous year
allAuthorTopicDist.prevYr <- allAuthorTopicDist %>%
  filter(year < 2019) %>%
  mutate(prev.year = year,
         year = prev.year+1,
         prev.author_mean = author_mean) %>%
  dplyr::select(-author_mean)

allAuthorTopicDist.time <- full_join(allAuthorTopicDist, allAuthorTopicDist.prevYr)
#write_csv(allAuthorTopicDist.time, "avg_topic_dist_author_this_prev.csv")

allAuthorTopicDist.diff <- allAuthorTopicDist.time %>%
  na.omit() %>%
  mutate(diff.author_mean = author_mean-prev.author_mean)

# Each author's neighbors' topic distribution for a given year
allNeighborTopicDist <-  allTopicDist %>%
  dplyr::select(year, authors) %>%
  mutate(neighbor_mean = pbmapply(get_neighbor_topic_dist_byYear,year,authors)) %>%
  unnest() %>%
  ungroup() %>%
  mutate(topic = paste0("topic",str_pad(rep(1:100, nrow(allTopicDist)), 3, pad = "0")))
#write_csv(allNeighborTopicDist, "avg_topic_dist_neighbors_year.csv")

# Author's neighbors' topic distribution for previous year
allNeighborTopicDist.prevYr <- allNeighborTopicDist %>%
  filter(year < 2019) %>%
  mutate(prev.year = year,
         year = prev.year+1,
         prev.neighbor_mean = neighbor_mean) %>%
  dplyr::select(-neighbor_mean)

allNeighborTopicDist.time <- full_join(allNeighborTopicDist, allNeighborTopicDist.prevYr)
#write_csv(allNeighborTopicDist.time, "avg_topic_dist_neighbors_this_prev.csv")

allNeighborTopicDist.diff <- allNeighborTopicDist.time %>%
  na.omit() %>%
  mutate(diff.neighbor_mean = neighbor_mean-prev.neighbor_mean)

combinedTopicDist <- full_join(allAuthorTopicDist.diff, allNeighborTopicDist.diff, by=c("year","authors","topic","prev.year")) %>%
  na.omit() %>%
  mutate(diff.prev.neighbor_author_mean = prev.neighbor_mean - prev.author_mean)
nrow(combinedTopicDist) #2,687 distinct authors/years

all_proj_angles <- combinedTopicDist %>%
  group_by(authors, prev.year) %>%
  summarise(proj_scalar_author = get_projection_angle(diff.author_mean, diff.prev.neighbor_author_mean)*sqrt(sum(diff.author_mean^2)),
            proj_scalar_neighbors = get_projection_angle(diff.neighbor_mean, diff.prev.neighbor_author_mean)*sqrt(sum(diff.neighbor_mean^2)))
all_proj_angles
write_csv(all_proj_angles, "all_projection_angles.csv")

ggplot(all_proj_angles, aes(x=proj_scalar_author, y=proj_scalar_neighbors)) +
  geom_point()




# Correlate with centrality
all_centrality <- data.frame()
for(i in 2000:2019){
  year_centrality <- read_csv(paste0("../networkByYear/centrality_",i,".csv")) %>%
    mutate(year=i)
  all_centrality <- bind_rows(all_centrality, year_centrality) %>%
    dplyr::select(-X1)
}
unique(all_centrality$CM)

all_centrality <- all_centrality %>%
  spread(CM, measure) %>%
  mutate(authors = label) %>%
  dplyr::select(-c(id, label))




all_proj_angles_central <- all_proj_angles %>%
  mutate(year = prev.year) %>%
  dplyr::select(-prev.year) %>%
  left_join(all_centrality) %>%
  mutate(logit.eigen = ifelse(eigen==0, -99,
                              ifelse(eigen==1, 99,
                                     prob_to_logit(eigen))))



cor.test(all_proj_angles_central$proj_scalar_author, all_proj_angles_central$proj_scalar_neighbors)

cor.test(all_proj_angles_central$logit.eigen, all_proj_angles_central$proj_scalar_neighbors)
cor.test(all_proj_angles_central$logit.eigen, all_proj_angles_central$proj_scalar_author)

cor.test(log(all_proj_angles_central$degree), all_proj_angles_central$proj_scalar_neighbors)
cor.test(log(all_proj_angles_central$degree), all_proj_angles_central$proj_scalar_author)

cor.test(log(all_proj_angles_central$close), all_proj_angles_central$proj_angle_neighbors)
cor.test(log(all_proj_angles_central$close), all_proj_angles_central$proj_angle_author)



# Bimodal distribution
ggplot(all_proj_angles_central, aes(x=logit.eigen)) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=log(degree))) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=log(close))) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=log(between))) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=proj_angle_author)) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=proj_angle_neighbors)) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=logit.eigen, y=proj_angle_neighbors)) +
  geom_point()

ggplot(all_proj_angles_central, aes(x=log(degree), y=proj_scalar_neighbors)) +
  geom_point()

ggplot(all_proj_angles_central, aes(x=log(degree), y=proj_scalar_author)) +
  geom_point()



ggplot(all_proj_angles_central, aes(x=log(close), y=proj_angle_neighbors)) +
  geom_point()


