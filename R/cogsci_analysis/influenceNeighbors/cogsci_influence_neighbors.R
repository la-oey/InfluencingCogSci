setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/influenceNeighbors")
library(tidyverse)
library(futile.matrix)
library(pbapply)
topic.df <- read_csv("fullcogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors)) %>%
  dplyr::select(-X1)

full_author_neighbor.df = c()
neighbor_mats_final = list()
years = 1981:2019
for(i in 1:length(years)){
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
  neighbor.matr = neighbor_mats_final[[year.select-1980]][author,]
  neighbors = neighbor.matr[which(neighbor.matr %in% neighbor.dist)]
  neighbor.topics <- topic.df %>%
    filter(year==year.select & length(neighbors > 0) & authors %in% names(neighbors)) %>%
    get_avg_topic_dist() %>%
    as.list()
  return(neighbor.topics)
}

get_neighbor_topic_dist_byYear.next <- function(year.select, author, neighbor.dist=2:4){
  neighbor.matr = neighbor_mats_final[[year.select-1980]][author,]
  neighbors = neighbor.matr[which(neighbor.matr %in% neighbor.dist)]
  neighbor.topics.next <- topic.df %>%
    filter(year==(year.select+1) & length(neighbors > 0) & authors %in% names(neighbors)) %>%
    get_avg_topic_dist() %>%
    as.list()
  return(neighbor.topics.next)
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

# Author's topic distribution for next year
allAuthorTopicDist.nextYr <- allAuthorTopicDist %>%
  filter(year > 1981) %>%
  mutate(year = year-1,
         next.year = year+1,
         next.author_mean = author_mean) %>%
  dplyr::select(-author_mean)

allAuthorTopicDist.time <- full_join(allAuthorTopicDist, allAuthorTopicDist.nextYr)
#write_csv(allAuthorTopicDist.time, "avg_topic_dist_author_this_prev.csv")

allAuthorTopicDist.diff <- allAuthorTopicDist.time %>%
  na.omit() %>%
  mutate(diff.author_mean = next.author_mean-author_mean)
nrow(allAuthorTopicDist.diff)/100 #7,740 authors with papers contributed in consecutive years

# Each author's neighbors' topic distribution for a given year
allNeighborTopicDist <- allTopicDist %>%
  dplyr::select(year, authors) %>%
  mutate(neighbor_mean = pbmapply(get_neighbor_topic_dist_byYear,year,authors)) %>%
  unnest() %>%
  ungroup() %>%
  mutate(topic = paste0("topic",str_pad(rep(1:100, nrow(allTopicDist)), 3, pad = "0")))
#write_csv(allNeighborTopicDist, "avg_topic_dist_neighbors_year.csv")

# Author's neighbors' topic distribution for next year
allNeighborTopicDist.nextYr <- allTopicDist %>%
  filter(year < 2019) %>%
  dplyr::select(year, authors) %>%
  mutate(neighbor_mean = pbmapply(get_neighbor_topic_dist_byYear.next,year,authors)) %>%
  unnest() %>%
  ungroup() %>%
  mutate(topic = paste0("topic",str_pad(rep(1:100, nrow(filter(allTopicDist, year<2019))), 3, pad = "0")),
         next.year = year+1,
         next.neighbor_mean = neighbor_mean) %>%
  dplyr::select(-neighbor_mean)

allNeighborTopicDist.time <- full_join(allNeighborTopicDist, allNeighborTopicDist.nextYr)
#write_csv(allNeighborTopicDist.time, "avg_topic_dist_neighbors_this_prev.csv")

allNeighborTopicDist.diff <- allNeighborTopicDist.time %>%
  na.omit() %>%
  mutate(diff.neighbor_mean = next.neighbor_mean-neighbor_mean)
nrow(allNeighborTopicDist.diff)/100 #8,962 authors's neighbors with papers contributed in consecutive years

combinedTopicDist <- full_join(allAuthorTopicDist.diff, allNeighborTopicDist.diff, by=c("year","authors","topic","next.year")) %>%
  na.omit() %>%
  mutate(diff.neighbor_author_mean = neighbor_mean - author_mean)
nrow(combinedTopicDist)/100 #2,687 (previously), 3,347 (now) distinct authors/years 

all_proj_angles <- combinedTopicDist %>%
  group_by(authors, year) %>%
  summarise(proj_scalar_author = get_projection_angle(diff.author_mean, diff.neighbor_author_mean)*sqrt(sum(diff.author_mean^2)),
            proj_scalar_neighbors = get_projection_angle(diff.neighbor_mean, diff.neighbor_author_mean)*sqrt(sum(diff.neighbor_mean^2)))
all_proj_angles
write_csv(all_proj_angles, "all_projection_angles.csv")

ggplot(all_proj_angles, aes(x=proj_scalar_author, y=proj_scalar_neighbors)) +
  geom_point() +
  geom_rug()
ggsave("authors_neighbors.png")



# Correlate with centrality
all_centrality <- data.frame()
for(i in 1981:2019){
  year_centrality <- read_csv(paste0("../networkByYear/centrality_",i,".csv")) %>%
    mutate(year=i)
  all_centrality <- bind_rows(all_centrality, year_centrality) %>%
    dplyr::select(-X1)
}

all_centrality <- all_centrality %>%
  spread(CM, measure) %>%
  mutate(authors = label) %>%
  dplyr::select(-c(id, label))




all_proj_angles_central <- all_proj_angles %>%
  left_join(all_centrality) %>%
  mutate(logit.eigen = ifelse(eigen==0, -99,
                              ifelse(eigen==1, 99,
                                     prob_to_logit(eigen))))



cor.test(all_proj_angles_central$proj_scalar_author, all_proj_angles_central$proj_scalar_neighbors)

cor.test(all_proj_angles_central$logit.eigen, all_proj_angles_central$proj_scalar_neighbors)
cor.test(all_proj_angles_central$logit.eigen, all_proj_angles_central$proj_scalar_author)

cor.test(log(all_proj_angles_central$degree), all_proj_angles_central$proj_scalar_neighbors)
cor.test(log(all_proj_angles_central$degree), all_proj_angles_central$proj_scalar_author)

cor.test(log(all_proj_angles_central$close), all_proj_angles_central$proj_scalar_neighbors)
cor.test(log(all_proj_angles_central$close), all_proj_angles_central$proj_scalar_author)



# Bimodal distribution
ggplot(all_proj_angles_central, aes(x=logit.eigen)) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=log(degree))) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=log(close))) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=log(between))) +
  geom_density()

ggplot(all_proj_angles_central, aes(x=logit.eigen, y=proj_scalar_neighbors)) +
  geom_point() +
  geom_smooth(method=lm)

ggplot(all_proj_angles_central, aes(x=log(degree), y=proj_scalar_neighbors)) +
  geom_point() +
  geom_smooth(method=lm)

ggplot(all_proj_angles_central, aes(x=log(degree), y=proj_scalar_author)) +
  geom_point() +
  geom_smooth(method=lm)

ggplot(all_proj_angles_central, aes(x=log(close), y=proj_scalar_neighbors)) +
  geom_point() +
  geom_smooth(method=lm)


#### NULL SHUFFLE PROJECTION AND CENTRALITY ####

corr<- function(dat){
  with(data=dat, cor(dat$central, dat$proj_scalar))
}

dat.shuffle.degree.neighbors <- function(dat){
  data.frame(central=log(dat$degree), 
             proj_scalar=sample(dat$proj_scalar_neighbors, length(dat$proj_scalar_neighbors), replace=F)) %>%
    na.omit()
}

dat.shuffle.degree.author <- function(dat){
  data.frame(central=log(dat$degree), 
             proj_scalar=sample(dat$proj_scalar_author, length(dat$proj_scalar_author), replace=F)) %>%
    na.omit()
}

dat.shuffle.eigen.neighbors <- function(dat){
  data.frame(central=dat$logit.eigen, 
             proj_scalar=sample(dat$proj_scalar_neighbors, length(dat$proj_scalar_neighbors), replace=F)) %>%
    na.omit()
}

dat.shuffle.eigen.author <- function(dat){
  data.frame(central=dat$logit.eigen, 
             proj_scalar=sample(dat$proj_scalar_author, length(dat$proj_scalar_author), replace=F)) %>%
    na.omit()
}




K = 10000
shuffle.samps.degree.neighbors <- replicate(K, corr(dat.shuffle.degree.neighbors(all_proj_angles_central)))
shuffle.samps.degree.author <- replicate(K, corr(dat.shuffle.degree.author(all_proj_angles_central)))
shuffle.samps.eigen.neighbors <- replicate(K, corr(dat.shuffle.eigen.neighbors(all_proj_angles_central)))
shuffle.samps.eigen.author <- replicate(K, corr(dat.shuffle.eigen.author(all_proj_angles_central)))

trueDegreeNeighborCorr <- cor(log(all_proj_angles_central$degree), all_proj_angles_central$proj_scalar_neighbors)
trueDegreeAuthorCorr <- cor(log(all_proj_angles_central$degree), all_proj_angles_central$proj_scalar_author)
trueEigenNeighborCorr <- cor(all_proj_angles_central$logit.eigen, all_proj_angles_central$proj_scalar_neighbors)
trueEigenAuthorCorr <- cor(all_proj_angles_central$logit.eigen, all_proj_angles_central$proj_scalar_author)

data.frame(r = shuffle.samps.degree.neighbors) %>%
  ggplot(aes(x=r)) +
  geom_histogram() +
  geom_vline(xintercept=trueNeighborCorr, colour="red") +
  ggtitle("Null Shuffle Log(Degree Centrality) vs Author Influence Neighbors")
ggsave("img/null.shuffle.simple_centralVsProj_influenceNeighbors.png")

data.frame(r = shuffle.samps.degree.author) %>%
  ggplot(aes(x=r)) +
  geom_histogram() +
  geom_vline(xintercept=trueAuthorCorr, colour="red") +
  ggtitle("Null Shuffle Log(Degree Centrality) vs Neighbors Influence Author")
ggsave("img/null.shuffle.simple_centralVsProj_influenceAuthor.png")

data.frame(r = shuffle.samps.eigen.neighbors) %>%
  ggplot(aes(x=r)) +
  geom_histogram() +
  geom_vline(xintercept=trueEigenNeighborCorr, colour="red") +
  ggtitle("Null Shuffle Logit(Eigen Centrality) vs Author Influence Neighbors")
ggsave("img/null.shuffle.simple_centralVsProj_influenceNeighbors_eigen.png")

data.frame(r = shuffle.samps.eigen.author) %>%
  ggplot(aes(x=r)) +
  geom_histogram() +
  geom_vline(xintercept=trueEigenAuthorCorr, colour="red") +
  ggtitle("Null Shuffle Logit(Eigen Centrality) vs Neighbors Influence Author")
ggsave("img/null.shuffle.simple_centralVsProj_influenceAuthor_eigen.png")
