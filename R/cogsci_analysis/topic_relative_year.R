setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(pbapply)
source('networkCentralityFunctions_cogsci.R')


topic.df <- read_csv("cogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors)) %>% #manually fixing incorrect author naming
  select(-X1)


# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}


# Similarity between authors
magnitude <- function(a){
  sqrt(sum(a^2))
}

cosine_similarity <- function(x, y){
  dot.product = sum(x*y)
  mag.x = magnitude(x)
  mag.y = magnitude(y)
  return(dot.product/(mag.x*mag.y))
}

get_projection_angle = function(vec.a, vec.b) { #not using this
  # get projection angles
  theta = acos(pmin(cosine_similarity(vec.a, vec.b),1)) # cos^-1 to get theta/angle in radians, pmin deals with NaN issues from acos()
  return(theta)
}

# define years.df and before running
selectYears <- function(currYear, n.prevYears){
  topic.df %>%
    filter(year %in% (currYear-n.prevYears+1):(currYear))
}

selectTopicByAuthor <- function(author){
  years.df %>%
    filter(authors == author) %>%
    get_avg_topic_dist()
}

authors_simToYear <- function(author){ #define global.year before running
  topic = selectTopicByAuthor(author)
  return(get_projection_angle(topic$topic.means, global.year))
}


global.means = get_avg_topic_dist(topic.df)


# test cosine similarity
years.df <- selectYears(2018, 1)
selectTopicByAuthor("J Tenenbaum")
global.year = get_avg_topic_dist(years.df)
authors_simToYear("J Tenenbaum")

TEST.AUTHORS = c("J Tenenbaum","T Griffiths","N Goodman","E Vul", "A Gopnik", "L Oey")
TEST.N.YEARS = 1
years.df <- selectYears(2018, TEST.N.YEARS)
global.year = get_avg_topic_dist(years.df)
test.df <- data.frame(author = TEST.AUTHORS) %>%
  mutate(authorsSim = pbmapply(authors_simToYear, author))

test.df


### topic similarity of authors to the average over year
N.YEARS = 1
ALL.YEARS = (min(topic.df$year)+N.YEARS-1):(max(topic.df$year))
allAuthorGlobal <- data.frame()
for(year in ALL.YEARS){
  years.df <- selectYears(year, N.YEARS)
  global.year = get_avg_topic_dist(years.df)
  full.author = years.df %>%
    mutate(author = authors) %>%
    select(author) %>%
    distinct() %>%
    arrange(author) %>%
    mutate(authorsSim = pbmapply(authors_simToYear, author),
           year=year)
  allAuthorGlobal <- bind_rows(allAuthorGlobal, full.author)
}

write_csv(allAuthorGlobal,"allAuthorGlobal.csv")

glimpse(allAuthorGlobal)
allAuthorGlobal %>%
  group_by(year) %>%
  top_n(-5, authorsSim) %>%
  View()

