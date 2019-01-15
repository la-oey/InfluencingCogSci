library(stm)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
require(cleanNLP)
require(udpipe)
require(stringi)
library(knitr)
library(GGally)
library(network)
library(sna)
library(wordcloud)

# Resources:
# http://www.structuraltopicmodel.com/

############################
### PROCESSING FUNCTIONS ###
############################

clean_abstracts <- function(data_frame) {
  # Clean abstract column (expects a data frame with an `abstract` column)
  # Removes punctuation and escape characters, "\\n", "\\t", "\\f".
  # Creates exception for words containing punctuation, "e.g." & "i.e."
  data_frame$abstract <- as.character(data_frame$abstract, na.omit = T)
  data_frame <- data_frame %>%
    mutate(abstract_cleaned = str_replace_all(abstract, c("e\\.g\\."="e1g1", "i\\.e\\."="i1e1")),
           abstract_cleaned = str_replace_all(abstract_cleaned, c("[^a-zA-Z0-9\\&\\s]"=" ", "[\\n\\t\\f]"="")),
           abstract_cleaned = str_replace_all(abstract_cleaned, c("e1g1"="e.g.", "i1e1"="i.e.")))
  return(data_frame)
}

structure_text <- function(documents, metadata = NA) {
  print("Processing documents")
  if (!is.na(metadata)) {
    processed <- textProcessor(documents, metadata = metadata) 
  } else { 
    processed <- textProcessor(documents)
  }
  
  print("Preparing documents for modeling")
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)
  
  return(out)
}

get_removed_docs <- function(documents) {
  processed <- textProcessor(documents)
  removed = processed$docs.removed
  return(removed)
}


#####################
### DATA MODELING ###
#####################

DATA = "cogsci_papers.csv"
DATA_ALT = "cogsci_abstracts.csv"


# Model original abstracts data
df.abstracts.alt <- read_csv(DATA_ALT)
df.abstracts.alt <- clean_abstracts(df.abstracts.alt)
abstract.model.framework <- structure_text(df.abstracts.alt$abstract_cleaned, df.abstracts.alt) # takes < 1 min.
# Fit model
abstract.model.manual <- stm(documents = abstract.model.framework$documents, 
                             vocab = abstract.model.framework$vocab,
                             K = 10,
                             max.em.its = 75, # K = 10 converges after ~25 iterations
                             init.type = "Spectral") # Takes 1-2 mins.
# Validate model
labelTopics(abstract.model.manual)
findThoughts(abstract.model.manual, texts = df.abstracts.alt$abstract_cleaned, n = 3)

# Visualize model
cloud(stmobj = abstract.model.manual,
      topic = 1,
      type = "model",
      max.words = 25) # word cloud of most probable 25 words in topic 1
cloud(stmobj = abstract.model.manual,
      topic = 1,
      type = "documents",
      documents = abstract.model.framework$documents,
      thresh = 0.8,
      max.words = 25) # word cloud of most probable 25 words in topic 1 selected from most likely documents


# Model newer abstracts data
df.abstracts <- read_csv(DATA)
df.abstracts <- clean_abstracts(df.abstracts)
abstract.model.framework <- structure_text(df.abstracts$abstract_cleaned, df.abstracts) # takes < 1 min.
# Fit model
abstract.model.manual <- stm(documents = abstract.model.framework$documents, 
                             vocab = abstract.model.framework$vocab,
                             K = 10,
                             max.em.its = 75, # K = 10 converges after ~50 iterations
                             init.type = "Spectral") # Takes 1-2 mins.
# Validate model
labelTopics(abstract.model.manual)



# Model full text
df.fulltext <- read_csv(DATA)
#  TODO why does metadata not work?
fulltext.model.framework <- structure_text(df.fulltext$full_text) # takes up to 20 mins.
# Fit model: can take 20+ mins. for 50 topics or more
fulltext.model.manual <- stm(documents = fulltext.model.framework$documents, 
                             vocab = fulltext.model.framework$vocab,
                             K = 50,
                             max.em.its = 100,
                             init.type = "Spectral") # note can also use "LDA" here for Gibbs sampler instead of variational inference

# k = 10 converges after 33 iterations
# k = 25 converges after 76 iterations
# k = 50 converges after 81 iterations


# Model fulltext from individual years
sample.year = '2017'
papers.sample.year <- df.fulltext %>%
  filter(year == sample.year)
removed.docs.sample.year = get_removed_docs(papers.sample.year$full_text)
docs.sample.year.cleaned = papers.sample.year[-removed.docs.sample.year,]

fulltext.model.framework.sample.year <- structure_text(papers.sample.year$full_text)
# Summary of fulltext.model.framework.2017:
# 885 documents, 4110 terms and 365292 tokens
fulltext.model.manual.sample.year <- stm(documents = fulltext.model.framework.sample.year$documents, 
                             vocab = fulltext.model.framework.sample.year$vocab,
                             K = 20, # converges after 82 iters for k = 20, very fast
                             max.em.its = 100,
                             init.type = "Spectral")
# validation
summary(fulltext.model.manual.sample.year)  # looks pretty good

# analysis
topic.dist = fulltext.model.manual.sample.year$theta
dim(topic.dist) ;topic.dist[1,]

df.papers = data.frame(title = docs.sample.year.cleaned$title,
                           authors = docs.sample.year.cleaned$authors,
                           year = docs.sample.year.cleaned$year)

df.topic.dist = cbind(df.papers, topic.dist) 

# Write to csv for processing elsewhere
write_csv(df.topic.dist, 'topic_dist_year.csv')



#####################
### VISUALIZATION ###
#####################

# Validate model
labelTopics(fulltext.model.manual)

# Graph model
cloud(stmobj = fulltext.model.manual,
      topic = 22,
      type = "model",
      max.words = 25) # word cloud of most probable 25 words in topic 1


#####################
### ANALYSIS      ###
#####################

summary.STM(fulltext.model.manual)

topic.dist = fulltext.model.manual$theta # Number of Documents by Number of Topics matrix of topic proportions.

# TODO reformat this to include title, author, and year of each document

# TODO write reformatted matrix to csv for use by companion functions

# TODO write companion functions for:
# calculating mean topic distribution over all docs
# compare topic dist for a particular doc to mean dist over all docs (should return vector of coefficients for global topic weight / doc topic weight?)

# TODO parse these out by year? ...
papers.2017 <- df.fulltext %>%
  filter(year == '2017')
fulltext.model.framework.2017 <- structure_text(papers.2017$full_text)

