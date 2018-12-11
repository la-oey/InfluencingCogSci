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

# Resources:
# http://www.structuraltopicmodel.com/

##################
### PROCESSING ###
##################

df <- read.csv("cogsci_abstracts.csv")

# Clean abstract column
# Removes punctuation and escape characters, "\\n", "\\t", "\\f".
# Creates exception for words containing punctuation, "e.g." & "i.e."
df$abstract <- as.character(df$abstract)
df <- df %>%
  mutate(abstract_cleaned = str_replace_all(abstract, c("e\\.g\\."="e1g1", "i\\.e\\."="i1e1")),
         abstract_cleaned = str_replace_all(abstract_cleaned, c("[^a-zA-Z0-9\\&\\s]"=" ", "[\\n\\t\\f]"="")),
         abstract_cleaned = str_replace_all(abstract_cleaned, c("e1g1"="e.g.", "i1e1"="i.e.")))
glimpse(df) # 7871 rows

# TODO tweaks, clean-up, etc.
# apply author clean-up that Lauren does in initial data cleaning then include author in metadata
processed <- textProcessor(df$abstract_cleaned, metadata = df)

# experiment with `lower.thresh` arg to prepDocuments
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 10))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta # null currently
length(docs) # 7844 compare to 7871 from initial read.csv
length(vocab)

# TODO what kind of additional processing needs doing to sanity check the above? 
# (e.g. seeing which docs were removed entirely)

################
### MODELING ###
################

# Try out a few checks for number of topics (note this takes some time to converge)
# TODO try passing in add'l params to searchK to include prevalence and metadata
# Note this can take a while to run (> 5 mins)
topic.check = searchK(out$documents, out$vocab, K = c(5, 10, 20, 50, 100)) # add'l params for `prevalence` and `data` (metadata)
topic.check$results # compare relevant metrics for 5 and 10 topics above

# Try determining best fitting model with selectModel
# TODO try passing in add'l params here for `prevalence` and `data`
# NOTE this runs for > 10 mins and may not converge
model.select = selectModel(out$documents, 
                           out$vocab, 
                           K = 10, 
                           max.em.its = 75, 
                           runs = 20, 
                           seed = 8458159)

plotModels(model.select) # These seem pretty indistinguishable
abstract.model <- model.select$runout[[1]]

# Create the model manually rather than using the tools above to explore best fit
# TODO try passing in add'l params for `prevalence` and `data`
abstract.model.manual <- stm(documents = out$documents, 
                             vocab = out$vocab,
                             K = 10,
                             max.em.its = 75,
                             init.type = "Spectral")

#######################
### INTERPRET MODEL ###
#######################

# View the top words for each topic
labelTopics(abstract.model) # this is neat, these seem relatively well-defined by top words

# View the top n documents for each topic
# TODO get this working: how to figure out which abstracts were removed during processing
topic.docs <- findThoughts(abstract.model, texts = df$abstract_cleaned, n = 3)


###########################
### GRAPHICAL SUMMARIES ###
###########################

plot(abstract.model, type = "summary")
plot(abstract.model, type = "perspectives", topics = c(5, 10))
