setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/neurips_analysis")

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
library(tidytext)
library(textstem)
knitr::opts_chunk$set(echo = TRUE)
df <- read_csv("nips_papers.csv", col_types = cols(abstract=col_character()))

glimpse(df)

unique(df$X1)


(total <- nrow(df))

missingText <- df %>%
  filter(is.na(full_text)) %>%
  nrow()
missingText

missingText / total

df %>%
  group_by(year) %>%
  summarise(count = n())


df$length <- str_count(df$full_text, pattern=" ")+1
glimpse(df)



byAuthor <- df %>%
  mutate(author=str_replace_all(authors, ",$", ""),
         author=strsplit(author, ", ")) %>%
  unnest(author) %>%
  mutate(authorAbbr = ifelse(word(author, -1) %in% c("II", "III", "IV", "Jr."), #catches
                             paste(substring(author,1,1), word(author, -2)),
                             paste(substring(author,1,1), word(author, -1))))

write_csv(byAuthor, "nips_byAuthor.csv")