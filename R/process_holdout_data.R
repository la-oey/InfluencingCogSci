#'
#' Script for processing co-authorship holdout data
#' 

setwd("/Users/erikbrockbank/web/InfluencingCogSci/R")

library(readr)
library(stringr)
library(tidyverse)



### GLOBALS ###
DATA_FILE = "Erik_Cogsci_2020_finalsub.csv"



#################
### FUNCTIONS ###
#################

# Read in data and keep useful columns only
read_data = function(filename) {
  dat = read_csv(filename,
                 #locale = readr::locale(encoding = "UTF-8")
                 locale = readr::locale(encoding = "latin1"))
  # Only keep useful columns
  dat = dat %>%
    select(Status,
           Title,
           `Short Title`,
           `Type of Submission`,
           `Author 1 - first`,
           `Author 1 - last`,
           `Author 2 - first`,
           `Author 2 - last`,
           `Author 3 - first`,
           `Author 3 - last`,
           `Author 4 - first`,
           `Author 4 - last`,
           `Author 5 - first`,
           `Author 5 - last`,
           `Author 6 - first`,
           `Author 6 - last`,
           `Author 7 - first`,
           `Author 7 - last`,
           `Author 8 - first`,
           `Author 8 - last`,
           `Author 9 - first`,
           `Author 9 - last`,
           `Author 10 - first`,
           `Author 10 - last`,
           `Author 11 - first`,
           `Author 11 - last`,
           `Author 12 - first`,
           `Author 12 - last`,
           `Author 13 - first`,
           `Author 13 - last`)
  return(dat)
}

# Utility function for converting author first and last name to "{first letter} {last name}"
format_author = function(author_first, author_last) {
  author_last = paste(toupper(substring(author_last, 1, 1)), 
                      substring(author_last, 2), 
                      sep = "")
  author_formatted = paste(toupper(substr(author_first, 1, 1)),
                           author_last,
                           sep = " ")
  return(author_formatted)
}


# Add processed author names to data
process_data = function(data) {
  data = data %>%
    mutate(`Author 1 - processed` = ifelse(is.na(`Author 1 - last`),
                                           `Author 1 - last`, 
                                           format_author(`Author 1 - first`, `Author 1 - last`)),
           `Author 2 - processed` = ifelse(is.na(`Author 2 - last`),
                                           `Author 2 - last`, 
                                           format_author(`Author 2 - first`, `Author 2 - last`)),
           `Author 3 - processed` = ifelse(is.na(`Author 3 - last`),
                                           `Author 3 - last`, 
                                           format_author(`Author 3 - first`, `Author 3 - last`)),
           `Author 4 - processed` = ifelse(is.na(`Author 4 - last`),
                                           `Author 4 - last`, 
                                           format_author(`Author 4 - first`, `Author 4 - last`)),
           `Author 5 - processed` = ifelse(is.na(`Author 5 - last`),
                                           `Author 5 - last`, 
                                           format_author(`Author 5 - first`, `Author 5 - last`)),
           `Author 6 - processed` = ifelse(is.na(`Author 6 - last`),
                                           `Author 6 - last`, 
                                           format_author(`Author 6 - first`, `Author 6 - last`)),
           `Author 7 - processed` = ifelse(is.na(`Author 7 - last`),
                                           `Author 7 - last`, 
                                           format_author(`Author 7 - first`, `Author 7 - last`)),
           `Author 8 - processed` = ifelse(is.na(`Author 8 - last`),
                                           `Author 8 - last`, 
                                           format_author(`Author 8 - first`, `Author 8 - last`)),
           `Author 9 - processed` = ifelse(is.na(`Author 9 - last`),
                                           `Author 9 - last`, 
                                           format_author(`Author 9 - first`, `Author 9 - last`)),
           `Author 10 - processed` = ifelse(is.na(`Author 10 - last`),
                                           `Author 10 - last`, 
                                           format_author(`Author 10 - first`, `Author 10 - last`)),
           `Author 11 - processed` = ifelse(is.na(`Author 11 - last`),
                                           `Author 11 - last`, 
                                           format_author(`Author 11 - first`, `Author 11 - last`)),
           `Author 12 - processed` = ifelse(is.na(`Author 12 - last`),
                                           `Author 12 - last`, 
                                           format_author(`Author 12 - first`, `Author 12 - last`)),
           `Author 13 - processed` = ifelse(is.na(`Author 13 - last`),
                                           `Author 13 - last`, 
                                           format_author(`Author 13 - first`, `Author 13 - last`)))
  return(data)
}




################
### ANALYSIS ###
################

data = read_data(DATA_FILE)
glimpse(data)

data = process_data(data)
# sanity checks
unique(data$`Author 1 - processed`)
unique(data$`Author 2 - processed`)







