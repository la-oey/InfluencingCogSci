#'
#' Script for processing co-authorship holdout data
#' 

setwd("/Users/erikbrockbank/web/InfluencingCogSci/R")

library(readr)
library(stringr)
library(tidyverse)



### GLOBALS ###
DATA_FILE = "Erik_Cogsci_2020_finalsub.csv"
AUTHOR_FILE = "cogsci2020_byAuthor.csv"



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


# Function to fetch author data
# writes a row to the data frame for each unique (author, title) tuple
get_author_df = function(data) {
  output_df = data.frame(
    year = numeric(),
    title = character(),
    authorAbbr = character(),
    stringsAsFactors = F
  )
  
  rows = data %>% nrow()
  for (i in 1:rows) {
    pub = data %>% filter(row_number() == i)
    
    output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 1 - processed`, stringsAsFactors = F))
    if (!is.na(pub$`Author 2 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 2 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 3 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 3 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 4 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 4 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 5 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 5 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 6 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 6 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 7 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 7 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 8 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 8 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 9 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 9 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 10 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 10 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 11 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 11 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 12 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 12 - processed`, stringsAsFactors = F))
    }
    if (!is.na(pub$`Author 13 - processed`)) {
      output_df = rbind(output_df, data.frame(year = 2020, title = pub$Title, authorAbbr = pub$`Author 13 - processed`, stringsAsFactors = F))
    }
  }
  
  return(output_df)
}


########################
### MATRIX FUNCTIONS ###
########################

# Function to get list of unique authors from holdout data frame
get_unique_authors = function(data) {
  unique_authors = union(
    unique(data$`Author 1 - processed`),
    unique(data$`Author 2 - processed`))
  unique_authors = union(
    unique_authors,
    unique(data$`Author 3 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 4 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 5 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 6 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 7 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 8 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 9 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 10 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 11 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 12 - processed`)
  )
  unique_authors = union(
    unique_authors,
    unique(data$`Author 13 - processed`)
  )

  return(unique_authors)
}


# Function to generate (empty) authorship matrix
get_author_mat = function(unique_authors) {
  author_mat = matrix(0, nrow = length(unique_authors), ncol = length(unique_authors))
  # set diagonals to 1
  diag(author_mat) = 1
  # set rownames and colnames
  rownames(author_mat) = unique_authors
  colnames(author_mat) = unique_authors
  
  return(author_mat)
}


# Function to populate author matrix
# NB: this populates the bottom left diagonal of the matrix
#     A   B   C
# A   1   0   0
# B   1   1   0
# C   1   1   1
# Sum columnwise and rowwise to get an author's pub count: sum(author_mat[,'J Tenenbaum'], author_mat['J Tenenbaum',]) - 2
populate_author_mat = function(author_mat, data) {
  rows = data %>% nrow()
  for (i in 1:rows) {
    pub = data %>% filter(row_number() == i)
    if (!is.na(pub$`Author 2 - processed`)) {
      author_mat[pub$`Author 2 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 3 - processed`)) {
      author_mat[pub$`Author 3 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 3 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 4 - processed`)) {
      author_mat[pub$`Author 4 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 4 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 4 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 5 - processed`)) {
      author_mat[pub$`Author 5 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 5 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 5 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 5 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 6 - processed`)) {
      author_mat[pub$`Author 6 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 6 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 6 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 6 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 6 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 7 - processed`)) {
      author_mat[pub$`Author 7 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 7 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 7 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 7 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 7 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 7 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 8 - processed`)) {
      author_mat[pub$`Author 8 - processed`, pub$`Author 7 - processed`] = 1
      author_mat[pub$`Author 8 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 8 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 8 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 8 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 8 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 8 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 9 - processed`)) {
      author_mat[pub$`Author 9 - processed`, pub$`Author 8 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 7 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 9 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 10 - processed`)) {
      author_mat[pub$`Author 10 - processed`, pub$`Author 9 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 8 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 7 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 10 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 11 - processed`)) {
      author_mat[pub$`Author 11 - processed`, pub$`Author 10 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 9 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 8 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 7 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 11 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 12 - processed`)) {
      author_mat[pub$`Author 12 - processed`, pub$`Author 11 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 10 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 9 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 8 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 7 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 12 - processed`, pub$`Author 1 - processed`] = 1
    }
    if (!is.na(pub$`Author 13 - processed`)) {
      author_mat[pub$`Author 13 - processed`, pub$`Author 12 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 11 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 10 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 9 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 8 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 7 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 6 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 5 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 4 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 3 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 2 - processed`] = 1
      author_mat[pub$`Author 13 - processed`, pub$`Author 1 - processed`] = 1
    }
  }
  return(author_mat)
}



################
### ANALYSIS ###
################

# Read in and process data
data = read_data(DATA_FILE)
  # sanity checks
glimpse(data)

data = process_data(data)
  # sanity checks
glimpse(data)
unique(data$`Author 1 - processed`)
unique(data$`Author 2 - processed`)

# Generate byAuthor csv: one row for each (authorAbbr, title) tuple
# i.e. for each publication, make a unique row for each author on the publication
author_df = get_author_df(data)
  # sanity check
glimpse(author_df)
write_csv(author_df, AUTHOR_FILE)


# Get unique author list for creating matrix
  # NB: `sort` removes NA
unique_authors = sort(get_unique_authors(data))
  # sanity checks
length(unique_authors)
sum(is.na(unique_authors)) # check for NAs

# Create matrix
author_mat = get_author_mat(unique_authors)
  # sanity check
sum(author_mat[1,])
author_mat[1,1]
author_mat[1,2]
sum(author_mat[,'J Tenenbaum'], author_mat['J Tenenbaum',]) - 2

# Populate matrix
author_mat = populate_author_mat(author_mat, data)
  # sanity checks
sum(author_mat['J Tenenbaum',], author_mat[,'J Tenenbaum']) - 2




