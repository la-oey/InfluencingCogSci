library(dplyr)
library(readr)
library(stringr)
file1 <- read_csv("cogsci_abstracts2000_2013.csv")
file2 <- read_csv("cogsci_abstracts2013-2014.csv")
file3 <- read_csv("cogsci_abstracts2015-2018.csv")
fileOrig <- read_csv("cogsci_abstracts.csv")

#Extracts abstract
str_extract(testFiles$full_text, "Abstract(.*?)Keywords")
gsub(".*Abstract*|Keywords.*", "", testFiles$full_text)
regmatches(testFiles$full_text,regexec("Abstract(.*?)Keywords",testFiles$full_text))[[1]][2]
grepl("Keywords", testFiles$full_text)
#Extracts keywords
regmatches(testFiles$full_text,regexec("Keywords(.*?)Introduction",testFiles$full_text))[[1]][2]

attachOrigAbstracts <- fileOrig %>%
  filter(year %in% 2009:2014) %>%
  select(html_link,abstract)

allFiles <- bind_rows(file1, file2, file3)
nrow(allFiles)

files2009.2014 <- allFiles %>%
  filter(year %in% 2009:2014) %>%
  select(-abstract) %>%
  left_join(attachOrigAbstracts, by="html_link")

allFiles <- allFiles %>%
  filter(!year %in% 2009:2014) %>%
  bind_rows(files2009.2014)

splitFullText <- function(text){
  return(unlist(str_split(text, "Abstract", 2))[2])
}

extractAbstract <- function(text){
  if(str_detect(text, "Abstract") & str_detect(text, "Keywords")){
    abstract <- regmatches(text,regexec("Abstract(.*?)Keywords",text))[[1]][2]
  } else if(str_detect(text, "Abstract") & str_detect(text, "Introduction")){
    abstract <- regmatches(text,regexec("Abstract(.*?)Introduction",text))[[1]][2]
  }
  else{
    abstract <- 'NA'
  }
  if(!is.na(abstract) & (str_count(abstract, pattern=" ")+1) > 600){
    abstract <- 'NA'
  }
  return(abstract)
}

extractKeyword <- function(text){
  if(str_detect(text, "Keywords") & str_detect(text, "Introduction")){
    keyword <- regmatches(text,regexec("Keywords(.*?)Introduction",text))[[1]][2]
  } else if(str_detect(text, "Keywords") & str_detect(text, "\n")){
   keyword <- regmatches(text,regexec("Keywords(.*?)\n",text))[[1]][2]
  }
  else{
    keyword <- 'NA'
  }
  if(!is.na(keyword) & (str_count(keyword, pattern=" ")+1) > 50){
    keyword <- 'NA'
  }
  return(keyword)
}

# processing
allFiles <- allFiles %>%
  filter(!title %in% c("Front Matter","Cognitive Science Society title")) %>% # removes full proceedings
  unique() %>%
  mutate(authors = ifelse(is.na(authors), "Michela Balconi", authors), # manual input of missing author
         #abstract = ifelse(is.na(abstract) & !is.na(full_text), mapply(extractAbstract, full_text), abstract),
         #abstract = str_replace_all(abstract, c("-\n"="", "\n"=" ")),
         #abstract = trimws(abstract),
         full_text = mapply(splitFullText, full_text),
         full_text = str_replace_all(full_text, c("-\n"="", "\n"=" ")),
         full_text = trimws(full_text))
         #keywords = mapply(extractKeyword, full_text),
         #keywords = str_replace_all(keywords, ":", ""),
         #keywords = trimws(keywords)) %>% # extracts relevant text after first "Abstract" string match
  #filter(!is.na(keywords)) %>%

write_csv(allFiles, "cogsci_papers.csv")
