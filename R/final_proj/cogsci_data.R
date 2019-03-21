library(tidyverse)
orig <- read_csv("../../cogsci_papers.csv")
df <- read_csv("topic_dist_fulltext_100.csv")
authorCounts <- read_csv("author_net.csv")

summary(orig)
summary(df)

orig %>%
  group_by(year) %>%
  summarise(count = n())
nrow(orig)

# number of papers per year with full text available + included in topic model
df %>%
  group_by(year) %>%
  summarise(count = n())
nrow(df)

1-nrow(df)/nrow(orig)

df %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=year, y=count)) +
  geom_line() +
  scale_x_continuous(limits=c(1999,2020)) +
  scale_y_continuous("count of papers") +
  theme_minimal()
ggsave("../graphs/tm_papersYear.png")

transform_authorAbbr = function(df){
  newAuthors <- getAuthorList(df) %>%
    group_by(title) %>%
    nest(authorAbbr) %>%
    mutate(authors = map(data, unlist),
           authors = map_chr(authors, paste, collapse = ", ")) %>%
    select(title, authors)
  newdf <- left_join(df, newAuthors, by="title") %>%
    mutate(authors = authors.y) %>%
    select(-c(authors.x,authors.y)) %>%
    select(title, authors, everything())
  return(newdf)
}

byAuth <- transform_authorAbbr(df) %>%
  mutate(author=strsplit(authors, ", ")) %>%
  unnest(author) %>%
  select(author, title, year)

topAuth <- authorCounts %>%
  top_n(6,n) %>%
  .$authorAbbr

byAuth %>%
  group_by(year, author) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(prop = count/n()) %>%
  filter(author %in% topAuth | author == "E Vul") %>%
  ggplot(aes(x=year, y=prop, colour=author)) +
  geom_smooth(se=F) +
  scale_x_continuous(limits=c(1999,2020)) +
  scale_y_continuous("proportion of papers") +
  theme_minimal()
ggsave("../graphs/tm_papersAuth.png")
  