library(tidyverse)
orig <- read_csv("../../cogsci_papers.csv")
df <- read_csv("topic_dist_fulltext_100.csv")

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
