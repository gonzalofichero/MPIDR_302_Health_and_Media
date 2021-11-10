#### Obtaining the data ####
# You do not need to run this, as the data are already provided in a CSV file.
# However, this section is included in case you would like to use the pushshift API yourself later to get other reddit data.

# library(jsonlite)
# library(httr)
# library(tidyverse)
# 
# n = 0
# afterdate = 1630454400 # starting on Sep 01, 2021, 00:00:00 GMT
# posts = tibble()
# while(afterdate < 1635724799){ # keep going until Oct 31, 2021, 23:59:59 GMT
#   url = paste0("https://api.pushshift.io/reddit/search/submission/?size=100&subreddit=covidlonghaulers&after=",
#                afterdate,
#                "&before=1635724799")
#   # posts = GET("https://api.pushshift.io/reddit/search/submission/?q=anxiety|anxious|worr*&subreddit=covid19_support")
#   newposts = GET(url)
#   newposts = fromJSON(httr::content(newposts, type = "text", encoding = "UTF-8"))$data
# 
#   afterdate = max(newposts$created_utc)
#   
#   newposts = filter(newposts, link_flair_text == "Symptoms")
#   posts = bind_rows(posts, newposts)
#   n = nrow(posts)
# }
# save(posts, file = "posts")
# posts = posts[, c("author", "id", "domain", "subreddit", "link_flair_text", 
#                   "title", "selftext", "num_comments", "upvote_ratio", "url", "created_utc")]
# write_csv(posts, "reddit_covidlonghaulers_SepOct2021.csv")

#### Data cleaning ####

library(tidyverse)
library(lubridate)
library(tidytext)
library(topicmodels)
library(SnowballC)
library(ldatuning)
library(tm)

posts = read_csv("reddit_covidlonghaulers_SepOct2021.csv")
posts = filter(posts, selftext != "[removed]") # exclude deleted posts
posts$created_utc = as_datetime(posts$created_utc)

text = unnest_tokens(posts, word, selftext)
# make everything lowercase, remove stopwords, and create a stemmed version:
text = text %>%
  mutate(word = tolower(word),
         # replace formatted apostrophes (right single quotation mark) 
         # with regular apostrophes to match the snowball lexicon:
         word = str_replace_all(word, "\u2019", "'")) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = "word") %>%
  mutate(stemmed = wordStem(word)) %>%
  anti_join(filter(stop_words, lexicon == "snowball"), by = c("stemmed" = "word")) # exclude again after the corrections
# create a document-term matrix:
textdtm = text %>%
  count(id, stemmed) %>%
  cast_dtm(document = id, term = stemmed, value = n)


# Find most common words -> keep deleting too common ones...
findFreqTerms(textdtm, lowfreq=50)





#### Topic model ####

# Optimum of K in topicmodeling
optim_k <- FindTopicsNumber(
  textdtm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(optim_k)
# Seem to be between 10 & 14 the best number of topics


# number of topics
k = 7
# number of top words considered
top = 15

textlda = LDA(textdtm, k = k, control = list(seed = 42))

# word loadings per topic
topics = tidy(textlda, matrix = "beta")
top_terms = topics %>%
  mutate(term = factor(term)) %>%
  group_by(topic) %>%
  slice_max(beta, n = top, with_ties = FALSE) %>%
  mutate(term = reorder(term, beta)) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(top = rep(seq(top), k))

pivot_wider(mutate(top_terms, term = as.character(term)), -beta, names_from = topic, values_from = term)

top_terms %>%
  mutate(term = reorder(factor(term), beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic), group = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + 
  coord_flip() 

# document loadings per topic
tidy(textlda, matrix = "gamma") %>% 
  group_by(topic) %>%
  summarise(M = mean(gamma), SD = sd(gamma)) %>%
  arrange(-M)

