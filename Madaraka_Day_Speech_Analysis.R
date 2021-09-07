# load the required packages
library(wordcloud) #for visualizing 
library(tidyverse) #for various data manipulation tasks
library(tidytext) #for text mining specifically, main package in book
library(stringr) #for various text operations
library(readtext) # for reading in txt files
library(igraph) # for creating networks of bigrams
library(ggraph) # for visualizing networks of bigrams
# read the data in R
# read single file into R
Speech1 = readtext("Madaraka_Day_2020.txt")
Speech2 = readtext("Madaraka_Day_2021.txt")
# we can read the two files in R with the following code
# this code reads all the files in the folder
Speech = readtext("Madaraka_Day_Speech/")

#The next step is tidying the data
#We have to put one observation per row
# put the data into a data frame
Data1 = tibble(line =1,text = Speech)
Data1

# we also try to analyze individual speeches
Data_1 = tibble(line =1,text = Speech1)
Data_1
Data_2 = tibble(line =1,text = Speech2)
Data_2

# using unnest_tokens(), we convert the data into tidy format, remove the punctuation and converts the words into lower case
Data2 = Data1 %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% count(word,sort = T)
Data2

Speech_2020 = Data_1 %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% count(word,sort = T)
Speech_2020

Speech_2021 = Data_2 %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% count(word,sort = T)
Speech_2021

#remove numbers from the data
Data3 = Data2[-grep("^[0-9]+", Data2$word),]
Data3

Speech_2020_no = Speech_2020[-grep("^[0-9]+", Speech_2020$word),]
Speech_2020_no

Speech_2021_no = Speech_2021[-grep("^[0-9]+", Speech_2021$word),]
Speech_2021_no

# Analysis on multiple datasets
#speech1
Text11 = Speech1 %>% tibble() %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% count(word,sort = T)
Text11

Text12 = Text11[-grep("^[0-9]+", Text11$word),] %>% filter(word != "page")
Text12

#speech2
Text22 = Speech2 %>% tibble() %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% group_by(doc_id) %>% count(word,sort = T)
Text22

Text23 <- Text22[-grep("^[0-9]+", Text22$word),] %>% filter(word != "page")
Text23

#Visualization of the data using ggplot2
Madaraka_2020 = Speech_2020_no %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
Madaraka_2020

Madaraka_2021 = Speech_2021_no %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
Madaraka_2021

library(gridExtra)
grid.arrange(Madaraka_2020, Madaraka_2021)

# Term frequency and inverse document frequency (tf-idf)
# tf-idf checks how important a word is to a document in a collection of documents.


Madaraka_Speech = Speech %>% tibble() %>% 
  unnest_tokens(word, text) %>% count(doc_id,word,sort = T)
Madaraka_Speech

Madaraka_Speech1 = tibble(line =1,text = Madaraka_Speech)

Madaraka_Speech2 <- Madaraka_Speech %>% anti_join(stop_words)
Madaraka_Speech2
head(Madaraka_Speech2)


meaningless_words = tibble(word = c("kenya","fathers", "ksh", "founding", "kenyans", "fellow", "day", "nation", "national"))
Madaraka_Speech3 <- Madaraka_Speech2 %>% anti_join(meaningless_words)
Madaraka_Speech3

Madaraka_Speech_tidy = Madaraka_Speech3[-grep("^[0-9]+", Madaraka_Speech3$word),] %>% filter(word != "page")
Madaraka_Speech_tidy

# Total words
total_words = Madaraka_Speech_tidy %>% group_by(doc_id) %>% 
  summarize(total = sum(n))
total_words

# Left join
Madaraka_Speech_join = left_join(Madaraka_Speech_tidy, total_words)
Madaraka_Speech_join

# term frequency
Madaraka_Speech_freq = Madaraka_Speech_join %>% mutate(rank = row_number(),tf = n/total)
Madaraka_Speech_freq

# bind tf-idf function
Speech_words_2020 = Madaraka_Speech_join %>%
  bind_tf_idf(word, doc_id , n) %>% arrange(desc(tf_idf))
Speech_words_2020 %>% filter(doc_id == "2020.txt")

Speech_words_2021 = Madaraka_Speech_join %>%
  bind_tf_idf(word, doc_id , n) %>% arrange(desc(tf_idf))
Speech_words_2021 %>% filter(doc_id == "2021.txt")

# Sentiment analysis/ opinion mining
#get the dictionaries
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

Sentiments = Speech %>% tibble() %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% group_by(doc_id) %>% inner_join(get_sentiments("bing"))
Sentiments

Sentiments %>% filter(doc_id == "2020.txt") %>% group_by(sentiment) %>% count(sentiment, sort = T)
Sentiments %>% filter(doc_id == "2021.txt") %>% group_by(sentiment) %>% count(sentiment, sort = T)

#word cloud visualization

# load the packages
library("tm")
library("SnowballC")

set.seed(1234)
Speech_words_2020 %>% 
  with(wordcloud(unique(word), n, min.words = 1, max.words = 50, random.order = F
                 , color = c("blue", "red"), scale = c(3, 0.3)))

Speech_words_2021 %>% 
  with(wordcloud(unique(word), n, min.words = 1, max.words = 100, random.order = F
                 , color = c("blue", "red"), scale = c(2.5, 0.25)))


# word frequencies
Madaraka_Speech_freq %>% count(word, sort = T)
Speech_words_2020 %>% count(word, sort = T)
Speech_words_2021 %>% count(word, sort = T)

Madaraka_Speech_F = Madaraka_Speech_tidy %>% 
  group_by(doc_id) %>% 
  count(word, sort = T)
head(Speech_words, 10)

#Plotting word frequencies with bar graphs

Madaraka_Speech_tidy %>% 
  filter(n>6 & doc_id == "2020.txt") %>% 
  ggplot(aes(x=n, y=reorder(word, n), fill=n)) +
  geom_col(show.legend=FALSE) +
  labs(
    x = "Word",
    y = "Frequency", 
    title = "Most frequent words in 2020 Speech"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

Madaraka_Speech_tidy %>% 
  filter(n>10 & doc_id == "2021.txt") %>% 
  ggplot(aes(x=n, y=reorder(word, n), fill=n)) +
  geom_col(show.legend=FALSE) +
  labs(
    x = "Word",
    y = "Frequency", 
    title = "Most frequent words in 2021 Speech"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


#tf-idf
Madaraka_Speech_idf <- Speech_words %>% 
  bind_tf_idf(word, doc_id, n)

Madaraka_Speech_idf %>%
  select(doc_id, word, tf_idf) %>% 
  arrange(desc(tf_idf))


Madaraka_Speech_idf$word <- as.factor(Madaraka_Speech_idf$word)
Madaraka_Speech_idf %>%
  group_by(doc_id) %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(10, tf_idf) %>% 
  ggplot(aes(x = tf_idf, y = reorder(word, tf_idf), fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~doc_id, scales = "free") +
  theme_minimal()

# Analyzing n-grams
# Unnest tokens in n-grams
# An n-gram is a combination of consecutive words of length n. Each bigram (n = 2), for example, consist of two words

Speech_bigrams = Speech %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
Speech_bigrams

Speech_2020_bigrams = Speech1 %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
Speech_2020_bigrams

Speech_2021_bigrams = Speech2 %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
Speech_2021_bigrams

# Counting n-grams
Speech_bigrams %>% 
  count(bigram) %>% 
  arrange(desc(n))

Speech_2020_bigrams %>% 
  count(bigram) %>% 
  arrange(desc(n))

Speech_2021_bigrams %>% 
  count(bigram) %>% 
  arrange(desc(n))

# removing stopwords from n-grams
Speech_bigrams_no = Speech_bigrams %>% 
  separate(col = bigram,
           into = c("word1", "word2"),
           sep = " ",
           remove = FALSE)
Speech_bigrams_no

Speech_2020_bigrams_no = Speech_2020_bigrams %>% 
    separate(col = bigram,
             into = c("word1", "word2"),
             sep = " ",
             remove = FALSE)
Speech_2020_bigrams_no

Speech_2021_bigrams_no = Speech_2021_bigrams %>% 
  separate(col = bigram,
           into = c("word1", "word2"),
           sep = " ",
           remove = FALSE)
Speech_2021_bigrams_no


#removing a row if it contains either words in stop words data
Speech_bigrams_tidy = Speech_bigrams_no %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)
Speech_bigrams_tidy

Speech_bigrams_tidy %>% 
  count(bigram, sort = TRUE)

Speech_2020_bigrams_tidy = Speech_2020_bigrams_no %>%
    filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)
Speech_2020_bigrams_tidy

Speech_2020_bigrams_tidy %>% 
  count(bigram, sort = TRUE)

Speech_2021_bigrams_tidy = Speech_2021_bigrams_no %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)
Speech_2021_bigrams_tidy

Speech_2021_bigrams_tidy %>% 
  count(bigram, sort = TRUE)

# Plotting bigram frequencies
Speech_bigrams_tidy %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 3) %>% 
  ggplot(aes(x = reorder(bigram, n),
             y = n,
             fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency", title = "Most frequent bigrams in 2020 and 2021 Speech") +
  coord_flip() +
  theme_minimal()

Speech_2020_bigrams_tidy %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 2) %>% 
  ggplot(aes(x = reorder(bigram, n),
             y = n,
             fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency", title = "Most frequent bigrams in 2020 Speech") +
  coord_flip() +
  theme_minimal()

Speech_2021_bigrams_tidy %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 2) %>% 
  ggplot(aes(x = reorder(bigram, n),
             y = n,
             fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "frequency", title = "Most frequent bigrams in 2021 Speech") +
  coord_flip() +
  theme_minimal()

#tf_idf in n-grams
Speech_bigrams_idf = Speech_bigrams_tidy %>% 
  count(doc_id, bigram) %>% 
  bind_tf_idf(bigram, doc_id, n)
Speech_bigrams_idf

Speech_2020_bigrams_idf %>% 
  arrange(desc(tf_idf))

Speech_2020_bigrams_idf = Speech_2020_bigrams_tidy %>% 
    count(doc_id, bigram) %>% 
    bind_tf_idf(bigram, doc_id, n)
Speech_2020_bigrams_idf

Speech_2020_bigrams_idf %>% 
  arrange(desc(tf_idf))

Speech_2021_bigrams_idf = Speech_2021_bigrams_tidy %>% 
  count(doc_id, bigram) %>% 
  bind_tf_idf(bigram, doc_id, n)
Speech_2021_bigrams_idf

Speech_2021_bigrams_idf %>% 
  arrange(desc(tf_idf))

#tf-idf plot
Speech_bigrams_idf %>%
  group_by(doc_id) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot() +
  aes(x = tf_idf, 
      y = fct_reorder(bigram, tf_idf), 
      fill = doc_id) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_minimal()

# Creating and Visualizing the Bigrams network
Speech_graph <- Speech_bigrams_tidy %>% 
  count(word1, word2) %>% # we need the words separated for this graph
  filter(n > 3) %>% 
  graph_from_data_frame()

set.seed(2021)
ggraph(Speech_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), 
                 vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(Speech_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), # the links are more transparent if the bigram is rare
                 show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) + #adding the arrows, making sure they don't touch the node
  geom_node_point(color = "#34013f", size = 3) + # larger, purple nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = 'Bigrams (two-word combinations) in "Madaraka Day Speech 2020 and 2021"')
