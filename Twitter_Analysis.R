library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(igraph)
library(tidyverse)
library(qdap)
library(gridExtra)
library(ggraph)

df.2017<-read.csv('2017.csv')
df.2018<-read.csv('2018.csv')
df.2019<-read.csv('2019.csv')
df.2020<-read.csv('2020.csv')
df.2021<-read.csv('2021.csv')
df.2022<-read.csv('2022.csv')
##############################################################################################
# I subset the dataset to input the tweet column. I created a variable to store stopwords.
# Further i unnested the columns and removed stopwords. I used the qdap package to create
# a dataframe for top 10 words which have atleast 2 words together.
##############################################################################################
df.2017<-subset(df.2017 , select=c(tweet))
text_df <- tibble( text = df.2017$tweet)
stop_words = tidytext::stop_words
x<-text_df%>%unnest_tokens(word,text)
y<-x%>%filter(!word %in% stop_words$word)
frequency2017<-freq_terms(y,top=10,atleast=2)
frequencyall2017<-freq_terms(y,top=count(y),atleast=2,extend=FALSE)
frequency2017$year<-2017
frequencyall2017$total=as.numeric(count(y))


df.2018<-subset(df.2018 , select=c(tweet))
text_df <- tibble( text = df.2018$tweet)
stop_words = tidytext::stop_words
x<-text_df%>%unnest_tokens(word,text)
y<-x%>%filter(!word %in% stop_words$word)
frequency2018<-freq_terms(y,top=10,atleast=2)
frequencyall2018<-freq_terms(y,top=count(y),atleast=2,extend=FALSE)
frequency2018$year<-2018
frequencyall2018$total=as.numeric(count(y))


df.2019<-subset(df.2019 , select=c(tweet))
text_df <- tibble( text = df.2019$tweet)
stop_words = tidytext::stop_words
x<-text_df%>%unnest_tokens(word,text)
y<-x%>%filter(!word %in% stop_words$word)
frequency2019<-freq_terms(y,top=10,atleast=2)
frequencyall2019<-freq_terms(y,top=count(y),atleast=2,extend=FALSE)
frequency2019$year<-2019
frequencyall2019$total=as.numeric(count(y))

df.2020<-subset(df.2020 , select=c(tweet))
text_df <- tibble( text = df.2020$tweet)
stop_words = tidytext::stop_words
x<-text_df%>%unnest_tokens(word,text)
y<-x%>%filter(!word %in% stop_words$word)
frequency2020<-freq_terms(y,top=10,atleast=2)
frequencyall2020<-freq_terms(y,top=count(y),atleast=2,extend=FALSE)
frequency2020$year<-2020
frequencyall2020$total=as.numeric(count(y))


df.2021<-subset(df.2021 , select=c(tweet))
text_df <- tibble( text = df.2021$tweet)
stop_words = tidytext::stop_words
x<-text_df%>%unnest_tokens(word,text)
y<-x%>%filter(!word %in% stop_words$word)
frequency2021<-freq_terms(y,top=10,atleast=2)
frequencyall2021<-freq_terms(y,top=count(y),atleast=2,extend=FALSE)
frequency2021$year<-2021
frequencyall2021$total=as.numeric(count(y))

df.2022<-subset(df.2022 , select=c(tweet))
text_df <- tibble( text = df.2022$tweet)
stop_words = tidytext::stop_words
x<-text_df%>%unnest_tokens(word,text)
y<-x%>%filter(!word %in% stop_words$word)
frequency2022<-freq_terms(y,top=10,atleast=2)
frequencyall2022<-freq_terms(y,top=count(y),atleast=2,extend=FALSE)
frequency2022$year<-2022
frequencyall2022$total=as.numeric(count(y))

frequencytop10<-bind_rows(frequency2017,frequency2018,frequency2019,frequency2020,frequency2021,frequency2022)
view(frequencytop10)

################################################################################################
# Plotting graphs for word frequency for each year. 
################################################################################################

gg2017<-ggplot(frequency2017,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2017')
gg2018<-ggplot(frequency2018,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2018')
gg2019<-ggplot(frequency2019,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2019')
gg2020<-ggplot(frequency2020,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2020')
gg2021<-ggplot(frequency2021,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2021')
gg2022<-ggplot(frequency2022,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2022')


grid.arrange(gg2017,gg2018,gg2019,gg2020,gg2021,gg2022,nrow=3)

################################################################################################
# I created a column for term frequency and created a variable to store the ggplot in 
# logarithmic scale to later compile them in a grid.
################################################################################################
freq_by_rank2017 <- frequencyall2017 %>% 
  mutate(rank = row_number(), 
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf2017<-freq_by_rank2017 %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+ggtitle('2017')

freq_by_rank2018 <- frequencyall2018 %>% 
  mutate(rank = row_number(), 
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf2018<-freq_by_rank2018 %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+ggtitle('2018')

freq_by_rank2019 <- frequencyall2019 %>% 
  mutate(rank = row_number(), 
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf2019<-freq_by_rank2019 %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+ggtitle('2019')

freq_by_rank2020 <- frequencyall2020 %>% 
  mutate(rank = row_number(), 
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf2020<-freq_by_rank2020 %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+ggtitle('2020')

freq_by_rank2021 <- frequencyall2021 %>% 
  mutate(rank = row_number(), 
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf2021<-freq_by_rank2021 %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+ggtitle('2021')

freq_by_rank2022 <- frequencyall2022 %>% 
  mutate(rank = row_number(), 
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf2022<-freq_by_rank2022 %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()+ggtitle('2022')


grid.arrange(zipf2017,zipf2018,zipf2019,zipf2020,zipf2021,zipf2022,nrow=3)

###############################################################################################
# Creating bigram graph for each year individually.
###############################################################################################

bigram_2017<-df.2017%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)
bigrams_separated <- bigram_2017 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n>7)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2017 Bigram')+
  theme_void()

bigram_2018<-df.2018%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)
bigrams_separated <- bigram_2018 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
   filter(n>7)%>%
   graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
  arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2018 Bigram')+
  theme_void()

bigram_2019<-df.2019%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)
bigrams_separated <- bigram_2019 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n>11)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2019 Bigram')+
  theme_void()

bigram_2020<-df.2020%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)
bigrams_separated <- bigram_2020 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n>15)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2020 Bigram')+
theme_void()

bigram_2021<-df.2021%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)
bigrams_separated <- bigram_2021 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n>7)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2021 Bigram')+
  theme_void()

bigram_2022<-df.2022%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)
bigrams_separated <- bigram_2022 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n>2)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2022 Bigram')+
  theme_void()

