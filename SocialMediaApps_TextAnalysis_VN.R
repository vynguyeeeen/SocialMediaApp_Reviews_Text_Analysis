#########################################################
## IMPORT PACKAGES/ LOAD LIBRARIES
#########################################################
library(dplyr)          
library(tidytext)       
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(ggplot2)
library(igraph)
library(ggraph)
#specify all .cvs files from one directory 
setwd("/Users/vynguyen/Downloads/Text Analytics/social media app reviews")
#load stop_words to clean data later
data(stop_words)  


#########################################################
## PREPARE DATASET; FILTER FOR TEXT ONLY 
#########################################################

#INSTAGRAM REVIEWS 
review = read.csv("Instagram_1Million_reviews.csv", stringsAsFactors = FALSE)
content <- review$content
ig_df <- data.frame(line=1:1000000, text=content) #turn reviews column into df 
head(ig_df,5)

#SNAPCHAT REVIEWS 
review = read.csv("Snapchat_1Million_reviews.csv", stringsAsFactors = FALSE)
content <- review$content
snap_df <- data.frame(line=1:1000000, text=content)  
head(snap_df,5)

#TIKTOK REVIEWS 
review = read.csv("TicToc_1Million_reviews.csv", stringsAsFactors = FALSE)
content <- review$content
tt_df <- data.frame(line=1:1000000, text=content)  
head(tt_df,5)

#########################################################
## TOKENIZING & COUNTING TOKEN FREQUENCIES FOR ALL 3 DFs 
#########################################################

#INSTAGRAM REVIEWS 
#tokenizing one token per row, removing stop-words 
tidy_ig <- ig_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)
print(tidy_ig)
#counting frequencies for tokens
tidy_ig %>%
  count(word, sort=TRUE)

#SNAPCHAT REVIEWS 
#tokenizing one token per row, removing stop-words 
tidy_snap <- snap_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)
#print(tidy_snap)
#counting frequencies for tokens
tidy_snap %>%
  count(word, sort=TRUE)

#TIKTOK REVIEWS 
#tokenizing one token per row, removing stop-words 
tidy_tt <- tt_df %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)
#print(tidy_tt)
#counting frequencies for tokens
tidy_tt %>%
  count(word, sort=TRUE)

#########################################################
## PLOTTING TOKEN FREQUENCIES 
#########################################################

#INSTAGRAM REVIEWS 
freq_hist_ig <-tidy_ig %>%
  count(word, sort=TRUE) %>%
  filter(n > 20000) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_ig)

#SNAPCHAT REVIEWS 
freq_hist_snap <-tidy_snap %>%
  count(word, sort=TRUE) %>%
  filter(n > 32000) %>% 
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_snap)

#TIKTOK REVIEWS 
freq_hist_tt <-tidy_tt %>%
  count(word, sort=TRUE) %>%
  filter(n > 28000) %>% 
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_tt)


#########################################################
## CONCATENATE ALL DATASETS FOR COMPARISON FRAMEWORKS
#########################################################
#prepare data for comparison: correlogram OR .corr() test 
#Instagram as a benchmark for comparison 
frequency <- bind_rows(mutate(tidy_ig, author="Instagram Reviews"),
                       mutate(tidy_snap, author= "Snapchat Reviews"),
                       mutate(tidy_tt, author="TikTok Reviews")) %>% 
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n /sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Snapchat Reviews`, `TikTok Reviews`) 


#########################################################
## CORRELATION COMPARISON
#1) visually see corr (keywords segmentation)  
#2) find corr coefficients
#########################################################

#FRAMEWORK 1: CORRELOGRAMS 
correlogram <- ggplot(frequency, aes(x=proportion, y=`Instagram Reviews`, 
                      color = abs(`Instagram Reviews`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Instagram Reviews", x=NULL)
print(correlogram)

#FRAMEWORK 2: .CORR() TEST 
#INSTAGRAM x SNAPCHAT
ig_snap_corr <- cor.test(data=frequency[frequency$author == "Snapchat Reviews",],
         ~proportion + `Instagram Reviews`)
print(ig_snap_corr)

#INSTAGRAM x TIKTOK
ig_tt_corr <- cor.test(data=frequency[frequency$author == "TikTok Reviews",],
         ~proportion + `Instagram Reviews`)
print(ig_tt_corr)


#########################################################
## SENTIMENT ANALYSIS 
#1) sentiment lexicon 
#2) TF-IDF framework
#########################################################

#load 3 important libraries for sentiment analysis 
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc") 
bing <- get_sentiments("bing")

sentiment <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

#FRAMEWORK 1: SENTIMENT LEXICON  
#INSTAGRAM - SENTIMENTS  
#filter to show words with anger sentiment 
nrcanger <- get_sentiments("nrc") %>%
            filter(sentiment == "anger")
#inner joining data and anger sentiments --> bad 9767
ig_anger <- tidy_ig %>%
            inner_join(nrcanger) %>%
            count(word, sort=T)

#SENTIMENT FRAMEWORK with JOY
nrcjoy <- get_sentiments("nrc") %>%
          filter(sentiment == "joy")
#inner joining data and joy sentiments --> love 72934 
ig_joy <- tidy_ig %>%
          inner_join(nrcjoy) %>%
          count(word, sort=T)

#print results for comparison 
head(ig_anger,3)
head(ig_joy,3)


#SNAPCHAT
#filter to show words with anger sentiment 
nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
#inner joining data and anger sentiments --> bad 14,680
snap_anger <- tidy_snap %>%
  inner_join(nrcanger) %>%
  count(word, sort=T)

#filter to show words with joy sentiment 
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
#inner joining data and joy sentiments --> love 131,651 
snap_joy <- tidy_snap %>%
  inner_join(nrcjoy) %>%
  count(word, sort=T)

#print results for comparison 
head(snap_anger,3)
head(snap_joy,3)


#TIKTOK
#filter to show words with anger sentiment 
nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
#inner joining data and anger sentiments --> bad 136,880
tt_anger <- tidy_tt %>%
  inner_join(nrcanger) %>%
  count(word, sort=T)

#filter to show words with joy sentiment 
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
#inner joining data and joy sentiments --> love 44,059 
tt_joy <- tidy_tt %>%
  inner_join(nrcjoy) %>%
  count(word, sort=T)

#print results for comparison 
head(tt_anger,3)
head(tt_joy,3)


#FRAMEWORK 2: TRIGRAM

#INSTAGRAM - TRIGRAM 
#prepare data; removing NA and stop words
trigram <- ig_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram))%>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#count trigram
trigram_counts <- trigram %>%
  count(word1, word2, word3, sort = TRUE)

#create matrix to draw trigram network
trigram_graph <- trigram_counts %>%
  filter(n>50) %>% 
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#SNAPCHAT - TRIGRAM 
#prepare data; removing NA and stop words
trigram <- snap_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram))%>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#count trigram
trigram_counts <- trigram %>%
  count(word1, word2, word3, sort = TRUE)

#create matrix to draw trigram network
trigram_graph <- trigram_counts %>%
  filter(n>50) %>% 
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightpink", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#TIKTOK - TRIGRAM
#prepare data; removing NA and stop words
trigram <- tt_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram))%>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#count trigram
trigram_counts <- trigram %>%
  count(word1, word2, word3, sort = TRUE)

#create matrix to draw trigram network
trigram_graph <- trigram_counts %>%
  filter(n>200) %>% 
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightseagreen", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


