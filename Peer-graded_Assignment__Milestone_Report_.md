---
title: "Peer-graded Assignment (Milestone Report)"
author: "Mohammed Abuarar"
date: "February 25, 2019"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---



# Capstone Project Exploratory Data Analysis

This exploratory report is to show distribution of words and relationship between them in a given large text (Data), in order to build a prediction model for the most likely next word based on Natural Language Processing (NLP) analysis.

The following link has the data we willuse as training context for prediction analysis, it contains text in three main **documents:** blogs, news and twitter in 4 different **languages:** German, English , Finnish and Russian. (I will consider english as my choice). 
https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

## Criteria of our text files

This is a quick summary of the three files (blogs, news and twitter) about: Size, Word counts, line counts and basic data tables.


|File_Title |File              |  Size_Mb| Word_Count| Line_Count|
|:----------|:-----------------|--------:|----------:|----------:|
|blogs      |en_US.blogs.txt   | 200.4242|   37570839|     899288|
|news       |en_US.news.txt    | 196.2775|    2651432|      77259|
|twitter    |en_US.twitter.txt | 159.3641|   30451128|    2360148|

## Sampling and cleaning the data

This dataset is fairly large. a common criterion found in text data is that its distribution comes with very long tails **(many words that occur rarely and fewer words that occur frequently)**.
Using this fact we don't need to load the entire dataset but a sample of a good size will represent the data population (n = 10000 Lines) see histogram below. 

For cleaning the data I used tidytext package [^1] (which I found during my search the most well organized package designed specifically for text processing and includes necessary functions from (tm) & (quanteda) built-in inside **tidytext functions**, further more it is part of **tidyverse** system that's greatly fits with **ggplot2, dplyr, tidyr, ...** packages which adopts tidy data approach by Hadley Wickham [^2]

the main cleaning processes are:

* Convert all characters to lower-case.
* Removing punctuations from text.
* Removing numbers from text.
* Removing foriegn language characters (like latin characters).
* Removing extra space (white space).
* Removing bad words.




## Exploratory visualizations

### Histogram of words frequency

Word distribution comes with very long tails (**many** words that occur **rarely**) and (**fewer** words that occur **frequently**) which is a common criterion in text data

<img src="Peer-graded_Assignment__Milestone_Report__files/figure-html/WordsHistogram-1.png" width="100%" style="display: block; margin: auto;" />

### Word Cloud of most frequent words

The following word cloud plot shows the most 100 frequent words in each documents, with the size of word font as big as its frequency in the related document (blogs, news, or twitter)


| blogs | news | twitter |
|:-----:|:----:|:-------:|
|       |      |         |

<img src="Peer-graded_Assignment__Milestone_Report__files/figure-html/WordsCloud-1.png" width="100%" style="display: block; margin: auto;" />


## N-grams

An **n-gram** is a contiguous sequence of n items from a given sample of text or speech [^3].
The process of converting text into n-grams is refered to as **"tokenizing"** [^4] , where n-grams are needed to be processed by machine learning algorithms for **natural langauge processing (NLP)** [^5] .





### Plotting N-grams: 

Unigram (1-gram), Bigram (2-gram) and Trigram (3-gram) top counts, for each document blogs, news, and twitter

<img src="Peer-graded_Assignment__Milestone_Report__files/figure-html/Trigrams-1.png" width="100%" style="display: block; margin: auto;" />

### Bigram network using ggraph

Visualizing relationship between a word and its preceeding word in bigram

<img src="Peer-graded_Assignment__Milestone_Report__files/figure-html/BigramGgraph-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

### Trigram network using ggraph

Visualizing relationship between a word and its preceeding bigram in a trigram

<img src="Peer-graded_Assignment__Milestone_Report__files/figure-html/TrigramGgraph-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

## Plans for next step in capstone project

* To build a prediction algorithm using N-gram models such as : smoothing, Katz's back-off, to estimate the probability of seen as well as unseen n-grams. 
* To build a data product capabable of predicting next word according to already writen context, lunching it on a website like **Shiny** [^6] such that users can use it taking in mind to reduce memory size required to run the model, and reduce time required to provide a reasonable experience to the user. 

_______________________________________________
[^1]: [Julia Silge, David Robinson, "tidytext: Text mining using dplyr, ggplot2, and other tidy tools"](https://github.com/juliasilge/tidytext)
[^2]: [Hadley Wickham, Tidy data](http://vita.had.co.nz/papers/tidy-data.html)
[^3]: [n-gram, Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/N-gram)
[^4]: [Lexical analysis, Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/Lexical_analysis#Tokenization)
[^5]: [Natural language processing, Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/Natural_language_processing)
[^6]: [shinyapps.io by RStudio](https://www.shinyapps.io/)



##Appendices


###Appendix-1: Downloading Data


```r
### Setting seed
set.seed(2019)

### This will download the data to the working directory
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",destfile = ".")
unzip("./Coursera-SwiftKey.zip", overwrite=FALSE)

### This will download badwords list to the working directory and read it into a tbl data frame
download.file("https://www.freewebheaders.com/download/files/full-list-of-bad-words_text-file_2018_07_30.zip","./full-list-of-bad-words_text-file_2018_07_30.zip",cacheOK = TRUE)
unzip("full-list-of-bad-words_text-file_2018_07_30.zip", overwrite=FALSE)
```


###Appendix-2: Getting, Summarising and Cleaning Data


```r
### Loading required packages
library(dplyr)
library(stringi)
library(knitr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(igraph)
library(ggraph)

### Reading text data into R
con1<-file("final/en_US/en_US.blogs.txt")
txtblogs<-readLines(con1, encoding="UTF-8")
con2<-file("final/en_US/en_US.news.txt")
txtnews<-readLines(con2, encoding="UTF-8")
con3<-file("final/en_US/en_US.twitter.txt")
txttwitter<-readLines(con3, encoding="UTF-8")
close(con1);close(con2);close(con3)
con4<-file("full-list-of-bad-words_text-file_2018_07_30.txt")
badwords<-tibble(word=readLines(con4)[-c(1:13)])
close(con4)

### Summary table
kable(data.frame(File_Title=c("blogs","news","twitter"),
                 File=c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"),
                 Size_Mb=c(file.info("final/en_US/en_US.blogs.txt")$size/1024^2,file.info("final/en_US/en_US.news.txt")$size/1024^2,
                 file.info("final/en_US/en_US.twitter.txt")$size/1024^2),
                 Word_Count=c(stri_stats_latex(txtblogs)[4],stri_stats_latex(txtnews)[4],stri_stats_latex(txttwitter)[4]),
                 Line_Count=c(length(txtblogs),length(txtnews),length(txttwitter))),format = "markdown")

### Sampling
set.seed(2019)
txtblogsSample<-sample(txtblogs,10000)
txtnewsSample<-sample(txtnews,10000)
txttwitterSample<-sample(txttwitter,10000)

### Cleaning and Tokenizing using (unnest_tokens) function
####### This will creat Unigram (1-gram) data frame per each document (blogs, news and twitter)
createTidyUnigram<-function(x,y){
  tibble(x) %>%
  mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
  mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
  unnest_tokens(word, x, to_lower = TRUE, strip_punct = TRUE, strip_numeric = TRUE) %>%
  filter(word!="") %>% #remove empty tokens
  count(document=y,word, sort = TRUE)
}
blogsSample_DT<-createTidyUnigram(txtblogsSample,"blogs")
newsSample_DT<-createTidyUnigram(txtnewsSample,"news")
twitterSample_DT<-createTidyUnigram(txttwitterSample,"twitter")
alldocument_DT<-rbind(blogsSample_DT,newsSample_DT,twitterSample_DT)

####### This will creat Bigram (2-gram) data frame per each document (blogs, news and twitter)
createTidyBigram<-function(x,y){
  tibble(x) %>%
  mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
  mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
  unnest_tokens(bigram, x,token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
  anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
  anti_join(badwords,by=c("word2" = "word")) %>% #remove badwords
  count(document=y,bigram,word1, word2, sort = TRUE)
}
blogsSampleBigram_DT<-createTidyBigram(txtblogsSample,"blogs")
newsSampleBigram_DT<-createTidyBigram(txtnewsSample,"news")
twitterSampleBigram_DT<-createTidyBigram(txttwitterSample,"twitter")
alldocumentBigram_DT<-rbind(blogsSampleBigram_DT,newsSampleBigram_DT,twitterSampleBigram_DT)

####### This will creat Trigram (3-gram) data frame per each document (blogs, news and twitter)
createTidyTrigram<-function(x,y){
  tibble(x) %>%
  mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
  mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
  unnest_tokens(Trigram, x,token = "ngrams", n = 3) %>%
  separate(Trigram, c("word1", "word2", "word3"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
  anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
  anti_join(badwords,by=c("word2" = "word")) %>% #remove badwords
  anti_join(badwords,by=c("word3" = "word")) %>% #remove badwords
  count(document=y,Trigram, word1, word2, word3, sort = TRUE)
}
blogsSampleTrigram_DT<-createTidyTrigram(txtblogsSample,"blogs")
newsSampleTrigram_DT<-createTidyTrigram(txtnewsSample,"news")
twitterSampleTrigram_DT<-createTidyTrigram(txttwitterSample,"twitter")
alldocumentTrigram_DT<-rbind(blogsSampleTrigram_DT,newsSampleTrigram_DT,twitterSampleTrigram_DT)
```

###Appendix-3: Exploratory plots and Visualizations


```r
### Histogram of words frequency
total_words <- alldocument_DT %>% 
  group_by(document) %>% 
  summarize(total = sum(n))
alldocument_DT %>%
  left_join(total_words) %>%
  ggplot(aes(n/total, fill = document)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~document, ncol = 3, scales = "free_y")

### Word Cloud
kable(data.frame(blogs=c("  "),news=c("  "),twitter=c("  ")),align='c',format = "markdown")
par(mfrow=c(1,3),mai=c(0,0,0,0),oma=c(0,0,0,0))
alldocument_DT %>%
  filter(document=="blogs") %>%
  with(wordcloud(word, n, max.words = 100,min.freq = 1, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
alldocument_DT %>%
  filter(document=="news") %>%
  with(wordcloud(word, n, max.words = 100,min.freq = 1, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
alldocument_DT %>%
  filter(document=="twitter") %>%
  with(wordcloud(word, n, max.words = 100,min.freq = 1, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))

### Plotting N-grams
####### This will plot Unigram (1-gram) ggplot per each document (blogs, news and twitter)
ggplotUni<-function(a,b){
  alldocument_DT %>%
  filter(document==a) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(10) %>% 
  ggplot(aes(word, n)) +
  geom_col(fill = b,show.legend = FALSE) +
  ggtitle(paste(a," (1-gram)"))+theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+
  labs(x = NULL, y = "n") +
  coord_flip()
}
ggblogsUni<-ggplotUni("blogs","#F8766D")
ggnewsUni<-ggplotUni("news","#00BA38")
ggtwitterUni<-ggplotUni("twitter","#619CFF")

####### This will plot Bigram (2-gram) ggplot per each document (blogs, news and twitter)
ggplotBi<-function(a,b){
  alldocumentBigram_DT %>%
  filter(document==a) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  top_n(10) %>% 
  ggplot(aes(bigram, n)) +
  geom_col(fill = b,show.legend = FALSE) +
  ggtitle(paste(a," (2-gram)"))+theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+
  labs(x = NULL, y = "n") +
  coord_flip()
}
ggblogsBi<-ggplotBi("blogs","#F8766D")
ggnewsBi<-ggplotBi("news","#00BA38")
ggtwitterBi<-ggplotBi("twitter","#619CFF")

####### This will plot Trigram (3-gram) ggplot per each document (blogs, news and twitter)
ggplotTri<-function(a,b){
  alldocumentTrigram_DT %>%
  filter(document==a) %>%
  mutate(Trigram = factor(Trigram, levels = rev(unique(Trigram)))) %>%
  top_n(10) %>% 
  ggplot(aes(Trigram, n)) +
  geom_col(fill = b,show.legend = FALSE) +
  ggtitle(paste(a," (3-gram)"))+theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+
  labs(x = NULL, y = "n") +
  coord_flip()
}
ggblogsTri<-ggplotTri("blogs","#F8766D")
ggnewsTri<-ggplotTri("news","#00BA38")
ggtwitterTri<-ggplotTri("twitter","#619CFF")

#######Plotting all n-grams
library(gridExtra)
grid.arrange(ggblogsUni,ggblogsBi,ggblogsTri,ggnewsUni,ggnewsBi,ggnewsTri,ggtwitterUni,ggtwitterBi,ggtwitterTri,ncol=3)

###Bigram network using ggraph
set.seed(2019)
bigram_graph <- alldocumentBigram_DT %>%
  select(word1,word2,n) %>%
  filter(n > 200) %>%
  graph_from_data_frame()
a <- grid::arrow(angle = 20, length = unit(.15, "inches"),ends = "last",type = "closed")
alldocumentBigram_DT %>%
  select(word1,word2,n) %>%
  filter(n > 300) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=3) +
  theme_void()

###Trigram network using ggraph
createTidyTrigramforGraph<-function(x,y){
  tibble(x) %>%
  mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
  mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
  unnest_tokens(Trigram, x,token = "ngrams", n = 3) %>%
  separate(Trigram, c("word1", "word2", "word3"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
  anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
  anti_join(badwords,by=c("word2" = "word")) %>% #remove badwords
  anti_join(badwords,by=c("word3" = "word")) %>% #remove badwords
  mutate(Bigram=paste(word1," ",word2)) %>%
  select(-c("word1","word2")) %>%
  count(document=y,Trigram, Bigram, word3, sort = TRUE)
}
blogsSampleTrigramforGraph_DT<-createTidyTrigramforGraph(txtblogsSample,"blogs")
newsSampleTrigramforGraph_DT<-createTidyTrigramforGraph(txtnewsSample,"news")
twitterSampleTrigramforGraph_DT<-createTidyTrigramforGraph(txttwitterSample,"twitter")
alldocumentTrigramforGraph_DT<-rbind(blogsSampleTrigramforGraph_DT,newsSampleTrigramforGraph_DT,twitterSampleTrigramforGraph_DT)

set.seed(2019)
TrigramforGraph_DT <- alldocumentTrigramforGraph_DT %>%
  select(Bigram,word3,n) %>%
  filter(n > 40) %>%
  graph_from_data_frame()
TrigramforGraph_DT %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=3) +
  theme_void()
```

