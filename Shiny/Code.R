#####################################################################################
########## Code of sampling and cleaning text data then uploading in RDS ############         ####### 1 #######
#####################################################################################

library(dplyr)
library(tidytext)
library(dplyr)
library(tidyr)

con1<-file("final/en_US/en_US.blogs.txt");con2<-file("final/en_US/en_US.news.txt");con3<-file("final/en_US/en_US.twitter.txt");con4<-file("full-list-of-bad-words_text-file_2018_07_30.txt")
txtblogsSample<-sample(readLines(con1, encoding="UTF-8"),10000)
txtnewsSample<-sample(readLines(con2, encoding="UTF-8"),10000)
txttwitterSample<-sample(readLines(con3, encoding="UTF-8"),10000)
badwords<-tibble(word=readLines(con4)[-c(1:13)])
close(con1);close(con2);close(con3);close(con4)

saveRDS(badwords, file = "badwords")


#################################
########## Bigram ###############
#################################
createTidyBigram<-function(x,y){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub('[[:digit:]]+', "", x)) %>% #gsub to remove numbers
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(bigram, x,token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
    anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word2" = "word")) #remove badwords
}
blogsSampleBigram_DT<-createTidyBigram(txtblogsSample,"blogs")
newsSampleBigram_DT<-createTidyBigram(txtnewsSample,"news")
twitterSampleBigram_DT<-createTidyBigram(txttwitterSample,"twitter")
alldocumentBigram_DT<-rbind(blogsSampleBigram_DT,newsSampleBigram_DT,twitterSampleBigram_DT) %>% 
  count(bigram,word1, word2, sort = TRUE) %>%
  mutate(d=1) # setting discount factor coloumn
for(i in 5:1){
  alldocumentBigram_DT[alldocumentBigram_DT$n==i,"d"]<-((i+1)/i)*(sum(alldocumentBigram_DT$n==i+1)/sum(alldocumentBigram_DT$n==i))
}

saveRDS(alldocumentBigram_DT, file = "data2")

#################################
########## Trigram ##############
#################################
createTidyTrigram<-function(x,y){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub('[[:digit:]]+', "", x)) %>% #gsub to remove numbers
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(Trigram, x,token = "ngrams", n = 3) %>%
    separate(Trigram, c("word1", "word2", "word3"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
    anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word2" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word3" = "word")) #remove badwords
}
blogsSampleTrigram_DT<-createTidyTrigram(txtblogsSample,"blogs")
newsSampleTrigram_DT<-createTidyTrigram(txtnewsSample,"news")
twitterSampleTrigram_DT<-createTidyTrigram(txttwitterSample,"twitter")
alldocumentTrigram_DT<-rbind(blogsSampleTrigram_DT,newsSampleTrigram_DT,twitterSampleTrigram_DT) %>% 
  count(Trigram, word1, word2, word3, sort = TRUE) %>%
  mutate(d=1) # setting discount factor coloumn
for(i in 5:1){
  alldocumentTrigram_DT[alldocumentTrigram_DT$n==i,"d"]<-((i+1)/i)*(sum(alldocumentTrigram_DT$n==i+1)/sum(alldocumentTrigram_DT$n==i))
}

saveRDS(alldocumentTrigram_DT, file = "data3")

#################################
########## Quadgram #############
#################################
createTidyQuadgram<-function(x,y){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub('[[:digit:]]+', "", x)) %>% #gsub to remove numbers
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(Quadgram, x,token = "ngrams", n = 4) %>%
    separate(Quadgram, c("word1", "word2", "word3", "word4"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
    anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word2" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word3" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word4" = "word")) #remove badwords
}
blogsSampleQuadgram_DT<-createTidyQuadgram(txtblogsSample,"blogs")
newsSampleQuadgram_DT<-createTidyQuadgram(txtnewsSample,"news")
twitterSampleQuadgram_DT<-createTidyQuadgram(txttwitterSample,"twitter")
alldocumentQuadgram_DT<-rbind(blogsSampleQuadgram_DT,newsSampleQuadgram_DT,twitterSampleQuadgram_DT) %>%
  count(Quadgram, word1, word2, word3 ,word4, sort = TRUE) %>%
  mutate(d=1) # setting discount factor coloumn
for(i in 5:1){
  alldocumentQuadgram_DT[alldocumentQuadgram_DT$n==i,"d"]<-((i+1)/i)*(sum(alldocumentQuadgram_DT$n==i+1)/sum(alldocumentQuadgram_DT$n==i))
}

saveRDS(alldocumentQuadgram_DT, file = "data4")






###########################################################################
########## Code of fuctions to be used in shiny application ###############         ####### 2 #######
###########################################################################

badwords<-readRDS(file = "badwords") #bad words are rejected in this POLITE application
alldocumentBigram_DT<-readRDS(file = "data2")
alldocumentTrigram_DT<-readRDS(file = "data3")
alldocumentQuadgram_DT<-readRDS(file = "data4")

Tokenize1<-function(x){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(word, x) %>%
    anti_join(badwords,by=c("word" = "word")) #remove badwords
}
Tokenize2<-function(x){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(Bigram, x,token = "ngrams", n = 2) %>%
    separate(Bigram, c("word1", "word2"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
    anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word2" = "word")) #remove badwords
}
Tokenize3<-function(x){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(Trigram, x,token = "ngrams", n = 3) %>%
    separate(Trigram, c("word1", "word2", "word3"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
    anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word2" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word3" = "word")) #remove badwords
}

PredictNextWord<-function(x,ngram=3){
  txt3<-tail(Tokenize3(x),1) #reading last trigram (3 words)
  txt2<-tail(Tokenize2(x),1) #reading last bigram (2 words)
  txt1<-tail(Tokenize1(x),1) #reading last unigram (1 words)
  
  if (length(unlist(strsplit(x," ")))==2) {ngram<-2}
  else if (length(unlist(strsplit(x," ")))==1) {ngram<-1}
  
  if (ngram==3 & (nrow(filter(alldocumentQuadgram_DT,word1==as.character(txt3[1,2]) & word2==as.character(txt3[1,3]) & word3==as.character(txt3[1,4]))) > 0)) {
    ### if triagram txt3 txt3[1,2:4] preceeding next word exist in corpora quadgram
    ConditionalQuadgramFilter<-alldocumentQuadgram_DT %>%
      filter(word1==as.character(txt3[1,2])) %>%
      filter(word2==as.character(txt3[1,3])) %>%
      filter(word3==as.character(txt3[1,4])) %>%
      mutate(ExistIn="Quadgram") %>%
      mutate(PredictedWord=word4) %>%
      mutate(Pt=n*d/sum(n)) #katz backoff probability calculation
    B<-1-sum(ConditionalQuadgramFilter$Pt) #leftover probability (Beta) calculation
    ConditionalBackoffQuadgramFilter<- alldocumentTrigram_DT %>% #Backoff to preceeding bigram txt3[1,3:4]
      filter(word1==as.character(txt3[1,3])) %>%
      filter(word2==as.character(txt3[1,4])) %>%
      filter(!(word3%in%ConditionalQuadgramFilter$word4)) %>% #deducting already weighed ngrams
      mutate(ExistIn="QuadgramBackoff") %>%
      mutate(PredictedWord=word3) %>%
      mutate(Pt=n*d*B/sum(n*d)) #redistributing leftover probability
    n4<-ConditionalQuadgramFilter %>%
      select(c("ExistIn","PredictedWord","Pt"))
    n3<-ConditionalBackoffQuadgramFilter %>%
      select(c("ExistIn","PredictedWord","Pt"))
    PredictedWordlist<-rbind(n4,n3) %>%
      arrange(desc(Pt))
    PredictedWordlist
  }
  
  else if (ngram%in%c(2,3) & (nrow(filter(alldocumentTrigram_DT,word1==as.character(txt2[1,2]) & word2==as.character(txt2[1,3]))) > 0)) {
    ### if bigram txt2[1,2:3] preceeding next word exist in corpora trigram    
    ConditionalTrigramFilter<- alldocumentTrigram_DT %>%
      filter(word1==as.character(txt2[1,2])) %>%
      filter(word2==as.character(txt2[1,3])) %>%
      mutate(ExistIn="Trigram") %>%
      mutate(PredictedWord=word3) %>%
      mutate(Pt=n*d/sum(n)) #katz backoff probability calculation
    B<-1-sum(ConditionalTrigramFilter$Pt) #leftover probability calculation
    ConditionalBackoffTrigramFilter<- alldocumentBigram_DT %>%
      filter(word1==as.character(txt2[1,3])) %>%
      filter(!(word2%in%ConditionalTrigramFilter$word3)) %>% #deducting already weighed ngrams 
      mutate(ExistIn="TrigramBackoff") %>%
      mutate(PredictedWord=word2) %>%
      mutate(Pt=n*d*B/sum(n*d)) #redistributing leftover probability
    
    n3<-ConditionalTrigramFilter %>%
      select(c("ExistIn","PredictedWord","Pt"))
    n2<-ConditionalBackoffTrigramFilter %>%
      select(c("ExistIn","PredictedWord","Pt"))
    PredictedWordlist<-rbind(n3,n2) %>%
      arrange(desc(Pt))
    PredictedWordlist
  }
  
  else if (ngram%in%c(1,2,3) & (nrow(filter(alldocumentBigram_DT,word1==as.character(txt1[1,1]))) > 0)) {
    ### if unigram txt1[1,1] preceeding next word exist in corpora bigram    
    ConditionalBigramFilter<- alldocumentBigram_DT %>%
      filter(word1==as.character(txt1[1,1])) %>%
      mutate(ExistIn="Bigram") %>%
      mutate(PredictedWord=word2) %>%
      mutate(Pt=n/sum(n)) #probability calculation
    PredictedWordlist<-ConditionalBigramFilter %>%
      select(c("ExistIn","PredictedWord","Pt")) %>%
      arrange(desc(Pt))
    PredictedWordlist
  }
}