#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidytext)
library(dplyr)
library(tidyr)
library(shinythemes)
library(DT)

badwords<-readRDS(file = "badwords") #bad words are rejected in this POLITE application
alldocumentBigram_DT<-readRDS(file = "data2")
alldocumentTrigram_DT<-readRDS(file = "data3")
alldocumentQuadgram_DT<-readRDS(file = "data4")

Tokenize1<-function(x){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub('[[:digit:]]+', "", x)) %>% #gsub to remove numbers
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(word, x) %>%
    anti_join(badwords,by=c("word" = "word")) #remove badwords
}
Tokenize2<-function(x){
  tibble(x) %>%
    mutate(x = iconv(x, "latin1", "ASCII", sub="")) %>% #remove latin characters
    mutate(x = gsub('[[:digit:]]+', "", x)) %>% #gsub to remove numbers
    mutate(x = gsub("[[:punct:]]", "", x)) %>% #gsub to remove special characters
    unnest_tokens(Bigram, x,token = "ngrams", n = 2) %>%
    separate(Bigram, c("word1", "word2"), sep = " ",remove = FALSE) %>% #This line will be make coloumns to be used later for prediction 
    anti_join(badwords,by=c("word1" = "word")) %>% #remove badwords
    anti_join(badwords,by=c("word2" = "word")) #remove badwords
}
Tokenize3<-function(x){
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
      mutate(NgramUsed="Quadgram") %>%
      mutate(PredictedWord=word4) %>%
      mutate(Pt=n*d/sum(n)) #katz backoff probability calculation
    B<-1-sum(ConditionalQuadgramFilter$Pt) #leftover probability (Beta) calculation
    ConditionalBackoffQuadgramFilter<- alldocumentTrigram_DT %>% #Backoff to preceeding bigram txt3[1,3:4]
      filter(word1==as.character(txt3[1,3])) %>%
      filter(word2==as.character(txt3[1,4])) %>%
      filter(!(word3%in%ConditionalQuadgramFilter$word4)) %>% #deducting already weighed ngrams
      mutate(NgramUsed="QuadgramBackoff") %>%
      mutate(PredictedWord=word3) %>%
      mutate(Pt=n*d*B/sum(n*d)) #redistributing leftover probability
    n4<-ConditionalQuadgramFilter %>%
      select(c("NgramUsed","PredictedWord","Pt"))
    n3<-ConditionalBackoffQuadgramFilter %>%
      select(c("NgramUsed","PredictedWord","Pt"))
    PredictedWordlist<-rbind(n4,n3) %>%
      mutate(Pt=Pt/sum(Pt)) %>% #Normalizing conditional probability on filtered given conditon (this will help if only discounted rare words are available as next word)
      arrange(desc(Pt))
    PredictedWordlist
  }
  
  else if (ngram%in%c(2,3) & (nrow(filter(alldocumentTrigram_DT,word1==as.character(txt2[1,2]) & word2==as.character(txt2[1,3]))) > 0)) {
    ### if bigram txt2[1,2:3] preceeding next word exist in corpora trigram    
    ConditionalTrigramFilter<- alldocumentTrigram_DT %>%
      filter(word1==as.character(txt2[1,2])) %>%
      filter(word2==as.character(txt2[1,3])) %>%
      mutate(NgramUsed="Trigram") %>%
      mutate(PredictedWord=word3) %>%
      mutate(Pt=n*d/sum(n)) #katz backoff probability calculation
    B<-1-sum(ConditionalTrigramFilter$Pt) #leftover probability calculation
    ConditionalBackoffTrigramFilter<- alldocumentBigram_DT %>%
      filter(word1==as.character(txt2[1,3])) %>%
      filter(!(word2%in%ConditionalTrigramFilter$word3)) %>% #deducting already weighed ngrams 
      mutate(NgramUsed="TrigramBackoff") %>%
      mutate(PredictedWord=word2) %>%
      mutate(Pt=n*d*B/sum(n*d)) #redistributing leftover probability
    n3<-ConditionalTrigramFilter %>%
      select(c("NgramUsed","PredictedWord","Pt"))
    n2<-ConditionalBackoffTrigramFilter %>%
      select(c("NgramUsed","PredictedWord","Pt"))
    PredictedWordlist<-rbind(n3,n2) %>%
      mutate(Pt=Pt/sum(Pt)) %>% #Normalizing conditional probability on filtered given conditon (this will help if only discounted rare words are available as next word)
      arrange(desc(Pt))
    PredictedWordlist
  }
  
  else if (ngram%in%c(1,2,3) & (nrow(filter(alldocumentBigram_DT,word1==as.character(txt1[1,1]))) > 0)) {
    ### if unigram txt1[1,1] preceeding next word exist in corpora bigram    
    ConditionalBigramFilter<- alldocumentBigram_DT %>%
      filter(word1==as.character(txt1[1,1])) %>%
      mutate(NgramUsed="Bigram") %>%
      mutate(PredictedWord=word2) %>%
      mutate(Pt=n/sum(n)) #probability calculation
    PredictedWordlist<-ConditionalBigramFilter %>%
      select(c("NgramUsed","PredictedWord","Pt")) %>%
      mutate(Pt=Pt/sum(Pt)) %>% #Normalizing conditional probability on filtered given conditon (this will help if only discounted rare words are available as next word)
      arrange(desc(Pt))
    PredictedWordlist
  }
}


# Define server logic required to predict a word
shinyServer(function(input, output, session) {
  
  
  #show the text you typed
  output$txtout <- renderText({
      input$txtin
  })
  
  #Make reactive prediction table from text input
  PredictedNextWordTable<-reactive({
    PredictNextWord(input$txtin)
  })
  
  #Update actionButtons label with 3 most probable next text
  observeEvent(PredictNextWord(input$txtin), {
    
    updateActionButton(session, "next1", label = as.character(PredictedNextWordTable()[1,2]))#Update first button with first prediction
    if(is.na(as.character(PredictedNextWordTable()[2,2]))){ #Condition to prevent label update in case of NA
      updateActionButton(session, "next2", label = "")
    }else{
      updateActionButton(session, "next2", label = as.character(PredictedNextWordTable()[2,2]))#Update second button with second prediction
    }
    if(is.na(as.character(PredictedNextWordTable()[3,2]))){ #Condition to prevent label update in case of NA
      updateActionButton(session, "next3", label = "")
    }else{
      updateActionButton(session, "next3", label = as.character(PredictedNextWordTable()[3,2]))#Update third button with third prediction
    }
  })
  
  #Make actionButton add last word to the text in input
  observeEvent(input$next1, {
    txt <- paste(input$txtin,as.character(PredictedNextWordTable()[1,2]))#Add predicted to already written script
    updateTextInput(session, "txtin", value = txt)
  })
  observeEvent(input$next2, {
    if(is.null(PredictNextWord(input$txtin))== FALSE){ #Condition to stop actionButton next2 in case of null prediction
      if(is.na(as.character(PredictedNextWordTable()[2,2]))==FALSE){
        txt <- paste(input$txtin,as.character(PredictedNextWordTable()[2,2]))#Add predicted to already written script
        updateTextInput(session, "txtin", value = txt)
      }
    }
  })
  observeEvent(input$next3, {
    if(is.null(PredictNextWord(input$txtin))== FALSE){ #Condition to stop actionButton next3 in case of null prediction
      if(is.na(as.character(PredictedNextWordTable()[3,2]))==FALSE){
      txt <- paste(input$txtin,as.character(PredictedNextWordTable()[3,2]))#Add predicted to already written script
      updateTextInput(session, "txtin", value = txt)
      }
    }
  })
  

  #Show table of most probable words from reactive prediction table
  output$PredictedWordlist<- DT::renderDataTable(
    DT::datatable(PredictedNextWordTable(),options = list(lengthMenu = list(15,"15"),pageLength = 15,columnDefs = list(list(className = 'dt-center', targets = 0:2)))) 
  )
  
})
