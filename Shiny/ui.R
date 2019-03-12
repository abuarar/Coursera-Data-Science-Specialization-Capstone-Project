#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for this application that predicts next word
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coursera Data Science Specialization Capstone Project"),
  theme = shinytheme("united"),
  # Sidebar with a slider input for number of bins 
  navbarPage("Mohammed Abuarar",
    tabPanel("Predict Next Word",
              sidebarPanel(width = 6,
              h3(strong(textInput("txtin","Enter your text here!")),align = "center"),
              p("Profanity and badwords are removed in our POLITE application",align = "center"),
              br(),
              h1(fluidRow(column(actionButton("next1"," ",width = "100%", style='padding:4px; font-size:80%'),width = 4),column(actionButton("next2","",width = "100%", style='padding:4px; font-size:80%'),width = 4),column(actionButton("next3"," ",width = "100%", style='padding:4px; font-size:80%'),width = 4)),align = "center"),
              br(),
              h3(strong("All the text you've entered so far:",align = "left")),
              h4(strong(textOutput("txtout")),align = "left")
              ),
              mainPanel(width = 6,
                       h6(DT::dataTableOutput("PredictedWordlist"),align = "center",width="50%",height="50%")
             )
            ),
    tabPanel("About",mainPanel(width = 6,includeMarkdown("About.md")))
  )
))
