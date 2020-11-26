# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
library(gridExtra)
library(grid)
library(png)
library(ggplot2)
library(shinythemes)
library(lubridate)
library(stringr)
library(shinyjs)

# Load Data ---------------------------------------------------------------


# Tidy Data ---------------------------------------------------------------



# User Interface ----------------------------------------------------------
ui <- navbarPage("2020 Elections and Tweets",
                 theme = shinytheme("superhero"),
                 tabPanel("Title Page",
                          fluidRow(style = 'top-padding:100px',
                              column(12,
                                     h1("2020 Elections and Tweets",
                                        align = "center"),
                                     h2("Aliya Alimujiang", align = "center"),
                                     h2("Minh Nguyen", align = "center"))
                              ),
                          fluidRow(
                              column(12,
                                     imageOutput('trvsbi'), align = "center")
                              )
                          ),
                 
                 tabPanel("Background",
                          p("Several works have shown the potential of online social media, in particular platforms like Twitter, for analyzing the public sentiment in general has a high impact on predictions. With the increasing importance of Twitter in political discussions, a considerable number of studies also investigated the possibility to analyze political processes and predict political elections from data collected on Twitter."),
                          p("For our analysis, we will be using twitter data directly scrapped from twitter using R. The daily tweets are from the following hashtags: biden2020, BidenHarris2020, trump2020,MAGA, vote. The goal is to classify the content of the tweets into Trump, Biden based on the tags as well as sentiments: immigration, economy, stimulus, tax, covid ,fake news ,racist, environment, Russia, security, etc."),
                          p("By classifying at the tweet level, we can correctly take into account the difference of activity of supporters to extract the percentage of users in favor of each candidate/party. This approach allows us to correctly interpret the Twitter opinion trend as the result of the variations in engagement of the supporters of each campaign and to gain unique insight on the dynamics and structure of the social network of Twitter users in relation to their political opinion."),
                          p("The goal of this analysis is to be able to predict the election results solely based on twitter data to figure out the impact of twitter as well as the prediction accuracy. We will be comparing our results with the real election results at the end to conclude and verify the role that twitter plays in terms of election.")
                          ),
                 tabPanel("Data Mining and Processing",
                          p("Live data from twitter extracted on 10/18/2020. We will be extracting data for a week for our training set. We will use another week of extracts as our test set."),
                          p("We will be first doing descriptive analytics to analyze as well as visualize the dataset. Look for correlations and if there are any violations. We will also be using variable selection process to decide which variables to keep in our final model. We will also be doing model selection process."),
                          p("So far, we have collected 2 days of data from twitter. We have done a preliminary data cleanup work in R and ran some basic analysis. Based on this it looks like Biden is getting more retweets and favorites.")
                          ),
                 tabPanel("Variable Selections",
                          p("Backward(16),forward (21), best subset (16)")
                          ),
                 tabPanel("Model Selections"
                          ),
                 tabPanel("Challenges"
                          ),
                 navbarMenu("Levels",
                            tabPanel("Sub-Component A",
                                     h4("Test")),
                            tabPanel("Sub-Component B",
                                     h4("Test 2"))),
                 tabPanel("Conclusions"
                          ),
                 tabPanel("References"
                          )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
    output$trvsbi <- renderImage({
        list(src = "www/trvsbi.jpg"
        )
    })
    

}


# Run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)

