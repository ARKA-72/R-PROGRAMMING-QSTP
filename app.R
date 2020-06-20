#loading libraries

library(shiny)
library(plyr)
library(rtweet)               #collecting twitter data
library(dplyr)                
library(glue)                 #interpreted string litrals
library(reactable)            
library(RColorBrewer)         
library(wordcloud)            #creating word clouds
library(tm)                   #text mining
library(stringr)             
library(SnowballC)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(textdata)


#COPY AND PASTE app name, consumer key, consumer secret, access token, access secret

token<-create_token(app="ENTER",consumer_key = "ENTER",consumer_secret = "ENTER",access_token = "ENTER",access_secret = "ENTER")



#USER INTERFACE PART


ui <- fluidPage(

    
    titlePanel("SENTIMENT_ANALYSIS_QSTP"),


    sidebarLayout(
        sidebarPanel(
            sliderInput("no_of_tweets",
                        "Number of Tweets to Display:",
                        min = 50,
                        max = 100,
                        value =50,
                        step=10   ),
            textInput("search_word",
                      "Search Word/Hashtag"),
            plotOutput("word_plot"),                       #PLOTTING FREQUENT WORDS
            plotOutput("word.cloud"),                      #PLOTTING WORD CLOUD
            plotOutput("Sentiment.table")                  #PLOTTING SENTIMENT TABLE
        ),

        
        mainPanel(
           reactableOutput("Search.Results"),              #DISPAYING SEARCH RESULTS
           
        )
    )
)



#SERVER PART

server <- function(input, output) {
  
    tweet_df <- reactive({
        search_tweets(input$search_word, n = input$no_of_tweets,include_rts = FALSE,lang="en")   #SEARCHING TWEETS
    })
    

    #FORMATTING THE TWEETS
    
    tweet_table_data <- reactive({
        req(tweet_df())
        tweet_df() %>%
            select(user_id, status_id, created_at, screen_name, text) %>%
            mutate(
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
            )%>%
            select(DateTime = created_at, User = screen_name,Tweet)
    })
    
    
    #DISPLAYING TWEETS
    
    output$Search.Results <- renderReactable({
        reactable::reactable(tweet_table_data(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc" , defaultPageSize = 100, 
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE)
                             )
        )
    })
    

    
    
    
    
        
#CLEANING TWEETS TEXT
    

    tweets_df2<-reactive({search_tweets(input$search_word, n = input$no_of_tweets,include_rts = FALSE,lang="en")})
    
    
    
    tweets_df3<- reactive({
        req(tweets_df2())
        tweets_df2() %>%                                               #REMOVING URLS
            select(text) %>%
            mutate(Text=gsub("http*","",  tweets_df2()$text),
            )
    })
    
    clean_table<- reactive({tweets_df3() %>%
        dplyr::select(Text) %>%                                       #REMOVING PUNCTUATIONS, SPACES ETC.
        tidytext::unnest_tokens(word,Text)})

    
    
    
        
    data("stop_words")                                               #REMOVING STOP WORDS
    clean_table2<-reactive({clean_table() %>% anti_join(stop_words)})
    
    output$word_plot<-renderPlot({clean_table2()%>%                  #PLOTTING FREQUENCY PLOT OF FREQUENTLY USED WORDS IN THE TWEETS
        count(word, sort=TRUE) %>%
        top_n(20) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(x = word, y = n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(x = "Count",
             y = "Unique words",
             title = "Count of unique words found in tweets")
    })
    
    word_table<-reactive({clean_table2()%>%                     
            count(word, sort=TRUE) %>%
            mutate(word = reorder(word, n))})
                                                                         #PLOTTING THE WORDCLOUD
                                                                
    output$word.cloud<-renderPlot({
        wordcloud(word_table()$word,min.freq=5,random.color = TRUE,
                  colors=brewer.pal(8, "Dark2"))
        
    })
    
  
    
    
    
    
#SENTIMENT ANALYSIS  
    
    get.sentiment<-reactive({
            inner_join(word_table(),get_sentiments("nrc"),by=c("word"="word")) %>%
            filter(!is.na(sentiment)) %>%
            count(sentiment, sort = TRUE)})
    
    
    #PLOTTING THE SENTIMENT
    
    output$Sentiment.table<-renderPlot({                                   
        ggplot(data=get.sentiment(),aes(x=sentiment,y=n))+geom_bar(aes(fill=sentiment),stat = "identity")+
            theme(legend.position="none")+
            xlab("Sentiments")+ylab("scores")+ggtitle("Sentiment Analysis")   
    })    

}


# RUN THE APPLICATION
shinyApp(ui = ui, server = server)
