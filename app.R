#library(twitteR)
#library(tm)
#library(wordcloud)
#library(dplyr)
#library(ggplot2)
#library(RColorBrewer)
#library(stringr)
#library(syuzhet) # for sentiment analysis
#library(rbokeh)
#library(base64enc) # fix for twitter oauth in shinyapps.io
#library(SnowballC) # fix for stemming issue in tm
#library(timeSeries)
#library(forecast)
#library(shiny)
#library(quantmod)

runOnline = T

# Load twitter authorization
if(runOnline){
  setup_twitter_oauth("xxxxxxxxxxxxxxxxxxxxxx",
                      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                      "xxxxxxxxxxxxxxxxxxxxxxxxx")
}

# Grab tweets
getTweets <- function(searchString, numTweets, rt_remove, isUser){
  if(runOnline & !isUser){
    st <- searchTwitter(searchString, n=numTweets, resultType = 'recent', lang = 'en')
    statuses <- data.frame(text=sapply(st, function(x) x$getText()),
                           user=sapply(st, function(x) x$getScreenName()),
                           RT=sapply(st, function(x) x$isRetweet),
                           latitude=sapply(st, function(x) as.numeric(x$latitude[1])),
                           longitude=sapply(st, function(x) as.numeric(x$longitude[1])),
                           time=sapply(st, function(x) format(x$created, format='%F %T'))
    )
  }
  
  if(isUser){
    if(numTweets > 3200){numTweets<-3200}
    st <- userTimeline(searchString, n=numTweets, includeRts=!rt_remove)
    statuses <- data.frame(text=sapply(st, function(x) x$getText()),
                           user=sapply(st, function(x) x$getScreenName()),
                           RT=sapply(st, function(x) x$isRetweet),
                           time=sapply(st, function(x) format(x$created, format='%F %T'))
    )
  }
  
  if(!runOnline){
    files <- list.files('data','tweets_')
    searchstring <- 'Apple'
    for(i in 1:length(files)) {
      selectedfile <- '/Users/strakul/software/r/shiny_twitter/data/tweets_politics_3970_2016-02-28.Rda'
      statuses <- readRDS(file=selectedfile)
    }
  }
  
  if(rt_remove){
    print('Removing Retweets')
    statuses <-
      statuses %>%
      filter(!RT)
  }
  return(statuses)
}

# Grab text data
getTextData <- function(statuses) {
  # Gather corpus
  textdata <- Corpus(VectorSource(statuses$text))
  textdata <-
    textdata %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removePunctuation) %>%
    tm_map(content_transformer(function(x) iconv(x, from='ASCII',
                                                 to='UTF-8', sub='byte'))) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(function(x) str_replace_all(x, "@\\w+", ""))) %>% # remove twitter handles
    tm_map(removeNumbers) %>%
    tm_map(stemDocument) %>%
    tm_map(stripWhitespace)
}

# Get sentiment data
getSentiments <- function(textdata){
  sentiments <- sapply(textdata, function(x) get_nrc_sentiment(as.character(x)))
  sentiments <- as.data.frame(aperm(sentiments)) # transpose and save as dataframe
  sentiments <- as.data.frame(lapply(sentiments, as.numeric)) # a bit more to organize
  sentiments <-
    sentiments %>%
    mutate(positivity = positive - negative)
}

# Do the PCA analysis
doPCA <- function(textdata, statuses, sentiments){
  dtm <- DocumentTermMatrix(textdata)
  dtm <- as.matrix(dtm) #inspect(dtm)
  words <- data.frame(term = colnames(dtm))
  words$count <- colSums(dtm)
  words <-
    words %>%
    mutate(freq = count/nrow(statuses)) %>%
    arrange(desc(count))
  tweets <- as.data.frame(dtm)
  ind <- data.frame('id'=seq.int(nrow(tweets)))
  tweets <- cbind(ind, tweets)
  
  # Eliminate very common terms (like the search term)
  numToCut <- max(1, sum(words$freq>0.9))
  words_100 <- as.character(words[1+numToCut:100+numToCut,'term'])
  tweets <- tweets[,c('id',words_100)]
  trans <- preProcess(tweets[,2:ncol(tweets)], method=c("pca"), thresh = 0.95)
  pca <- predict(trans, tweets[,2:ncol(tweets)])
  statuses <- cbind(statuses, pca[,1:5], sentiments)
  return(list("statuses"=statuses, "pca"=trans))
}


##################-------UI------###############
numChoices <- c(1000, 2000, 3000, 4000, 5000)
colChoices <- c('positivity','anger','anticipation','disgust','fear','joy',
                'sadness','surprise','trust')
allChoices <- c('positivity','anger','anticipation','disgust','fear','joy',
                'sadness','surprise','trust')
ui<-shinyUI(
  navbarPage("Dublin Business School",theme = shinythemes::shinytheme("cyborg"),
             tabPanel("Load Tweets",
                      fluidPage(
                        sidebarLayout(
                          # Sidebar with a slider and selection inputs
                          sidebarPanel(
                            # Text box
                            textInput("searchString","Search Twitter for:","Apple"),
                            
                            selectInput("numTweets", "Number of Tweets:",choices = numChoices),
                            checkboxInput("rt_remove", "Eliminate Retweets", value=F),
                            checkboxInput("isUser", "Search is a Screen Name",value=F),
                            actionButton("update", "Search")
                          ),
                          mainPanel(plotOutput("plot"),
                                    verbatimTextOutput("tweetCount")
                          )
                        )
                      )),
             
             tabPanel("Sentiments",
                      fluidPage(
                        titlePanel("Sentiment Analysis"),
                        mainPanel(plotOutput("sentiment"))
                      )),
             
             tabPanel("Time Graph",
                      fluidPage(
                        titlePanel("Time Graph"),
                        mainPanel(rbokehOutput("timeplot")),
                        fluidRow(
                          br(),
                          br(),
                          br(),
                          br(),
                          selectInput('yvar_time','Y Variable',allChoices,
                                      selected = allChoices[2])
                        )
                      )),
             
             tabPanel("Stock",
                      fluidPage(
                        titlePanel("Stock"),
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Select a stock to examine.
                                     Information will be collected from Yahoo finance."),
                            textInput("symb", "Symbol", "AAPL"),
                            dateRangeInput("dates",
                                           "Date range",
                                           start = "2013-01-01",
                                           end = as.character(Sys.Date())),
                            br(),
                            br(),
                            checkboxInput("log", "Plot y axis on log scale",
                                          value = FALSE),
                            checkboxInput("adjust",
                                          "Adjust prices for inflation", value = FALSE)
                            ),
                          mainPanel(plotOutput("plot1"))
                        ),
                        fluidRow(
                          box(
                            plotOutput("auto.arima", height = 350)
                          ),
                          box(
                            title = "forecast values for next 5 days",
                            width = 6,
                            tableOutput("auto.arima1"),
                            height = 380
                          ),
                          box(
                            title = "ARIMA model Error measures",
                            tableOutput("autoforecast")
                            
                          )
                        )
             )
             )
  ))


#######################------SERVER----------#############

server<-function(input, output, session) {
  
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = F)
  })
  
  dataInput2 <- reactive({
    getSymbols(input$sym, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = F)
  })
  
  output$plot2 <- renderPlot({
    data <- dataInput2()
    if (input$adjust) data <- adjust(dataInput2())
    chartSeries(data, theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  output$plot1 <- renderPlot({
    data <- dataInput()
    if (input$adjust) data <- adjust(dataInput())
    
    chartSeries(data, theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  output$plot3 <- renderPlot({
    data <- dataInput()
    if (input$adjust) data <- adjust(dataInput())
    
    chartSeries(data, theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
  
  output$auto.arima <- renderPlot({
    Stock <- as.character(input$symb)
    Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                       src = "yahoo", from = "2016-01-01", env = NULL))
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4]
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
    Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    adj_rental <- seasadj(decomp_rental)
    fit <- auto.arima(Stock_df$Close,ic="bic")
    fit.forecast <- forecast(fit)
    plot(fit.forecast,  main= Stock)
    fit.forecast
  })
  
  output$auto.arima1 <- renderTable({
    Stock <- as.character(input$symb)
    Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                       src = "yahoo", from = "2016-01-01", env = NULL))
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4]
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
    Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    adj_rental <- seasadj(decomp_rental)
    fit <- auto.arima(Stock_df$Close,ic="bic")
    f_ets = forecast(fit, h = 5)
    plot(f_ets)
  })
  
  runpca <- reactive({
    # Change when the "update" button is pressed...
    #input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Running PCA...")
      doPCA(textdata(), statuses(), sentiments())
    })
  })
  
  output$timeplot <- renderRbokeh({
    df <- runpca()[1]$statuses
    df <-
      df %>%
      mutate(time = as.POSIXct(time, format='%F %T')) %>%
      mutate(text = iconv(text, from='ASCII', to='UTF-8', sub='byte'),
             user = iconv(user, from='ASCII', to='UTF-8', sub='byte')) %>%
      arrange(time)
    figure(tools = c("pan", "wheel_zoom", "box_zoom", "resize",
                     "reset", "save")) %>%
      ly_points(time, input$yvar_time, data=df,
                hover=list(user, text)) %>%
      x_axis(label='Date')
  })
  
  output$autoforecast <- renderTable({
    Stock <- as.character(input$symb)
    Stock_df<-as.data.frame(getSymbols(Symbols = Stock, src = "yahoo", from = "2016-01-01", env = NULL))
    Stock_df$Open = Stock_df[,1]
    Stock_df$High = Stock_df[,2]
    Stock_df$Low = Stock_df[,3]
    Stock_df$Close = Stock_df[,4]
    Stock_df$Volume = Stock_df[,5]
    Stock_df$Adj = Stock_df[,6]
    Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
    Stock_df$v7_MA = ma(Stock_df$Close, order=7)
    Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
    rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    adj_rental <- seasadj(decomp_rental)
    fit2 <- auto.arima(Stock_df$Close,ic="bic")
    summary(fit2)
  })
  
  statuses <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Gathering tweets...")
        getTweets(input$searchString, input$numTweets,
                  input$rt_remove, input$isUser)
      })
    })
  })
  
  textdata <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    getTextData(statuses())
  })
  
  sentiments <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    #isolate({
    withProgress({
      setProgress(message = "Gathering sentiments...")
      sentiments <- getSentiments(textdata())
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    wordcloud_rep(textdata(), scale=c(4,0.5),
                  min.freq=3, max.words=100,
                  colors=brewer.pal(8, "RdBu"), random.order=F,
                  rot.per=0.1, use.r.layout=F)
  })
  
  output$tweetCount  <- renderText({
    df <- statuses()
    if(input$isUser){
      paste("Number of Tweets Found: ", as.character(nrow(statuses())),
            "\nUser: ",as.character(df$user[1]),
            "\nDescription: ",as.character(getUser(df$user[1])$description)
      )
    }else{
      paste("Number of Tweets Found: ", as.character(nrow(df)))
    }
    
  })
  
  output$sentiment <- renderPlot({
    v <- sentiments()
    emotions <- data.frame("count"=colSums(v[,c(1:8)]))
    emotions <- cbind("sentiment" = rownames(emotions), emotions)
    ggplot(data = emotions, aes(x = sentiment, y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      xlab("Sentiment") + ylab("Total Count") +
      scale_fill_brewer(palette='RdBu') +
      theme_bw() + theme(legend.position='none')
  })
  
}


# Run the app
shinyApp(ui, server)

#library(xts)
#library(zoo)
#library(shinydashboard)
