server <- function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Uber" = uber,
           "Lyft" = lyft)}
    
  )
  
  getPage<-function() {
    return(includeHTML("read_me_twitter.html"))
  }
  
  output$read <-renderUI({getPage()})
  
  output$hist <- renderPlotly({
    
    hist_uber <- ggplot(data = sentiment_summary_uber) +
      aes(x = review_sentiment) +
      geom_histogram(bins = input$bins, fill = "#0c4c8a") +
      labs(title = "Uber Sentiment Analysis using BING Lexicon",
           x = "Sentiment Score",
           y = "Frequency") +
      theme_gray()
    ggplotly(hist_uber)}
  )
  
  
  output$pos1 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(uber_pos$column, uber_pos$freq,
              scale=c(8,1), min.freq = input$freq, max.words = input$max, random.order = FALSE,
              rot.per = .15, family="Dubai", colors=ubercols)
  })
  
  output$pos2 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(lyft_pos$column, lyft_pos$freq,
              scale=c(8,1), min.freq = input$freq, max.words = input$max, random.order = FALSE,
              rot.per = .15, family="Dubai", colors=lyftcols)
  })
  
  output$neg1 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(uber_neg$column, uber_neg$freq,
              scale=c(6,1), min.freq = input$freq, max.words = input$max, random.order = FALSE,
              rot.per = .15,  family="Dubai", colors=ubercols)
  })
  
  output$neg2 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(lyft_neg$column, lyft_neg$freq,
              scale=c(6,.8), min.freq = input$freq, max.words = input$max, random.order = FALSE,
              rot.per = .15, family="Dubai", colors=lyftcols)
  })
  
  output$hist1 <- renderPlotly({
    
    hist_lyft <- ggplot(data = sentiment_summary_lyft) +
      aes(x = review_sentiment) +
      geom_histogram(bins = input$bins, fill = "#e7298a") +
      labs(title = "Lyft Sentiment Analysis using BING Lexicon",
           x = "Sentiment Score",
           y = "Frequency") +
      theme_gray()
    ggplotly(hist_lyft)}
  )
  
  output$wc1 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(uber_vocab$term, uber_vocab$term_count,
              scale=c(6,.6),min.freq=input$freq,max.words=input$max, random.order=FALSE,
              rot.per=.15, family="Dubai", colors=ubercols)}
  )
  
  output$wc2 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(lyft_vocab$term, lyft_vocab$term_count,
              scale=c(6,.6),min.freq=input$freq,max.words=input$max, random.order=FALSE,
              rot.per=.15, family="Dubai", colors=lyftcols)}
  )
  
  output$wc3 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(bi_uber_vocab$term, bi_uber_vocab$term_count, scale=c(6,.6),min.freq=input$freq,max.words=input$max, 
              random.order=FALSE, rot.per =.15, family="Dubai", colors=ubercols)}
  )
  
  output$wc4 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(bi_lyft_vocab$term, bi_lyft_vocab$term_count, scale=c(6,.6),min.freq=input$freq,max.words=input$max, 
              random.order=FALSE, rot.per =.15, family="Dubai", colors=lyftcols)}
  )
  
  output$wc5 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(tri_uber_vocab$term, tri_uber_vocab$term_count, scale=c(6,.6),min.freq=input$freq,max.words=input$max, 
              random.order=FALSE, rot.per =.15, family="Dubai", colors=ubercols)}
  )
  
  output$wc6 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(tri_lyft_vocab$term, tri_lyft_vocab$term_count, scale=c(6,.6),min.freq=input$freq,max.words=input$max, 
              random.order=FALSE, rot.per =.15, family="Dubai", colors=lyftcols)}
  ) 
  
  output$rc1 <- renderChartJSRadar({
    chartJSRadar(scores = scores, labs = labels, showToolTipLabel=TRUE, colMatrix = color)
  })
  
  output$wcTF1 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(tfidf_freq_uber$words, tfidf_freq_uber$freq, scale=c(6,.6),min.freq=input$freq,max.words=input$max,
              family="Dubai", colors=ubercols)
  })
  
  output$wcTF2 <- renderPlot({
    par(mar=rep(0,4))
    wordcloud(tfidf_freq_lyft$words, tfidf_freq_lyft$freq, scale=c(6,.6),min.freq=input$freq,max.words=input$max,
              family="Dubai", colors=lyftcols)
  })
  
  output$com1 <- renderPlot({
    par(mar=rep(0,4))
    commonality.cloud(TDM_both_m, scale=c(8,.8),colors=brewer.pal(8, "Dark2"),max.words=input$max, random.order=FALSE)}
  ) 
  
  output$com2 <- renderPlot({
    par(mar=rep(0,4))
    comparison.cloud(TDM_both_m, scale=c(8,.8), colors=c(Uber="royalblue4",Lyft="hotpink3"),max.words = input$max, random.order=FALSE)}
  )
  
  output$bc1 <- renderPlotly({
    uber_nrc_plot <- ggplot(data=uber_score,aes(x=sentiment,y=n))+geom_bar(aes(fill=sentiment),stat = "identity")+
      theme(legend.position="none")+
      xlab("Sentiments")+ylab("Scores")+ggtitle("Uber Emotional Analysis using NRC Lexicon")
    ggplotly(uber_nrc_plot)}
  )
  
  output$bc2 <- renderPlotly({
    lyft_nrc_plot <- ggplot(data=lyft_score,aes(x=sentiment,y=n))+geom_bar(aes(fill=sentiment),stat = "identity")+
      theme(legend.position="none")+
      xlab("Sentiments")+ylab("Scores")+ggtitle("Lyft Emotional Analysis using NRC Lexicon")
    ggplotly(lyft_nrc_plot)}
  )
  
  output$summary <- renderDataTable({
    select(datasetInput(), c("text"))
  })
  
  
  
  
}  