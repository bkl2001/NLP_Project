ui <- fluidPage(theme = shinytheme("yeti"),
                titlePanel(h2(strong("Uber vs. Lyft"))),
                sidebarLayout(
                  sidebarPanel(
                    HTML('<center><img src="test2.png", height = 200, width="280"></center>'),
                    p("                                             "),
                    tabsetPanel(
                      tabPanel("Histogram Parameters",
                               sliderInput("bins", "Number of Bins:", min=10, max=50, value=30)),
                      tabPanel("Word Cloud Parameters",
                               sliderInput("freq","Minimum Frequency:", min = 1, max = 50, value = 5),
                               sliderInput("max", "Maximum Number of Words:", min = 1, max = 300, value = 200)),
                      tabPanel("Twitter Data",
                               selectInput("dataset", "Inspect Tweets:", choices =
                                             driving_companies))
                      
                      
                    )
                    
                    
                    
                  ),
                  mainPanel(
                    tabsetPanel(
                      navbarMenu("Sentiment Analysis",
                                 tabPanel("Histogram",
                                          plotlyOutput("hist"),
                                          plotlyOutput("hist1")),
                                 tabPanel("Positive",
                                          h4(strong("Uber Positive Word Cloud"), align = "center"),
                                          plotOutput("pos1"),
                                          h4(strong("Lyft Positive Word Cloud"), align = "center"),
                                          plotOutput("pos2")),
                                 tabPanel("Negative",
                                          h4(strong("Uber Negative Word Cloud"), align = "center"),
                                          plotOutput("neg1"),
                                          h4(strong("Lyft Negative Word Cloud"), align = "center"),
                                          plotOutput("neg2"))
                      ),
                      navbarMenu("Word Clouds",
                                 tabPanel("Unigram",
                                          h4(strong("Uber Unigram Word Cloud"), align = "center"),
                                          plotOutput("wc1"),
                                          h4(strong("Lyft Unigram Word Cloud"), align = "center"),
                                          plotOutput("wc2")),
                                 tabPanel("Bigram",
                                          h4(strong("Uber Bigram Word Cloud"), align = "center"),
                                          plotOutput("wc3"),
                                          h4(strong("Lyft Bigram Word Cloud"), align = "center"),
                                          plotOutput("wc4")),
                                 tabPanel("Trigram",
                                          h4(strong("Uber Trigram Word Cloud"), align = "center"),
                                          plotOutput("wc5"),
                                          h4(strong("Lyft Trigram Word Cloud"), align = "center"),
                                          plotOutput("wc6")),
                                 tabPanel("TF-IDF",
                                          h4(strong("Uber TF-IDF Word Cloud"), align = "center"),
                                          plotOutput("wcTF1"),
                                          h4(strong("Lyft TF-IDF Word Cloud"), align = "center"),
                                          plotOutput("wcTF2")),
                                 tabPanel("Commonality & Comparison",
                                          h4(strong("Uber and Lyft Commonality Cloud"), align = "center"),
                                          plotOutput("com1"),
                                          h4(strong("Uber and Lyft Comparison Cloud"), align = "center"),
                                          plotOutput("com2"))),
                      navbarMenu("Emotional Analysis",
                                 tabPanel("Radar Chart",
                                          h4(strong("Uber and Lyft Radar Chart"), align = "center"),
                                          chartJSRadarOutput("rc1", height = 280)),
                                 tabPanel("Bar Chart",
                                          plotlyOutput("bc1"),
                                          plotlyOutput("bc2"))
                      ),
                      tabPanel("Tweets",
                               dataTableOutput("summary")
                      ),
                      tabPanel("ReadMe",
                               htmlOutput("read")
                               
                               
                      )
                    )
                    
                  )
                )
)
