# PART 3.2
# written by Dot_Blue

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#####################################################################
#Adding packages
if("shiny" %in% rownames(installed.packages()) == FALSE)
{install.packages("shiny")}

if("shinyFiles" %in% rownames(installed.packages()) == FALSE)
{install.packages("shinyFiles")}

#First we need to define the Rand_Gen functions! 
source('randomFunctions.R')
source('mleFunctions.R')


library(shiny)
library(shinyFiles)

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Probability Distributions"),
  tabsetPanel(
    #TAB 2
    tabPanel("dugen", fluid = TRUE,
       sidebarLayout(
         sidebarPanel(
           helpText("In this distribution we generate uniform and discrete RVs"),
           sliderInput("bins2",
                       "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30),
           numericInput("TAB2_Min","Min",0),
           numericInput("TAB2_Max","Max",100),
           actionButton("plotTAB2", "Draw Plot"),
           actionButton("RVTAB2", "Give Random"),
           shinyFilesButton("TAB2_GetFile", "Choose a file" ,
                            title = "Please select a file:", multiple = FALSE,
                            buttonType = "default", class = NULL)
         ),
         
         # Show a plot of the generated distribution
         mainPanel(
           plotOutput("distPlot2"),
           textOutput("RV2")
         )
       )
    ),
    #TAB 3
    tabPanel("cugen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("In this distribution we generate uniform and continuous RVs between 0 and 1"),
                 sliderInput("bins3",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 actionButton("plotTAB3", "Draw Plot"),
                 actionButton("RVTAB3", "Give Random")
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot3"),
                 textOutput("RV3")
               )
             )
    ),
    #TAB 4
    tabPanel("brgen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("The bernoulli distribution with the probability input"),
                 numericInput("TAB4_P","Probability of Success",0.01,step=0.1),
                 actionButton("plotTAB4", "Draw Plot"),
                 actionButton("RVTAB4", "Give Random"),
                 shinyFilesButton("TAB4_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot4"),
                 textOutput("RV4")
               )
             )
    ),
    #TAB 5
    tabPanel("bigen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("The binomial distribution which gives the output of a set of bernoulli events"),
                 sliderInput("bins5",
                             "Number of bins:",
                             min = 1,
                             max = 20,
                             value = 10),
                 numericInput("TAB5_n","n",25,step=1),
                 numericInput("TAB5_p","p",0.6,step=0.1),
                 actionButton("plotTAB5", "Draw Plot"),
                 actionButton("RVTAB5", "Give Random"),
                 shinyFilesButton("TAB5_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot5"),
                 textOutput("RV5")
               )
             )
    ),
    #TAB 6
    tabPanel("gegen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("The geometric distribution which is the number of failures in Bernoulli trials"),
                 sliderInput("bins6",
                             "Number of bins:",
                             min = 1,
                             max = 20,
                             value = 10),
                 numericInput("TAB6_l","lambda",0.1,step=0.1),
                 actionButton("plotTAB6", "Draw Plot"),
                 actionButton("RVTAB6", "Give Random"),
                 shinyFilesButton("TAB6_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot6"),
                 textOutput("RV6")
               )
             )
    ),
    #TAB 7
    tabPanel("expgen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("The exponential distribution is used to model waiting times and memoryless processes"),
                 sliderInput("bins7",
                             "Number of bins:",
                             min = 1,
                             max = 100,
                             value = 40),
                 numericInput("TAB7_l","lambda",4,step=0.1),
                 actionButton("plotTAB7", "Draw Plot"),
                 actionButton("RVTAB7", "Give Random"),
                 shinyFilesButton("TAB7_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot7"),
                 textOutput("RV7")
               )
             )
    ),
    #TAB 8
    tabPanel("gagen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("The gamma distribution RV is the Summation of k sets of exponential RVs"),
                 sliderInput("bins8",
                             "Number of bins:",
                             min = 1,
                             max = 20,
                             value = 10),
                 numericInput("TAB8_l","lambda",4,step=0.1),
                 numericInput("TAB8_k","k",25,step=1),
                 actionButton("plotTAB8", "Draw Plot"),
                 actionButton("RVTAB8", "Give Random"),
                 shinyFilesButton("TAB8_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot8"),
                 textOutput("RV8")
               )
             )
    ),
    #TAB 9
    tabPanel("pogen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("If an exponentially distributed variable is modeled as the waiting time before an arrival, the Poisson
distributed variable can be modeled as the number of arrivals during a period of time of length t"),
                 sliderInput("bins9",
                             "Number of bins:",
                             min = 1,
                             max = 20,
                             value = 10),
                 numericInput("TAB9_l","lambda",4,step=0.1),
                 numericInput("TAB9_t","t",10,step=1),
                 actionButton("plotTAB9", "Draw Plot"),
                 actionButton("RVTAB9", "Give Random"),
                 shinyFilesButton("TAB9_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot9"),
                 textOutput("RV9")
               )
             )
    ),
    #TAB 10
    tabPanel("nogen", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("The normal distribution is like the binomial distribution but its continuous
                          and has a mean(u) and a variance(s)"),
                 sliderInput("bins10",
                             "Number of bins:",
                             min = 1,
                             max = 20,
                             value = 10),
                 numericInput("TAB10_u","mean(u)",2,step=0.1),
                 numericInput("TAB10_s","variance(s)",0.5,step=0.1),
                 actionButton("plotTAB10", "Draw Plot"),
                 actionButton("RVTAB10", "Give Random"),
                 shinyFilesButton("TAB10_GetFile", "Choose a file" ,
                                  title = "Please select a file:", multiple = FALSE,
                                  buttonType = "default", class = NULL)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot10"),
                 textOutput("RV10")
               )
             )
    )
    
  )
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #File sample
  volumes = getVolumes()
  observe({  
    shinyFileChoose(input, "TAB2_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB4_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB5_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB6_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB7_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB8_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB9_GetFile", roots = volumes, session = session)
    shinyFileChoose(input, "TAB10_GetFile", roots = volumes, session = session)
    
    if(!is.null(input$TAB2_GetFile)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$TAB2_GetFile)
      
      x <- dugen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB2_Min",value = x[1])
      updateNumericInput(session,"TAB2_Max",value = x[2])
    }
    else if(!is.null(input$TAB4_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB4_GetFile)
      x <- brgen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB4_P",value = x[1])
    }
    else if(!is.null(input$TAB5_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB5_GetFile)
      x <- bigen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB5_n",value = x[1])
      updateNumericInput(session,"TAB5_p",value = x[2])
    }
    else if(!is.null(input$TAB6_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB6_GetFile)
      x <- gegen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB6_l",value = x[1])
    }
    else if(!is.null(input$TAB7_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB7_GetFile)
      x <- expgen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB7_l",value = x[1])
    }
    else if(!is.null(input$TAB8_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB8_GetFile)
      x <- gagen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB8_l",value = x[1])
      updateNumericInput(session,"TAB8_k",value = x[2])
    }
    else if(!is.null(input$TAB9_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB9_GetFile)
      x <- pogen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB9_l",value = x[1])
      updateNumericInput(session,"TAB9_t",value = 1)
    }else if(!is.null(input$TAB10_GetFile)){
      file_selected<-parseFilePaths(volumes, input$TAB10_GetFile)
      x <- nogen_mle(as.character(file_selected$datapath))
      updateNumericInput(session,"TAB10_u",value = x[1])
      updateNumericInput(session,"TAB10_s",value = x[2])
    }
  })
  
  #TAB2 dugen 
  DrawPlot2 <- eventReactive(input$plotTAB2, {
    dugenPlotVal <- dugen(input$TAB2_Min,input$TAB2_Max,1000)
    
    bins <- seq(min(dugenPlotVal), max(dugenPlotVal), length.out = input$bins2 + 1)
    hist(dugenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV2 <- eventReactive(input$RVTAB2,{
    THE_RV <- dugen(input$TAB2_Min,input$TAB2_Max,1,as.numeric(Sys.time()))
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot2 <- renderPlot({
    DrawPlot2()
  })
  output$RV2 <- renderText({
    GiveRV2()
  })
  
  
  #TAB3 cugen
  DrawPlot3 <- eventReactive(input$plotTAB3, {
    cugenPlotVal <- c()
    for(i in 1:1000){
      cugenPlotVal <- c(cugenPlotVal, cugen(i))
    }
    
    bins <- seq(min(cugenPlotVal), max(cugenPlotVal), length.out = input$bins3 + 1)
    hist(cugenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV3 <- eventReactive(input$RVTAB3,{
    THE_RV <- cugen()
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot3 <- renderPlot({
    DrawPlot3()
  })
  output$RV3 <- renderText({
    GiveRV3()
  })
  
  #TAB4 brgen
  DrawPlot4 <- eventReactive(input$plotTAB4, {
    brgenPlotVal <- c()
    for(i in 1:1000){
      brgenPlotVal <- c(brgenPlotVal, brgen(input$TAB4_P))
    }
    
    bins <- seq(min(brgenPlotVal), max(brgenPlotVal), length.out = 3)
    hist(brgenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV4 <- eventReactive(input$RVTAB4,{
    THE_RV <- brgen(input$TAB4_P)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot4 <- renderPlot({
    DrawPlot4()
  })
  output$RV4 <- renderText({
    GiveRV4()
  })
   
  #TAB5 bigen
  DrawPlot5 <- eventReactive(input$plotTAB5, {
    bigenPlotVal <- c()
    for(i in 1:200){
      bigenPlotVal <- c(bigenPlotVal, bigen(input$TAB5_n,input$TAB5_p))
    }
    
    bins <- seq(min(bigenPlotVal), max(bigenPlotVal), length.out = input$bins5 + 1)
    hist(bigenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV5 <- eventReactive(input$RVTAB5,{
    THE_RV <- bigen(input$TAB5_n,input$TAB5_p)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot5 <- renderPlot({
    DrawPlot5()
  })
  output$RV5 <- renderText({
    GiveRV5()
  })
   
  #TAB6 gegen
  DrawPlot6 <- eventReactive(input$plotTAB6, {
    gegenPlotVal <- c()
    for(i in 1:200){
      gegenPlotVal <- c(gegenPlotVal, gegen(input$TAB6_l))
    }
    
    bins <- seq(min(gegenPlotVal), max(gegenPlotVal), length.out = input$bins6 + 1)
    hist(gegenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV6 <- eventReactive(input$RVTAB6,{
    THE_RV <- gegen(input$TAB6_l)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot6 <- renderPlot({
    DrawPlot6()
  })
  output$RV6 <- renderText({
    GiveRV6()
  })
  
  #TAB7 expgen
  DrawPlot7 <- eventReactive(input$plotTAB7, {
    expgenPlotVal <- c()
    for(i in 1:10000){
      expgenPlotVal <- c(expgenPlotVal, expgen(input$TAB7_l))
    }
    
    bins <- seq(min(expgenPlotVal), max(expgenPlotVal), length.out = input$bins7 + 1)
    hist(expgenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV7 <- eventReactive(input$RVTAB7,{
    THE_RV <- expgen(input$TAB7_l)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot7 <- renderPlot({
    DrawPlot7()
  })
  output$RV7 <- renderText({
    GiveRV7()
  })
  
  #TAB8 gagen
  DrawPlot8 <- eventReactive(input$plotTAB8, {
    gagenPlotVal <- c()
    for(i in 1:200){
      gagenPlotVal <- c(gagenPlotVal, gagen(input$TAB8_l,input$TAB8_k))
    }
    
    bins <- seq(min(gagenPlotVal), max(gagenPlotVal), length.out = input$bins8 + 1)
    hist(gagenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV8 <- eventReactive(input$RVTAB8,{
    THE_RV <- gagen(input$TAB8_l,input$TAB8_k)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot8 <- renderPlot({
    DrawPlot8()
  })
  output$RV8 <- renderText({
    GiveRV8()
  })
  
  #TAB9 pogen
  DrawPlot9 <- eventReactive(input$plotTAB9, {
    pogenPlotVal <- c()
    for(i in 1:100){
      pogenPlotVal <- c(pogenPlotVal, pogen(input$TAB9_l,input$TAB9_t))
    }
    
    bins <- seq(min(pogenPlotVal), max(pogenPlotVal), length.out = input$bins9 + 1)
    hist(pogenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV9 <- eventReactive(input$RVTAB9,{
    THE_RV <- pogen(input$TAB9_l,input$TAB9_t)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot9 <- renderPlot({
    DrawPlot9()
  })
  output$RV9 <- renderText({
    GiveRV9()
  })
  
  #TAB10 nogen
  DrawPlot10 <- eventReactive(input$plotTAB10, {
    nogenPlotVal <- c()
    for(i in 1:50){
      nogenPlotVal <- c(nogenPlotVal, nogen(input$TAB10_u,input$TAB10_s))
    }
    
    bins <- seq(min(nogenPlotVal), max(nogenPlotVal), length.out = input$bins10 + 1)
    hist(nogenPlotVal, breaks = bins, col = 'blue', border = 'lightblue')
  })
  GiveRV10 <- eventReactive(input$RVTAB10,{
    THE_RV <- nogen(input$TAB10_u,input$TAB10_s)
    paste("A random variable with this distribution is: ",
          THE_RV)
  })
  output$distPlot10 <- renderPlot({
    DrawPlot10()
  })
  output$RV10 <- renderText({
    GiveRV10()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

