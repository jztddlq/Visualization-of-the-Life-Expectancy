library(shiny)
library(shinythemes)
# Define UI to show the correlation application
shinyUI(
  # pageWithSidebar
  fluidPage
  (
    theme=shinytheme('superhero'),
  # Application title
  headerPanel("Visualization of the Life Expectancy & Healthcare Expenditure Indicators in WHO Dataset"),
  
  # Sidebar with controls to select the threshhold of the correlation to filter variable who has strong relation
  # and then show the variable in the selectinput box
  sidebarPanel(
    conditionalPanel(
      condition = "input.tabs == 'A'",
      numericInput("threshhold", "the maxumum correlation to show:", 0.5,max=1,step=0.1),
      selectInput("variable", "Variable for histogram:",choice=1:3)
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'B'",
      selectInput("vvv", "variable for clustering:",choice=1:3,multiple = TRUE,selected=1),
      numericInput("k", "the number of clusters:", 7)
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'C'",
      selectInput("map", "select the visualization map:",choice=1:3),
      br()
    )
    
  ),
  
  # Show the caption and plot of the plot in page 1
  mainPanel(
    
    tabsetPanel(
      id = "tabs",
      tabPanel("Variable Filter  ", value = 'A',
               h4("Correlation Matrix Map"),
               
               plotOutput("corplot"),
             
               h4("Histogram of the selected variable"),
               h6("this plot is visualization of variable distribution,
                  give people intuitive perception of the variable, it is useful for next process."),
               plotOutput("hplot")),
      tabPanel("Variable and K Selection ", value = 'B',
               h4("K plot of the clustering result"),
               
               plotOutput("kplot"),
               h4("Clustering plot of the clustering result"),
               plotOutput("cplot")),
      tabPanel("MAP and Circle Plot",value = 'C',
               h4(textOutput("caption")),
               plotOutput("clustervisual"))
    )
    
  )
))

