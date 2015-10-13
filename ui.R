library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Power as a function of standard deviation, effect size, significance level, and sample size"),
  
  # Sidebar with a slider input for the number of means
  sidebarLayout(
    sidebarPanel(
      sliderInput("Mn",
                    "Null hypothesis mean:",
                  min = 5,
                  max = 150,
                  value = 10),
                  
      sliderInput("Sd",
                    "Population Standard Deviation:",
                  min = 1,
                  max = 100,
                  value = 10),
                  
                  sliderInput("effectSize",
                    "Effect size:",
                  min = 1,
                  max = 100,
                  value = 10),
                  
                  sliderInput("alpha",
                    "Significance level:",
                  min = .0001,
                  max = .1000,
                  value = .05),
                  
                  sliderInput("N",
                    "Sample size:",
                  min = 1,
                  max = 50,
                  value = 20)
   
    ),
    
 

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
   )
 ))
