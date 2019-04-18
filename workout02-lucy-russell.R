###Workout2 R Script

library(shiny)
library(ggplot2)
library(tidyr)

ui <- fluidPage(
  h1("Investment Modalities Simulation"),
  fluidRow(
    column(4,
           sliderInput("initial",
                       "Initial amount",
                       min = 0,
                       max = 100000,
                       value = 1000,
                       step = 500), 
           sliderInput("contribution",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500)),
    column(4,
           sliderInput("return",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = .1),
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = .1)),
    column(4,
           sliderInput("years", 
                       "Number of years",
                       min = 0, 
                       max = 50,
                       value = 20,
                       step = 1),
           selectInput("facet",
                       "Facet",
                       choices = list("Yes" = 1, "No" = 2),
                       selected = 2))
    
  ),
  hr(),
  h4("Timeline"),
  fluidRow(
    column(12, plotOutput("distPlot"))),
  hr(),
  h4("Balances"),
  fluidRow(
    column(12, verbatimTextOutput("returns"))
  ))


server <- function(input, output){
  
  investment <- reactive({
    fixed_contrib <- rep(0, input$years)
    growing_contrib <- rep(0, input$years)
    no_contrib <- rep(0, input$years)
    year <- c(0:input$years)
    
    for(n in 0:input$years){
      term1 <- input$initial*(1 + input$return/100)^n
      no_contrib[n+1] <- term1
    }
    for(n in 0:input$years){
      term2 <- input$initial*(1 + input$return/100)^n + input$contribution*(((1 + input$return/100)^n - 1)/(input$return/100))
      fixed_contrib[n+1] <- term2
    }
    for(n in 0:input$years){
      term3 <- input$initial*(1 + input$return/100)^n + input$contribution*(((1 + input$return/100)^n - (1 + input$growth/100)^n)/(input$return/100 - input$growth/100))
      growing_contrib[n+1] <- term3
    }
    investment <- data.frame(
      year = 0:input$years,
      fixed_contrib = fixed_contrib,
      no_contrib = no_contrib,
      growing_contrib = growing_contrib
    )
    return(investment)
  })
  plot1 <- reactive({
    if(input$facet == 1){
      facet_data <- gather(investment(), key = "modalities", value = "value", c("no_contrib", "fixed_contrib", "growing_contrib"))
      ggplot(facet_data, aes(x = year, y = value, color = modalities, fill = modalities)) +
        geom_area(stat = 'identity', alpha = 0.2) +
        facet_wrap( ~ modalities) +
        labs(title = "Investing Modalities", x = "Year", y = "US Dollars") +
        theme_bw()
    }else{
      ggplot(investment(), aes(x = year)) +
        geom_line(aes(y = no_contrib, color = "No Contribution")) +
        geom_line(aes(y = fixed_contrib, color = "Fixed Contribution")) +
        geom_line(aes(y = growing_contrib, color = "Growing Contribution")) +
        labs(title = "Investing Modalities", x = "Year", y = "US Dollars") +
        scale_color_manual(name = "modalities", values = c("red", "blue", "green")) +
        theme_bw()
    }
  })
  output$distPlot <- renderPlot({
    plot1()
  })
  output$returns <- renderPrint({
    investment()
  })
}

shinyApp(ui = ui, server = server)