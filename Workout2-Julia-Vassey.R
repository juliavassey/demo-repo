#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
  



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investments modules"),
     fluidRow(
       column(4, 
      sliderInput("value",
                 label= "Initial Amount:",
                  min = 1,
                  max = 100000,
                  value = 20000), 
      sliderInput(inputId = "contribution", 
                  label = "Annual Contribution:", 
                  min = 0, max=50000, 
                  value = 10000)),
      column(4,
      sliderInput(inputId = "growth",
                  label = "Growth Rate:", 
                  min = 0, 
                  max=20, 
                  value = 5),
      sliderInput(inputId = "return", 
                  label = "Return Rate:", 
                  min = 0, max=20, 
                  value = 2)),
        column(4,
      sliderInput(inputId = "duration", 
                  label = "Year:",
                  min = 1, 
                  max=50, 
                  value = 5),
      selectInput(inputId = "facet",
                  label = "Facet?",
                  choices = c("No", "Yes"))
      
      )),
      plotOutput(outputId = "distPlot"),
      verbatimTextOutput(outputId = "balance")
      )
    
    

# Define server logic required to draw a plot
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    initial_amount = input$value
    contribution = input$contribution
    rates = input$return/100
    years = input$duration
    growth_rate = input$growth/100
    
  
    
    future_value <- function(amount, rate, years) {
      amount * (1 + rate)^years
    }
    
    annuity= function(contrib,rate,years){
      contrib*((1+rate)**years-1)/rate
    }
    
    growing_annuity = function(contrib,rate, growth, years){
      contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
    }
    
    no_contrib =  c()
    fixed_contrib = c()
    growing_contrib = c()
    
      for(y in 0:years){
        fv = future_value(initial_amount, rates, y)
        fva = annuity(contribution, rates,y )
        fvga =growing_annuity(contribution, rates, growth_rate, y) 
        
        no_contrib[y+1] = fv
        fixed_contrib[y+1] = fv+fva
        growing_contrib[y+1] = fv+fvga
    }
    
    balance = data.frame("years" = 0:years, "no_contrib" = no_contrib, 
                         "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib) 
    
  
    
    
    balance_1_long = gather(balance, no_contrib:growing_contrib, key = "type_of_contrib", value = "value")
  
      
    
      #melt(data=balance, id.vars = "years")
                    
    
     #draw the plot with the specified number of bins
    
    if(input$facet == "No" ){
      ggplot(balance_1_long, aes(x=years, y=value)) +
        geom_line(aes(color = factor(type_of_contrib)))+
        guides(color=guide_legend("Type of Contribution"))
    }
    
    else{
    
    ggplot(balance_1_long, aes(x=years, y=value)) +
      facet_wrap(~factor(type_of_contrib))+
        geom_area(aes(fill = type_of_contrib),  alpha = 0.3) +  
      geom_line(aes(colour = factor(type_of_contrib)))+
        guides(colour = FALSE)+
        guides(fill = FALSE)+
        guides(fill=guide_legend("Type of Contribution"))
                 
            
    }
  })
  
  
  
  output$balance = renderPrint({
    
    initial_amount = input$value
    contribution = input$contribution
    rates = input$return/100
    years = input$duration
    growth_rate = input$growth/100
    

    
    future_value <- function(amount, rate, years) {
      amount * (1 + rate)^years
    }
    
    annuity= function(contrib,rate,years){
      contrib*((1+rate)^years-1)/rate
    }
    
    growing_annuity = function(contrib,rate, growth, years){
      contrib*((1+rate)^years-(1+growth)^years)/(rate-growth)
    }
    
    no_contrib =  c()
    fixed_contrib = c()
    growing_contrib = c()
    
    for(y in 0:years){
      fv = future_value(initial_amount, rates, y)
      fva = annuity(contribution, rates,y )
      fvga =growing_annuity(contribution, rates, growth_rate, y) 
      
      no_contrib[y+1] = fv
      fixed_contrib[y+1] = fv+fva
      growing_contrib[y+1] = fv+fvga
    }
    
    balance = data.frame("years" = 0:years, "no_contrib" = no_contrib, 
                         "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib) 
    
    balance
  }

      
      
   )

       }
# Run the application 
shinyApp(ui = ui, server = server)





