library(shiny)

runApp(
  list(
    ui = fluidPage(
      dateRangeInput("dates", 
                     "Date range",
                     start = "2015-01-01", 
                     end = as.character(Sys.Date())),
      textOutput("DateRange")
    ),
    
    server = function(input, output){
      output$DateRange <- renderText({
        # make sure end date later than start date
        validate(
          need(input$dates[2] > input$dates[1], "end date is earlier than start date"
          )
        )
        
        # make sure greater than 2 week difference
        validate(
          need(difftime(input$dates[2], input$dates[1], "days") > 14, "date range less the 14 days"
          )
        )
        
        paste("Your date range is", 
              difftime(input$dates[2], input$dates[1], units="days"),
              "days")
      })
    }
  ))