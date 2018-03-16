library(shiny)
dat <- data.frame(date = seq.Date(Sys.Date(),by = 1,length.out = 5),
                  temp = runif(5))

ui <- fluidPage(
  fluidRow(column(4,dataTableOutput('dto')),
           column(4,tableOutput('to')))
)

server <- function(input,output){
  
  output$dto <- renderDataTable({dat})
  dat$date <- format(dat$date,'%Y-%m-%d')
  output$to <- renderTable(dat)
  
}

runApp(list(ui = ui,server = server))
