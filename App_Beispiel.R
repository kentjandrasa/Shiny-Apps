# Einbinden der Bibliotheken ----
if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if (!require(scales)) {
  install.packages("scales")
  require(scales)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if (!require(zoo)) {
  install.packages("zoo")
  require(zoo)
}

# Datensatz----

hallo <- data.frame(date = c("2012-06-01", "2013-01-06", "2013-01-06", "2013-02-20", 
                             "2014-01-06", "2014-04-30", "2014-05-31", "2014-07-31"),
                    anzahl = c(10, 2, 5, 7, 8, 9, 4, 5),
                    verlassen = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
                    angekommen = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))

hallo$date <- ymd(hallo$date)

hallo$year <- year(hallo$date)

hallo$month <- month(hallo$date)

hallo$day <- day(hallo$date)

hallo$monthyear <- paste(hallo$year, hallo$month, hallo$day, sep = "-") %>%
  ymd %>%
  as.yearmon("%m-%Y") %>%
  as.Date

hallo <- as.tibble(hallo)

farben <- list("Schwarz" = "black", "Grau" = "grey", "Rot" = "red", 
               "Magenta" = "magenta", "Braun" = "brown", "Orange" = "orange", 
               "Rosa" = "pink","Grün" = "green","Gelb" = "yellow",
               "Dunkelblau" = "dark blue", "Blau" = "blue", "Hellblau" = "light blue")

# UI----
ui <- fluidPage(
  headerPanel(title = "Karosserie-Produzent 217"),
  sidebarLayout( 
    sidebarPanel( 
      dateRangeInput(inputId = "zeitintervall",
                     label = "Zeitintervall auswählen:",
                     language = "de",
                     start  = "2012-06-01",
                     end    = "2014-07-31",
                     min    = "2012-06-01",
                     max    = "2014-07-31",
                     format = "yyyy-mm-dd",
                     weekstart = 1,
                     separator = " bis "),
      actionButton(
        inputId = "reset",
        label = "Zurücksetzen"
      )
    ), 
    # Main panel
    mainPanel(
      # erster Reiter
      tabsetPanel(
        tabPanel(
          title = "Daten",
          tableOutput(
            outputId = "daten_final1"
          ),
          tableOutput(
            outputId = "daten_final2"
          )
          ),
        tabPanel(
          title = "Produktionsmenge",
          div("Produktionsmenge der Karosserie “K7” je Monat über den gesamten 
              Produktionszeitraum, Zeitraum frei waehlbar"),
          fluidRow(
            plotOutput(
              outputId = "produktionsmenge"
            )
          ),
          fluidRow(
            plotOutput(
              outputId = "produktionsmenge2"
            )
          ),
          div("Farben auswählen:"),
          selectInput(
            inputId = "januar",
            label = "Januar",
            choices = farben
          ),
          selectInput(
            inputId = "februar",
            label = "Februar",
            choices = farben
          ),
          selectInput(
            inputId = "maerz",
            label = "März",
            choices = farben
          ),
          selectInput(
            inputId = "april",
            label = "April",
            choices = farben
          ),
          selectInput(
            inputId = "mai",
            label = "Mai",
            choices = farben
          ),
          selectInput(
            inputId = "juni",
            label = "Juni",
            choices = farben
          ),
          selectInput(
            inputId = "juli",
            label = "Juli",
            choices = farben
          ),
          selectInput(
            inputId = "august",
            label = "August",
            choices = farben
          ),
          selectInput(
            inputId = "september",
            label = "September",
            choices = farben
          ),
          selectInput(
            inputId = "oktober",
            label = "Oktober",
            choices = farben
          ),
          selectInput(
            inputId = "november",
            label = "November",
            choices = farben
          ),
          selectInput(
            inputId = "dezember",
            label = "Dezember",
            choices = farben
          )
          ),
        # zweiter Reiter
        tabPanel(
          title = "Logistik",
          div("wie viele Karosserien am jeweiligen Tag den Warenausgang bereits 
              verlassen haben, aber noch nicht angekommen sind. zusätzlich 
              eine horizontale Linie in das Diagramm, deren Position für den gewählten 
              Zeitraum die durchschnittliche Anzahl an Komponenten, die unterwegs sind"),
          fluidRow(
            plotOutput(
              outputId = "logistik"
            )
          )
          )
    )
)
)
) 


# Server ----
server <- function(session, input, output){
  
  hallo$date <- format(hallo$date,'%Y-%m-%d')
#  hallo$month <- format(hallo$month,'%Y-%m-%d')
  hallo$monthyear <- format(hallo$monthyear, '%Y-%m-%d')
  
  # Daten
  output$daten_final1 <- renderTable({
    data1()
  })
  
  data1 <- reactive({
    data1 <- filter_date()
  })
  
  filter_date <- reactive({
    filter(hallo, date >= input$zeitintervall[[1]] & date <= input$zeitintervall[[2]])
#    min <- filter(hallo, date >= input$zeitintervall[[1]])
#    max <- filter(hallo, date <= input$zeitintervall[[2]])
#    intersect(min, max)
  })     
  
  output$daten_final2 <- renderTable({
    data2()
  })
  
  data2 <- reactive({
    data2 <- filter_lieferung()
  })
  
  filter_lieferung <- reactive({
    filter(hallo,  (date >= input$zeitintervall[[1]] & date <= input$zeitintervall[[2]]) & 
      (verlassen == TRUE & angekommen == FALSE))
  })
  
  # reset button
  observeEvent(input$reset, {
    updateDateRangeInput(session,
                         inputId = "zeitintervall",
                         start  = "2012-06-01",
                         end    = "2014-07-31",
                         min    = "2012-06-01",
                         max    = "2014-07-31")
    
  })
  
  # output produktionsmenge
  output$produktionsmenge <- renderPlot(
    ggplot(data = data1(),
           mapping = aes(x = month, y = anzahl, fill = factor(month))) +
#      geom_bar() +
      stat_summary(fun.y = sum, # adds up all observations for the month
                   geom = "bar") + 
      scale_fill_manual("Monat", values = c("1" = input$januar, "2" = input$februar, 
                                             "3" = input$maerz, "4" = input$april, 
                                             "5" = input$mai, "6" = input$juni,
                                             "7" = input$juli, "8" = input$august, 
                                             "9" = input$september, "10" = input$oktober,
                                             "11" = input$november, "12" = input$dezember)) #+
#      scale_x_date(
#        labels = date_format("%Y-%m"),
#        date_breaks = "1 month") # custom x-axis labels
  )
  # output produktionsmenge
  output$produktionsmenge2 <- renderPlot(
    ggplot(data = data1(),
           mapping = aes(x = monthyear, y = anzahl, fill = factor(month))) +
      #      geom_bar() +
      stat_summary(fun.y = sum, # adds up all observations for the month
                   geom = "bar") + 
      scale_fill_manual("Monat", values = c("1" = input$januar, "2" = input$februar, 
                                            "3" = input$maerz, "4" = input$april, 
                                            "5" = input$mai, "6" = input$juni,
                                            "7" = input$juli, "8" = input$august, 
                                            "9" = input$september, "10" = input$oktober,
                                            "11" = input$november, "12" = input$dezember)))
  # output logistik
  output$logistik <- renderPlot(
    ggplot(data = data2(), 
           mapping = aes(x = date, y = anzahl)) + 
      stat_summary(fun.y = sum, geom = "bar") +
      geom_hline(aes(yintercept = mean(anzahl)))
  )
}

shinyApp(ui = ui, server = server)