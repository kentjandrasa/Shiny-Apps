# Einbinden der Bibliotheken ----
if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}

if (!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

# Daten einbinden...----


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
                     format = "dd.mm.yyyy",
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
          title = "Produktionsmenge",
          div("Produktionsmenge der Karosserie “K7” je Monat über den gesamten 
              Produktionszeitraum"),
          fluidRow(
            plotOutput(
              outputId = "produktionsmenge"
            )
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
  
  # reset button
  observeEvent(input$reset, {
    updateDateRangeInput(session,
                         inputId = "zeitintervall",
                         start  = "2012-06-01",
                         end    = "2014-07-31",
                         min    = "2012-06-01",
                         max    = "2014-07-31")
  })
  
  # Plot1: produktionsmenge...-----
  
}



shinyApp(ui = ui, server = server)













