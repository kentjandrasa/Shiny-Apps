# Einbinden der Bibliotheken ----
if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if (!require(readxl)) {
  install.packages("readxl")
  require(readxl)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

# Daten einbinden----

# karosserie_k7 <- 

# UI----
ui <- fluidPage(
  headerPanel(title = "Karosserie-Produzent 217"),
  sidebarLayout( 
    sidebarPanel( 
      # Von
      fluidRow(
        column(
          width = 12,
          div("Von")
        )
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "von_welchem_tag",
            label = "Tag",
            choices = c(1:31),
            selected = 1
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = "von_welchem_monat",
            label = "Monat",
            choices = c(1:12),
            selected = 6
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = "von_welchem_jahr",
            label = "Jahr",
            choices = c(2012:2014),
            selected = 2012
          )
        )
      ),
      # Bis
      fluidRow(
        column(
          width = 12,
          div("Bis")
        )
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "bis_welchem_tag",
            label = "Tag",
            choices = c(1:31),
            selected = 31
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = "bis_welchem_monat",
            label = "Monat",
            choices = c(1:12),
            selected = 7
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = "bis_welchem_jahr",
            label = "Jahr",
            choices = c(2012:2014),
            selected = 2014
          )
        )
      ),
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
              Produktionszeitraum, Zeitraum frei waehlbar"),
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
    updateSelectInput(session,
                      inputId = "von_welchem_tag",
                      selected = 1)
    updateSelectInput(session,
                      inputId = "von_welchem_monat",
                      selected = 6)
    updateSelectInput(session,
                      inputId = "von_welchem_jahr",
                      selected = 2012)
    updateSelectInput(session,
                      inputId = "bis_welchem_tag",
                      selected = 31)
    updateSelectInput(session,
                      inputId = "bis_welchem_monat",
                      selected = 7)
    updateSelectInput(session,
                      inputId = "bis_welchem_jahr",
                      selected = 2014)
  })
}

shinyApp(ui = ui, server = server)













