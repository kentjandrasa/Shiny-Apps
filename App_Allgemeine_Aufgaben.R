setwd("~/IDA/Dateien_Case_Study5/Logistikverzug")

# Einbinden der Bibliotheken ----
if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
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

if (!require(zoo)) {
  install.packages("zoo")
  require(zoo)
}

if (!require(colourpicker)) {
  install.packages("colourpicker")
  require(colourpicker)
}

# Daten einbinden ----

comp <- read.csv("Komponente_K7.csv", header = TRUE, sep = ";") %>%
  mutate(IDNummer = as.character(IDNummer), Produktionsdatum = as.Date(Produktionsdatum),
         Herstellernummer = as.integer(Herstellernummer), Werksnummer = as.integer(Werksnummer),
         Fehlerhaft = as.logical(Fehlerhaft))

comp_delay <- read.csv("Logistikverzug_K7.csv", header = TRUE, sep = ";") %>%
  mutate(IDNummer = as.character(IDNummer), Wareneingang = as.Date(Wareneingang, format="%d.%m.%Y"))

Logistikverzug <- comp %>%
  inner_join(comp_delay, by = "IDNummer") %>%
  mutate(Durchlaufzeit = -(Produktionsdatum - Wareneingang))

# Daten fuer Produktionsmenge ----

k7 <- Logistikverzug %>%
  select(Produktionsdatum, Wareneingang)

# Erstellen [306490 X 1] Matrix mit 1 gefuellt
komponenteAnzahl <- matrix(1, nrow = nrow(k7), ncol = 1)

# zwei Matrizen zusammenbinden
k7 <- cbind(k7, komponenteAnzahl)

k7 <- as.tibble(k7)

# Gruppieren nach dem Produktionsdatum, Komponentenanzahl am gleichen Tag werden
# aufsummiert
summeK7 <- k7 %>% 
  group_by(Produktionsdatum) %>% 
  summarise_all(funs(sum))

# Jahre, Monate und Daten vom Produktionsdatum in einer neuen Spalten auflisten
summeK7$year <- year(summeK7$Produktionsdatum)

summeK7$month <- month(summeK7$Produktionsdatum)

summeK7$day <- day(summeK7$Produktionsdatum)

# Eine neue Spalte fuer den Monat als Datumformat
summeK7$monthyear <- paste(summeK7$year, summeK7$month, summeK7$day, sep = "-") %>%
  ymd %>%
  as.yearmon("%m-%Y") %>%
  as.Date

# Daten fuer Logistikverzuege ----

# Wann verlassen die Karosserien den Warenausgang?
verlassen <- summeK7 %>%
  select(Datum = Produktionsdatum, Warenausgang = komponenteAnzahl)

angekommen <- k7 %>%
  group_by(Wareneingang) %>%
  summarise_all(funs(sum))

# Wann sind die Karosserien angekommen?
angekommen <- angekommen %>%
  select(Datum = Wareneingang, Wareneingang = komponenteAnzahl)

# zusammenfuehren der beiden Dantensaetze
logistikK7 <- full_join(verlassen, angekommen, by = "Datum")

# NA Werte in Warenausgang und -eingang mit 0 ersetzen
logistikK7[is.na(logistikK7)] <- 0 

logistikK7$month <- month(logistikK7$Datum)

# Berechnung der Karosserien, die noch unterwegs sind pro Tag
x <- c()

x[1] <- logistikK7$Warenausgang[1] - logistikK7$Wareneingang[1]

loop <- nrow(logistikK7) - 1

for (i in 1:loop) {
  x[i + 1] <- x[i] + logistikK7$Warenausgang[i + 1] - logistikK7$Wareneingang[i + 1]
}

# Vektor x mit dem Datensatz zusammenbinden
logistikK7 <- cbind(logistikK7, unterwegs = x)

# UI----
ui <- fluidPage(
  headerPanel(title = "Komponente K7"),
  sidebarLayout( 
    sidebarPanel( 
      # Widget fuer Datumauswahl
      dateRangeInput(inputId = "zeitintervall",
                     label = "Zeitintervall im Format yyyy-mm-dd eingeben/auswählen:",
                     language = "de",
                     start  = "2008-11-12",
                     end    = "2016-11-20",
                     min    = "2008-11-12",
                     max    = "2016-11-20",
                     format = "yyyy-mm-dd",
                     weekstart = 1,
                     separator = " bis "),
      # Button fuer das Zuruecksetzen
      actionButton(
        inputId = "reset",
        label = "Datum zurücksetzen"
      ),
      p(), # eine leere Zeile hinzufuegen
      strong("Farben auswählen:"), # fett geschrieben
      p(), # eine leere Zeile hinzufuegen
      # Widget fuer Farbenauswahl jedes Monats
      fluidRow(
        column(
          width = 4,
          colourInput(
            inputId = "januar",
            label = "Januar",
            value = "gray"
            )
          ),
        column(
        width = 4,
        colourInput(
          inputId = "februar",
          label = "Februar",
          value = "gray"
          )
        ),
        column(
          width = 4,
        colourInput(
          inputId = "maerz",
          label = "März",
          value = "gray"
          )
        )),
      fluidRow(
        column(
          width = 4,
          colourInput(
            inputId = "april",
            label = "April",
            value = "gray"
          )
        ),
        column(
          width = 4,
          colourInput(
            inputId = "mai",
            label = "Mai",
            value = "gray"
          )
        ),
        column(
          width = 4,
          colourInput(
            inputId = "juni",
            label = "Juni",
            value = "gray"
          )
        )),
      fluidRow(
        column(
          width = 4,
          colourInput(
            inputId = "juli",
            label = "Juli",
            value = "gray"
          )
        ),
        column(
          width = 4,
          colourInput(
            inputId = "august",
            label = "August",
            value = "gray"
          )
        ),
        column(
          width = 4,
          colourInput(
            inputId = "september",
            label = "September",
            value = "gray"
          )
        )),
      fluidRow(
        column(
          width = 4,
          colourInput(
            inputId = "oktober",
            label = "Oktober",
            value = "gray"
          )
        ),
        column(
          width = 4,
          colourInput(
            inputId = "november",
            label = "November",
            value = "gray"
          )
        ),
        column(
          width = 4,
          colourInput(
            inputId = "dezember",
            label = "Dezember",
            value = "gray"
          )
        ))
    ), 
    # Main panel
    mainPanel(
      # Daten 
      tabsetPanel(
        # wurde fuer Kontrolle der Daten benutzt, nicht relevant fuer die Aufgaben
#        tabPanel(
#          title = "Daten",
#          tableOutput(
#            outputId = "daten_final1"
#          ),
#          tableOutput(
#            outputId = "daten_final2"
#          )
#        ),
        # erster Reiter
        tabPanel(
          title = "Produktionsmenge",
          div("Produktionsmenge der Karosserie “K7” je Monat über den gesamten 
              Produktionszeitraum."),
          p(),
          # passt die Datumauswahl nicht zur Bedingung, wird ein ERROR Satz ausgegeben
          fluidRow(
            textOutput(
              outputId = "error1"
            )
          ),
          fluidRow(
            plotOutput(
              outputId = "produktionsmenge" ,
              height = "1500px"
            )
          )
          ),
        # zweiter Reiter
        tabPanel(
          title = "Logistik",
          div("Anzahl der Karosserien am jeweiligen Tag, die den Warenausgang bereits 
              verlassen haben, aber noch nicht angekommen sind. Die rote horizontale Linie 
              in dem Diagramm bezeichnet die durchschnittliche Anzahl an Komponenten, 
              die noch unterwegs sind."),
          p(),
          # passt die Datumauswahl nicht zur Bedingung, wird ein ERROR Satz ausgegeben
          fluidRow(
            textOutput(
              outputId = "error2"
            )
          ),
          fluidRow(
            plotOutput(
              outputId = "logistik",
              height = "750px"
            )
          )
          )
    )
)
)
  ) 



# Server ----
server <- function(session, input, output){
  
  # Datentypen fuer die Ausgabe kontrollieren
  summeK7$Produktionsdatum <- format(summeK7$Produktionsdatum,'%Y-%m-%d')
  summeK7$monthyear <- format(summeK7$monthyear, '%Y-%m-%d')
  
  # reaktive Daten fuer ersten Reiter
  output$daten_final1 <- renderTable({
    data1()
  })
 
  data1 <- reactive({
    data1 <- filter_date()
  })
  
  # Filter Bedingung
  filter_date <- reactive({
    filter(summeK7, Produktionsdatum >= input$zeitintervall[[1]] & 
             Produktionsdatum <= input$zeitintervall[[2]])
  })   
  
  # reaktive Daten fuer zweiten Reiter
  output$daten_final2 <- renderTable({
    data2()
  })
  
  data2 <- reactive({
    data2 <- filter_logistik()
  })
  
  # Filter Bedingung
  filter_logistik <- reactive({
    filter(logistikK7, Datum >= input$zeitintervall[[1]] & 
             Datum <= input$zeitintervall[[2]])
  })
  
  # reset button
  observeEvent(input$reset, {
    updateDateRangeInput(session,
                         inputId = "zeitintervall",
                         start  = "2008-11-12",
                         end    = "2016-11-20",
                         min    = "2008-11-12",
                         max    = "2016-11-20")
  })
  
  # Plot1: Produktionsmenge -----
  # ERROR Satz 1
  output$error1 <- renderText({
    
    validate(
      need(input$zeitintervall[[2]] >= input$zeitintervall[[1]], 
           "Enddatum ist früher als Startdatum. Bitte neues Datum wählen.")
    )
  })
    
  output$produktionsmenge <- renderPlot(
    ggplot(data = data1(),
           mapping = aes(x = monthyear, y = komponenteAnzahl, fill = factor(month))) +
      stat_summary(fun.y = sum, # komponenteAnzahl aufsummieren
                   geom = "bar", width = 0.25) + 
      labs(x = "Monat", y = "Komponentenanzahl") +
      scale_y_continuous(breaks = seq(0, 4000, 250)) +
      scale_fill_manual("Monat", values = c("1" = input$januar, "2" = input$februar, 
                                            "3" = input$maerz, "4" = input$april, 
                                            "5" = input$mai, "6" = input$juni,
                                            "7" = input$juli, "8" = input$august, 
                                            "9" = input$september, "10" = input$oktober,
                                            "11" = input$november, "12" = input$dezember),
                        labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", 
                                   "Juli", "August", "September", "Oktober", "November", 
                                   "Dezember")) +
      theme(legend.position = "top") +
      coord_flip() # fuer bessere Uebersichtlichkeit werden die x und y Achsen vertauscht
    )
  
  # Plot2: Logistikverzuege ----
  # ERROR Satz 2
  output$error2 <- renderText({
    
    validate(
      need(input$zeitintervall[[2]] >= input$zeitintervall[[1]], 
           "Enddatum ist früher als Startdatum. Bitte neues Datum wählen.")
    )
  })
  
  output$logistik <- renderPlot(
    ggplot(data = data2(), 
           mapping = aes(x = Datum, y = unterwegs, fill = factor(month))) + 
      stat_summary(fun.y = sum, geom = "bar", width = 0.25) +
      scale_y_continuous(breaks = seq(0, 600, 100)) +
      geom_hline(aes(yintercept = mean(unterwegs)), color = "red") +
      labs(x = "Datum", y = "Anzahl der Komponente, die unterwegs sind") +
      scale_fill_manual("Monat", values = c("1" = input$januar, "2" = input$februar, 
                                            "3" = input$maerz, "4" = input$april, 
                                            "5" = input$mai, "6" = input$juni,
                                            "7" = input$juli, "8" = input$august, 
                                            "9" = input$september, "10" = input$oktober,
                                            "11" = input$november, "12" = input$dezember),
                        labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", 
                                   "Juli", "August", "September", "Oktober", "November", 
                                   "Dezember")) +
      theme(legend.position = "top")
    )
}

shinyApp(ui = ui, server = server)