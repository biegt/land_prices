#########################
library(semantic.dashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(here)
library(plotly)
library(readr)
library(stringr)
library(sf)
#########################


# Datenimport -------------------------------------------------------------
col_pal <- c("#67c5c2", "#c84127")

fc_data <- read_csv(here("01_data", "processed", "fc_data.csv"))

ts_data <-
  read_csv(here("01_data", "processed", "metrics_by_buyer_year_district.csv")) %>%
  mutate(Bezirk = factor(
    str_trim(Bezirk),
    levels = c(
      "1. Innere Stadt",
      "2. Leopoldstadt",
      "3. Landstraße",
      "4. Wieden",
      "5. Margareten",
      "6. Mariahilf",
      "7. Neubau",
      "8. Josefstadt",
      "9. Alsergrund",
      "10. Favoriten",
      "11. Simmering",
      "12. Meidling",
      "13. Hietzing",
      "14. Penzing",
      "15. Rudolfsheim-Fünfhaus",
      "16. Ottakring",
      "17. Hernals",
      "18. Währing",
      "19. Döbling",
      "20. Brigittenau",
      "21. Floridsdorf",
      "22. Donaustadt",
      "23. Liesing"
    )
  ))

map_data <- st_read(here("01_data", "processed", "districts.shp"))


# User Interface ----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Grundstückspreise in Wien",
    titleWidth = "",
    logo_path = "brand_logo.PNG",
    menu_button_label = "",
    show_menu_button = TRUE
  ) %>% tagAppendAttributes(class = 'dashboard_header', style = "height: 50px"),
  dashboardSidebar(
    size = "",
    sidebarMenu(
      menuItem(
        tabName = "overview",
        text = "Überblick",
        icon = icon("arrows alternate")
      ),
      menuItem(
        tabName = "prices",
        text = "Exploration Zeitreihe",
        icon = icon("chart area")
      ),
      menuItem(
        tabName = "map",
        text = "Exploration Karte",
        icon = icon("map")
      ),
      menuItem(
        tabName = "forecast",
        text = "Vorhersage",
        icon = icon("chart line")
      ),
      menuItem(
        tabName = "download",
        text = "Download",
        icon = icon("download")
      )
    )
  ),
  dashboardBody(
    setSliderColor(rep(col_pal[1], 2), 1:2),
    
    tabItems(
      tab_item(
        tabName = "overview",
        fluidRow(box(
          width = 14,
          title = "Überblick",
          column(
            width = 14,
            align = "center",
            "Um Potentiale für Investionen zu identifizieren und unsere Geomarketing-Strategie zu verbessern,
           haben wir Transaktionendaten von Privatpersonen und privaten Unternehmen (GmbH, KGs, etc.) über die Zeit und nach Bezirk analysiert."
            
          )
         )
        ),
        fluidRow(
          semantic.dashboard::valueBox(
            subtitle = "Quelle",
            "data.gv.at",
            color = "teal",
            size = "tiny",
            width = 7,
            icon = icon("database", style = "color: #67c5c2")
          ),
          semantic.dashboard::valueBox(
            subtitle = "Gesamt-Volumen 2019 (€)",
            "2,52 Mrd.",
            color = "teal",
            size = "tiny",
            width = 7,
            icon = icon("euro", style = "color: #67c5c2")
          )
        ),
        fluidRow(
          semantic.dashboard::valueBox(
            subtitle = "Transaktionen",
            sum(ts_data$`Anzahl Transaktionen`),
            color = "teal",
            size = "tiny",
            width = 7,
            icon = icon("hashtag", style = "color: #67c5c2")
          ),
          semantic.dashboard::valueBox(
            subtitle = "Zeitliche Ausdehnung",
            paste0(min(ts_data$Jahr), " - ", max(ts_data$Jahr)),
            color = "teal",
            size = "tiny",
            width = 7,
            icon = icon("calendar", style = "color: #67c5c2")
          )
        ),
        fluidRow(box(
          width = 14,
          title = "Take Home Message",
          column(
            width = 14,
            align = "center",
            tags$div(
              tags$p(
                "Es lässt sich ein Trend in den Außenbezirken Wiens (u.a. Favoriten, Floridsdorf, Liesing, Donaustadt) erkennen, bei dem die Anzahl an Transaktionen, bei den privaten Unternehmen in der Rolle des Erwerbers sind, zunehmen",
                tags$strong("(siehe Reiter 'Exploration Zeitreihe')"),
                ". Dies ist ein Hinweis auf hohes Marktpotential in diesen Bezirken."
              ),
              tags$p(
                "Eine Vorhersage der Transaktionszahl für diese Bezirke auf Basis verschiedener Zeitreihen-Modelle zeigt, dass vor allem in den Bezirken",
                tags$strong("Floridsdorf und Donaustadt"),
                " mit einem weiteren Anstieg der Käufen durch Unternehmen in den kommenden Jahren zu rechnen ist",
                tags$strong("(siehe Reiter 'Vorhersage')."),
                tags$strong("Daher besteht die berechtigte Überlegung unsere Investionen in diesen Bezirken für gewerbliche Grundstücke ehestmöglich zu verstärken.")
              ),
              tags$p(
                "Als Limitation ist anzumerken, dass die Qualtität des zugrundeliegenden Datensatzes eher gering ist (sodass auch die Preise nicht Hauptgegenstand unserer Analyse waren). Die Zeitreihen-Modelle dürfen darüber hinaus aufgrund der jährlichen Aggregationen, vergleichsweise kurzen Zeitreihen und fehlender Berücksichtigung äußerer Faktoren nu als grober Indikator verstanden werden."
              ),
              tags$p(
                "Das Dashboard steht dem Kollegium für weitere Exploration zur Verfügung."
              )
            )
          )
        )),
      ),
      tabItem(tabName = "prices",
              fluidRow(
                box(
                  width = 14,
                  title = "Optionen",
                  shiny::splitLayout(
                    cellWidths = c("55%", "20%", "25%"),
                    sliderInput(
                      inputId = "ts_bezirke",
                      label = h4("Auswahl Bezirk"),
                      min = min(ts_data$bez_num),
                      max = max(ts_data$bez_num),
                      width = "90%",
                      step = 1,
                      ticks = FALSE,
                      value = c(1, 23)
                    ),
                    shiny.semantic::multiple_checkbox(
                      "ts_erwerber",
                      width = "60%",
                      label = h4("Erwerber"),
                      choices = unique(ts_data$Erwerber),
                      choices_value = unique(ts_data$Erwerber),
                      selected = unique(ts_data$Erwerber)
                    ),
                    shiny.semantic::multiple_radio(
                      "ts_vars",
                      h4("Metrik"),
                      rev(names(ts_data)[4:7]),
                      selected = "Anzahl Transaktionen",
                      width = "90%"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 14,
                  plotlyOutput(
                    outputId = "ts_plot",
                    width = "100%",
                    height = "480px"
                  )
                )
              )
      ), 
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Optionen",
                  width = 14,
                  shiny::splitLayout(
                    cellWidths = c("70%", "30%"),
                    sliderInput(
                      inputId = "map_years",
                      label = tags$u(h4("Jahr")),
                      min = min(ts_data$Jahr),
                      max = max(ts_data$Jahr),
                      width = "90%",
                      step = 1,
                      ticks = FALSE,
                      value = 2019
                    ),
                    shiny.semantic::multiple_radio(
                      "map_vars",
                      h4("Metrik"),
                      rev(names(ts_data)[4:7]),
                      selected = "Anzahl Transaktionen",
                      width = "90%"
                    )
                  )
                )
              ),
              
              fluidRow(
                column(tags$h3("Unternehmen"), leafletOutput("map_plot1"), width = 7),
                column(
                  tags$h3("Privatpersonen"),
                  leafletOutput("map_plot2"),
                  width = 7
                )
              )),
      tabItem(tabName = "forecast",
              fluidRow(
                box(
                  width = 14,
                  title = "Optionen",
                  shiny::splitLayout(
                    shiny.semantic::multiple_checkbox(
                      "fc_modelle",
                      h4("Modelle"),
                      unique(fc_data$Modell),
                      selected = unique(fc_data$Modell),
                      width = "60%"
                    ),
                    shiny.semantic::multiple_checkbox(
                      "fc_bezirke",
                      h4("Bezirke"),
                      unique(fc_data$Bezirk),
                      selected = unique(fc_data$Bezirk),
                      width = "60%"
                    )
                  )
                )
              ),
              fluidRow(column(
                width = 14,
                plotlyOutput(
                  outputId = "fc_plot",
                  width = "100%",
                  height = "480px"
                )
              ))),
      tabItem(tabName = "download",
              fluidRow(
                box(
                  title = "Auswahl",
                  width = 14,
                  style = "color: #FFFFFF",
                  shiny.semantic::selectInput(
                    "dataset",
                    tagList("Wähle einen Datensatz aus",
                            HTML('<a href="https://www.data.gv.at/katalog/dataset/kaufpreissammlung-liegenschaften-wien" style="color: #67c5c2">(für Orignaldaten siehe hier)</a>')),
                    choices = c("Aggregationen nach Erwerber, Jahr, Bezirk", "Daten zum Forecast")
                  ),
                  tags$br(),
                  downloadButton("downloadData", "Download", style = 'padding:4px; font-size:30px; align = center; color:#67c5c2')
                ),
              ),
              
              fluidRow(box(
                width = 14,
                dataTableOutput(outputId = "durations_table")
              )))
    )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  options(DT.options = list(
    pageLength = 10,
    language = list(
      # url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json',
      search = 'Filtern nach:',
      decimal = '',
      infoEmpty = '0 von 0 Einträgen',
      emptyTable = 'Keine Einträge vorhanden.',
      info = ' _START_ bis _END_ von _TOTAL_ Einträgen',
      lengthMenu = '_MENU_ Einträge anzeigen',
      paginate = list(previous = 'Vorherige',
                      `next` = 'Nächste')
    )
  ))
  
  datasetInput <- reactive({
    switch(
      input$dataset,
      "Aggregationen nach Erwerber, Jahr, Bezirk" = ts_data,
      "Daten zum Forecast" = fc_data
    )
  })
  
  
  output$ts_plot <- renderPlotly({
    ts_plot <- ts_data %>%
      filter(
        bez_num %in% input$ts_bezirke[1]:input$ts_bezirke[2],
        Erwerber %in% input$ts_erwerber
      ) %>%
      ggplot(aes_string(
        x = "Jahr",
        y = paste0("`", input$ts_vars, "`"),
        color = "Erwerber"
      )) +
      geom_line() +
      facet_wrap(~ Bezirk, scales = "free_y", nrow = 5) +
      theme_bw() +
      theme(strip.background = element_blank(),
            legend.position = "none") +
      labs(y = NULL, x = NULL) +
      scale_color_manual(values = col_pal)
    
    ggplotly(ts_plot)
    
  })
  
  output$fc_plot <- renderPlotly({
    fc_plot <- fc_data %>%
      filter(Bezirk %in% input$fc_bezirke, Modell %in% input$fc_modelle) %>%
      ggplot(aes(x = Jahr)) +
      geom_ribbon(aes(
        ymin = Low95,
        ymax = High95,
        fill = "95%"
      ), fill = "#c0edec") +
      geom_ribbon(aes(
        ymin = Low80,
        ymax = High80,
        fill = "80%"
      ), fill = "#67c5c2") +
      geom_line(aes(y = Daten, group = 1), colour = "black", size = 1.1) +
      geom_line(aes(y = Vorhersage, group = 1),
                color = "#595959",
                size = 0.75) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "white"),
            text = element_text(size = 8)) +
      labs(x = NULL, y = "Anzahl Transaktionen") +
      facet_grid(vars(Bezirk), vars(Modell), scales = "free_y")
    
    ggplotly(fc_plot, tooltip = c("x", "y"))
    
  })
  
  
  output$map_plot1 <- renderLeaflet({
    map_data_joined_u <- map_data %>%
      left_join(
        ts_data %>%
          select(names(ts_data)[4:7], Jahr, bez_num, Erwerber) %>%
          dplyr::filter(Jahr == input$map_years, Erwerber == "Unternehmen"),
        by = "bez_num"
      )
    
    u_pal <-
      colorNumeric("viridis", domain = map_data_joined_u[[input$map_vars]])
    
    map_data_joined_u %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "#222",
        weight = 0.7,
        opacity = 1,
        fillColor = ~ u_pal(map_data_joined_u[[input$map_vars]]),
        fillOpacity = 0.7,
        label = ~ lapply(
          paste0(
            "<br>",
            map_data_joined_u$Bezirk,
            "</b><br> Wert:",
            round(map_data_joined_u[[input$map_vars]], 2)
          ),
          HTML
        ),
        labelOptions = labelOptions(direction = "top"),
        highlight = highlightOptions(color = "#FFF", bringToFront = TRUE)
      )
  })
  
  output$map_plot2 <- renderLeaflet({
    map_data_joined_p <- map_data %>%
      left_join(
        ts_data %>% select(names(ts_data)[4:7], Jahr, bez_num, Erwerber) %>% dplyr::filter(Jahr == input$map_years, Erwerber == "Privatpersonen"),
        by = "bez_num"
      )
    
    
    p_pal <-
      colorNumeric("viridis", map_data_joined_p[[input$map_vars]])
    
    map_data_joined_p %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "#222",
        weight = 0.7,
        opacity = 1,
        fillColor = ~ p_pal(map_data_joined_p[[input$map_vars]]),
        fillOpacity = 0.7,
        label = ~ lapply(
          paste0(
            "<br>",
            map_data_joined_p$Bezirk,
            "</b><br> Wert:",
            round(map_data_joined_p[[input$map_vars]], 2)
          ),
          HTML
        ),
        labelOptions = labelOptions(direction = "top"),
        highlight = highlightOptions(color = "#FFF", bringToFront = TRUE)
      )
    
  })
  
  output$durations_table <- DT::renderDataTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  
}

shinyApp(ui, server)
