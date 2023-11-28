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
library(dplyr)
library(tidyr)
library(TSA)
library(lubridate)
library(leaflet)
library(leafem)
library(sf)
library(shinythemes)
library(DT)

#Read in data
data <- read.csv("conflict.csv")
data <- data %>%
  filter(grepl("Israel", actor1, actor2))%>%
  filter(!(sub_event_type %in% c("Disrupted weapons use", "Change to group/activity", "Other", 
                                 "Government regains territory", "Agreement", 
                                 "Headquarters or base established", "Non-violent transfer of territory"))) %>%
  filter(country %in% c("Israel", "Palestine") & admin1 != "")
locations <- unique(data$admin1)
events <- unique(data$sub_event_type)
data$event_date <- as.Date(data$event_date, format = "%d %B %Y")
data$event_date <- format(data$event_date, format = "%Y-%m-%d")
data$event_date <- as.Date(data$event_date, format = "%Y-%m-%d")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                
                navbarPage("Israel-Palestine Conflict 2016-Present",
                           
                           tabPanel("Data Selection Tab",
                                    titlePanel("Violence in Israel and Palestinian Territories"),
                                    
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        dateRangeInput(label = "Select a date:", inputId = "Date", format = "yyyy-mm-dd", min = "2016-01-01", start = "2023-01-01", end = "2023-11-24"),
                                        checkboxGroupInput(label = "Select event types:", inputId = "Sub", choices = events, selected = events),
                                        checkboxGroupInput(label = "Select locations:", inputId = "Loc", choices = locations, selected = locations),
                                      ),
                                      
                                      mainPanel(
                                        actionButton(inputId = "plot", "Plot Map and Perform PCA", style = "color: orange"),
                                        verbatimTextOutput("info"),
                                        div(
                                          style = "max-height: 500px; overflow-y: auto; max-width: 1000px; overflow-x: auto",
                                          tableOutput("fdata")
                                        )
                                      )
                                    )
                           ),
                           
                           tabPanel("Map",
                                    mainPanel(
                                      fluidRow(
                                        column(8,
                                               leafletOutput("map", width = "600px", height = "600px") 
                                        ),
                                        column(4,
                                               DTOutput("click"),
                                               textOutput("notes")
                                        )
                                      )
                                    )),
                           
                           tabPanel("Principal Component Analysis",
                                    mainPanel(
                                         plotOutput("plot_pca_data"),
                                         textOutput("not_enough"),
                                         verbatimTextOutput("pcasum"),
                                         plotOutput("pca")
                                        )
                           )
                )
                # Application title
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$info <- renderText({
    start_date <- as.Date(input$Date[1])
    end_date <- as.Date(input$Date[2])
    paste(start_date, " to ", end_date)
  })
  
  
  
  
  filtered_data <- reactive({
    start_date <- as.Date(input$Date[1])
    end_date <- as.Date(input$Date[2])
    events <- input$Sub
    locs <- input$Loc
    
    fdata <- data %>%
      filter(between(event_date, start_date, end_date)) %>%
      arrange(event_date, desc = TRUE) %>%
      filter(admin1 %in% locs) %>%
      filter(sub_event_type %in% events) %>%
      mutate(event_date = format(event_date, "%Y-%m-%d"))
    return(fdata)
  })
  
  
  observeEvent(input$plot, {
    req(filtered_data())
    #Draw Map
    pts <- filtered_data()
    
    color_palette <- colorFactor(
      palette = c("royalblue1", "orange1", "yellow1",
                  "green2", "deeppink", "white",
                  "turquoise1", "firebrick1", "olivedrab", "purple",
                  "brown4", "midnightblue", "tan", "pink1"),
      domain = pts$sub_event_type
    )
    
    pts_sf <- st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 4)) %>%
        setView(lat = 31.7683, lng = 35.2137, zoom = 6) %>%
        setMaxBounds(lng1 = 25,
                     lng2 = 45,
                     lat1 = 25,
                     lat2 = 45) %>%
        addTiles() %>%
        addCircleMarkers(data = pts,
                         lng = ~longitude,
                         lat = ~latitude,
                         fillColor = ~color_palette(sub_event_type),
                         color = ~ifelse(fatalities>0, "black", NA),
                         weight = 2.5,
                         fillOpacity = 1,
                         radius = 5
        ) %>%
        addLegend(
          title = "Event Type",
          pal = color_palette,
          values = pts$sub_event_type,
          opacity = 1
        )
      
    })
    
    
    #Plot PCA
    
    pcadata <- filtered_data() %>%
      group_by(admin1, sub_event_type) %>%
      summarise(count = n())
    
    pcadata$sub_event_type <- as.factor(pcadata$sub_event_type)
    
    output$plot_pca_data <- renderPlot({
      ggplot(pcadata, aes(x = admin1, y = count, fill = sub_event_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("royalblue1", "orange1", "yellow1",
                                     "green2", "deeppink", "white",
                                     "turquoise1", "firebrick1", "olivedrab", "purple",
                                     "brown4", "midnightblue", "tan", "pink1")) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(title = "Counts by Location and Event Type",
             x = "Location",
             y = "Count")
    })
      
      if (length(unique(pcadata$admin1)) < 2 | length(unique(pcadata$sub_event_type)) < 2) {
        output$not_enough <- renderText({
          paste("Choose at least two of each variable.")
        })
      }
      
      else {
        t <- pcadata %>%
          pivot_wider(names_from = admin1, values_from = count, values_fill = 0) %>%
          tibble::column_to_rownames("sub_event_type")
        
        pca <- prcomp(t, scale = TRUE)
        
        output$pca <- renderPlot({
          biplot(pca, col = c("blue", "red"), xlim = c(-1, 1), ylim = c(-1, 1), cex = 1.5)
        }, width = 800, height = 800)
        output$pcasum <- renderPrint({
          summary(pca)
        })
      }
    
    
  })
  
  output$fdata <- renderTable({
    d <- filtered_data()
    dplyr::select(d, event_date, sub_event_type, actor1, actor2)
  })


  observeEvent(input$map_marker_click, {
    
    
    
    pt <- input$map_marker_click
    lat <- pt$lat
    lng <- pt$lng
    clickpt <- filtered_data() %>%
      filter(latitude == lat & longitude == lng) %>%
      dplyr::select(event_id_cnty, event_date, sub_event_type, actor1, actor2, civilian_targeting, fatalities)
    notes <- filtered_data() %>%
      filter(latitude == lat & longitude == lng) %>%
      dplyr::select(notes)
    
    output$click <- DT::renderDT({
      datatable(clickpt,
                options = list(
                  searching = FALSE,
                  lengthChange = FALSE,
                  ordering = FALSE,
                  info = FALSE,
                  paging = FALSE
                ),
                style = "bootstrap"
      ) %>%
        formatStyle(
          names(clickpt),
          color = "blue"
        )
    })
    
    observe({
      rows <- input$click_rows_selected
      if (!is.null(rows)) {
        output$notes <- renderText({
          print(notes[rows,])
        })
      }
    })
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
