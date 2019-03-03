library(shiny)
library(ggplot2)
library(ggthemr)
library(scales)
library(shinyWidgets)
source("setup.R")

ggthemr("fresh")

ui <- fluidPage(
  titlePanel("SNCF Train Departures"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(tags$div(tags$h4("How to use"),"You can choose the depature and arrival station to see the # of trips that are delayed between 
                        the two over a period of time. You can also toggle showing absolute values vs percents"),tags$br()),
      fluidRow(selectizeInput("departure_station",
                              "Departure Station",
                              departure_stations)),
      fluidRow(uiOutput("arrival_station_opt")),
      fluidRow(tags$h5("Show as %")),
      fluidRow(column(width = 6,fluidRow(materialSwitch(inputId = "arrivals_switch", label = "Departures"))),
               column(width = 6,fluidRow(materialSwitch(inputId = "detail_switch", label = "Delay Detail"))))),
    
    mainPanel(
      fluidRow(plotOutput("station_timing_plot")),
      
      fluidRow(plotOutput("num_delays_plot"))
    )
  )
)

server <- function(input, output) {
  
  selected_trains <- reactive({
    full_trains %>% 
      filter(departure_station == input$departure_station,
             arrival_station == input$arrival_station)
  })
  
  output$arrival_station_opt <- renderUI({
    
    current_arrival_stations <- 
      full_trains %>% 
      select(departure_station, arrival_station) %>% 
      filter(departure_station == input$departure_station) %>% 
      pull(arrival_station) %>% 
      unique() %>% 
      sort()
    
    arrival_choices <- 
      set_names(current_arrival_stations,
                toTitleCase(tolower(current_arrival_stations)))
    
    selectizeInput("arrival_station",
                   "Arrival Station",
                   arrival_choices,
                   multiple = FALSE)
  })
  
  output$num_delays_plot <- renderPlot({
    
    req(input$arrival_station)
    
    selected_trains() %>% 
      gather(key,value,c(num_greater_15_min_late,num_greater_30_min_late,num_greater_60_min_late)) %>% 
      group_by(observation_date) %>% 
      mutate(pct = value/sum(value)) %>% 
      {
        if (input$detail_switch) {
          ggplot(.,aes(x = observation_date,y = pct , fill = key ))
        } else {
          ggplot(.,aes(x = observation_date, y = value, fill = key ))
        }
      } + 
      geom_col() +
      scale_x_date(date_breaks = "3 months",
                   labels = function(x) {format(x,format = "%b\n%Y")}) +
                   {
                     if (input$detail_switch) {
                       scale_y_continuous(labels = percent )
                     }                   
                   } +
      scale_fill_manual(labels = c("> 15 minutes",">30 minutes",">60 minutes"),
                        values = c("#FCAC27","#FB6521","#FB401F")) +
      labs(title = "Delay Detail",
           x = "",
           y = "# of trips",
           fill = "Delayed by:") +
      legend_bottom()
    
  })
  
  output$station_timing_plot <- renderPlot({
    req(input$arrival_station)
    
    selected_trains()  %>% 
      mutate(on_time_trains = total_num_trips - num_arriving_late) %>% 
      select(departure_station,observation_date,on_time_trains,num_arriving_late) %>% 
      gather(key,value,c(on_time_trains,num_arriving_late)) %>% 
      group_by(observation_date) %>% 
      mutate(pct = value/sum(value)) %>% 
      {
        if (input$arrivals_switch) {
          ggplot(.,aes(x = observation_date,y = pct , fill = fct_rev(key) ))
        } else {
          ggplot(.,aes(x = observation_date, y = value, fill = fct_rev(key) ))
        }
      } + 
      geom_col() +
      scale_x_date(date_breaks = "3 months",
                   labels = function(x) {format(x,format = "%b\n%Y")}) +
      scale_fill_manual(labels = c("Arrived on Time","Arrived Late"),
                        values = c("#23ACB4","#F84343")) +
                        {
                          if (input$arrivals_switch) {
                            scale_y_continuous(labels = percent )
                          }                   
                        } +
      labs(x = "",
           y = "# of trips",
           title = "Train Departures",
           fill = "Arrival Status") +
      legend_bottom()
  })
}

shinyApp(ui = ui, server = server)

