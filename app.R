## app.R ##
library(shiny)
library(shinydashboard)
library(bupaR)
library(eventdataR)
library(xesreadR)
library(edeaR)
library(processmapR)
library(processmonitR)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(DiagrammeR)

ev <- eventdataR::patients

# Eventlog filter
{
  filter <- wellPanel(
  numericInput(
    "pct_variant",
    "Percentage of Variants",
    value = 1,
    min = 0,
    max = 1,
    step = 0.1,
    width = "25%"
  )
  ,
  dateRangeInput(
    "date_range",
    "Date Interval",
    start = min(as_date(ev$time)),
    end = max(as_date(ev$time)),
    min = min(as_date(ev$time)),
    max = max(as_date(ev$time))
  ),
  selectInput("interval_filter",
              "Interval Method",
              choices = 
                c("contained","start","complete","intersecting","trim"),
              selected = "contained"),
  textOutput("filter_explanation", container = div),
  br(),
  sliderInput(
    "throughput_time",
    "Throughput Time",
    min = 0,
    max = 365,
    value = c(0,30),
    step = 1,
    width = "100%"
  ),
  actionButton("update", "Update Filter")
)
}

# Dashboard sidebar
{
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Filter",
               tabName = "eventlog_filter",
               icon = icon("filter", lib = "font-awesome")
               ),
    menuItem(
      "Process Map",
      tabName = "process_map",
      icon = icon("sitemap", lib = "font-awesome")
    ),
    menuItem(
      "Precedence Matrix",
      tabName = "precedence_matrix",
      icon = icon("table", lib = "font-awesome")
    ),
    menuItem(
      "Trace Explorer",
      tabName = "trace_explorer",
      icon = icon("search", lib = "font-awesome")
    ),
    menuItem(
      "Dot Plot",
      tabName = "dot_plot",
      icon = icon("chart-bar", lib = "font-awesome")
    ),
    menuItem(
      "Resource Map",
      tabName = "resource_map",
      icon = icon("map-marked", lib = "font-awesome")
    ),
    menuItem(
      "Resource Matrix",
      tabName = "resource_matrix",
      icon = icon("database", lib = "font-awesome")
    )
  )
  )# icons at https://fontawesome.com/icons?d=gallery&q=list)
}

# Dashboard body
{
  body <- dashboardBody(
    tags$head(tags$style(
      HTML('
        /* body */
        .content-wrapper, .right-side {
        background-color: #ffffff;
        }'
  )
      )),
  tabItems(
    tabItem(
      tabName = "eventlog_filter",
      h3("Event Log Filter"),
      filter
    ),
    tabItem(
      tabName = "process_map",
      h3("Process Map"),
      h5("By default, the process map is annotated with frequencies of activities and flows. This is what is called the frequency profile, and can be created explicitly using the frequency function. This function has a value argument, which can be used to adjust the frequencies shown, for instance using relative frequencies instead of the default absolute ones."),
      br(),
      selectInput(
        "rank_dir",
        "Graph Orientation",
        choices =
          c(
            "Top-Bottom" = "TB",
            "Left-Right" = "LR",
            "Bottom-Top" = "BT",
            "Right-Left" = "RL"
          ),
        selected = "LR",
        width = "25%"
      ),
      selectInput("graph_type",
                  "Graph Type",
                  choices = c(
                    "Absolute Frequency" = "absolute",
                    "Relative Frequency" = "relative",
                    "Absolute Case Frequency" = "absolute_case",
                    "Relative Case Frequency" = "relative_case"
                  ),
                  selected = "absolute",
                  width = "25%"),
      grVizOutput(outputId = "plt_process_map")
    ),
    tabItem(
      tabName = "precedence_matrix",
      h3("Precedence Matrix"),
      h5("The Precedence Matrix shows how activities are followed by each other."),
      br(),
      selectInput("graph_type",
                  "Graph Type",
                  choices = c(
                    "Absolute Frequency" = "absolute",
                    "Relative Frequency" = "relative",
                    "Absolute Case Frequency" = "absolute_case",
                    "Relative Case Frequency" = "relative_case"
                  ),
                  selected = "absolute",
                  width = "25%"),
      plotOutput(outputId = "plt_precedence_matrix")
    ),
    tabItem(
      tabName = "trace_explorer",
      h3("Trace Explorer"),
      h5("The trace explorer shows process traces, giving the user the ability to identify the most common (or least common) process traces."),
      br(),
      plotOutput(outputId = "plt_trace_explorer")
    ),
    tabItem(
      tabName = "dot_plot",
      h3("Dot Plot"),
      h5("The dot plot shows events as dots and can be used to identify resource bottlenecks"),
      br(),
      plotOutput(outputId = "plt_dot_plot")
    ),
    tabItem(
      tabName = "resource_map",
      h3("Resource Map"),
      h5("The resource map shows the hand-offs between resources.  This chart can be used to understand resource processing time and the frequnecy of hand-offs."),
      br(),
      grVizOutput(outputId = "plt_resource_map")
    ),
    tabItem(
      tabName = "resource_matrix",
      h3("Resource Matrix"),
      h5("The resource matrix shows "),
      br(),
      plotOutput(outputId = "plt_resource_matrix")
    )
  )
      )
  }

# Ui Component
{ui <- dashboardPage(dashboardHeader(
  title = "Deloitte",
  dropdownMenu(
    type = "tasks",
    badgeStatus = "success",
    icon = icon("filter", lib = "font-awesome"),
    taskItem(value = 90, color = "green",
             "Documentation"),
    taskItem(value = 17, color = "aqua",
             "Project X"),
    taskItem(value = 75, color = "yellow",
             "Server deployment"),
    taskItem(value = 80, color = "red",
             "Overall project")
  )
),
sidebar,
body,
skin = "black")
}

# Server Component
server <- function(input, output) {
  fl <- eventReactive(input$update, {
     ev %>%
      filter_activity_frequency(percentage = input$pct_variant) %>%
      filter_time_period(interval = input$date_range, filter_method = input$interval_filter) %>%
      filter_throughput_time(interval = input$throughput_time, units = c("days"))
  }, ignoreNULL = FALSE)
  
  output$filter_explanation <- renderText({
    explanation <- case_when(
      input$interval_filter == "contained" ~ "All events from cases contained in the time period",
      input$interval_filter == "complete" ~ "All events from cases completed in the time period",
      input$interval_filter == "start" ~ "All events related to cases started in the time period",
      input$interval_filter == "intersecting" ~ "All the events related to cases in which at least one event started and/or ended in the time period",
      input$interval_filter == "trim" ~ "All the events which started and ended in the time frame"
    )
    paste("Interval Selection Criteria: ", explanation)
  })
  
  output$plt_process_map <- renderGrViz({
    fl() %>%
      process_map(rankdir = input$rank_dir, type = frequency(input$graph_type))
  })
  
  output$plt_precedence_matrix <- renderPlot({
    fl() %>%
      precedence_matrix() %>%
      plot()
  })
  
  output$plt_trace_explorer <- renderPlot({
    fl() %>%
      trace_explorer()
  })
  
  output$plt_dot_plot <- renderPlot({
    fl() %>%
      dotted_chart(x = "relative", y = "duration", color = "employee")
  })
  
  output$plt_resource_map <- renderGrViz({
    fl() %>%
      resource_map()
  })
  
  output$plt_resource_matrix <- renderPlot({
    fl() %>% 
      resource_matrix() %>%
      plot()
  })
  
} # end server

shinyApp(ui, server)