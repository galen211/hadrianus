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
    )
    # ,
    # menuItem(
    #   "Resource Map",
    #   tabName = "resource_map",
    #   icon = icon("map-marked", lib = "font-awesome")
    # ),
    # menuItem(
    #   "Resource Matrix",
    #   tabName = "resource_matrix",
    #   icon = icon("database", lib = "font-awesome")
    # )
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
      h5("Leave the below settings unchanged to run the default example, which is referenced in explanations of each dashboard tab."),
      filter
    ),
    tabItem(
      tabName = "process_map",
      h3("Process Map"),
      h5("A process map shows the steps a process can take from the start of the process until the end of the process.  For every completed process, all the steps taken are represented by nodes on the process map.  Processes are defined by a set of common steps, but these steps may occur in different sequences, and sometimes missing steps, extra steps, or steps occurring in the wrong sequence can be interpreted as process variants that should be standardized.  The nodes in the process graph can display different types of information about the underlying step.  For example, if we are analyzing 497 cases of a process, we would expect the \"start\" node to display the absolute frequency of cases passing through that step.  Since there are no predecessors to the \"start\" step (i.e. every cases starts with the \"start\" step), there should be 497 cases.  Process maps can also be annotated with different measures associated with process execution such as time elapsed between steps."),
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
      h5("A precedence matrix shows the before-after relationship between different steps in a process.  It can be used to answer questions like \"how often does a blood test occur before an MRI\" (answer: in 236 out of 497 cases)."),
      br(),
      # selectInput("graph_type",
      #             "Graph Type",
      #             choices = c(
      #               "Absolute Frequency" = "absolute",
      #               "Relative Frequency" = "relative",
      #               "Absolute Case Frequency" = "absolute_case",
      #               "Relative Case Frequency" = "relative_case"
      #             ),
      #             selected = "absolute",
      #             width = "25%"),
      plotOutput(outputId = "plt_precedence_matrix")
    ),
    tabItem(
      tabName = "trace_explorer",
      h3("Trace Explorer"),
      h5("The trace explorer shows the most common process variants.  In the default example below, there is only one variant, which is the process variant Start -> Registration -> Triage and Assessment -> X-Ray -> Discuss Results -> Check Out.  This process variant occurs in 258 out of 497 cases (51.91%) in the default example.  Usually the most frequently executed process variant represents the \"ideal process\", but assuming this were the case, 48.09% of the times this process is executed do not follow the ideal path!"),
      br(),
      plotOutput(outputId = "plt_trace_explorer")
    ),
    tabItem(
      tabName = "dot_plot",
      h3("Dot Plot"),
      h5("The dot plot is intended to show elapsed time between resource hand-offs.  In this default example, resource 7 (r7) handles the  case last in most cases.  If we look at cases with the longest elapsed time, we can see that r2-r6 become scattered the longer a case takes.  By looking at time elapsed between resource hand-offs, we can identify if a resource is creating a bottleneck in a process."),
      br(),
      plotOutput(outputId = "plt_dot_plot")
    )
    # ,
    # tabItem(
    #   tabName = "resource_map",
    #   h3("Resource Map"),
    #   h5("The resource map shows the hand-offs between resources.  This chart can be used to understand resource processing time and the frequnecy of hand-offs."),
    #   br(),
    #   grVizOutput(outputId = "plt_resource_map")
    # ),
    # tabItem(
    #   tabName = "resource_matrix",
    #   h3("Resource Matrix"),
    #   h5("The resource matrix shows "),
    #   br(),
    #   plotOutput(outputId = "plt_resource_matrix")
    # )
  )
      )
  }

# Ui Component
{ui <- dashboardPage(dashboardHeader(
  title = "Process Mining",
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
  
  # output$plt_resource_map <- renderGrViz({
  #   fl() %>%
  #     resource_map()
  # })
  # 
  # output$plt_resource_matrix <- renderPlot({
  #   fl() %>% 
  #     resource_matrix() %>%
  #     plot()
  # })
  
} # end server

shinyApp(ui, server)