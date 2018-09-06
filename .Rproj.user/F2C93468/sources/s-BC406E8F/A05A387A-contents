## app.R ##
library(shiny)
library(shinydashboard)

# Dashboard sidebar
{sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Executive Overview",
      tabName = "exec_summary",
      startExpanded = TRUE,
      icon = icon("list-ul", lib = "font-awesome"),
      menuSubItem("Working Capital Metrics", tabName = "exec_wc", selected = TRUE),
      menuSubItem("Margin Metrics", tabName = "exec_margin")
    ),
    menuItem(
      "Discounts",
      tabName = "disc_summary",
      icon = icon("file-invoice", lib = "font-awesome"),
      menuSubItem("Gross To Net By Channel", tabName = "disc_gross_net"),
      menuSubItem("Gross Sales", tabName = "disc_gross_sales"),
      menuSubItem("Discounts Distribution", tabName = "disc_distribution")
    ),
    menuItem(
      "Credit & Risk",
      tabName = "cr_summary",
      icon = icon("exclamation-circle", lib = "font-awesome"),
      menuSubItem("High Risk A/R", tabName = "cr_ar_risk"),
      menuSubItem("High Risk Customers", tabName = "cr_cust_risk")
    ),
    menuItem(
      "Order",
      tabName = "ord_summary",
      icon = icon("receipt", lib = "font-awesome"),
      menuSubItem("Backorder Trend", tabName = "ord_backorder"),
      menuSubItem("Order Exceptions", tabName = "ord_exceptions"),
      menuSubItem("Cost To Serve", tabName = "ord_cost")
    ),
    menuItem(
      "Cash Application",
      tabName = "cash_summary",
      icon = icon("money-bill", lib = "font-awesome"),
      menuSubItem("Payment Conversion", tabName = "cash_conversion"),
      menuSubItem("Customer Remittances", tabName = "cash_cust_remittances"),
      menuSubItem("Unapplied Payments", tabName = "cash_unapplied_pmt")
    ),
    menuItem(
      "Collections",
      tabName = "coll_summary",
      icon = icon("hands-usd", lib = "font-awesome"),
      menuSubItem("DSO", tabName = "coll_dso"),
      menuSubItem("A/R Aging", tabName = "coll_ar_aging")
    ),
    menuItem(
      "Disputes",
      tabName = "disp_summary",
      icon = icon("balance-scale", lib = "font-awesome"),
      menuSubItem("Trend", tabName = "disp_trend"),
      menuSubItem("Rejected Disputes", tabName = "disp_rej")
    )
  ) # icons at https://fontawesome.com/icons?d=gallery&q=list
)
}

# Dashboard body
{body <- dashboardBody(
  tabItems(
    tabItem(tabName = "exec_summary",
            h2("Executive Summary")
            ),
    tabItem(tabName = "exec_wc",
            fluidRow(
              h2("Working Capital Metrics"),
              valueBox(10*2, "New Orders", icon = icon("credit-card")),
              valueBoxOutput("progressBox"),
              valueBoxOutput("approvalBox")
            ),
            fluidRow(
              # Clicking this will increment the progress amount
              box(width = 4, actionButton("count", "Increment progress"))
            )
    ),
    tabItem(tabName = "exec_margin",
            h2("Margin Metrics")
    ),
    tabItem(tabName = "disc_summary",
            h2("Discounts")
            ),
    tabItem(tabName = "disc_gross_net",
            h2("Gross To Net By Channel")
    ),
    tabItem(tabName = "disc_gross_sales",
            h2("Gross Sales")
    ),
    tabItem(tabName = "disc_distribution",
            h2("Discounts Distribution")
    ),
    tabItem(tabName = "cr_summary",
            h2("Credit & Risk")
            ),
    tabItem(tabName = "cr_ar_risk",
            h2("High Risk A/R")
    ),
    tabItem(tabName = "cr_cust_risk",
            h2("High Risk Customers")
    ),
    tabItem(tabName = "ord_summary",
            h2("Order")
    ),
    tabItem(tabName = "ord_backorder",
            h2("Backorder Trend")
            ),
    tabItem(tabName = "ord_exceptions",
            h2("Order Exceptions")
    ),
    tabItem(tabName = "ord_cost",
            h2("Cost To Serve")
    ),
    tabItem(tabName = "cash_summary",
            h2("Cash Application")
            ),
    tabItem(tabName = "cash_conversion",
            h2("Payment Conversion")
    ),
    tabItem(tabName = "cash_cust_remittances",
            h2("Customer Remittances")
    ),
    tabItem(tabName = "cash_unapplied_pmt",
            h2("Unapplied Payments")
    ),
    tabItem(tabName = "coll_summary",
            h2("Collections")
            ),
    tabItem(tabName = "coll_dso",
            h2("DSO")
    ),
    tabItem(tabName = "coll_ar_aging",
            h2("A/R Aging")
    ),
    tabItem(tabName = "disp_summary",
            h2("Disputes")
            ),
    tabItem(tabName = "disp_trend",
            h2("Trend")
    ),
    tabItem(tabName = "disp_rej",
            h2("Rejected Disputes")
    )
  )
)}

ui <- dashboardPage(
  dashboardHeader(title = "GE Dashboard"),
  sidebar,
  body
)

server <- function(input, output) {
  data("mtcars")
  mtcars
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
}

shinyApp(ui, server)