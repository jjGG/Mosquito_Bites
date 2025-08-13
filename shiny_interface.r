# Interactive Shiny App for Mosquito Simulation
# Save this as "shiny_app.R"

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Source the main simulation (make sure mosquito_simulation.R is in same directory)
# source("mosquito_simulation.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Mosquito Bite Simulation"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Environment", tabName = "environment", icon = icon("home")),
      menuItem("Parameters", tabName = "parameters", icon = icon("sliders-h")),
      menuItem("Simulation", tabName = "simulation", icon = icon("play")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Sensitivity", tabName = "sensitivity", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Environment tab
      tabItem(tabName = "environment",
        fluidRow(
          box(width = 12, title = "Simulation Environment", status = "primary", solidHeader = TRUE,
            plotOutput("env_plot", height = "600px")
          )
        ),
        fluidRow(
          box(width = 6, title = "Environment Info", status = "info",
            verbatimTextOutput("env_info")
          ),
          box(width = 6, title = "People Locations", status = "info",
            DT::dataTableOutput("people_table")
          )
        )
      ),
      
      # Parameters tab
      tabItem(tabName = "parameters",
        fluidRow(
          box(width = 6, title = "Mosquito Parameters", status = "primary", solidHeader = TRUE,
            numericInput("M", "Initial Mosquitoes:", value = 50, min = 10, max = 200, step = 10),
            sliderInput("v", "Speed (v):", min = 0.5, max = 5, value = 2, step = 0.1),
            sliderInput("l", "Light Attraction (l):", min = 0, max = 1, value = 0.3, step = 0.05),
            numericInput("Y", "Offspring per Bite:", value = 3, min = 1, max = 10),
            numericInput("survival_time", "Survival Time:", value = 200, min = 50, max = 500, step = 25)
          ),
          box(width = 6, title = "Simulation Parameters", status = "primary", solidHeader = TRUE,
            numericInput("sim_time", "Simulation Time:", value = 1000, min = 100, max = 2000, step = 100),
            sliderInput("bite_range", "Bite Range:", min = 1, max = 10, value = 3, step = 0.5),
            sliderInput("window_range", "Window Entry Range:", min = 1, max = 5, value = 2, step = 0.5),
            numericInput("repro_cooldown", "Reproduction Cooldown:", value = 50, min = 10, max = 100, step = 10)
          )
        ),
        fluidRow(
          box(width = 12, title = "People Settings", status = "info", solidHeader = TRUE,
            column(3, 
              h5("Person N1 (House 1)"),
              numericInput("n1_skin", "Skin Area:", value = 100, min = 50, max = 150)
            ),
            column(3,
              h5("Person N2 (House 2)"),
              numericInput("n2_skin", "Skin Area:", value = 100, min = 50, max = 150)
            ),
            column(3,
              h5("Person N3 (Outside, Clothed)"),
              numericInput("n3_skin", "Skin Area:", value = 80, min = 30, max = 100)
            ),
            column(3,
              h5("Person N4 (Outside)"),
              numericInput("n4_skin", "Skin Area:", value = 100, min = 50, max = 150)
            )
          )
        )
      ),
      
      # Simulation tab
      tabItem(tabName = "simulation",
        fluidRow(
          box(width = 12, title = "Run Simulation", status = "success", solidHeader = TRUE,
            actionButton("run_sim", "Run Simulation", class = "btn-success", icon = icon("play")),
            br(), br(),
            verbatimTextOutput("sim_status")
          )
        )
      ),
      
      # Results tab
      tabItem(tabName = "results",
        fluidRow(
          box(width = 6, title = "Bite Distribution", status = "primary", solidHeader = TRUE,
            plotlyOutput("bite_plot")
          ),
          box(width = 6, title = "Population Over Time", status = "primary", solidHeader = TRUE,
            plotlyOutput("population_plot")
          )
        ),
        fluidRow(
          box(width = 12, title = "Bite Timeline", status = "info", solidHeader = TRUE,
            plotlyOutput("timeline_plot")
          )
        ),
        fluidRow(
          box(width = 12, title = "Detailed Bite Log", status = "info", solidHeader = TRUE,
            DT::dataTableOutput("bite_table")
          )
        )
      ),
      
      # Sensitivity Analysis tab
      tabItem(tabName = "sensitivity",
        fluidRow(
          box(width = 12, title = "Parameter Sensitivity Analysis", status = "warning", solidHeader = TRUE,
            p("This analysis tests how different parameter values affect the simulation outcomes."),
            actionButton("run_sensitivity", "Run Sensitivity Analysis", class = "btn-warning", icon = icon("search")),
            br(), br(),
            verbatimTextOutput("sensitivity_status")
          )
        ),
        fluidRow(
          box(width = 6, title = "Total Bites Sensitivity", status = "primary", solidHeader = TRUE,
            plotlyOutput("sensitivity_bites_plot")
          ),
          box(width = 6, title = "Population Sensitivity", status = "primary", solidHeader = TRUE,
            plotlyOutput("sensitivity_pop_plot")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to store simulation results
  sim_results <- reactiveVal(NULL)
  sensitivity_results <- reactiveVal(NULL)
  
  # Create reactive parameter set
  current_params <- reactive({
    params <- default_params
    params$M <- input$M
    params$v <- input$v
    params$l <- input$l
    params$Y <- input$Y
    params$survival_time <- input$survival_time
    params$simulation_time <- input$sim_time
    params$bite_range <- input$bite_range
    params$window_entry_range <- input$window_range
    params$reproduction_cooldown <- input$repro_cooldown
    
    # Update people parameters
    params$people$N1$skin_area <- input$n1_skin
    params$people$N2$skin_area <- input$n2_skin
    params$people$N3$skin_area <- input$n3_skin
    params$people$N4$skin_area <- input$n4_skin
    
    return(params)
  })
  
  # Environment plot
  output$env_plot <- renderPlot({
    plot_environment(current_params())
  })
  
  # Environment info
  output$env_info <- renderText({
    params <- current_params()
    paste(
      "Area Size:", params$area_width, "x", params$area_height, "\n",
      "Number of Houses: 2\n",
      "Light Source: House 1\n",
      "Total Windows: 3\n",
      "Initial Mosquitoes:", params$M, "\n",
      "Simulation Time:", params$simulation_time, "steps"
    )
  })
  
  # People table
  output$people_table <- DT::renderDataTable({
    params <- current_params()
    people_df <- data.frame(
      Person = names(params$people),
      Location = sapply(params$people, function(p) p$location),
      X = sapply(params$people, function(p) p$x),
      Y = sapply(params$people, function(p) p