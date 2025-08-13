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
      Y = sapply(params$people, function(p) p$y),
      Skin_Area = sapply(params$people, function(p) p$skin_area),
      stringsAsFactors = FALSE
    )
    DT::datatable(people_df, options = list(pageLength = 4, dom = 't'))
  })
  
  # Run simulation
  observeEvent(input$run_sim, {
    output$sim_status <- renderText("Running simulation... Please wait.")
    
    # Run simulation with current parameters
    withProgress(message = 'Running simulation...', value = 0, {
      incProgress(0.5, detail = "Initializing mosquitoes...")
      results <- run_simulation(current_params())
      incProgress(1, detail = "Complete!")
      sim_results(results)
    })
    
    output$sim_status <- renderText({
      results <- sim_results()
      if (is.null(results)) return("No simulation run yet.")
      
      paste(
        "Simulation completed!\n",
        "Total bites:", nrow(results$bite_log), "\n",
        "Final mosquito count:", length(results$final_mosquitoes), "\n",
        "Simulation time:", max(results$population_log$time), "steps"
      )
    })
  })
  
  # Bite distribution plot
  output$bite_plot <- renderPlotly({
    results <- sim_results()
    if (is.null(results) || nrow(results$bite_log) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    bite_counts <- results$bite_log %>%
      group_by(person) %>%
      summarise(bites = n(), .groups = 'drop')
    
    p <- ggplot(bite_counts, aes(x = person, y = bites, fill = person)) +
      geom_col() +
      scale_fill_viridis_d() +
      labs(title = "Total Bites per Person", x = "Person", y = "Number of Bites") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Population plot
  output$population_plot <- renderPlotly({
    results <- sim_results()
    if (is.null(results)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(results$population_log, aes(x = time, y = count)) +
      geom_line(color = "darkgreen", size = 1) +
      labs(title = "Mosquito Population Over Time", x = "Time Step", y = "Number of Mosquitoes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlotly({
    results <- sim_results()
    if (is.null(results) || nrow(results$bite_log) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No bite data available", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    bite_timeline <- results$bite_log %>%
      mutate(time_bin = floor(time / 50) * 50) %>%
      group_by(time_bin, person) %>%
      summarise(bites = n(), .groups = 'drop')
    
    p <- ggplot(bite_timeline, aes(x = time_bin, y = bites, color = person)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_d() +
      labs(title = "Bites Over Time", x = "Time Step", y = "Bites per 50 time steps") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Bite table
  output$bite_table <- DT::renderDataTable({
    results <- sim_results()
    if (is.null(results) || nrow(results$bite_log) == 0) {
      return(DT::datatable(data.frame(Message = "No bite data available")))
    }
    
    bite_data <- results$bite_log %>%
      mutate(
        mosquito_x = round(mosquito_x, 2),
        mosquito_y = round(mosquito_y, 2)
      )
    
    DT::datatable(bite_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Run sensitivity analysis
  observeEvent(input$run_sensitivity, {
    output$sensitivity_status <- renderText("Running parameter sensitivity analysis... This may take several minutes.")
    
    withProgress(message = 'Running sensitivity analysis...', value = 0, {
      incProgress(0.2, detail = "Testing speed parameter...")
      
      # Custom sensitivity analysis for Shiny
      test_params <- list(
        v = seq(1, 4, by = 0.5),      # speed
        l = seq(0.1, 0.8, by = 0.2),  # light attraction (reduced for speed)
        Y = c(1, 2, 3, 4, 5),         # offspring per bite
        M = seq(20, 80, by = 30)      # initial mosquito count (reduced for speed)
      )
      
      results_sensitivity <- data.frame()
      total_tests <- sum(sapply(test_params, length))
      current_test <- 0
      
      for (param_name in names(test_params)) {
        for (param_value in test_params[[param_name]]) {
          current_test <- current_test + 1
          incProgress(0.8/total_tests, detail = paste("Testing", param_name, "=", param_value))
          
          # Create modified parameters
          test_param_set <- default_params
          test_param_set[[param_name]] <- param_value
          test_param_set$simulation_time <- 300  # shorter for sensitivity analysis
          
          # Run simulation
          sim_result <- run_simulation(test_param_set)
          
          # Calculate metrics
          total_bites <- nrow(sim_result$bite_log)
          bite_distribution <- sim_result$bite_log %>%
            group_by(person) %>%
            summarise(bites = n(), .groups = 'drop')
          
          # Add results
          result_row <- data.frame(
            parameter = param_name,
            value = param_value,
            total_bites = total_bites,
            max_individual_bites = if(nrow(bite_distribution) > 0) max(bite_distribution$bites) else 0,
            final_mosquito_count = length(sim_result$final_mosquitoes),
            stringsAsFactors = FALSE
          )
          
          results_sensitivity <- rbind(results_sensitivity, result_row)
        }
      }
      
      incProgress(1, detail = "Analysis complete!")
      sensitivity_results(results_sensitivity)
    })
    
    output$sensitivity_status <- renderText({
      results <- sensitivity_results()
      if (is.null(results)) return("No sensitivity analysis run yet.")
      paste("Sensitivity analysis completed with", nrow(results), "parameter combinations tested.")
    })
  })
  
  # Sensitivity bites plot
  output$sensitivity_bites_plot <- renderPlotly({
    results <- sensitivity_results()
    if (is.null(results)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis first", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(results, aes(x = value, y = total_bites, color = parameter)) +
      geom_line() +
      geom_point() +
      facet_wrap(~parameter, scales = "free_x") +
      labs(title = "Parameter Sensitivity: Total Bites",
           x = "Parameter Value", y = "Total Bites") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Sensitivity population plot
  output$sensitivity_pop_plot <- renderPlotly({
    results <- sensitivity_results()
    if (is.null(results)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run sensitivity analysis first", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(results, aes(x = value, y = final_mosquito_count, color = parameter)) +
      geom_line() +
      geom_point() +
      facet_wrap(~parameter, scales = "free_x") +
      labs(title = "Parameter Sensitivity: Final Mosquito Population",
           x = "Parameter Value", y = "Final Mosquito Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}

# Run the application 
# shinyApp(ui = ui, server = server)

# To run the app, save this file as "shiny_app.R" and use:
# shiny::runApp("shiny_app.R")