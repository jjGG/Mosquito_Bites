# Install required packages first
packages <- c("ggplot2", "dplyr", "gridExtra", "viridis", "plotly",
              "shiny", "shinydashboard", "DT")
install.packages(packages[!packages %in% installed.packages()])

# Load and run basic simulation
source("mosquito_bites.R")
results <- run_simulation()
plot_environment(default_params)
plot_bite_analysis(results)



# Launch the Shiny app for interactive parameter testing
shiny::runApp("shiny_app.R")


# Run all example analyses (takes 5-10 minutes)
source("example_analysis.R")
# Check results/ folder for all outputs



