# Mosquito Bites Simulation Project

## Overview

This R project simulates mosquito behavior and bite patterns in a 2D environment with houses, people, and environmental factors. The simulation helps analyze which parameters most influence mosquito bite distribution among different people.

## Project Structure

```
mosquito_bites/
├── Mosquito_Bites.Rproj    # R Project file
├── mosquito_bites.R        # Main simulation code
├── shiny_app.R                 # Interactive Shiny interface
├── README.md                   # This file
├── example_analysis.R          # Example usage scripts
└── results/                    # Directory for saving results
    ├── plots/
    └── data/
```

## Files Description

### 1. mosquito_bites.R
The main simulation engine containing:
- **Environment setup**: 2D area with houses, windows, light sources
- **Mosquito behavior**: Movement, feeding, reproduction, survival
- **Bite mechanics**: Distance-based biting with skin area influence
- **Visualization functions**: Environment plots and result analysis
- **Parameter sensitivity analysis**: Systematic parameter testing

### 2. shiny_app.R
Interactive web interface featuring:
- **Parameter adjustment**: Real-time parameter modification
- **Environment visualization**: Interactive plots of the simulation area
- **Results dashboard**: Bite distributions, population dynamics
- **Sensitivity analysis**: Automated parameter testing with visualizations

### 3. example_analysis.R
Ready-to-run example scripts demonstrating:
- Basic simulation runs
- Parameter comparisons
- Custom scenario testing
- Result interpretation

## Key Parameters

### Mosquito Parameters
- **M**: Initial number of mosquitoes (default: 50)
- **v**: Flying speed (default: 2.0)
- **l**: Light attraction strength (default: 0.3)
- **Y**: Offspring per successful bite (default: 3)
- **survival_time**: Time steps before death without feeding (default: 200)

### Environment Parameters
- **area_width/height**: Simulation area dimensions (default: 100x100)
- **houses**: Two houses with configurable windows
- **light**: Light source in house1 attracting mosquitoes
- **people**: Four people with different skin areas and locations

### Behavioral Parameters
- **bite_range**: Distance for successful biting (default: 3)
- **window_entry_range**: Distance to enter through windows (default: 2)
- **reproduction_cooldown**: Time between reproductions (default: 50)

## Installation and Setup

### Required R Packages
```r
# Install required packages
packages <- c("ggplot2", "dplyr", "gridExtra", "viridis", "plotly", 
              "shiny", "shinydashboard", "DT")
install.packages(packages[!packages %in% installed.packages()])
```

### Running the Simulation

#### Option 1: Command Line Interface
```r
# Source the main simulation
source("mosquite_bites.R")

# Run basic simulation
results <- run_simulation()

# Visualize environment
plot_environment(default_params)

# Analyze results
plot_bite_analysis(results)

# Parameter sensitivity analysis
sensitivity <- parameter_sensitivity_analysis()
plot_sensitivity_analysis(sensitivity)
```

#### Option 2: Interactive Shiny Interface
```r
# Launch interactive app
shiny::runApp("shiny_app.R")
```

## Model Description

### Environment
- **2D rectangular area** (100x100 units by default)
- **Two houses** with windows for mosquito entry/exit
- **Light source** in house1 attracting mosquitoes
- **Four people** positioned in different locations:
  - N1: Inside house1 (near light)
  - N2: Inside house2 (no light)
  - N3: Outside, clothed (reduced skin area)
  - N4: Outside, normal skin area

### Mosquito Behavior
1. **Movement**: Random walk with velocity persistence
2. **Light attraction**: Gravitational-like attraction to light source
3. **House navigation**: Can enter/exit through windows
4. **Feeding**: Probabilistic biting based on proximity and skin area
5. **Reproduction**: Create offspring after successful feeding
6. **Survival**: Limited lifespan without feeding

### Bite Mechanics
- Mosquitoes must be within `bite_range` of a person
- Bite probability depends on person's skin area
- Only accessible people can be bitten (location matching)
- Successful bites reset mosquito energy and enable reproduction

## Example Scenarios

### Scenario 1: Light Attraction Effect
```r
# Test different light attraction strengths
params_low_light <- default_params
params_low_light$l <- 0.1

params_high_light <- default_params  
params_high_light$l <- 0.8

results_low <- run_simulation(params_low_light)
results_high <- run_simulation(params_high_light)

# Compare bite distributions
```

### Scenario 2: Population Pressure
```r
# Test different initial mosquito populations
for(M in c(20, 50, 100, 200)) {
  params <- default_params
  params$M <- M
  results <- run_simulation(params)
  # Analyze results...
}
```

### Scenario 3: Speed vs. Attraction Trade-off
```r
# Create parameter grid
speed_vals <- seq(1, 4, by = 0.5)
attraction_vals <- seq(0.1, 0.8, by = 0.1)

# Run simulations for all combinations
# Analyze interaction effects
```

## Key Insights and Expected Results

### Parameter Influence Ranking (Expected)
1. **Light attraction (l)**: Strong effect on indoor vs. outdoor bite distribution
2. **Initial population (M)**: Linear scaling of total bites
3. **Speed (v)**: Affects encounter rates and exploration efficiency  
4. **Offspring per bite (Y)**: Exponential population growth potential
5. **Survival time**: Determines population sustainability

### Interesting Phenomena
- **House effect**: People in house1 (with light) typically get more bites
- **Clothing protection**: Person N3 with reduced skin area shows lower bite rates
- **Population dynamics**: Boom-bust cycles depending on feeding success
- **Spatial clustering**: Mosquito populations may concentrate near successful feeding areas

## Customization Options

### Adding New Parameters
The simulation is designed for easy extension:

```r
# Add weather effects
params$wind_strength <- 0.5
params$humidity <- 0.7

# Add multiple light sources
params$lights <- list(
  list(x = 32, y = 30, intensity = 50),
  list(x = 67, y = 70, intensity = 30)
)

# Add more people or different locations
params$people$N5 <- list(x = 50, y = 50, skin_area = 90, location = "outside")
```

### Custom Analysis Functions
```r
# Create custom metrics
calculate_bite_inequality <- function(results) {
  bite_counts <- table(results$bite_log$person)
  gini_coefficient(bite_counts)
}

# Spatial analysis
analyze_bite_locations <- function(results) {
  ggplot(results$bite_log, aes(x = mosquito_x, y = mosquito_y)) +
    geom_density_2d() +
    geom_point(alpha = 0.5) +
    facet_wrap(~person)
}
```

## Performance Notes

- **Default simulation**: ~1000 time steps with 50 mosquitoes takes 5-15 seconds
- **Sensitivity analysis**: Tests 20-30 parameter combinations, takes 2-5 minutes
- **Memory usage**: Scales with mosquito population and simulation time
- **Optimization**: For large-scale studies, consider parallel processing

## Future Extensions

### Potential Enhancements
1. **3D environment**: Add height dimension for more realistic movement
2. **Weather effects**: Wind, rain, temperature influences
3. **Mosquito species**: Different behavior patterns and preferences
4. **Disease transmission**: Model pathogen spread through bites
5. **Intervention strategies**: Nets, repellents, traps
6. **Seasonal dynamics**: Day/night cycles, breeding seasons
7. **Learning behavior**: Mosquitoes learning successful feeding locations

### Research Applications
- **Public health**: Optimize mosquito control strategies
- **Architecture**: Design buildings to minimize mosquito entry
- **Behavioral ecology**: Test hypotheses about mosquito behavior
- **Risk assessment**: Predict bite risk in different environments

## Troubleshooting

### Common Issues
1. **No bites recorded**: Check parameter ranges, increase simulation time
2. **All mosquitoes die**: Reduce survival pressure, increase bite success
3. **Simulation too slow**: Reduce population size or simulation time
4. **Shiny app not loading**: Ensure all packages installed, check file paths

### Performance Tips
- Use shorter simulation times (200-500 steps) for parameter exploration
- Reduce mosquito populations for sensitivity analysis
- Save intermediate results to avoid re-running long simulations

## Contact and Support

This simulation was created as an ecological modeling tool. For questions, suggestions, or contributions, please refer to the code documentation and comments within the R files.

---

*Last updated: August 2025*
