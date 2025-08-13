# Mosquito Bite Simulation
# Author: Generated for ecological modeling
# Date: August 2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(plotly)
library(shiny)

# Set seed for reproducibility
set.seed(123)

# =================== SIMULATION PARAMETERS ===================
# Default simulation parameters
default_params <- list(
  # Environment
  area_width = 100,
  area_height = 100,
  simulation_time = 1000,  # time steps
  dt = 0.1,  # time step size
  
  # Houses
  house1 = list(x = 20, y = 20, width = 25, height = 20, 
                windows = list(list(x = 22, y = 20, width = 5, height = 2))),
  house2 = list(x = 55, y = 60, width = 25, height = 20,
                windows = list(list(x = 65, y = 60, width = 4, height = 2),
                              list(x = 75, y = 80, width = 3, height = 2))),
  
  # Light source (in house1)
  light = list(x = 32, y = 30, intensity = 50),
  
  # People
  people = list(
    N1 = list(x = 30, y = 30, skin_area = 100, location = "house1"),  # in house1
    N2 = list(x = 67, y = 70, skin_area = 100, location = "house2"),  # in house2
    N3 = list(x = 15, y = 85, skin_area = 80, location = "outside"),   # outside, clothed
    N4 = list(x = 85, y = 15, skin_area = 100, location = "outside")   # outside
  ),
  
  # Mosquito parameters
  M = 50,  # initial number of mosquitoes
  v = 2.0,  # base speed
  l = 0.3,  # light attraction strength
  Y = 3,    # offspring per successful bite
  survival_time = 200,  # time steps before death if no feeding
  bite_range = 3,  # distance within which mosquito can bite
  window_entry_range = 2,  # distance to enter through window
  reproduction_cooldown = 50  # time steps between reproductions
)

# =================== MOSQUITO CLASS ===================
create_mosquito <- function(x, y, params) {
  list(
    x = x,
    y = y,
    vx = rnorm(1, 0, 0.5),  # velocity components
    vy = rnorm(1, 0, 0.5),
    energy = params$survival_time,
    last_bite = -params$reproduction_cooldown,  # can bite immediately
    alive = TRUE,
    inside_house = "outside"  # "outside", "house1", "house2"
  )
}

# =================== UTILITY FUNCTIONS ===================
distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

is_inside_house <- function(x, y, house) {
  x >= house$x && x <= (house$x + house$width) && 
  y >= house$y && y <= (house$y + house$height)
}

can_enter_window <- function(x, y, window, range) {
  # Check if mosquito is near window opening
  window_center_x <- window$x + window$width/2
  window_center_y <- window$y + window$height/2
  distance(x, y, window_center_x, window_center_y) <= range
}

# =================== MOVEMENT FUNCTIONS ===================
update_mosquito_position <- function(mosquito, params, time_step) {
  # Random movement component
  mosquito$vx <- mosquito$vx + rnorm(1, 0, 0.1)
  mosquito$vy <- mosquito$vy + rnorm(1, 0, 0.1)
  
  # Light attraction if light is accessible
  if (mosquito$inside_house == "house1" || mosquito$inside_house == "outside") {
    light_dist <- distance(mosquito$x, mosquito$y, params$light$x, params$light$y)
    if (light_dist > 0) {
      attraction_strength <- params$l * params$light$intensity / (light_dist^2 + 1)
      mosquito$vx <- mosquito$vx + attraction_strength * (params$light$x - mosquito$x) / light_dist
      mosquito$vy <- mosquito$vy + attraction_strength * (params$light$y - mosquito$y) / light_dist
    }
  }
  
  # Limit velocity
  vel_magnitude <- sqrt(mosquito$vx^2 + mosquito$vy^2)
  if (vel_magnitude > params$v) {
    mosquito$vx <- mosquito$vx * params$v / vel_magnitude
    mosquito$vy <- mosquito$vy * params$v / vel_magnitude
  }
  
  # Update position
  new_x <- mosquito$x + mosquito$vx * params$dt
  new_y <- mosquito$y + mosquito$vy * params$dt
  
  # Handle house entry/exit and boundaries
  if (mosquito$inside_house == "outside") {
    # Check window entry
    for (house_name in c("house1", "house2")) {
      house <- params[[house_name]]
      for (window in house$windows) {
        if (can_enter_window(new_x, new_y, window, params$window_entry_range)) {
          mosquito$inside_house <- house_name
          break
        }
      }
      if (mosquito$inside_house != "outside") break
    }
    
    # Boundary conditions for outside
    new_x <- max(0, min(params$area_width, new_x))
    new_y <- max(0, min(params$area_height, new_y))
  } else {
    # Inside house
    house <- params[[mosquito$inside_house]]
    
    # Check if trying to exit through window
    exiting <- FALSE
    for (window in house$windows) {
      if (can_enter_window(new_x, new_y, window, params$window_entry_range)) {
        mosquito$inside_house <- "outside"
        exiting <- TRUE
        break
      }
    }
    
    if (!exiting) {
      # Keep within house boundaries
      new_x <- max(house$x, min(house$x + house$width, new_x))
      new_y <- max(house$y, min(house$y + house$height, new_y))
    }
  }
  
  mosquito$x <- new_x
  mosquito$y <- new_y
  
  # Decrease energy
  mosquito$energy <- mosquito$energy - 1
  if (mosquito$energy <= 0) {
    mosquito$alive <- FALSE
  }
  
  return(mosquito)
}

# =================== BITING FUNCTION ===================
attempt_bite <- function(mosquito, people, params, time_step, bite_log) {
  new_mosquitoes <- list()
  
  if (!mosquito$alive) return(list(mosquito = mosquito, new_mosquitoes = new_mosquitoes, bite_log = bite_log))
  
  for (person_name in names(people)) {
    person <- people[[person_name]]
    
    # Check if person is accessible to mosquito
    person_accessible <- FALSE
    if (mosquito$inside_house == "outside" && person$location == "outside") {
      person_accessible <- TRUE
    } else if (mosquito$inside_house == person$location) {
      person_accessible <- TRUE
    }
    
    if (person_accessible) {
      dist <- distance(mosquito$x, mosquito$y, person$x, person$y)
      
      if (dist <= params$bite_range) {
        # Bite probability based on skin area
        bite_prob <- person$skin_area / 1000  # normalize
        
        if (runif(1) < bite_prob && (time_step - mosquito$last_bite) >= params$reproduction_cooldown) {
          # Successful bite!
          mosquito$last_bite <- time_step
          mosquito$energy <- params$survival_time  # reset energy
          
          # Log the bite
          bite_log <- rbind(bite_log, data.frame(
            time = time_step,
            person = person_name,
            mosquito_x = mosquito$x,
            mosquito_y = mosquito$y,
            stringsAsFactors = FALSE
          ))
          
          # Create offspring
          for (i in 1:params$Y) {
            offspring_x <- mosquito$x + rnorm(1, 0, 2)
            offspring_y <- mosquito$y + rnorm(1, 0, 2)
            new_mosquitoes <- append(new_mosquitoes, list(create_mosquito(offspring_x, offspring_y, params)))
          }
          
          break  # One bite per time step
        }
      }
    }
  }
  
  return(list(mosquito = mosquito, new_mosquitoes = new_mosquitoes, bite_log = bite_log))
}

# =================== MAIN SIMULATION FUNCTION ===================
run_simulation <- function(params = default_params) {
  cat("Starting simulation with", params$M, "mosquitoes...\n")
  
  # Initialize mosquitoes
  mosquitoes <- list()
  for (i in 1:params$M) {
    x <- runif(1, 0, params$area_width)
    y <- runif(1, 0, params$area_height)
    mosquitoes <- append(mosquitoes, list(create_mosquito(x, y, params)))
  }
  
  # Initialize bite log
  bite_log <- data.frame(time = numeric(), person = character(), 
                        mosquito_x = numeric(), mosquito_y = numeric(),
                        stringsAsFactors = FALSE)
  
  # Store mosquito populations over time
  population_log <- data.frame(time = numeric(), count = numeric())
  
  # Run simulation
  for (t in 1:params$simulation_time) {
    if (t %% 100 == 0) cat("Time step:", t, "- Mosquitoes:", length(mosquitoes), "\n")
    
    # Update mosquito positions and handle biting
    new_mosquitoes_all <- list()
    for (i in length(mosquitoes):1) {  # reverse order for safe removal
      if (mosquitoes[[i]]$alive) {
        mosquitoes[[i]] <- update_mosquito_position(mosquitoes[[i]], params, t)
        
        bite_result <- attempt_bite(mosquitoes[[i]], params$people, params, t, bite_log)
        mosquitoes[[i]] <- bite_result$mosquito
        new_mosquitoes_all <- c(new_mosquitoes_all, bite_result$new_mosquitoes)
        bite_log <- bite_result$bite_log
      }
      
      if (!mosquitoes[[i]]$alive) {
        mosquitoes <- mosquitoes[-i]
      }
    }
    
    # Add new mosquitoes
    mosquitoes <- c(mosquitoes, new_mosquitoes_all)
    
    # Log population
    population_log <- rbind(population_log, data.frame(time = t, count = length(mosquitoes)))
    
    # Stop if no mosquitoes left
    if (length(mosquitoes) == 0) {
      cat("All mosquitoes died at time step:", t, "\n")
      break
    }
  }
  
  cat("Simulation completed!\n")
  cat("Total bites:", nrow(bite_log), "\n")
  
  return(list(
    bite_log = bite_log,
    population_log = population_log,
    final_mosquitoes = mosquitoes,
    params = params
  ))
}

# =================== VISUALIZATION FUNCTIONS ===================
plot_environment <- function(params) {
  # Create environment plot
  p <- ggplot() +
    # Area boundary
    geom_rect(aes(xmin = 0, ymin = 0, xmax = params$area_width, ymax = params$area_height),
              fill = "lightblue", alpha = 0.1, color = "black") +
    
    # Houses
    geom_rect(aes(xmin = params$house1$x, ymin = params$house1$y, 
                  xmax = params$house1$x + params$house1$width, 
                  ymax = params$house1$y + params$house1$height),
              fill = "brown", alpha = 0.3, color = "black") +
    geom_rect(aes(xmin = params$house2$x, ymin = params$house2$y, 
                  xmax = params$house2$x + params$house2$width, 
                  ymax = params$house2$y + params$house2$height),
              fill = "brown", alpha = 0.3, color = "black") +
    
    # Windows for house1
    geom_rect(aes(xmin = 22, ymin = 20, xmax = 27, ymax = 22),
              fill = "white", color = "blue") +
    # Windows for house2
    geom_rect(aes(xmin = 65, ymin = 60, xmax = 69, ymax = 62),
              fill = "white", color = "blue") +
    geom_rect(aes(xmin = 75, ymin = 80, xmax = 78, ymax = 82),
              fill = "white", color = "blue") +
    
    # Light source
    geom_point(aes(x = params$light$x, y = params$light$y), 
               color = "yellow", size = 5, shape = 8) +
    
    # People
    geom_point(aes(x = params$people$N1$x, y = params$people$N1$y), 
               color = "red", size = 4) +
    geom_point(aes(x = params$people$N2$x, y = params$people$N2$y), 
               color = "red", size = 4) +
    geom_point(aes(x = params$people$N3$x, y = params$people$N3$y), 
               color = "orange", size = 3) +  # clothed person (smaller)
    geom_point(aes(x = params$people$N4$x, y = params$people$N4$y), 
               color = "red", size = 4) +
    
    # Labels
    annotate("text", x = params$people$N1$x, y = params$people$N1$y + 3, label = "N1", size = 3) +
    annotate("text", x = params$people$N2$x, y = params$people$N2$y + 3, label = "N2", size = 3) +
    annotate("text", x = params$people$N3$x, y = params$people$N3$y + 3, label = "N3", size = 3) +
    annotate("text", x = params$people$N4$x, y = params$people$N4$y + 3, label = "N4", size = 3) +
    annotate("text", x = params$house1$x + 12, y = params$house1$y + 10, label = "House 1", size = 3) +
    annotate("text", x = params$house2$x + 12, y = params$house2$y + 10, label = "House 2", size = 3) +
    annotate("text", x = params$light$x, y = params$light$y - 3, label = "Light", size = 3) +
    
    coord_fixed() +
    labs(title = "Mosquito Simulation Environment",
         subtitle = "Red = People, Orange = Clothed Person, Yellow Star = Light, Blue = Windows",
         x = "X Position", y = "Y Position") +
    theme_minimal()
  
  return(p)
}

plot_bite_analysis <- function(results) {
  if (nrow(results$bite_log) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No bites recorded", size = 6) +
           theme_void())
  }
  
  # Bite counts by person
  bite_counts <- results$bite_log %>%
    group_by(person) %>%
    summarise(bites = n(), .groups = 'drop')
  
  p1 <- ggplot(bite_counts, aes(x = person, y = bites, fill = person)) +
    geom_col() +
    scale_fill_viridis_d() +
    labs(title = "Total Bites per Person", x = "Person", y = "Number of Bites") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Bites over time
  bite_timeline <- results$bite_log %>%
    mutate(time_bin = floor(time / 50) * 50) %>%
    group_by(time_bin, person) %>%
    summarise(bites = n(), .groups = 'drop')
  
  p2 <- ggplot(bite_timeline, aes(x = time_bin, y = bites, color = person)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    labs(title = "Bites Over Time", x = "Time Step", y = "Bites per 50 time steps") +
    theme_minimal()
  
  # Population over time
  p3 <- ggplot(results$population_log, aes(x = time, y = count)) +
    geom_line(color = "darkgreen", size = 1) +
    labs(title = "Mosquito Population Over Time", x = "Time Step", y = "Number of Mosquitoes") +
    theme_minimal()
  
  return(grid.arrange(p1, p2, p3, ncol = 1))
}

# =================== PARAMETER SENSITIVITY ANALYSIS ===================
parameter_sensitivity_analysis <- function() {
  cat("Running parameter sensitivity analysis...\n")
  
  # Parameters to test
  test_params <- list(
    v = seq(1, 4, by = 0.5),      # speed
    l = seq(0.1, 0.8, by = 0.1),  # light attraction
    Y = c(1, 2, 3, 4, 5),         # offspring per bite
    M = seq(20, 80, by = 20)      # initial mosquito count
  )
  
  results_sensitivity <- data.frame()
  
  for (param_name in names(test_params)) {
    for (param_value in test_params[[param_name]]) {
      cat(paste("Testing", param_name, "=", param_value, "\n"))
      
      # Create modified parameters
      test_param_set <- default_params
      test_param_set[[param_name]] <- param_value
      test_param_set$simulation_time <- 500  # shorter for sensitivity analysis
      
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
  
  return(results_sensitivity)
}

plot_sensitivity_analysis <- function(sensitivity_results) {
  p1 <- ggplot(sensitivity_results, aes(x = value, y = total_bites, color = parameter)) +
    geom_line() +
    geom_point() +
    facet_wrap(~parameter, scales = "free_x") +
    labs(title = "Parameter Sensitivity: Total Bites",
         x = "Parameter Value", y = "Total Bites") +
    theme_minimal() +
    theme(legend.position = "none")
  
  p2 <- ggplot(sensitivity_results, aes(x = value, y = final_mosquito_count, color = parameter)) +
    geom_line() +
    geom_point() +
    facet_wrap(~parameter, scales = "free_x") +
    labs(title = "Parameter Sensitivity: Final Mosquito Population",
         x = "Parameter Value", y = "Final Mosquito Count") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(grid.arrange(p1, p2, ncol = 1))
}

# =================== MAIN EXECUTION ===================
cat("Mosquito Bite Simulation Initialized!\n")
cat("Use the following functions:\n")
cat("1. run_simulation() - Run main simulation\n")
cat("2. plot_environment(default_params) - Visualize environment\n") 
cat("3. parameter_sensitivity_analysis() - Test parameter effects\n")
cat("4. Example: results <- run_simulation(); plot_bite_analysis(results)\n")

# Example run (uncomment to execute immediately)
# cat("\nRunning example simulation...\n")
# results <- run_simulation()
# plot_environment(default_params)
# plot_bite_analysis(results)
