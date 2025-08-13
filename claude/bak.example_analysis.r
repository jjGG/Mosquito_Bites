# Example Analysis Script for Mosquito Simulation
# Save as "example_analysis.R"

# Load the main simulation
source("mosquito_simulation.R")

# Create results directory if it doesn't exist
if (!dir.exists("results")) dir.create("results")
if (!dir.exists("results/plots")) dir.create("results/plots")
if (!dir.exists("results/data")) dir.create("results/data")

cat("=== MOSQUITO BITE SIMULATION - EXAMPLE ANALYSIS ===\n\n")

# =================== BASIC SIMULATION RUN ===================
cat("1. Running basic simulation with default parameters...\n")
basic_results <- run_simulation(default_params)

# Save basic results
write.csv(basic_results$bite_log, "results/data/basic_bite_log.csv", row.names = FALSE)
write.csv(basic_results$population_log, "results/data/basic_population_log.csv", row.names = FALSE)

# Basic analysis
cat("\nBasic Simulation Results:\n")
cat("Total bites:", nrow(basic_results$bite_log), "\n")
if (nrow(basic_results$bite_log) > 0) {
  bite_summary <- table(basic_results$bite_log$person)
  cat("Bite distribution:\n")
  print(bite_summary)
  
  # Calculate bite rates per person
  cat("\nBite rates (bites per 100 time steps):\n")
  max_time <- max(basic_results$population_log$time)
  bite_rates <- bite_summary / max_time * 100
  print(round(bite_rates, 2))
}

# Visualize basic results
ggsave("results/plots/basic_environment.png", plot_environment(default_params), 
       width = 10, height = 8, dpi = 300)

if (nrow(basic_results$bite_log) > 0) {
  png("results/plots/basic_analysis.png", width = 1200, height = 1600, res = 150)
  plot_bite_analysis(basic_results)
  dev.off()
}

cat("\n" , rep("=", 60), "\n")

# =================== PARAMETER COMPARISON STUDY ===================
cat("2. Running parameter comparison study...\n")

# Test different light attraction levels
light_levels <- c(0.1, 0.3, 0.5, 0.8)
light_results <- list()

cat("Testing light attraction levels:", paste(light_levels, collapse = ", "), "\n")

for (i in seq_along(light_levels)) {
  cat("  Testing l =", light_levels[i], "...")
  
  params <- default_params
  params$l <- light_levels[i]
  params$simulation_time <- 500  # Shorter for comparison study
  
  result <- run_simulation(params)
  light_results[[paste0("l_", light_levels[i])]] <- result
  
  cat(" Total bites:", nrow(result$bite_log), "\n")
}

# Analyze light attraction effects
light_comparison <- data.frame()
for (name in names(light_results)) {
  result <- light_results[[name]]
  light_val <- as.numeric(gsub("l_", "", name))
  
  if (nrow(result$bite_log) > 0) {
    bite_counts <- table(result$bite_log$person)
    for (person in names(bite_counts)) {
      light_comparison <- rbind(light_comparison, data.frame(
        light_attraction = light_val,
        person = person,
        bites = as.numeric(bite_counts[person]),
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Plot light comparison
if (nrow(light_comparison) > 0) {
  p_light <- ggplot(light_comparison, aes(x = light_attraction, y = bites, color = person)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    scale_color_viridis_d() +
    labs(title = "Effect of Light Attraction on Bite Distribution",
         subtitle = "How light attraction parameter affects bites per person",
         x = "Light Attraction Strength (l)", 
         y = "Number of Bites",
         color = "Person") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("results/plots/light_attraction_comparison.png", p_light, 
         width = 10, height = 6, dpi = 300)
}

write.csv(light_comparison, "results/data/light_comparison.csv", row.names = FALSE)

cat("\n" , rep("=", 60), "\n")

# =================== POPULATION DYNAMICS STUDY ===================
cat("3. Running population dynamics study...\n")

# Test different initial mosquito populations
populations <- c(20, 50, 100, 150)
pop_results <- list()

cat("Testing initial populations:", paste(populations, collapse = ", "), "\n")

for (i in seq_along(populations)) {
  cat("  Testing M =", populations[i], "...")
  
  params <- default_params
  params$M <- populations[i]
  params$simulation_time <- 600
  
  result <- run_simulation(params)
  pop_results[[paste0("M_", populations[i])]] <- result
  
  cat(" Total bites:", nrow(result$bite_log), 
      " Final mosquitoes:", length(result$final_mosquitoes), "\n")
}

# Analyze population effects
pop_comparison <- data.frame()
for (name in names(pop_results)) {
  result <- pop_results[[name]]
  pop_val <- as.numeric(gsub("M_", "", name))
  
  pop_comparison <- rbind(pop_comparison, data.frame(
    initial_population = pop_val,
    total_bites = nrow(result$bite_log),
    final_mosquitoes = length(result$final_mosquitoes),
    max_population = max(result$population_log$count),
    avg_population = mean(result$population_log$count),
    stringsAsFactors = FALSE
  ))
}

# Plot population dynamics
p_pop1 <- ggplot(pop_comparison, aes(x = initial_population, y = total_bites)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Population Size vs Total Bites",
       x = "Initial Mosquito Population", 
       y = "Total Bites") +
  theme_minimal()

p_pop2 <- ggplot(pop_comparison, aes(x = initial_population, y = max_population)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Initial vs Maximum Population",
       x = "Initial Mosquito Population", 
       y = "Maximum Population Reached") +
  theme_minimal()

png("results/plots/population_dynamics.png", width = 1200, height = 800, res = 150)
grid.arrange(p_pop1, p_pop2, ncol = 2)
dev.off()

write.csv(pop_comparison, "results/data/population_comparison.csv", row.names = FALSE)

cat("\n" , rep("=", 60), "\n")

# =================== SPEED VS ATTRACTION INTERACTION ===================
cat("4. Running speed vs attraction interaction study...\n")

# Test interaction between speed and light attraction
speeds <- c(1, 2, 3)
attractions <- c(0.2, 0.5, 0.8)

interaction_results <- data.frame()

cat("Testing", length(speeds) * length(attractions), "parameter combinations...\n")

for (v in speeds) {
  for (l in attractions) {
    cat("  Testing v =", v, ", l =", l, "...")
    
    params <- default_params
    params$v <- v
    params$l <- l
    params$simulation_time <- 400  # Shorter for interaction study
    
    result <- run_simulation(params)
    
    # Calculate metrics
    total_bites <- nrow(result$bite_log)
    indoor_bites <- sum(result$bite_log$person %in% c("N1", "N2"))
    outdoor_bites <- sum(result$bite_log$person %in% c("N3", "N4"))
    
    interaction_results <- rbind(interaction_results, data.frame(
      speed = v,
      attraction = l,
      total_bites = total_bites,
      indoor_bites = indoor_bites,
      outdoor_bites = outdoor_bites,
      indoor_ratio = if(total_bites > 0) indoor_bites / total_bites else 0,
      stringsAsFactors = FALSE
    ))
    
    cat(" Total:", total_bites, "bites\n")
  }
}

# Create interaction heatmap
p_interaction <- ggplot(interaction_results, aes(x = factor(speed), y = factor(attraction), fill = total_bites)) +
  geom_tile() +
  geom_text(aes(label = total_bites), color = "white", size = 4) +
  scale_fill_viridis_c() +
  labs(title = "Speed vs Light Attraction: Total Bites",
       subtitle = "Interaction effect on mosquito biting success",
       x = "Speed (v)", 
       y = "Light Attraction (l)",
       fill = "Total Bites") +
  theme_minimal()

ggsave("results/plots/speed_attraction_interaction.png", p_interaction, 
       width = 8, height = 6, dpi = 300)

write.csv(interaction_results, "results/data/interaction_results.csv", row.names = FALSE)

cat("\n" , rep("=", 60), "\n")

# =================== HOUSE PROTECTION ANALYSIS ===================
cat("5. Running house protection analysis...\n")

# Compare scenarios with and without houses (windows closed)
cat("Comparing open houses vs closed houses scenario...\n")

# Scenario 1: Normal (houses with windows)
normal_params <- default_params
normal_params$simulation_time <- 500

# Scenario 2: Houses with very small windows (protective effect)
protected_params <- default_params
protected_params$simulation_time <- 500
protected_params$window_entry_range <- 0.5  # Much harder to enter

normal_result <- run_simulation(normal_params)
protected_result <- run_simulation(protected_params)

# Compare protection effectiveness
protection_analysis <- data.frame(
  scenario = c("Normal Windows", "Small Windows"),
  total_bites = c(nrow(normal_result$bite_log), nrow(protected_result$bite_log)),
  indoor_bites = c(
    sum(normal_result$bite_log$person %in% c("N1", "N2")),
    sum(protected_result$bite_log$person %in% c("N1", "N2"))
  ),
  outdoor_bites = c(
    sum(normal_result$bite_log$person %in% c("N3", "N4")),
    sum(protected_result$bite_log$person %in% c("N3", "N4"))
  ),
  stringsAsFactors = FALSE
)

protection_analysis$indoor_ratio <- protection_analysis$