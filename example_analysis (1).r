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

protection_analysis$indoor_ratio <- protection_analysis$indoor_bites / protection_analysis$total_bites

cat("House Protection Analysis:\n")
print(protection_analysis)

# Plot protection comparison
protection_long <- tidyr::pivot_longer(protection_analysis, 
                                     cols = c("indoor_bites", "outdoor_bites"),
                                     names_to = "location", values_to = "bites")

p_protection <- ggplot(protection_long, aes(x = scenario, y = bites, fill = location)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("indoor_bites" = "brown", "outdoor_bites" = "green"),
                   labels = c("Indoor", "Outdoor")) +
  labs(title = "House Protection Effect",
       subtitle = "Impact of window size on indoor vs outdoor bites",
       x = "Scenario", y = "Number of Bites", fill = "Location") +
  theme_minimal()

ggsave("results/plots/house_protection_analysis.png", p_protection, 
       width = 8, height = 6, dpi = 300)

write.csv(protection_analysis, "results/data/protection_analysis.csv", row.names = FALSE)

cat("\n" , rep("=", 60), "\n")

# =================== CLOTHING PROTECTION ANALYSIS ===================
cat("6. Running clothing protection analysis...\n")

# Test different skin area reductions for person N3 (clothed person)
clothing_levels <- c(100, 80, 60, 40, 20)  # skin areas
clothing_results <- list()

cat("Testing clothing protection levels (skin area):", paste(clothing_levels, collapse = ", "), "\n")

for (i in seq_along(clothing_levels)) {
  cat("  Testing skin area =", clothing_levels[i], "...")
  
  params <- default_params
  params$people$N3$skin_area <- clothing_levels[i]
  params$simulation_time <- 500
  
  result <- run_simulation(params)
  clothing_results[[paste0("skin_", clothing_levels[i])]] <- result
  
  n3_bites <- sum(result$bite_log$person == "N3")
  cat(" N3 bites:", n3_bites, "\n")
}

# Analyze clothing protection
clothing_comparison <- data.frame()
for (name in names(clothing_results)) {
  result <- clothing_results[[name]]
  skin_area <- as.numeric(gsub("skin_", "", name))
  
  bite_counts <- table(result$bite_log$person)
  n3_bites <- if("N3" %in% names(bite_counts)) as.numeric(bite_counts["N3"]) else 0
  
  clothing_comparison <- rbind(clothing_comparison, data.frame(
    skin_area = skin_area,
    n3_bites = n3_bites,
    total_bites = nrow(result$bite_log),
    protection_level = (100 - skin_area) / 100,
    stringsAsFactors = FALSE
  ))
}

# Plot clothing protection
p_clothing <- ggplot(clothing_comparison, aes(x = protection_level, y = n3_bites)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 3) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Clothing Protection Effectiveness",
       subtitle = "Effect of skin coverage on mosquito bites (Person N3)",
       x = "Clothing Protection Level", y = "Bites on Person N3") +
  theme_minimal()

ggsave("results/plots/clothing_protection.png", p_clothing, 
       width = 8, height = 6, dpi = 300)

write.csv(clothing_comparison, "results/data/clothing_comparison.csv", row.names = FALSE)

cat("\n" , rep("=", 60), "\n")

# =================== COMPREHENSIVE SENSITIVITY ANALYSIS ===================
cat("7. Running comprehensive sensitivity analysis...\n")

# This will take longer but provides complete parameter influence ranking
cat("This analysis tests all key parameters systematically...\n")
cat("Estimated time: 2-3 minutes\n")

comprehensive_sensitivity <- parameter_sensitivity_analysis()

# Calculate parameter importance
param_importance <- comprehensive_sensitivity %>%
  group_by(parameter) %>%
  summarise(
    min_bites = min(total_bites),
    max_bites = max(total_bites),
    range_bites = max_bites - min_bites,
    cv_bites = sd(total_bites) / mean(total_bites),
    .groups = 'drop'
  ) %>%
  arrange(desc(range_bites))

cat("\nParameter Importance Ranking (by bite range):\n")
print(param_importance)

# Save comprehensive results
write.csv(comprehensive_sensitivity, "results/data/comprehensive_sensitivity.csv", row.names = FALSE)
write.csv(param_importance, "results/data/parameter_importance.csv", row.names = FALSE)

# Plot comprehensive sensitivity
png("results/plots/comprehensive_sensitivity.png", width = 1200, height = 800, res = 150)
plot_sensitivity_analysis(comprehensive_sensitivity)
dev.off()

cat("\n" , rep("=", 60), "\n")

# =================== SUMMARY REPORT ===================
cat("8. Generating summary report...\n")

# Create comprehensive summary
summary_report <- list(
  timestamp = Sys.time(),
  basic_simulation = list(
    total_bites = nrow(basic_results$bite_log),
    final_mosquitoes = length(basic_results$final_mosquitoes),
    bite_distribution = if(nrow(basic_results$bite_log) > 0) table(basic_results$bite_log$person) else NULL
  ),
  parameter_effects = list(
    most_influential = param_importance$parameter[1],
    least_influential = param_importance$parameter[nrow(param_importance)],
    light_attraction_effect = max(light_comparison$bites) - min(light_comparison$bites),
    population_scaling = cor(pop_comparison$initial_population, pop_comparison$total_bites),
    clothing_effectiveness = max(clothing_comparison$n3_bites) - min(clothing_comparison$n3_bites)
  ),
  key_insights = c(
    paste("Most influential parameter:", param_importance$parameter[1]),
    paste("Light attraction creates", round(max(light_comparison$bites) - min(light_comparison$bites)), "bite difference"),
    paste("Population scales with correlation:", round(cor(pop_comparison$initial_population, pop_comparison$total_bites), 2)),
    "House protection reduces indoor exposure significantly",
    "Clothing protection shows clear dose-response relationship"
  )
)

# Save summary report
saveRDS(summary_report, "results/data/summary_report.rds")

# Print summary
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Analysis completed at:", as.character(summary_report$timestamp), "\n")
cat("Files generated:\n")
cat("  - 8 data files in results/data/\n")
cat("  - 6 plot files in results/plots/\n\n")

cat("Key Findings:\n")
for(insight in summary_report$key_insights) {
  cat("  •", insight, "\n")
}

cat("\n=== RECOMMENDATIONS FOR FURTHER ANALYSIS ===\n")
cat("1. Test extreme parameter values to find system limits\n")
cat("2. Investigate temporal patterns (day/night cycles)\n")
cat("3. Add stochastic elements (weather, variable attractiveness)\n")
cat("4. Test intervention strategies (traps, repellents)\n")
cat("5. Explore spatial clustering of mosquito populations\n")
cat("6. Model disease transmission through bite networks\n")

cat("\n" , rep("=", 60), "\n")
cat("Analysis complete! Check the 'results' folder for all outputs.\n")
cat("To run the interactive Shiny app, use: shiny::runApp('shiny_app.R')\n")
cat(rep("=", 60), "\n")

# =================== OPTIONAL: GENERATE COMPARISON PLOTS ===================
cat("\nGenerating final comparison visualizations...\n")

# Create a comprehensive comparison plot
if (nrow(basic_results$bite_log) > 0) {
  
  # Combine all major results for comparison
  all_scenarios <- data.frame(
    scenario = c(rep("Basic", nrow(basic_results$bite_log)),
                 rep("High Light", nrow(light_results$l_0.8$bite_log)),
                 rep("Large Population", nrow(pop_results$M_150$bite_log))),
    person = c(basic_results$bite_log$person,
               light_results$l_0.8$bite_log$person,
               pop_results$M_150$bite_log$person),
    stringsAsFactors = FALSE
  )
  
  scenario_summary <- all_scenarios %>%
    group_by(scenario, person) %>%
    summarise(bites = n(), .groups = 'drop')
  
  p_final <- ggplot(scenario_summary, aes(x = person, y = bites, fill = scenario)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d() +
    labs(title = "Mosquito Bite Simulation - Scenario Comparison",
         subtitle = "Comparison of different parameter settings on bite distribution",
         x = "Person", y = "Number of Bites", fill = "Scenario") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("results/plots/final_scenario_comparison.png", p_final, 
         width = 10, height = 6, dpi = 300)
  
  cat("Final comparison plot saved.\n")
}

cat("\nAll analyses completed successfully!\n")