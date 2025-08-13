# Quick demo run
source("R/mosquitoSim.R")

world <- default_world()

sim <- simulate_bites(
  world,
  M = 12000,          # starting mosquitoes
  steps = 600,        # time steps
  dt = 1.0,
  v_mean = 1.3,
  v_sd = 0.3,
  light_bias = 0.5,   # attraction to light (0..1)
  Y = 2L,             # offspring per successful bite
  feed_timeout = 200, # must feed within this many steps
  bite_cooldown = 25,
  r_scale = 0.35,     # skin area -> radius scaling
  p_bite = 0.35,
  max_mosq = 150000,
  seed = 7
)

print(sim$bites_per_person)
print(sim$final_count)

# Visualize the world and bite results
# if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

p1 <- plot_world(sim$world, sim$people)
p2 <- plot_bites_bar(sim)

print(p1)
print(p2)

