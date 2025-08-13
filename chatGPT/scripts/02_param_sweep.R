# Parameter sweep to see influence on per-person bites
source("R/mosquitoSim.R")

#if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

world <- default_world()

# Define ranges to explore (edit freely)
grid <- expand.grid(
  light_bias = c(0.0, 0.25, 0.5, 0.75, 1.0),
  v_mean     = c(0.8, 1.2, 1.6),
  Y          = c(0, 1, 2, 3),
  p_bite     = c(0.2, 0.35, 0.5),
  r_scale    = c(0.25, 0.35, 0.45),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)

run_one <- function(pars, seed_offset = 0) {
  s <- simulate_bites(
    world,
    M = 10000,
    steps = 500,
    dt = 1.0,
    v_mean = pars$v_mean,
    v_sd = 0.25,
    light_bias = pars$light_bias,
    Y = pars$Y,
    feed_timeout = 180,
    bite_cooldown = 25,
    r_scale = pars$r_scale,
    p_bite = pars$p_bite,
    max_mosq = 120000,
    seed = 123 + seed_offset
  )
  data.frame(
    light_bias = pars$light_bias,
    v_mean     = pars$v_mean,
    Y          = pars$Y,
    p_bite     = pars$p_bite,
    r_scale    = pars$r_scale,
    N1 = s$people$bites[s$people$name == "N1"],
    N2 = s$people$bites[s$people$name == "N2"],
    N3 = s$people$bites[s$people$name == "N3"],
    N4 = s$people$bites[s$people$name == "N4"],
    total = sum(s$people$bites)
  )
}

# Run (optionally parallelize with future.apply / parallel if you like)
res_list <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  res_list[[i]] <- run_one(grid[i, ], seed_offset = i)
}
res <- do.call(rbind, res_list)

# Save
dir.create("results", showWarnings = FALSE)
fn <- file.path("results", sprintf("sweep_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")))
write.csv(res, fn, row.names = FALSE)
message("Saved: ", fn)

# Simple “influence” look via linear model (quick-and-dirty)
lm_total <- lm(total ~ light_bias + v_mean + Y + p_bite + r_scale, data = res)
print(summary(lm_total))

# Visuals: partial views
library(ggplot2)

# Total bites vs light bias (facet by Y)
ggplot(res, aes(x = light_bias, y = total, group = Y)) +
  geom_point() + geom_line() +
  facet_wrap(~ Y, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(title = "Total bites vs light_bias by Y", x = "light_bias", y = "Total bites")

# Per-person sensitivity snapshot: effect of r_scale
res_long <- reshape(
  res[, c("light_bias","v_mean","Y","p_bite","r_scale","N1","N2","N3","N4")],
  varying = list(6:9), v.names = "bites",
  timevar = "person", times = c("N1","N2","N3","N4"),
  direction = "long"
)
rownames(res_long) <- NULL

ggplot(res_long, aes(x = r_scale, y = bites, color = person)) +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  theme_minimal(base_size = 12) +
  labs(title = "Per-person bites vs r_scale", x = "r_scale", y = "Bites")

