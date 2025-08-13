#' Mosquito bite simulation (fast, vectorized)
#' All code is base R + ggplot2 for visuals. No C++/Rcpp required.
#' Works well for >=10k mosquitoes with modest time steps.

# ==============================
# Utility geometry constructors
# ==============================
new_area <- function(width = 100, height = 60) {
  list(xmin = 0, xmax = width, ymin = 0, ymax = height, width = width, height = height)
}

new_house <- function(xmin, ymin, xmax, ymax, name) {
  stopifnot(xmax > xmin, ymax > ymin)
  list(name = name, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
}

new_window <- function(house, side = c("left","right","top","bottom"),
                       center, w) {
  side <- match.arg(side)
  # Windows are axis-aligned short segments in the wall
  if (side %in% c("left","right")) {
    y1 <- max(house$ymin, min(house$ymax, center - w/2))
    y2 <- max(house$ymin, min(house$ymax, center + w/2))
    x  <- if (side == "left") house$xmin else house$xmax
    list(house = house$name, side = side, x1 = x, y1 = y1, x2 = x, y2 = y2, w = w)
  } else {
    x1 <- max(house$xmin, min(house$xmax, center - w/2))
    x2 <- max(house$xmin, min(house$xmax, center + w/2))
    y  <- if (side == "bottom") house$ymin else house$ymax
    list(house = house$name, side = side, x1 = x1, y1 = y, x2 = x2, y2 = y, w = w)
  }
}

# Person object
# zone: "outside", "H1", "H2", etc. (matching house$name)
# s: effective skin area. Bite radius ~ sqrt(s / pi) * r_scale
new_person <- function(x, y, zone, s, name, covered = FALSE, cover_factor = 0.3) {
  eff_s <- if (covered) s * cover_factor else s
  list(name = name, x = x, y = y, zone = zone, s = s, covered = covered,
       eff_s = eff_s)
}

# Light source object (belongs to a zone)
new_light <- function(x, y, zone, strength = 1.0) {
  list(x = x, y = y, zone = zone, strength = strength)
}

# ==============================
# World builder
# ==============================
build_world <- function(area,
                        houses,
                        windows,
                        people,
                        light) {
  house_index <- setNames(seq_along(houses), vapply(houses, `[[`, "", "name"))
  list(
    area = area,
    houses = houses,
    windows = windows,
    people = people,
    light = light,
    house_index = house_index
  )
}

# Helper: point-in-rect
inside_rect <- function(x, y, rect) {
  (x >= rect$xmin) & (x <= rect$xmax) & (y >= rect$ymin) & (y <= rect$ymax)
}

# Helper: reflect off rectangle walls (outer area)
reflect_bounds <- function(x, y, vx, vy, area) {
  hit_left   <- x < area$xmin
  hit_right  <- x > area$xmax
  hit_bottom <- y < area$ymin
  hit_top    <- y > area$ymax

  vx[hit_left | hit_right] <- -vx[hit_left | hit_right]
  vy[hit_bottom | hit_top] <- -vy[hit_bottom | hit_top]

  x <- pmax(area$xmin, pmin(area$xmax, x))
  y <- pmax(area$ymin, pmin(area$ymax, y))
  list(x = x, y = y, vx = vx, vy = vy)
}

# Helper: reflect off a house's walls for mosquitoes *inside* that house
reflect_house <- function(x, y, vx, vy, house) {
  hit_left   <- x < house$xmin
  hit_right  <- x > house$xmax
  hit_bottom <- y < house$ymin
  hit_top    <- y > house$ymax

  vx[hit_left | hit_right] <- -vx[hit_left | hit_right]
  vy[hit_bottom | hit_top] <- -vy[hit_bottom | hit_top]

  x <- pmax(house$xmin, pmin(house$xmax, x))
  y <- pmax(house$ymin, pmin(house$ymax, y))
  list(x = x, y = y, vx = vx, vy = vy)
}

# Helper: window crossing (outside <-> inside), epsilon proximity
attempt_window_cross <- function(x, y, zone, windows, houses_map, eps = 0.5,
                                 p_enter = 0.6, p_exit = 0.4) {
  # zone values: "outside", or house names
  # returns possibly updated (zone, x, y) if crossing
  n <- length(x)
  if (n == 0L) return(list(zone = zone, x = x, y = y))
  for (w in windows) {
    # Build logical vector of agents adjacent to this window from either side
    if (w$side %in% c("left","right")) {
      near_y <- (y >= min(w$y1,w$y2)-eps) & (y <= max(w$y1,w$y2)+eps)
      near_x <- abs(x - w$x1) <= eps
      near <- near_x & near_y
    } else {
      near_x <- (x >= min(w$x1,w$x2)-eps) & (x <= max(w$x1,w$x2)+eps)
      near_y <- abs(y - w$y1) <= eps
      near <- near_x & near_y
    }
    if (!any(near)) next

    in_house <- zone == w$house
    from_out <- zone == "outside"

    # Outside -> house
    idx_in  <- which(near & from_out)
    if (length(idx_in)) {
      cross <- runif(length(idx_in)) < p_enter
      idx_c <- idx_in[cross]
      zone[idx_c] <- w$house
      # Nudge them just inside the wall to avoid instant reflection
      if (w$side == "left")  x[idx_c] <- x[idx_c] + eps
      if (w$side == "right") x[idx_c] <- x[idx_c] - eps
      if (w$side == "bottom") y[idx_c] <- y[idx_c] + eps
      if (w$side == "top")    y[idx_c] <- y[idx_c] - eps
    }

    # House -> outside
    idx_out <- which(near & in_house)
    if (length(idx_out)) {
      cross <- runif(length(idx_out)) < p_exit
      idx_c <- idx_out[cross]
      zone[idx_c] <- "outside"
      if (w$side == "left")  x[idx_c] <- x[idx_c] - eps
      if (w$side == "right") x[idx_c] <- x[idx_c] + eps
      if (w$side == "bottom") y[idx_c] <- y[idx_c] - eps
      if (w$side == "top")    y[idx_c] <- y[idx_c] + eps
    }
  }
  list(zone = zone, x = x, y = y)
}

# ==============================
# Core simulation
# ==============================
# Parameters:
# M: initial mosquitoes
# steps: total time steps to simulate
# dt: time step size
# v_mean, v_sd: speed parameters
# light_bias (l): [0,1], weight toward light vector in same zone
# Y: offspring per successful bite
# feed_timeout: number of steps a mosquito can go without feeding (dies otherwise)
# bite_cooldown: min steps between two successful bites for the same mosquito
# r_scale: converts skin area to bite radius: r = sqrt(eff_s/pi) * r_scale
# p_bite: probability of bite when within radius in a step
# max_mosq: optional cap to avoid unbounded growth
simulate_bites <- function(world,
                           M = 10000,
                           steps = 600,
                           dt = 1.0,
                           v_mean = 1.2,
                           v_sd = 0.25,
                           light_bias = 0.4,
                           Y = 2L,
                           feed_timeout = 180L,
                           bite_cooldown = 30L,
                           r_scale = 0.3,
                           p_bite = 0.35,
                           max_mosq = 2e5,
                           seed = 1L) {
  stopifnot(M > 0, steps > 0)
  set.seed(seed)

  area <- world$area
  houses <- world$houses
  people <- world$people
  light <- world$light

  # Precompute bite radii
  ppl_names <- vapply(people, `[[`, "", "name")
  ppl_zone  <- vapply(people, `[[`, "", "zone")
  ppl_x     <- vapply(people, `[[`, 0.0, "x")
  ppl_y     <- vapply(people, `[[`, 0.0, "y")
  ppl_r     <- sqrt(vapply(people, `[[`, 0.0, "eff_s") / pi) * r_scale

  # Initial mosquitoes: positions uniform outside
  x <- runif(M, area$xmin, area$xmax)
  y <- runif(M, area$ymin, area$ymax)
  zone <- rep("outside", M)

  # Random initial headings (unit vectors)
  theta <- runif(M, 0, 2*pi)
  vmag  <- pmax(0, rnorm(M, v_mean, v_sd))
  vx <- vmag * cos(theta)
  vy <- vmag * sin(theta)

  alive <- rep(TRUE, M)
  last_feed <- rep(0L, M)    # steps since last bite
  last_bite <- rep(-1e9L, M) # last bite step index

  # Tracking
  total_bites <- integer(length(people))
  names(total_bites) <- ppl_names

  # Pre-allocate offspring buffers
  add_buf_size <- max(1000L, as.integer(M * 0.2))
  buf_x <- numeric(add_buf_size)
  buf_y <- numeric(add_buf_size)
  buf_zone <- character(add_buf_size)
  buf_fill <- 0L

  # Light vectors per zone
  light_zone <- light$zone
  lx <- light$x; ly <- light$y

  # Step loop
  for (t in seq_len(steps)) {
    if (!any(alive)) break

    # ===== Movement =====
    # Random exploration vector (unit)
    th <- runif(sum(alive), 0, 2*pi)
    rand_vx <- cos(th)
    rand_vy <- sin(th)

    # Light drift (only for mosquitoes in same zone)
    idx <- which(alive)
    zx <- x[idx]; zy <- y[idx]
    zzone <- zone[idx]

    toward_lx <- lx - zx
    toward_ly <- ly - zy
    dist_l <- sqrt(toward_lx^2 + toward_ly^2)
    nz <- dist_l > 0
    toward_lx[nz] <- toward_lx[nz] / dist_l[nz]
    toward_ly[nz] <- toward_ly[nz] / dist_l[nz]

    same_zone <- zzone == light_zone
    drift_vx <- ifelse(same_zone, toward_lx, 0)
    drift_vy <- ifelse(same_zone, toward_ly, 0)

    # Combine random + drift; keep previous speed magnitude
    cur_vmag <- sqrt(vx[idx]^2 + vy[idx]^2)
    dir_vx <- (1 - light_bias) * rand_vx + light_bias * drift_vx
    dir_vy <- (1 - light_bias) * rand_vy + light_bias * drift_vy
    dir_norm <- sqrt(dir_vx^2 + dir_vy^2)
    dir_norm[dir_norm == 0] <- 1
    dir_vx <- dir_vx / dir_norm
    dir_vy <- dir_vy / dir_norm

    vx[idx] <- cur_vmag * dir_vx
    vy[idx] <- cur_vmag * dir_vy

    # Euler step
    x[idx] <- x[idx] + vx[idx] * dt
    y[idx] <- y[idx] + vy[idx] * dt

    # Reflect on area bounds for outside mosquitoes
    idx_out <- idx[zone[idx] == "outside"]
    if (length(idx_out)) {
      r <- reflect_bounds(x[idx_out], y[idx_out], vx[idx_out], vy[idx_out], area)
      x[idx_out]  <- r$x; y[idx_out]  <- r$y
      vx[idx_out] <- r$vx; vy[idx_out] <- r$vy
    }

    # Reflect on each house for mosquitoes *inside* that house
    if (length(houses)) {
      for (h in houses) {
        idx_inh <- idx[zone[idx] == h$name]
        if (!length(idx_inh)) next
        r <- reflect_house(x[idx_inh], y[idx_inh], vx[idx_inh], vy[idx_inh], h)
        x[idx_inh]  <- r$x; y[idx_inh]  <- r$y
        vx[idx_inh] <- r$vx; vy[idx_inh] <- r$vy
      }
    }

    # Attempt window crosses (outside <-> houses)
    if (length(world$windows)) {
      cross <- attempt_window_cross(x[idx], y[idx], zone[idx], world$windows, world$house_index)
      x[idx] <- cross$x; y[idx] <- cross$y; zone[idx] <- cross$zone
    }

    # ===== Feeding / bites =====
    # Update hunger timers and mortality
    last_feed[alive] <- last_feed[alive] + 1L
    died <- alive & (last_feed > feed_timeout)
    if (any(died)) alive[died] <- FALSE

    # For each person, check distances for mosquitoes in same zone
    for (p in seq_along(people)) {
      if (!any(alive)) break
      zmatch <- alive & (zone == ppl_zone[p])
      if (!any(zmatch)) next
      dx <- x[zmatch] - ppl_x[p]
      dy <- y[zmatch] - ppl_y[p]
      d2 <- dx*dx + dy*dy
      within <- d2 <= (ppl_r[p]^2)
      if (!any(within)) next

      ids <- which(zmatch)[within]
      # Respect bite cooldown
      can_bite <- (t - last_bite[ids]) >= bite_cooldown
      ids <- ids[can_bite]
      if (!length(ids)) next

      success <- ids[runif(length(ids)) < p_bite]
      if (!length(success)) next

      # Register bites
      total_bites[p] <- total_bites[p] + length(success)
      last_bite[success] <- t
      last_feed[success] <- 0L

      # Spawn offspring Y per successful bite (into same zone, near parent)
      if (Y > 0L) {
        nnew <- as.integer(length(success) * Y)
        if (nnew > 0L) {
          # Grow buffers if needed
          if (buf_fill + nnew > length(buf_x)) {
            grow <- max(nnew, length(buf_x))
            buf_x <- c(buf_x, numeric(grow))
            buf_y <- c(buf_y, numeric(grow))
            buf_zone <- c(buf_zone, character(grow))
          }
          # Place near parent with small random jitter
          px <- x[success]; py <- y[success]; pz <- zone[success]
          jitter_idx <- sample(seq_along(success), nnew, replace = TRUE)
          buf_x[(buf_fill+1):(buf_fill+nnew)] <- px[jitter_idx] + rnorm(nnew, 0, 0.5)
          buf_y[(buf_fill+1):(buf_fill+nnew)] <- py[jitter_idx] + rnorm(nnew, 0, 0.5)
          buf_zone[(buf_fill+1):(buf_fill+nnew)] <- pz[jitter_idx]
          buf_fill <- buf_fill + nnew
        }
      }
    }

    # ===== Integrate offspring (cap population) =====
    if (buf_fill > 0L && sum(alive) < max_mosq) {
      allow <- min(buf_fill, max_mosq - sum(alive))
      # Append
      x <- c(x, buf_x[seq_len(allow)])
      y <- c(y, buf_y[seq_len(allow)])
      zone <- c(zone, buf_zone[seq_len(allow)])

      # Randomize speeds for offspring
      th2 <- runif(allow, 0, 2*pi)
      vmag2 <- pmax(0, rnorm(allow, v_mean, v_sd))
      vx <- c(vx, vmag2 * cos(th2))
      vy <- c(vy, vmag2 * sin(th2))

      alive <- c(alive, rep(TRUE, allow))
      last_feed <- c(last_feed, rep(0L, allow))
      last_bite <- c(last_bite, rep(-1e9L, allow))

      buf_fill <- 0L
    } else {
      buf_fill <- 0L
    }
  }

  # Compose results
  list(
    bites_per_person = total_bites,
    final_count = sum(alive),
    total_spawned = length(alive) - M,
    params = list(M = M, steps = steps, dt = dt, v_mean = v_mean, v_sd = v_sd,
                  light_bias = light_bias, Y = Y, feed_timeout = feed_timeout,
                  bite_cooldown = bite_cooldown, r_scale = r_scale,
                  p_bite = p_bite, max_mosq = max_mosq),
    people = data.frame(name = ppl_names, zone = ppl_zone,
                        x = ppl_x, y = ppl_y, r = ppl_r,
                        bites = as.integer(total_bites)),
    world = world
  )
}

# ==============================
# Visualization helpers (ggplot2)
# ==============================
plot_world <- function(world, people_df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 for plotting: install.packages('ggplot2')")
  }
  library(ggplot2)

  area <- world$area
  houses <- world$houses
  windows <- world$windows
  light <- world$light

  p <- ggplot() +
    coord_fixed(xlim = c(area$xmin, area$xmax),
                ylim = c(area$ymin, area$ymax),
                expand = FALSE) +
    # Outer area
    annotate("rect",
             xmin = area$xmin, xmax = area$xmax,
             ymin = area$ymin, ymax = area$ymax,
             fill = NA, color = "black")

  # Houses
  if (length(houses)) {
    for (h in houses) {
      p <- p + annotate("rect",
                        xmin = h$xmin, xmax = h$xmax,
                        ymin = h$ymin, ymax = h$ymax,
                        fill = NA, color = "gray30", linetype = "solid", size = 0.6)
    }
  }

  # Windows
  if (length(windows)) {
    for (w in windows) {
      p <- p + annotate("segment",
                        x = w$x1, y = w$y1, xend = w$x2, yend = w$y2, color = "steelblue", size = 1.3)
    }
  }

  # People (points)
  p <- p + geom_point(data = people_df, aes(x = x, y = y), size = 2)

  # Bite radii (drawn as paths)
  for (i in seq_len(nrow(people_df))) {
    circ <- data.frame(
      x = people_df$x[i] + people_df$r[i] * cos(seq(0, 2*pi, length.out = 180)),
      y = people_df$y[i] + people_df$r[i] * sin(seq(0, 2*pi, length.out = 180))
    )
    p <- p + geom_path(data = circ, aes(x = x, y = y), size = 0.3)
  }

  # Labels
  p <- p + geom_label(data = people_df, aes(x = x, y = y, label = name),
                      size = 3, label.size = 0.1, vjust = -1)

  # Light
  p <- p +
    annotate("point", x = light$x, y = light$y, shape = 8, size = 3) +
    annotate("label", x = light$x, y = light$y, label = "Light", vjust = 1.5, size = 3)

  p + theme_minimal(base_size = 12)
}


# plot_world <- function(world, people_df) {
#   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#     stop("Please install ggplot2 for plotting: install.packages('ggplot2')")
#   }
#   library(ggplot2)
#
#   area <- world$area
#   houses <- world$houses
#   windows <- world$windows
#   light <- world$light
#
#   p <- ggplot() +
#     coord_fixed(xlim = c(area$xmin, area$xmax), ylim = c(area$ymin, area$ymax), expand = FALSE) +
#     # Area
#     annotate("rect", xmin = area$xmin, xmax = area$xmax, ymin = area$ymin, ymax = area$ymax, fill = NA, color = "black")
#
#   # Houses
#   if (length(houses)) {
#     for (h in houses) {
#       p <- p + annotate("rect", xmin = h$xmin, xmax = h$xmax, ymin = h$ymin, ymax = h$ymax,
#                         fill = NA, color = "gray30", linetype = "solid", size = 0.6)
#     }
#   }
#
#   # Windows
#   if (length(windows)) {
#     for (w in windows) {
#       p <- p + annotate("segment", x = w$x1, y = w$y1, xend = w$x2, yend = w$y2,
#                         color = "steelblue", size = 1.3)
#     }
#   }
#
#   # People (circles with bite radius)
#   p <- p +
#     geom_point(data = people_df, aes(x = x, y = y), size = 2) +
#     ggplot2::geom_point(data = people_df, aes(x = x, y = y), size = 0, alpha = 0) +
#     ggplot2::geom_circle <- function(...) { } # placeholder to avoid NOTE
#
#   # Draw bite radii
#   for (i in seq_len(nrow(people_df))) {
#     circ <- data.frame(
#       x = people_df$x[i] + people_df$r[i] * cos(seq(0, 2*pi, length.out = 180)),
#       y = people_df$y[i] + people_df$r[i] * sin(seq(0, 2*pi, length.out = 180))
#     )
#     p <- p + geom_path(data = circ, aes(x = x, y = y), linewidth = 0.3)
#   }
#
#   # Labels
#   p <- p + geom_label(data = people_df, aes(x = x, y = y, label = name), size = 3, label.size = 0.1, vjust = -1)
#
#   # Light
#   p <- p + annotate("point", x = light$x, y = light$y, shape = 8, size = 3) +
#     annotate("label", x = light$x, y = light$y, label = "Light", vjust = 1.5, size = 3)
#
#   p + ggplot2::theme_minimal(base_size = 12)
# }

plot_bites_bar <- function(sim) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 for plotting: install.packages('ggplot2')")
  }
  library(ggplot2)
  df <- sim$people
  ggplot(df, aes(x = name, y = bites)) +
    geom_col() +
    ggplot2::theme_minimal(base_size = 12) +
    labs(x = "Person", y = "Total bites", title = "Bites per person")
}

# ==============================
# Convenience world presets
# ==============================
default_world <- function() {
  area <- new_area(100, 60)
  H1 <- new_house(10, 15, 35, 45, "H1")
  H2 <- new_house(65, 10, 90, 35, "H2")

  W1 <- new_window(H1, side = "right", center = 30, w = 8)
  W2 <- new_window(H2, side = "left",  center = 22, w = 6)

  # People: two indoors (any houses), two outdoors; one outdoors covered
  P1 <- new_person(x = 20, y = 30, zone = "H1", s = 1.5, name = "N1")
  P2 <- new_person(x = 75, y = 22, zone = "H2", s = 1.5, name = "N2")
  P3 <- new_person(x = 50, y = 50, zone = "outside", s = 2.0, name = "N3")
  P4 <- new_person(x = 40, y = 12, zone = "outside", s = 2.0, name = "N4",
                   covered = TRUE, cover_factor = 0.35)

  # Light in H1 by default
  L  <- new_light(x = 30, y = 40, zone = "H1", strength = 1.0)

  build_world(area,
              houses = list(H1, H2),
              windows = list(W1, W2),
              people  = list(P1, P2, P3, P4),
              light   = L)
}
