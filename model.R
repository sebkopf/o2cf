# utility functions for modeling ==========

#' Retrieve value for a constant (from constants data.frame)
#' 
#' Make it easy to pull out specific values (and check that they have the expected units)
#' @param id paramter id (e.g. D_o2)
#' @param units if set, checks that the def_param value has recorded the expected units
get_default <- function(id, units = NULL) {
  if (!exists("constants")) 
    stop("The default parameters data.frame ('constants') does not seem to be set. Make sure it is loaded into the workspace.")
  value <- subset(constants, ID == id, select = "Value", drop = T)
  if (length(value) != 1L) 
    stop("Can't clearly identifiy the default for '", id, "', ", length(value), " entries found.")
  if (!is.null(units) && !identical(units, (punits <- subset(constants, ID == id, select = "Units", drop = T)))) 
    stop("Recorded units (", punits, ") and expected units (", units, ") are not identical!")
  return(value)
}


# non-dimensionalization functions for modeling ======

#' non-dimensionalize concentrations 
#' @param conc_o2 in µM = µmol / L
#' @param k_o2 oxygen half saturation constant in mol O2 / m^3 = 1000 µmol / L
non_dim_o2 <- function (conc_o2, k_o2 = get_default("k_o2", "mol O2 m-3")) { 
  k_o2 <- 1000 * k_o2 #  convert mol O2 / m^3 to µmol/L
  return(conc_o2 / k_o2)
}
re_dim_o2 <- function(non_dim_o2, k_o2 = get_default("k_o2", "mol O2 m-3")) 
  return(non_dim_o2 * 1000 * k_o2)

#' non-dimensionalize spatial scales
#' @param x in µm
#' @param scaling spatial scaling factor in 1/m
non_dim_x <- function(x, scaling = calc_spatial_scaling_factor(...), ...) {
  x <- 10^(-6) * x # convert µm to m
  return(x * scaling)
} 
re_dim_x <- function(non_dim_x, scaling = calc_spatial_scaling_factor(...), ...) 
  return(10^6 * non_dim_x / scaling)

#' non-dimensionalize temporal scales
#' @param t in s
#' @param scaling temporal scaling factor in 1/s
non_dim_t <- function(t, scaling = calc_temporal_scaling_factor(...), ...) {
  return(t * scaling)
}
re_dim_t <- function(non_dim_t, scaling = calc_temporal_scaling_factor(...), ...) 
  return(non_dim_t / scaling)

#' calculate the spatial scaling factor
#' @param D_o2 oxygen diffusion coefficient in 10^-6 cm^2 / s
#' @param k_o2 oxygen half saturation constant in mol O2 / m^3
#' @param mu_max maximal specific growth rate in 1/s
#' @param yield growth yield per oxygen in g cell / mol O2
#' @param cells log of cells/ml
#' @param weight single cell weight in 10^-12 g/cell
calc_spatial_scaling_factor <- 
  function(D_o2 = get_default("D_o2_biofilm", "10-6 cm2 s-1"), k_o2 = get_default("k_o2", "mol O2 m-3"),
           mu_max = get_default("µ_max", "s-1"), yield = get_default("Y_o2", "g cell / mol O2"), 
           cells = get_default("cells", "log (cells / ml)"), cell_weight = get_default("weight", "10-12 g / cell"), ...) {
    D_o2 <- D_o2 * 10^(-10) # convert from 10^-6 cm2/s to m2/s
    cells <- 10^6 * 10^cells * 10^(-12) * cell_weight  # convert from cells/ml to cells/m3 to g/m3
    return(sqrt(cells * mu_max / ( k_o2 * yield * D_o2 )))
  }

#' calculate the temporal scaling factor
#' @param same as spatial scaling except for diffusion coefficient
calc_temporal_scaling_factor <-
  function(k_o2 = get_default("k_o2", "mol O2 m-3"), mu_max = get_default("µ_max", "s-1"), yield = get_default("Y_o2", "g cell / mol O2"), 
           cells = get_default("cells", "log (cells / ml)"), cell_weight = get_default("weight", "10-12 g / cell"), ...) {
    cells <- 10^6 * 10^cells * 10^(-12) * cell_weight  # convert from cells/ml to cells/m3 to g/m3
    return(cells * mu_max / ( k_o2 * yield ))
  }

#' calculate the maintenance constant
#' @param P_o2 maintenance coefficient for O2 in 10^-7 mol O2 / cells s
calc_maintenance_term <-
  function(P_o2 = get_default("P_o2", "10-7 mol O2 g cells-1 s-1"), mu_max = get_default("µ_max", "s-1"), yield = get_default("Y_o2", "g cell / mol O2"), ...) {
    return(yield * 10^-7 * P_o2 / mu_max)
  }

# direct modeling functions ==============

#' Model steady-state O2 concentrations
#' 
#' @param x_max the outer boundary of the model domain (in µm)
#' @param x_min the inner boundary of the model domain (in µm)
#' @param o2_max maximum oxygen at air-mucus boundary in µM
#' @param x_scaling spatial scaling factor (calculated from default parameters by default)
#' @param g maintenance term (caculated from default parameters by default)
#' @param geometry whether model simulates a "column" (O2 diffusion along a straight line), a "ring" (radial diffusion) or a "sphere" (spherical diffusion)
#' @param direction whether the oxygen maximum is at the inner boundary / x_min ("outward") or outer boundary / x_max ("inward")
#' @param step_func the function that takes the oxygen concentration, diffusive change and maintenance energy to calculate the overall dO2
#' @param N number of grid points (spatial resolution)
#' @param [arc]tol tolerance levels for steady state, see stode documentation for details
#' @param verbose whether to run solver verbose
#' @param ... passed to the scaling and maintenance calculation terms so could be e.g. k_o2
model_o2 <- function(x_max, x_min = 0, o2_max = get_default("o2_max", "µM"), 
  x_factor = calc_spatial_scaling_factor(...), g = calc_maintenance_term(...), 
  geometry = c("column", "ring", "sphere"), direction = c("inward", "outward"),
  step_func = o2_diff_resp_step, N = 1000, 
  atol = 1e-8, rtol = 1e-8, ctol = 1e-8, verbose = FALSE, ...) {
  
  # diffusion type and direction check
  geometry <- match.arg(geometry)
  direction <- match.arg(direction)
  
  # consistency checks
  if (x_max <= x_min)
    stop("The outer boundary (x_max = ", x_max, ") must be larger than than the inner boundary (x_min = ", x_min, "). ",
         "To reverse the direction of oxygen diffusion, change the 'direction' parameter instead.")
  
  if (x_min < 0 | x_max <= 0 | o2_max <= 0)
    stop("Boundary conditions cannot be negative and the outer boundary (x_max) and oxygen maximum must be positive.")
  
  if (x_min == 0 && direction == "outward" && geometry != "column")
    stop("For outward diffusion in ring or spherical geometries, the inner boundary (x_min) must be greater than 0.")
  
  # model parameters
  non_dim_x1 = non_dim_x(x_min, x_factor) # normalized inner radius
  non_dim_x2 = non_dim_x(x_max, x_factor) # normalized outer radius
  non_dim_o2_max = non_dim_o2(o2_max) # normalized o2 max
  
  if (verbose) {
    message("Running O2 model to steady state with the following parameters:")
    message("Spatial scaling: ", round(x_factor, 1), ", norm x1: ", round(non_dim_x1, 1), 
            ", norm x2: ", round(non_dim_x2, 1), ", norm o2 max: ", round(non_dim_o2_max, 1), 
            ", grid points: ", N)
  }
  
  # model grid
  dx <- (non_dim_x2 - non_dim_x1)/N # grid size
  non_dim_x <- seq (non_dim_x1, non_dim_x2, len = N+1) # grid
  
  # geometry
  column   <- 1
  ring   <- 2*pi*non_dim_x
  sphere <- 4*pi*non_dim_x^2
  area <- get(geometry)
  
  # the model with inward diffusion
  diffuse_in <- function (t, y, parms)  {
    do2_diff <- tran.1D (y, C.down = non_dim_o2_max, flux.up = 0, D = 1, A = area, dx = dx)$dC
    do2 <- step_func(o2 = y, do2_diff, g)
    return (list(do2))
  }
  
  # the model with outward diffusion
  diffuse_out <- function (t, y, parms)  {
    do2_diff <- tran.1D (y, C.up = non_dim_o2_max, flux.down = 0, D = 1, A = area, dx = dx)$dC
    do2 <- step_func(o2 = y, do2_diff, g)
    return (list(do2))
  }
  
  # run to steady state
  model <- if (direction == "inward") diffuse_in else diffuse_out
  timing <- system.time(
    out <- steady.1D (y = rep(non_dim_o2_max, N), nspec = 1, positive = TRUE,
                      func = model,
                      atol = atol, rtol = rtol, ctol = ctol, verbose = verbose))
  
  if (attributes(out)$steady && verbose) 
    message("Reached steady state (elapsed time: ", signif(timing[['elapsed']], 5), "s).")
  else if (!attributes(out)$steady)
    warning("Did not reach steady state (elapsed time: ", signif(timing[['elapsed']], 5), "s). Please run verbose for more information.")
  
  # return re-dimensionalized output
  result <- data.frame(
    non_dim_x = non_dim_x, 
    non_dim_o2 = switch(direction,
                        outward = c(non_dim_o2_max, out$y),
                        inward = c(out$y, non_dim_o2_max)))
  result <- 
    mutate(result,
           x = re_dim_x(non_dim_x, x_factor),
           o2 = re_dim_o2(non_dim_o2),
           scaling = x_factor, # not the most efficient way to return these stats but makes it easier to work with ddply
           elapsed = timing[['elapsed']], 
           steady = attributes(out)$steady)
  return(result)
}

#' Defines what happens at a modeling step.
#' @param o2 the concentration at the previous time step
#' @param do2_diff the change in concentration purely from diffusion
#' @param g the maintenance term
#' @return the total change in concentration of this time step
o2_diff_resp_step <- function(o2, do2_diff, g) {
  # diffusion, respiration and maintenance
  do2 <- do2_diff - o2 / (1 + o2) - g 
  
  # check where oxygen falls below 0 --> cells are dead, no longer any maintenance 
  # --> remove non-linear maintenance term (g) at these grid points
  idx <- (o2 + do2) <= 0 
  if (length(idx) > 0) 
    do2[idx] <- do2[idx] + g
  
  return(do2)
}

#' calculate the monod growth rate from oxygen concentration
#' @param o2 the oxygen concentration, in µM
#' @param k_o2 oxygen half saturation constant in mol O2 / m^3
#' @param mu_max maximal specific growth rate in 1/s
#' @return growth rate in 1/hr (!)
calculate_monod_growth_rate <- 
  function(o2, k_o2 = get_default("k_o2", "mol O2 m-3"), mu_max = get_default("µ_max", "s-1")) {  
  k_o2 <- 1000 * k_o2 #  convert mol O2 / m^3 to µmol/L
  mu_max <- 60 * 60 * mu_max # convert from s-1 to hr-1
  mu.hr <- rep(0, length(o2)) # growth rate array 
  mu.hr[o2>0] <- mu_max * o2[o2>0] / (k_o2 + o2[o2>0]) # calculate rate everywhere o2 > 0
  return (mu.hr)
}

#' summarize model results with summary statistics
#' @param data the data frame with the model output
#' @param params the parameter data frame that went into the model
summarize_model_results <- function(data, params) {
  ddply(data, names(params), summarize, 
        `scaling factor` = unique(scaling),
        `grid size` = length(x),
        `inner radius [µm]` = min(x),
        `outer radius [µm]` = max(x),
        `O2 minimum [µM]` = signif(min(o2),3),
        `model` = ifelse(all(steady), 
                         paste0("Reached steady state (elapsed time: ", signif(unique(elapsed), 5), "s)."),
                         paste0("Did not reach steady state (elapsed time: ", signif(unique(elapsed), 5), "s)."))
  )
}


# model evaluation functions ==============

#' Find the minimal mucus thickness at which oxygen concentrations reach below the indicated level
#' @param o2 the cutoff concentration for which to find the minimal thickness
#' @param start_x the minimal thickness to consider (in µm)
#' @param ... all other parameters are passed on to the model_o2 function (see parameters)
find_mucus_thickness <- function(o2, start_x = 50, ...) {
  if (o2 <= 0)
    stop("cutoff concentrations must be greater than 0")
  
  if (!is.null(list(...)$x_max))
    stop("this function searches for x_max, it can not be a specified parameter!")
  
  # make sure start_x is valid
  start_o2 <- tryCatch(min(model_o2(start_x, ...)$o2), warning = function(w) w)
  if (is(start_o2, "warning") && grepl("steady-state not reached", start_o2)) {
    stop("could not find a steady-state solution for the oxygen model at the starting condition ",
         "(start_x = ", start_x, "), please increase start_x")
  } else if (!is(start_o2, "numeric")) {
    stop(start_o2)
  } else if (start_o2 <= o2) {
    stop("starting condition (start_x = ", start_x, ") is already thicker than minimum required ",
         "to reach the o2 cutoff (start o2 = ", signif(start_o2, 2), ", cutoff = ", o2, "), please decrease start_x")
  }
    
  # minimization function
  minimal_o2 <- function(x, cutoff, ...) min(suppressWarnings(model_o2(x, ...))$o2) - cutoff
  
  timing <- system.time(
    out <- uniroot(minimal_o2, lower = start_x, upper = 10*start_x, extendInt = "downX", cutoff = o2, ...))
  
  return(data.frame(o2_cutoff = o2, depth = out$root, elapsed = timing[['elapsed']]))
}

# plotting functions for modelling ===============

#' Convert polar data to a cartesian grid
#' @param r the radial grid
#' @param z the data at each radial grid point
#' @param N number of grid points in the default x and y grids
#' @param x the x coordinate grid (by default from max -r to +r in N steps)
#' @param y the y coordinate grid (by default from max -r to +r in N steps)
#' @return data.frame with x, y and the interpolated z for each (x,y) point that is valid (r_min^2 <= x^2 + y^2 <= r_max^2)
convert_polar_to_cartesian <- function(r, z, N = 401, x = default_grid(r, N), y = default_grid(r, N)) {
  load_pkgs("ReacTran")
  default_grid <- function(r, N) seq(-max(r), max(r), length.out=N)
  cart <- data.frame(
    x = rep(x, times = length(y)),
    y = rep(y, each = length(x)),
    z = as.vector(polar2cart(z, r = r, theta = NULL, x = x, y = y))
  )
  return(subset(cart, x^2 + y^2 >= min(r)^2 & x^2 + y^2 <= max(r)^2))
}