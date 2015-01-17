# data reading functions ===============

#' Read O2 profiles from the sheet of an excel file.
#' Assumes spreadsheet to have Date and Patient info in the 1st row, Depth and Concentration header in the 2nd, and data starting in the 3rd.
#' @param file the path to the excel file
#' @param sheet the name of the sheet to read
#' @param long format data frame with columns: profile, depth, conc, date, patient, orp, h2s, n2o
read_oxycline_data <- function(file, sheet) {
  info <- read.xlsx(file, startRow = 1, endRow = 5, sheetName = sheet, stringsAsFactors = F, header = F, check.names=F)
  profiles <- read.xlsx(file, startRow = 5, sheetName = sheet, stringsAsFactors = F, header=T, check.names=F)
  conc_cols <- grep("Concentration", names(profiles)) 
  date <- read_excel_date(info[1, conc_cols - 1]) 
  patient <- as.character(info[1, conc_cols])
  orp <- as.character(info[2, conc_cols - 1])
  h2s <- as.character(info[3, conc_cols - 1])
  n2o <- as.character(info[4, conc_cols - 1])
  data <- 
    merge(
      na.omit(cbind(
        melt(setNames(profiles[conc_cols - 1], seq_along(conc_cols)), id.vars = NULL, variable.name = "profile", value.name = "depth"),
        melt(setNames(profiles[conc_cols], seq_along(conc_cols)), id.vars = NULL, value.name = "conc")[,-1, drop=F]
      )),
      data.frame(profile = seq_along(conc_cols), date = date, patient = patient,
                 orp = ifelse(is.na(orp), "N/A", orp), h2s = ifelse(is.na(h2s), "N/A", h2s), n2o = ifelse(is.na(n2o), "N/A", n2o)),
      by = "profile")
  return(mutate(data, profile = paste0(sheet, ".", profile)))
}

#' Finds mid points in o2 profiles by determining the half way point between when o2 first falls below / raises back 
#' up above the threshold value.
#' @param low_o2_threshold low oxygen concentration threshold (in µM)
#' @return df with additional columns low_o2, min/max_low_o2_depth, midpoint_depth
find_midpoints <- function(df, low_o2_threshold = 1, group_by = c("profile", "date", "patient")) {
  df$low_o2_threshold <- low_o2_threshold
  ddply(df, group_by, mutate, 
        low_o2 = conc < low_o2_threshold,
        min_low_o2_depth = min(depth[low_o2]),
        max_low_o2_depth = max(depth[low_o2]),
        midpoint_depth = (min_low_o2_depth + max_low_o2_depth)/2,
        above_midpoint = depth <= midpoint_depth
  )
}

#' Identifies beginning of the oxycline in each profile.
#' Modeled after laplacian of gaussian (LoG) edge detection approaches (but only in 1D)
#' 
#' Oxycline = the biggest continuous drop in O2 concentration (i.e. largest dC over range where gradient dC/dx < 0).
#' Oxycline start = the steepest change in the gradient (local minimum of -d2C/dx2)
#' The oxygen profile is smoothed with a gaussian kernel for this purpose to reduce noise.
#' 
#' @param kernel_width smoothing width of the gaussian
#' @param no_o2_cutoff cutoff concentration for determining end of oxycline (in µM)
#' @return df with additional columns conc_smooth, conc_x, conc_xx, oxycline_start, oxycline_conc, is_oxycline (TRUE/FALSE), n_oxycline_points
find_oxyclines <- function(df, kernel_width = 5, no_o2_cutoff = get_default("o2_cutoff", "µM"), group_by = c("profile", "date", "patient")) {
  
  # normalized gaussian smoothing kernel
  smooth_kernel <- dnorm(-kernel_width:kernel_width)
  smooth_kernel <- smooth_kernel/sum(smooth_kernel)
    
  apply_kernel <- function(x, kernel) {
    width <- (length(kernel) - 1)/2
    as.numeric(
      filter(c(rep(x[1], width), x, rep(x[length(x)], width)), kernel)
    )[(1 + width):(length(x) + width)]
  }
  
  # identify oxycline
  ddply(df, group_by, function(sub_df) {
    with(sub_df, {
      conc_smooth <- apply_kernel(conc, smooth_kernel) # smooth oxygen profile to reduce noise
      conc_x <- apply_kernel(conc_smooth, c(1/2, 0, -1/2)) # 1st spatial derivative 
      conc_xx <- apply_kernel(conc_smooth, c(1, -2, 1)) # 2nd spatial derivative 
      
      # divide oxygen profile into segments by the 0 crossings of the gradient
      # --> for efficiency consider only the top half of the profile
      segs <- c(TRUE, diff(conc_x >= 0) != 0) & (depth < midpoint_depth)
      segs[abs(depth - midpoint_depth) == min(abs(depth - midpoint_depth))] <- TRUE # make sure last segment before midpoint is evaluated
      segs_idx <- which(segs)
      
      # find biggest continuous drop
      oxycline_start <- min(depth)
      dC <- 0
      for (i in 2:length(segs_idx)) {
        int <- segs_idx[i-1]:segs_idx[i] # segment interval
        o2_diff <- conc[int[1]] - conc[tail(int,1)] # concentration drop
        if (o2_diff > dC) {
          dC <- o2_diff
          oxycline_start <- depth[int][conc_xx[int] == min(conc_xx[int])] # highest gradient change
        }
      }
      
      # return results
      return(mutate(sub_df, 
        conc_smooth = conc_smooth, conc_xx = conc_xx, conc_x = conc_x,
        oxycline_start = oxycline_start,
        oxycline_end = min(depth[conc < no_o2_cutoff]),
        is_oxycline = (depth >= oxycline_start) & (depth <= oxycline_end)))
    })
  }) 
}

# data model fit functions =============

#' find cell density by fitting data from oxycline to the model
#' @param df oxycline data with depth (in µm) and conc (in µM) columns
#' @param n number of data points for model grid
find_cell_density_from_oxycline <- function(df, n=400, ...) {
  # offset to make the first point 0 and round for aligning the model by appropriate depth steps
  df <- mutate(df, offset_depth = round(depth - min(depth), 2)) 
  
  # parameters
  thickness <- max(df$offset_depth)
  o2_initial <- max(df$conc)
  dx <- min(diff(df$offset_depth)) # smallest step size
  if ( any((df$offset_depth/dx) %% 1 != 0) )
    stop ("for profile ", unique(df$profile), " it seems that the step sizes are not all multiples ",
          "of each other, can't make a matchin model grid")
  n_data <- thickness/dx + 1
  n_grid <- floor(n/(n_data - 1)) * (n_data - 1)
  
  # find fit to data
  timing <- system.time(
    cells_best_fit <- optimize(calculate_cell_density_residuals, interval = c(3, 12), data = df,
                               x_max = thickness, o2_max = o2_initial, N = n_grid, ...)
  )
  
  # return bestfit
  best_model <- mutate(
    model_o2(cells = cells_best_fit$minimum, x_max = thickness, o2_max = o2_initial, N = n_grid, ...),
    cell_density = cells_best_fit$minimum, 
    model_depth = rev(x) + min(df$depth), # offset correct the model
    elapsed = timing[['elapsed']],
    rms = sqrt(cells_best_fit$objective))
  return(best_model)
}

#' compare model to data set
#' @param cells the paramter this is optimizing by
#' @param data the dataset to cpmare to
calculate_cell_density_residuals <- function(cells, data, ...){
  model <- model_o2(cells = cells, ...)
  model$x <- round(rev(model$x), 2) # scale is inverted from oxycline profile + round
  residuals <- merge(data, model, by.x = "offset_depth", by.y = "x", all.x = T, all.y = F)
  if (any(is.na(residuals$o2))) {
    print(residuals)
    stop("can't match up all data depths with model depths! can't calculate rms")
  }
  return(sum((residuals$conc - residuals$o2)^2))
}

