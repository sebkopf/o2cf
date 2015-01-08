# utility functions ===============

#' Load packages after checking whether they are installed.
#' 
#' If install flag is set, attempts to install the missing packages.
#' Throws an error if any package is (still) not installed.
#' 
#' @param packages the names of packages to check for
#' @param install whether to attempt to install missing packages (default = FALSE)
#' @param load whether to load the packages
#' @return TRUE unless an error was thrown
load_pkgs <- function(packages, install = T) {
  missing_pkgs <- setdiff(packages, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    if (install) {
      install.packages(missing_pkgs, depen=T)
      still_missing_pkgs <- setdiff(packages, rownames(installed.packages()))
      if (length(still_missing_pkgs) > 0)
        stop("The following packages could not be installed automatically: ", 
             paste(still_missing_pkgs, collapse=", "), "\nPlease install them manually.")
    } else
      stop("The following packages are missing, please install them: ", paste(missing_pkgs, collapse=", "))
  }
  
  sapply(packages, function(i) library(i, character.only = TRUE))
  return (invisible(TRUE))
}

#' Duration labels to the closest large denominator (e.g. 86400s = ~24 hours)
#' @param ds vector of lubridate:::duration objects or time (+ specifying units)
duration_label <- function(ds, units = "seconds") {
  load_pkgs("lubridate")
  sapply(ds, function(x) { 
    if (grepl("^[0-9\\.]+s$", (d <- duration(x, units)))) return(paste0(round(as.numeric(d), 1), " seconds"))
    else return(sub(".+\\s\\(~(.+)?\\)$", "\\1", d))
  })
}

#' Expand data frames (same as expand.grid but parameters can also be whole data frames)
#' in a way this could also be achieved with ddply and expand.grid but I like this
#' functionlization and it's extremely fast.
expand.df <- function(...) {
  # convert all params to data.frames
  l <- list(...)
  dfs <- lapply(1:length(l), function(i) if(is.data.frame(l[[i]])) l[[i]] else as.data.frame(l[i]))
  
  # get indices grid
  indices <- lapply(rev(dfs), function(df) seq_len(nrow(df)))
  ind.grid <- do.call(expand.grid, indices)
  
  #use subsetting and cbind
  exp.dfs <- lapply(1:length(dfs), function(i) dfs[[i]][ind.grid[,length(dfs)+1-i], , drop = F])
  do.call(cbind, exp.dfs)
}

#' Null default
#' Analog of || from ruby
#' @author Hadley Wickham
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' convert integer from excel date field to Date object
#' @param excel_date integer value from excel date field
#' @return Date
read_excel_date <- function(excel_date) {
  as.Date(as.integer(excel_date) - 25569, origin="1970-01-01") 
}

#' convert excel date + time field to POSIX object
#' @param excel_date_time numeric value from excel date time field
#' @return POSIX
read_excel_date_time <- function(excel_date_time) {
  as.POSIXct((as.numeric(excel_date_time)-25569)*86400, tz="GMT", origin="1970-01-01")
}

# plotting functions ================

theme_nogrid <- function() theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
theme_nolegend <- function() theme(legend.position = "none") 
theme_xangle <- function(angle = 60, hjust = 1, vjust = 1) theme(axis.text.x = element_text(angle=angle, hjust = hjust, vjust = vjust))
theme_nobgrd <- function() theme(plot.background = element_blank(), panel.background = element_blank())
theme_frame <- function() theme(panel.border = element_rect(color="black", size=1), strip.background = element_rect(color="black", linetype = 1))
theme_public <- function(font_size = 18) theme_nobgrd() + theme_frame() + theme(text = element_text(size = font_size))
theme_facet <- function(margin.cm = 1) theme(panel.margin = unit(margin.cm, "cm"))  # requires grid library

#' Guides for plots with filled points and separate shapes so that the legend displays things correctly
guides_fill_shape <- function(){
  guides(fill = guide_legend(override.aes = list(colour = "white", size=8, shape = 22)), 
         shape = guide_legend(override.aes = list(fill = "gray"))) 
}

#' simple function to get two line expressions working
#' normal in any labels, you can just use expressions e.g. labs(y = expression(alpha^beta)) or labs(y = bquote(y == .(var_val)))
#' using two_line_label() you can do exactly the same and it will put the two lines on top of each other (+centered)
#' e.g. labs(x=two_line_label(beta~alpha*5+bold(y), bquote(y == .(var_val)))) 
#' @note only possible with two labels because it uses atop, additional labels would be made much
#' smaller but if really want to do it, can use the nested expression:
#' x = expression(atop(paste(x^2), atop(alpha*beta, "line3"~beta[x])))
two_line_label <- function(line1, line2) {
  # paste collapse only necessary because deparse sometimes gives back multiple lines (not sure why)
  line1 <- paste(deparse(line1), collapse="") 
  line2 <- paste(deparse(line2), collapse="")
  parse(text = paste("atop(paste(", paste(line1, collapse=""), "), paste(", line2, "))"))
}

#' Gets a discrete color palette with the right number of colors
#'
#' @param var the variable who is determining colours (will get as many colours as there are unique values in var)
#' @param palette name of the preferred colour palette, default "Set1_ytb" (Set1 withblack instead of yellow), see display.brewer.all() for details,
#' if the preferred palette does not have enough values, switches to equal distribution around the colour wheel
#' (the default in ggplot) with a warning, can set to this deliberately by choosing palette="gg"
#' @note An additional scale is defined by me, that is Set1_ytb which is Set1 with black as the last color and without yellow (Can be hard to see)
discrete_colours <- function(var = 1:9, palette = "Set1_ytb") {
  # get necessary library
  library(RColorBrewer)
  
  # palettes info
  homemade <- data.frame(row.names = 'Set1_ytb', maxcolors = 9, category = 'seq')
  all.pals <- rbind(brewer.pal.info, homemade)
  
  if (!palette %in% c("gg", row.names(all.pals)))
    stop("don't know where to find pallette ", palette, " - it's not in the colorbrewer")
  n <- length(unique(var))
  
  if (palette != "gg" && (n > (npal <- all.pals[palette,]$maxcolors)) == TRUE) {
    warning("not enough colors in palette ", palette, " (only have ", npal, " but need ", n, ")",
            "\n using ggplot default instead --> to avoid this warning, set palette='gg'")
    palette <- "gg"
  }
  
  if (palette == "gg") {
    hues = seq(15, 375, length=n+1)
    return(hcl(h=hues, l=65, c=100)[1:n])
  } else if (palette == "Set1_ytb") {
    return(c(brewer.pal(n, "Set1")[-6], "#000000"))
  } else
    return (brewer.pal(n, palette))
}

# knitr pdf save hook ===============
load_pkgs("knitr")

#' Save a pdf
#' 
#' This function is setup as a knitr hook that automatically saves the last plot of a code chunk if the options save_pdf=TRUE is set. 
#' It can also be called directly inside a chunk by passing all parameters manually or passing the current chunk's options via savepdf(p, options = opts_current$get())
#' 
#' One can set options pdfsave.prefix and pdfsave.folder at the beginning of the .rmd file if want to specify the default target folder and pdf prefix: options(pdfsave.prefix = "xx - ")
#' 
#' @note Paths are always relative to the location of the .rmd file itself when executed as a hook. Make sure to set opts_knit$set(root.dir="<PATH>") at the beginning of the .rmd file if this is NOT the desired behaviour. 
#' @param filename default determined from options$label and the prefix of the running knitr file
#' @param folder default determined from the savepdf.folder option of the running knitr file, otherwise .rmd file directory
#' @param height plot height in inches, default determined from the fig.height chunk option of the current chunk
#' @param width plot widht in inches, default determiend form the fig.height chunk option of the current chunk
#' @param options the list of chunk options (automatically passed from the hook), only need to be specified if other params are not explicitely set
#' @param ... additional paramters are passed on to ggsave
save_pdf <- function(plot, filename = default_filename(), folder = default_folder(),
                     height = options$fig.height, width = options$fig.width, 
                     options = list(), ...) {
  load_pkgs("knitr")
  
  # default folder
  default_folder <- function() getOption("pdfsave.folder") %||% "." 
  
  # default filename
  default_filename <- function() {
    prefix <- getOption('pdfsave.prefix') %||% sub("(.+)\\.[Rr]md", "\\1", knitr:::knit_concord$get("infile"))
    paste0(prefix, options$label, ".pdf")
  }
  
  if (!file.exists(folder)) dir.create(folder)
  
  # encoding is necessary for permil symbol which is not part of the ISOLatin1 character set
  # useDingbats=FALSE is necessary for all data point symbols to be drawn instead of represented as dingbat characters (which illustrator can have trouble with sometimes)
  ggsave(file.path(folder, filename), plot = plot, height = height, width = width, encoding="WinAnsi", useDingbats=FALSE, ...)
  return (paste0("\n\nPlot saved as a pdf in '", file.path(folder, filename), "' (dimensions ", width, 'x', height,")."))
}

#' hook to automatically save last plot in a code chunck (see documentation
#' of save_pdf)
knit_hooks$set(save_pdf = function(before, options, envir) {
  if (!before) {
    ret <- save_pdf(last_plot(), options = options)
    if (options$results != 'hide') return (ret)
    return (NULL)
  }
})



