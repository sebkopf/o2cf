## Supplementary Information

This repository holds all supplementary source code needed to reproduce the modelling and data fitting component of Cowley et. al (submitted to mBio, 2015).

For the easiest way to run the two [R Markdown](http://rmarkdown.rstudio.com/) (.Rmd) files that produce the figures and HTML reports, please follow the instructions below.

### What is R Markdown?

[R Markdown](http://rmarkdown.rstudio.com/) is a so-called "literate programming" format that enables easy creation of dynamic documents with the [R](http://www.r-project.org/) language. HTML reports (such as those provided in the journal's SI for this publication) can be generated from R Markdown files using [knitr](http://yihui.name/knitr/) and [pandoc](http://johnmacfarlane.net/pandoc/), which can be installed automatically with [RStudio](http://www.rstudio.com/), and are fully integrated into this cross-platform IDE. All software used for these reports (R, RStudio, etc.) is freely available and completely open-source. 

### How can I run this code?

The quickest and easiest way is to use RStudio.

 1. Download and install [R](http://cran.rstudio.com/) for your operating system
 1. Download and install [RStudio](http://www.rstudio.com/products/rstudio/download/) for your operating system
 1. Download a [zip file of this repository](https://github.com/sebkopf/o2cf/archive/master.zip) and unpack it in an easy to find directory on your computer
 1. Start RStudio and select File --> New Project from the menu, select the "Existing Directory" option and browse to the repository folder from the zip file in the "Project working directory" field, then select "Create Project"
 1. Open either of the R Markdown (.Rmd) files in the file browser to generate the "Oxygen diffusion and respiratory consumption in lung mucus" report (```o2_lung_diffusion.Rmd```) or the "Oxycline implications for microbial density" report (```oxycline_fitting.Rmd```)
 1. To generate the HTML report ("knit HTML"), select File --> Knit from the menu. The HTML report will be displayed upon successful completion (it might take a minute or two because of the complex figures) and is saved as a standalone file in the same directory (these are the files made available in the journal's SI). All generated figures are saved as PDFs in the figures/ sub-directory, all generated tables are saved as comma-separated-value files in the tables/ sub-directory.
 
### What can I do with this code?

We hope that this code, or any part of it, might prove useful to other members of the scientific community interested in the subject matter. All code is completely open-access and can be modified and repurposed in every way. If significant portions are reused in a scientific publication, please consider citing our work. Please make sure to cite this work if re-using any of our data (profiles.xlsx).

### References

Key R packages used for data processing, plotting and modelling:

 - Soetaert K. (2009). rootSolve: Nonlinear root finding, equilibrium and steady-state analysis of ordinary
  differential equations. R-package v. 1.6
 - Soetaert, K. and Meysman, F. (2012). Reactive transport in aquatic ecosystems: Rapid model
  prototyping in the open source software R Environmental Modelling & Software, 32, 49-60.
 - Wickham, H. (2009) ggplot2: elegant graphics for data analysis. Springer New York.
 - Wickham, H. (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12),
  1-20.
 - Wickham, H. (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical
  Software, 40(1), 1-29.
 - Xie, Y. (2014). knitr: A general-purpose package for dynamic report generation in R. R package version
  1.7.
  
#### Troubleshooting notes

The R Markdown files in this repository make use of various R modules for data processing, plotting and modelling. All of these should be installed automatically when the first R Markdown file is knitted (if the knitting fails because of a missing package, please install it manually, an error will indicate which package could not be installed). If the xlsx package causes an error when reading the Excel files in this repository, it is likely because of a missing Java installation, which is required by xlsx. Please install Java for your operating system if you encounter this problem.
 