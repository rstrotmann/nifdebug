pkgname <- "nifdebug"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('nifdebug')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("debug_time_plot")
### * debug_time_plot

flush(stderr()); flush(stdout())

### Name: debug_time_plot
### Title: Interactive time metric comparison plot
### Aliases: debug_time_plot

### ** Examples

if (interactive()) {
  debug_time_plot(examplinib_poc_nif)
  debug_time_plot(examplinib_poc_nif, color = "ANALYTE")
  debug_time_plot(examplinib_poc_nif, analyte = "RS2023", facet = NULL)
}



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
