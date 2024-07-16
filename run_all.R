
pkgs <- c('exuber','here', "imputeTS", "transx")
install.packages(pkgs)

remote_pkgs <- c('kvasilopoulos/ihdpr','kvasilopoulos/ivx','kvasilopoulos/nationwider')
for (pkg in remote_pkgs){
  remotes::install_github('kvasilopoulos/nationwider')
}
  

# uk ----------------------------------------------------------------------

source("uk/02-write-json.R")
source("uk/03-write-plotly.R")


# int --------------------------------------------------------------------

# source("int/01-download-int.R")
source("int/02-write-json.R")
source("int/03-write-plotly.R")

# get all ----------------------------------------------------------------
