### This script contaisn all supporting packages

#### prepare all necessary global settings
#### Create data folder
output.folders <- c(paste0(getwd(), "/output"))


#### Create output folder
for (y in output.folders) {
    if(!dir.exists(y)) {
        dir.create(y, showWarnings = FALSE)
    }
}


#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               ggplot2,
               cowplot,
               viridis,
               sciplot,
               RColorBrewer,
               plantecophys,
               lattice,
               reshape2,
               data.table,
               gridExtra,
               grid)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)
