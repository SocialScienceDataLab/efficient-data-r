# Efficient Data Management in R

### Workflow for a Reproducible Data Project

##### Initializing the project with a .Rprofile

## ----The .Rprofile-----------------------------------
.First <- function() {
        dir.create(paste0(getwd(), "/figures"), showWarnings = F)
        dir.create(paste0(getwd(), "/processed-data"), showWarnings = F)
        dir.create(paste0(getwd(), "/raw-data"), showWarnings = F)
        dir.create(paste0(getwd(), "/scripts"), showWarnings = F)
        dir.create(paste0(getwd(), "/manuscript"), showWarnings = F)
        
        if (!("renv" %in% list.files())) {
                renv::init()
        } else {
                source("renv/activate.R")
        }
        
        cat("\nWelcome to your R-Project:", basename(getwd()), "\n")
}
