#' ---
#' title: "Towards a General Methodology of Bridging Ideological Spaces"
#' author: "Tzu-Ping Liu & Gento Kato"
#' date: "July 24, 2020"
#' ---

#'
#' # Preparation
#'

## Clear Workspace
rm(list = ls())

## Set Working Directory (Automatically) ##
require(rprojroot); require(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

#'
#' # Figure 1 (Illustration of Transformation Types)
#'

## Clear Workspace
rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

## Run Code
source(paste0(projdir, "/Codes/Figure_1.R"))

#'
#' # Figure 2 (Apply Bridging to Senate Data)
#' 

## Clear Workspace
rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

#+ eval=FALSE
## Rearrange Data
source(paste0(projdir, "/Codes/Figure_2/Figure_2_0_data_rearrange.R"))

#+ eval=FALSE
## Bridge Ideal Points
source(paste0(projdir, "/Codes/Figure_2/Figure_2_1_ip_estimation.R"))

#+ 
## Visualize Results
source(paste0(projdir, "/Codes/Figure_2/Figure_2_2_plot.R"))

#'
#' # Figure 3 (Apply Bridging to UTAS12 Data)
#' 

rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

#+ eval=FALSE
## Rearrange Data
source(paste0(projdir, "/Codes/Figure_3/Figure_3_0_data_rearrange1.R"))

#+ eval=FALSE
## Rearrange Data Again
rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))
source(paste0(projdir, "/Codes/Figure_3/Figure_3_0_data_rearrange2.R"))

#+ eval=FALSE
## Bridge Ideal Points
source(paste0(projdir, "/Codes/Figure_3/Figure_3_1_ip_estimation.R"))

#+ 
## Visualize Results
source(paste0(projdir, "/Codes/Figure_3/Figure_3_2_plot.R"))

#'
#' # Figure 4 (Validating an Assumption through UTAS12 Data)
#' 

rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

#+ eval=FALSE
## Run Simulation *The version used for poster is missing
## Run Simulation *Not the version used for poster
# source(paste0(projdir, "/Codes/Figure_4/Figure_4_0b_data_creation.R"))

#+ eval=FALSE
## Rearrange Results * The version used for poster
rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))
source(paste0(projdir, "/Codes/Figure_4/Figure_4_1a_data_rearrange2.R"))
## Rearrange Results * Not the version used for poster
# rm(list = ls())
# projdir <- find_root(has_file("thisishome.txt"))
# source(paste0(projdir, "/Codes/Figure_4/Figure_4_1b_data_rearrange2.R"))

#+ 
## Export Plot
source(paste0(projdir, "/Codes/Figure_4/Figure_4_2_plot.R"))

#'
#' # Figure 5 (Evaluating Performance through Simulation)
#' 

rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

#+ eval=FALSE
## Implementing Simulation 
source(paste0(projdir, "/Codes/Figure_5/Figure_5_0_data_creation.R"))
rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

#+ 
## Estimate Ideal Points
source(paste0(projdir, "/Codes/Figure_5/Figure_5_1_plot.R"))

#'
#' # Figure A1 (Apply Bridging to UTAS09 Data)
#' 

rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))

#+ eval=FALSE
## Rearrange Data
source(paste0(projdir, "/Codes/Figure_A1/Figure_A1_0_data_rearrange1.R"))

#+ eval=FALSE
## Rearrange Data Again
rm(list = ls())
projdir <- find_root(has_file("thisishome.txt"))
source(paste0(projdir, "/Codes/Figure_A1/Figure_A1_0_data_rearrange2.R"))

#+ eval=FALSE
## Estimate Ideal Points
source(paste0(projdir, "/Codes/Figure_A1/Figure_A1_1_ip_estimation.R"))

#+ 
## Estimate Ideal Points
source(paste0(projdir, "/Codes/Figure_A1/Figure_A1_2_plot.R"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('./Codes/Replication_Summary.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')
# tmp <- list.files("./Codes/")
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.tex",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0("./Codes/",tmp[i]))
