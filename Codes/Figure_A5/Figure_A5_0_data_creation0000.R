## Validating Synthetic Anchors (Senator Data) Data Creation
## (Ingroup=Senators, Outgroup=Voters, Outsample-N=0)
## Author: Tzu-Ping Liu & Gento Kato
## Date: 08/03/2020
## Environment: R 4.0.2 and Ubuntu 20.04

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

## Load Data
load(paste0(projdir,"/Outputs/application/senator_data.rda"))

## Load Function
source(paste0(projdir, "/Codes/ipbridge_density.R"))

## Other Packages
require(pbapply)

## Set Number of experiments
ntimes <- 400

#################
## Experiments ## 
#################

#### (ingroup: senators, outgroup: voters) ####

## No Outsamples (ingroup: senators, outgroup: voters)
set.seed(20200725)
d_clean <- oc(rollcall(dc), dims=2, minvotes=5, lop=0.025, polarity = rep(1,2))
d_clean <- d_clean$legislators[,grepl("coord", colnames(d_clean$legislators))]
d_clean <- data.frame(d_clean)

saveRDS(d_clean, paste0(projdir, "/Outputs/simulation/senator_sim_400_clean_cand.rds"))
rm(d_clean)