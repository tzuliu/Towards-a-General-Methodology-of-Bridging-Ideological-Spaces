## Validating Synthetic Anchors (UTAS 2012) Data Creation
## (Ingroup=Candidates, Outgroup=Voters, Outsample-N=0)
## Author: Tzu-Ping Liu & Gento Kato
## Date: 07/25/2020
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
load(paste0(projdir,"/Outputs/application/utas12_data.rda"))

## Load Function
source(paste0(projdir, "/Codes/ipbridge_density.R"))

## Other Packages
require(pbapply)

## Set Number of experiments
ntimes <- 400

#################
## Experiments ## 
#################

#### (ingroup: candidates, outgroup: voters) ####

## No Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d_clean <- ooc(utas2_c_p, dims=2, min=10, lop=0.0001, polarity=rep(utas2_rightcand_c,2), iter=25, nv.method="svm.reg", cost=1)
d_clean <- d_clean$respondents[,grepl("coord", colnames(d_clean$respondents))]
d_clean <- data.frame(d_clean)

saveRDS(d_clean, paste0(projdir, "/Outputs/simulation/utas12_sim_400_clean_cand.rds"))
rm(d_clean)