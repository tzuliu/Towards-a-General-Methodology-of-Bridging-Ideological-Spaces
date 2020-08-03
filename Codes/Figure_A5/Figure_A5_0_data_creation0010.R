## Validating Synthetic Anchors (Senator Data) Data Creation
## (Ingroup=Senators, Outgroup=Voters, Outsample-N=10)
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

#### (ingroup: senators outgroup: voters) ####

## 10 Outsamples (ingroup: senators, outgroup: voters)
set.seed(20200725)
d <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(dv,dc,10,polarity1=rep(3,2),polarity2=rep(1,2),model="oc")
  )
})

save(d, file=paste0(projdir, "/Outputs/simulation/senator_sim_400_0010s_cand.rda"))
rm(d)