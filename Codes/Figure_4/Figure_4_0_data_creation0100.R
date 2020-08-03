## Validating Synthetic Anchors (UTAS 2012) Data Creation
## (Ingroup=Candidates, Outgroup=Voters, Outsample-N=100)
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

## 100 Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(utas2_v_p,utas2_c_p,100,polarity1=rep(utas2_rightcand_v,2),
                     polarity2=rep(utas2_rightcand_c,2))
  )
})

save(d, file=paste0(projdir, "/Outputs/simulation/utas12_sim_400_0100s_cand.rda"))
rm(d)