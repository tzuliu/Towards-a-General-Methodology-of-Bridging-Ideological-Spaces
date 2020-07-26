## Monte Carlo Simulation Code
## Author: Tzu-Ping Liu & Gento Kato
## Date: 07/25/2020
## Environment: R 4.0.2

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

saveRDS(d_clean, paste0(projdir, "Outputs/simulation/utas12_sim_400_clean_cand.rds"))
rm(d_clean)

## 20 Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d20 <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(utas2_v_p,utas2_c_p,20,polarity1=rep(utas2_rightcand_v,2),
                              polarity2=rep(utas2_rightcand_c,2))
    )
})

saveRDS(d20, paste0(projdir, "Outputs/simulation/utas12_sim_400_0020s_cand.rds"))
rm(d20)

## 50 Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d50 <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(utas2_v_p,utas2_c_p,50,polarity1=rep(utas2_rightcand_v,2),
                     polarity2=rep(utas2_rightcand_c,2))
  )
})

saveRDS(d50, paste0(projdir, "Outputs/simulation/utas12_sim_400_0050s_cand.rds"))
rm(d50)

## 100 Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d100 <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(utas2_v_p,utas2_c_p,50,polarity1=rep(utas2_rightcand_v,2),
                     polarity2=rep(utas2_rightcand_c,2))
  )
})

saveRDS(d100, paste0(projdir, "Outputs/simulation/utas12_sim_400_0100s_cand.rds"))
rm(d100)

## 500 Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d500 <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(utas2_v_p,utas2_c_p,50,polarity1=rep(utas2_rightcand_v,2),
                     polarity2=rep(utas2_rightcand_c,2))
  )
})

saveRDS(d500, paste0(projdir, "Outputs/simulation/utas12_sim_400_0500s_cand.rds"))
rm(d500)

## 1000 Outsamples (ingroup: candidates, outgroup: voters)
set.seed(20200725)
d1000 <- pblapply(1:ntimes, function(i) {
  data.frame(
    ipbridge_density(utas2_v_p,utas2_c_p,50,polarity1=rep(utas2_rightcand_v,2),
                     polarity2=rep(utas2_rightcand_c,2))
  )
})

saveRDS(d1000, paste0(projdir, "Outputs/simulation/utas12_sim_400_1000s_cand.rds"))
rm(d1000)