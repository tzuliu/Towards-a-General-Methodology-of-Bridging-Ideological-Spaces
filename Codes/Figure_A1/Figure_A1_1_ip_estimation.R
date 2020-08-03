## Ideal Point Analysis for UTAS 2009
## Author: Tzu-Ping Liu & Gento Kato
## Date: 07/25/2020
## Environment: R 4.0.2 on Ubuntu 20.04

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
load(paste0(projdir,"/Outputs/application/utas09_data.rda"))

## Source functions
source(paste0(projdir,"/Codes/ipbridge.R"))

###########
## Joint ##
###########

polar <- nrow(utas1_c_p) + utas1_rightcand_v
utas1_j_p <- rbind.data.frame(utas1_c_p,utas1_v_p)

## Estimate
t <-ooc(utas1_j_p, dims=2, min=10, lop=0.0001, polarity=rep(polar,2), iter=25, nv.method="svm.reg", cost=1)

tj <- t$respondents[,grepl("coord", colnames(t$respondents))]
tj <- data.frame(tj)
z <- c(as.character(utas1_c$psup_short),as.character(utas1_v$psup_short))
z <- factor(z, levels=c("LDP","DPJ","JCP","Other/NA", "SDP","YP","CGP (Komei)","PNP","Abstained"))
tj$party <- z
tj$cv <- c(rep("Candidate",nrow(utas1_c_p)), rep("Voter",nrow(utas1_v_p)))
ip_j <- tj %>% filter(party == "LDP" | party == "DPJ" )

#################################
## Candidates on voter's space ##
#################################

## set.seed
set.seed(20191005)
## Estimate
t1 <-ipbridge(utas1_c_p,utas1_v_p,20,polarity1=rep(utas1_rightcand_c,2),
             polarity2=rep(utas1_rightcand_v,2))

## Create IP data for candidates
dcp <- utas1_c$psup_short
dcp_new <- dcp[-t1[[5]]]
dcp <- c(as.character(dcp_new), as.character(dcp[t1[[5]]]))
dcp <- factor(dcp)
ipc_dt1_pro <- data.frame(t1[[2]], dcp)
ipc_dt1_reg <- data.frame(t1[[3]], dcp)
colnames(ipc_dt1_pro) <- c("coord1D","coord2D","party")
colnames(ipc_dt1_reg) <- c("coord1D","coord2D","party")
ipc_dt1_pro2 <- ipc_dt1_pro %>% filter(party == "LDP" | party == "DPJ" )
ipc_dt1_pro2 <- ipc_dt1_pro2 %>% mutate(cv = "Candidate")
ipc_dt1_reg2 <- ipc_dt1_reg %>% filter(party == "LDP" | party == "DPJ" )
ipc_dt1_reg2 <- ipc_dt1_reg2 %>% mutate(cv = "Candidate")
## Plot Candidate IP
#ggplot(data=ipc_dt1_2, aes(x=coord1D, y=coord2D, color=party)) + geom_point(size=2, alpha=0.6) + 
#xlim(-1,2.5) +
#ylim(-1,2.5)

## Create IP data for voters
ipv_dt1 <- data.frame(t1[[4]], utas1_v$psup_short)
colnames(ipv_dt1) <- c("coord1D","coord2D","party")
ipv_dt1_2 <- ipv_dt1 %>% filter(party == "LDP" | party == "DPJ" )
ipv_dt1_2 <- ipv_dt1_2 %>% mutate(cv = "Voter")
## Plot Voter IP
#ggplot(data=ipv_dt1_2, aes(x=coord1D, y=coord2D, shape=party, color=party)) + geom_point() + 
#xlim(-1,2.5) +
#ylim(-1,2.5)

## Create candidate & voter IP data (Procrustes)
ip_dt1_pro <- rbind(ipc_dt1_pro2, ipv_dt1_2)
ip_dt1_pro$party <- factor(ip_dt1_pro$party, levels=c("LDP","DPJ","JCP","Other/NA", "YP","CGP (Komei)","SDP","PNP","Abstained"))

## Create candidate & voter IP data (Regression)
ip_dt1_reg <- rbind(ipc_dt1_reg2, ipv_dt1_2)
ip_dt1_reg$party <- factor(ip_dt1_reg$party, levels=c("LDP","DPJ","JCP","Other/NA", "YP","CGP (Komei)","SDP","PNP","Abstained"))

## Create IP data for (original) voters
ipc_dt1o <- data.frame(t1[[1]], dcp)
colnames(ipc_dt1o) <- c("coord1D","coord2D","party")
ipc_dt1o_2 <- ipc_dt1o %>% filter(party == "LDP" | party == "DPJ" )
ipc_dt1o_2 <- ipc_dt1o_2 %>% mutate(cv = "Candidate")

## Create candidate & (original) voter IP data
ip_dt1 <- rbind(ipc_dt1o_2, ipv_dt1_2)
ip_dt1$party <- factor(ip_dt1$party, levels=c("LDP","DPJ","JCP","Other/NA", "YP","CGP (Komei)","SDP","PNP","Abstained"))

#################################
## Voters on candidates' space ##
#################################

##set.seed
set.seed(20191005)
## Estimate
t2 <-ipbridge(utas1_v_p,utas1_c_p,20,polarity1=rep(utas1_rightcand_v,2),
             polarity2=rep(utas1_rightcand_c,2))

## Create IP data for candidates
ipc_dt2 <- data.frame(t2[[4]], utas1_c$psup_short)
colnames(ipc_dt2) <- c("coord1D","coord2D","party")
ipc_dt2_2 <- ipc_dt2 %>% filter(party == "LDP" | party == "DPJ" )
ipc_dt2_2 <- ipc_dt2_2 %>% mutate(cv = "Candidate")

## Create IP data for voters
dvp <- utas1_v$psup_short
dvp_new <- dvp[-t2[[5]]]
dvp <- c(as.character(dvp_new), as.character(dvp[t2[[5]]]))
dvp <- factor(dvp)
ipv_dt2_pro <- data.frame(t2[[2]], dvp)
ipv_dt2_reg <- data.frame(t2[[3]], dvp)
colnames(ipv_dt2_pro) <- c("coord1D","coord2D","party")
colnames(ipv_dt2_reg) <- c("coord1D","coord2D","party")
ipv_dt2_pro2 <- ipv_dt2_pro %>% filter(party == "LDP" | party == "DPJ" )
ipv_dt2_pro2 <- ipv_dt2_pro2 %>% mutate(cv = "Voter")
ipv_dt2_reg2 <- ipv_dt2_reg %>% filter(party == "LDP" | party == "DPJ" )
ipv_dt2_reg2 <- ipv_dt2_reg2 %>% mutate(cv = "Voter")

## Create candidate & voter IP data
ip_dt2_pro <- rbind(ipc_dt2_2, ipv_dt2_pro2)

## Create candidate & voter IP data
ip_dt2_reg <- rbind(ipc_dt2_2, ipv_dt2_reg2)

## Create IP data for (original) voters
ipv_dt2o <- data.frame(t2[[1]], dvp)
colnames(ipv_dt2o) <- c("coord1D","coord2D","party")
ipv_dt2o_2 <- ipv_dt2o %>% filter(party == "LDP" | party == "DPJ" )
ipv_dt2o_2 <- ipv_dt2o_2 %>% mutate(cv = "Voter")

## Create candidate & (original) voter IP data
ip_dt2 <- rbind(ipc_dt2_2, ipv_dt2o_2)

####################
## Save workspace ##
####################

tmpdir <- projdir
rm(projdir)
save.image(paste0(tmpdir,"/Outputs/application/utas09_ip.rda"))