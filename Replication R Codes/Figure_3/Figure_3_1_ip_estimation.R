## Clear Workspace
rm(list = ls())

## Load Data
load("./utas12_data.rda")

## Source functions
source("./ipbridge.R")

###########
## Joint ##
###########

utas2_j_p <- rbind.data.frame(utas2_c_p,utas2_v_p)

## Estimate
t <-ooc(utas2_j_p, dims=2, min=10, lop=0.0001, polarity=rep(jointpol,2), iter=25, nv.method="svm.reg", cost=1)

tj <- t$respondents[,grepl("coord", colnames(t$respondents))]
tj <- data.frame(tj)
z <- c(as.character(utas2_c$psup_short),as.character(utas2_v$psup_short))
z <- factor(z, levels=c("LDP","DPJ","JRP","JCP","Other/NA", "YP","TPJ","CGP (Komei)","SDP","Abstained"))
tj$party <- z
tj$cv <- c(rep("Candidate",nrow(utas2_c_p)), rep("Voter",nrow(utas2_v_p)))
ip_j <- tj %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")

############
## C to V ##
############

##Set.seed
set.seed(20191005)
## Estimate
t1 <-ipbridge(utas2_c_p,utas2_v_p,20,polarity1=rep(utas2_rightcand_c,2),
              polarity2=rep(utas2_rightcand_v,2))

## Create IP data for candidates
dcp <- utas2_c$psup_short
dcp_new <- dcp[-t1[[5]]]
dcp <- c(as.character(dcp_new), as.character(dcp[t1[[5]]]))
dcp <- factor(dcp)
ipc_dt1_pro <- data.frame(t1[[2]], dcp)
ipc_dt1_reg <- data.frame(t1[[3]], dcp)
colnames(ipc_dt1_pro) <- c("coord1D","coord2D","party")
colnames(ipc_dt1_reg) <- c("coord1D","coord2D","party")
ipc_dt1_pro2 <- ipc_dt1_pro %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipc_dt1_pro2 <- ipc_dt1_pro2 %>% mutate(cv = "Candidate")
ipc_dt1_reg2 <- ipc_dt1_reg %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipc_dt1_reg2 <- ipc_dt1_reg2 %>% mutate(cv = "Candidate")

## Create IP data for voters
ipv_dt1 <- data.frame(t1[[4]], utas2_v$psup_short)
colnames(ipv_dt1) <- c("coord1D","coord2D","party")
ipv_dt1_2 <- ipv_dt1 %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipv_dt1_2 <- ipv_dt1_2 %>% mutate(cv = "Voter")

## Create candidate & voter IP data
ip_dt1_pro <- rbind(ipc_dt1_pro2, ipv_dt1_2)
ip_dt1_pro$party <- factor(ip_dt1_pro$party, levels=c("LDP","DPJ","JRP","JCP","Other/NA", "YP","TPJ","CGP (Komei)","SDP","Abstained"))

## Create candidate & voter IP data, second
ip_dt1_reg <- rbind(ipc_dt1_reg2, ipv_dt1_2)
ip_dt1_reg$party <- factor(ip_dt1_reg$party, levels=c("LDP","DPJ","JRP","JCP","Other/NA", "YP","TPJ","CGP (Komei)","SDP","Abstained"))

## Create IP data for (original) voters
ipc_dt1o <- data.frame(t1[[1]], dcp)
colnames(ipc_dt1o) <- c("coord1D","coord2D","party")
ipc_dt1o_2 <- ipc_dt1o %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipc_dt1o_2 <- ipc_dt1o_2 %>% mutate(cv = "Candidate")

## Create candidate & (original) voter IP data
ip_dt1 <- rbind(ipc_dt1o_2, ipv_dt1_2)
ip_dt1$party <- factor(ip_dt1$party, levels=c("LDP","DPJ","JRP","JCP","Other/NA", "YP","TPJ","CGP (Komei)","SDP","Abstained"))

ip_dt1$cv2 <- ifelse(ip_dt1$cv=="Candidate",
                     "Candidate\n(untransformed)",
                     "Voter\n(untransformed)")
ip_dt1_pro$cv2 <- ifelse(ip_dt1_pro$cv=="Candidate",
                         "Candidate\n(Procrustes)",
                         "Voter\n(XProcrustes)")
ip_dt1_reg$cv2 <- ifelse(ip_dt1_reg$cv=="Candidate",
                         "Candidate\n(Regression)",
                         "Voter\n(XRegression)")
ip_dt1 <- rbind(ip_dt1,ip_dt1_pro,ip_dt1_reg)
ip_dt1$cv2 <- factor(ip_dt1$cv2, levels=unique(ip_dt1$cv2)) 

############
## V to C ##
############

## set.seed
set.seed(20191005)

## Estimate
t2 <-ipbridge(utas2_v_p,utas2_c_p,20,polarity1=rep(utas2_rightcand_v,2),
              polarity2=rep(utas2_rightcand_c,2))

## Create IP data for candidates
ipc_dt2 <- data.frame(t2[[4]], utas2_c$psup_short)
ipc_dt2_2 <- ipc_dt2 %>% filter(utas2_c.psup_short == "LDP" | utas2_c.psup_short == "DPJ" | utas2_c.psup_short == "JRP")
colnames(ipc_dt2_2) <- c("coord1D","coord2D","party")
ipc_dt2_2 <- ipc_dt2_2 %>% mutate(cv = "Candidate")

## Create IP data for voters
dvp <- utas2_v$psup_short
dvp_new <- dvp[-t2[[5]]]
dvp <- c(as.character(dvp_new), as.character(dvp[t2[[5]]]))
dvp <- factor(dvp)
ipv_dt2_pro <- data.frame(t2[[2]], dvp)
ipv_dt2_reg <- data.frame(t2[[3]], dvp)
colnames(ipv_dt2_pro) <- c("coord1D","coord2D","party")
colnames(ipv_dt2_reg) <- c("coord1D","coord2D","party")
ipv_dt2_pro2 <- ipv_dt2_pro %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipv_dt2_pro2 <- ipv_dt2_pro2 %>% mutate(cv = "Voter")
ipv_dt2_reg2 <- ipv_dt2_reg %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipv_dt2_reg2 <- ipv_dt2_reg2 %>% mutate(cv = "Voter")

## Create candidate & voter IP data
ip_dt2_pro <- rbind(ipc_dt2_2, ipv_dt2_pro2)

## Create candidate & voter IP data
ip_dt2_reg <- rbind(ipc_dt2_2, ipv_dt2_reg2)

## Create IP data for (original) voters
ipv_dt2o <- data.frame(t2[[1]], dvp)
colnames(ipv_dt2o) <- c("coord1D","coord2D","party")
ipv_dt2o_2 <- ipv_dt2o %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipv_dt2o_2 <- ipv_dt2o_2 %>% mutate(cv = "Voter")

## Create candidate & (original) voter IP data
ip_dt2 <- rbind(ipc_dt2_2, ipv_dt2o_2)
ipv_dt2o <- data.frame(t2[[1]], dvp)
colnames(ipv_dt2o) <- c("coord1D","coord2D","party")
ipv_dt2o_2 <- ipv_dt2o %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")
ipv_dt2o_2 <- ipv_dt2o_2 %>% mutate(cv = "Voter")

ip_dt2$cv2 <- ifelse(ip_dt2$cv=="Candidate",
                     "Candidate\n(untransformed)",
                     "Voter\n(untransformed)")
ip_dt2_pro$cv2 <- ifelse(ip_dt2_pro$cv=="Candidate",
                         "Candidate\n(XProcrustes)",
                         "Voter\n(Procrustes)")
ip_dt2_reg$cv2 <- ifelse(ip_dt2_reg$cv=="Candidate",
                         "Candidate\n(XRegression)",
                         "Voter\n(Regression)")
ip_dt2 <- rbind(ip_dt2,ip_dt2_pro,ip_dt2_reg)
ip_dt2$cv2 <- factor(ip_dt2$cv2, levels=unique(ip_dt2$cv2)) 

rm(ip_dt1_pro, ip_dt1_reg, ip_dt2_pro, ip_dt2_reg, ipc_dt1_pro, ipc_dt1_pro2, 
   ipc_dt1_reg, ipc_dt1_reg2, ipc_dt1o, ipc_dt1o_2, ipc_dt2, ipc_dt2_2, ipv_dt1,
   ipv_dt1_2, ipv_dt2_pro, ipv_dt2_pro2, ipv_dt2_reg, ipv_dt2_reg2, ipv_dt2o, 
   ipv_dt2o_2, t, t1, t2, tj, utas2_c, utas2_c_p, utas2_j_p, utas2_v, utas2_v_p)

rm(dcp, dcp_new, dvp, dvp_new, jointpol, utas2_rightcand_c, utas2_rightcand_v, z, ipbridge)

#save.image("utas12_ip.rda")




