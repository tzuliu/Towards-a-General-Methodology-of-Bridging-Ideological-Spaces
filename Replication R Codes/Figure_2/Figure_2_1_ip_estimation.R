## Clear Workspace
rm(list = ls())

## Load Data
load("./senator_data.rda")

## Source functions
source("./ipbridge.R")

###################
## Joint Scaling ##
###################

dj <- rbind.data.frame(dc,dv)

t <- oc(rollcall(dj), dims=2, minvotes=5, lop=0.025, polarity = rep(1,2))
tj <- t$legislators[,grepl("coord", colnames(t$legislators))]
tj <- data.frame(tj)

tj$party <- c(dcp,dvp); tj$party <- factor(tj$party, levels=c(1,2), labels=c("Democrat","Republican"))
tj$cv <- c(rep(1,nrow(dc)), rep(2,nrow(dv))); tj$cv <- factor(tj$cv, levels=c(1,2), labels=c("Senator","Voter"))
ip_j <- tj %>% filter(party == "Democrat" | party == "Republican")

######################
## senator to voter ##
######################

##set.seed
set.seed(20191005)
## oc
t1 <-ipbridge(dc,dv,20,polarity1=rep(1,2),
              polarity2=rep(3,2), model="oc")

dcp_new <- dcp[-t1[[5]]]
dcp_new <- c(dcp_new, dcp[t1[[5]]])
dcp_new <- factor(dcp_new, levels=c(1,2), labels=c("Democrat","Republican"))
## creating IP data (coordinates and party id) by respondents type
tv <- data.frame(t1[[4]]);colnames(tv) <- c("coord1D","coord2D");tv$cv <- rep(2, dim(tv)[1]);tv$party <- dvp
tc_pro <- data.frame(t1[[2]]);colnames(tc_pro) <- c("coord1D","coord2D");tc_pro$cv <- rep(1, dim(tc_pro)[1]);tc_pro$party <- dcp_new
tc_reg <- data.frame(t1[[3]]);colnames(tc_reg) <- c("coord1D","coord2D");tc_reg$cv <- rep(1, dim(tc_reg)[1]);tc_reg$party <- dcp_new

## first, transformed senators and original voters
ip_t1_pro <- rbind.data.frame(tc_pro,tv)
ip_t1_pro <- ip_t1_pro %>% filter(party == "Democrat" | party == "Republican")
ip_t1_pro$cv <- factor(ip_t1_pro$cv,  levels=c(1,2), labels=c("Senator","Voter"))

## second, transformed senators and original voters
ip_t1_reg <- rbind.data.frame(tc_reg,tv)
ip_t1_reg <- ip_t1_reg %>% filter(party == "Democrat" | party == "Republican")
ip_t1_reg$cv <- factor(ip_t1_reg$cv,  levels=c(1,2), labels=c("Senator","Voter"))

## creating IP data (coordinates and party id) for original senators IP
tc_o <- data.frame(t1[[1]]);colnames(tc_o) <- c("coord1D","coord2D");tc_o$cv <- rep(1, dim(tc_o)[1]);tc_o$party <- dcp_new
## second, original senators and original voters
ip_t1o <- rbind.data.frame(tc_o,tv)
ip_t1o <- ip_t1o %>% filter(party == "Democrat" | party == "Republican")
ip_t1o$cv <- factor(ip_t1o$cv,  levels=c(1,2), labels=c("Senator","Voter"))

######################
## Create C to V IP ##
######################
ip_t1o$cv2 <- ifelse(ip_t1o$cv=="Senator",
                     "Senator\n(untransformed)",
                     "Voter\n(untransformed)")
ip_t1_pro$cv2 <- ifelse(ip_t1_pro$cv=="Senator",
                        "Senator\n(Procrustes)",
                        "Voter\n(XProcrustes)")
ip_t1_reg$cv2 <- ifelse(ip_t1_reg$cv=="Senator",
                        "Senator\n(Regression)",
                        "Voter\n(XRegression)")
ip_t1 <- rbind(ip_t1o,ip_t1_pro,ip_t1_reg)
ip_t1$cv2 <- factor(ip_t1$cv2, levels=unique(ip_t1$cv2)) 

######################
## voter to senator ##
######################

## set.seed
set.seed(20191005)
## oc
t2 <-ipbridge(dv,dc,20,polarity1=rep(3,2),
              polarity2=rep(1,2), model="oc")

dvp_new <- dvp[-t2[[5]]]
dvp_new <- c(dvp_new, dvp[t2[[5]]])
dvp_new <- factor(dvp_new, levels=c(1,2), labels=c("Democrat","Republican"))

## creating IP data (coordinates and party id) by respondents type
tc <- data.frame(t2[[4]]);colnames(tc) <- c("coord1D","coord2D");tc$cv <- rep(1, dim(tc)[1]);tc$party <- dcp
tv_pro <- data.frame(t2[[2]]);colnames(tv_pro) <- c("coord1D","coord2D");tv_pro$cv <- rep(2, dim(tv_pro)[1]);tv_pro$party <- dvp_new
tv_reg <- data.frame(t2[[3]]);colnames(tv_reg) <- c("coord1D","coord2D");tv_reg$cv <- rep(2, dim(tv_reg)[1]);tv_reg$party <- dvp_new

## first, transformed senators and original voters
ip_t2_pro <- rbind.data.frame(tc,tv_pro)
ip_t2_pro <- ip_t2_pro %>% filter(party == "Democrat" | party == "Republican")
ip_t2_pro$cv <- factor(ip_t2_pro$cv,  levels=c(1,2), labels=c("Senator","Voter"))

## second, transformed senators and original voters
ip_t2_reg <- rbind.data.frame(tc,tv_reg)
ip_t2_reg <- ip_t2_reg %>% filter(party == "Democrat" | party == "Republican")
ip_t2_reg$cv <- factor(ip_t2_reg$cv,  levels=c(1,2), labels=c("Senator","Voter"))

## creating IP data (coordinates and party id) for original voters IP
tv_o <- data.frame(t2[[1]]);colnames(tv_o) <- c("coord1D","coord2D");tv_o$cv <- rep(2, dim(tv_pro)[1]);tv_o$party <- dvp_new
## original senators and original voters
ip_t2o <- rbind.data.frame(tc,tv_o)
ip_t2o <- ip_t2o %>% filter(party == "Democrat" | party == "Republican")
ip_t2o$cv <- factor(ip_t2o$cv,  levels=c(1,2), labels=c("Senator","Voter"))

######################
## Create C to V IP ##
######################
ip_t2o$cv2 <- ifelse(ip_t2o$cv=="Senator",
                     "Senator\n(untransformed)",
                     "Voter\n(untransformed)")
ip_t2_pro$cv2 <- ifelse(ip_t2_pro$cv=="Senator",
                        "Senator\n(XProcrustes)",
                        "Voter\n(Procrustes)")
ip_t2_reg$cv2 <- ifelse(ip_t2_reg$cv=="Senator",
                        "Senator\n(XRegression)",
                        "Voter\n(Regression)")
ip_t2 <- rbind(ip_t2o,ip_t2_pro,ip_t2_reg)
ip_t2$cv2 <- factor(ip_t2$cv2, levels=unique(ip_t2$cv2)) 

rm(dc, dj, dv, t, t1, t2, tc, tc_o, tc_pro, tc_reg, tj, tv, tv_o, tv_pro, tv_reg,
   ip_t1_pro, ip_t1_reg, ip_t1o, ip_t2_pro, ip_t2_reg, ip_t2o)

rm(dcp, dcp_new, dvp, dvp_new, ipbridge)

##################
## Save IP Data ##
##################

#save.image("senator_ip.rda")
