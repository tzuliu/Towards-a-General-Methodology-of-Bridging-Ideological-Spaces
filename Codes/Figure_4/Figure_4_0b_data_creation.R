#
library(MASS)
library(MCMCpack)
library(hitandrun)
library(ooc)
library(Matrix)
library(ggplot2)
library(statar)
library(reshape)
library(viridis)
library(foreach)
library(smacof)
library(doParallel)
library(plyr)
library(dplyr)
library(data.table)

## ipbridge function ##
ipbridge_density <-function(data1,data2,anchor=35,polarity2=c(5,5),model="ooc",del=c()){
  ## extracting anchors and stiching them to the other data
  if(anchor==0){
    if(model=="ooc"){
      res_2 <- ooc(data2, dims=2, min=10, lop=0.0001, polarity=polarity2, iter=25, nv.method="svm.reg", cost=1)
      ## estimated IPs
      x_2 <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]
    }else if(model=="oc"){
      res_2 <- oc(rollcall(data2), dims=2, minvotes=5, lop=0.025, polarity = polarity2)
      ## estimated IPs
      x_2 <- res_2$legislators[,grepl("coord", colnames(res_2$legislators))]
    } 
  }else{
    #del <- sample(1:nrow(data1),anchor,replace=FALSE)
    bot <- data1[del,]
    data1 <- data1[-del,]
    data1 <- rbind(data1,bot)
    data2 <- rbind(data2,bot)
    ## ooc
    if(model=="ooc"){
      res_2 <- ooc(data2, dims=2, min=10, lop=0.0001, polarity=polarity2, iter=25, nv.method="svm.reg", cost=1)
      ## estimated IPs
      x_2 <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]
      x_2 <- x_2[1:(nrow(x_2)-anchor),]
    }else if(model=="oc"){
      res_2 <- oc(rollcall(data2), dims=2, minvotes=5, lop=0.025, polarity = polarity2)
      ## estimated IPs
      x_2 <- res_2$legislators[,grepl("coord", colnames(res_2$legislators))]
      x_2 <- x_2[1:(nrow(x_2)-anchor),]
    }
  }
  return(x_2)
}


lsv <- read.csv(paste0(projdir, "/Data/utas_data/utas_variable_list_utf-8.csv",
                stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
utas2 <- readRDS(paste0(projdir,"/Outputs/application/utas12_ooc.rds"))
utas2_v <- utas2[utas2$cv=="voter",]
utas2_v_p <- utas2_v[,15:49]
detvs <- names(utas2)[-c(1:5)][names(utas2)[-c(1:5)] %in% lsv$qid[!is.na(lsv$q_right)]]
detis <- lsv$q_right[!is.na(lsv$q_right)][lsv$qid[!is.na(lsv$q_right)] %in% names(utas2)[-c(1:5)]]
tmp_v <- unlist(sapply(2:length(detvs), function(i) which(utas2_v[,detvs[i]]*detis[i] >= 4)))
tmp_0 <- which(utas2_v[,detvs[1]]*detis[1] >= 9)
tmp_v <- c(tmp_v, tmp_0)
utas2_rightcand_v <- table(tmp_v)[which(as.vector(table(tmp_v))==max(table(tmp_v)))]
utas2_v[as.numeric(names(utas2_rightcand_v)),c(3:5)]
utas2[which(utas2$cv=="voter"),][as.numeric(names(utas2_rightcand_v)),c(3:4)]
utas2_rightcand_v <- as.numeric(names(utas2_rightcand_v)[2])

utas2_c <- utas2[utas2$cv=="candidate",]
utas2_c_new <- rbind(utas2_c, utas2_v[(nrow(utas2_v_p)-(29:0)),])
utas2_c_p <- utas2_c[,15:49]
utas2_c_p_new <-  rbind(utas2_c_p, utas2_v_p[(nrow(utas2_v_p)-(49:0)),])
tmp_c <- unlist(sapply(2:length(detvs), function(i) which(utas2_c[,detvs[i]]*detis[i] >= 4)))
tmp_c0 <- which(utas2_c[,detvs[1]]*detis[1] >= 9)
tmp_c <- c(tmp_c, tmp_c0)
utas2_rightcand_c <- table(tmp_c)[which(as.vector(table(tmp_c))==max(table(tmp_c)))]
utas2_c[as.numeric(names(utas2_rightcand_c)),c(3:5)]
utas2_rightcand_c <- as.numeric(names(utas2_rightcand_c)[1])

## 20 Outsamples (candidates) ##
d <- list()
del_list <- list()
time <- 2000
set.seed(19821983)
for(i in 1:time){
  del <- sample(1:nrow(utas2_v_p),20,replace=FALSE)
  del_list[[i]] <- del
}

for(i in 1:time){
  d[[i]] <- ipbridge_density(utas2_v_p,utas2_c_p,15,polarity2=rep(utas2_rightcand_c,2),del=del_list[[i]])
}


for(i in 1:length(d)){
  d[[i]] <- data.frame(d[[i]])
}

##
res_2 <- ooc(utas2_c_p, dims=2, min=10, lop=0.0001, polarity=rep(utas2_rightcand_c,2), iter=25, nv.method="svm.reg", cost=1)
d_clean <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]

d[[(time+1)]] <- data.frame(d_clean)

## Deriving the mean of each respondent ##
coor1 <- vector()
coor2 <- vector()
for(i in 1:nrow(utas2_c_p)){
  coor1 <- append(coor1, mean(unlist(sapply(d, function(x) x[i,"coord1D"])),na.rm=TRUE))
  coor2 <- append(coor2, mean(unlist(sapply(d, function(x) x[i,"coord2D"])),na.rm=TRUE))
}

d[[(time+2)]] <- cbind.data.frame(coor1, coor2)

save(d, file=paste0(projdir,"/Outputs/simulation/20s_2000t_ooc.RData"))

## 50 Outsamples (candidates) ##
d <- list()
del_list <- list()
time <- 2000
set.seed(19821983)
for(i in 1:time){
  del <- sample(1:nrow(utas2_v_p),50,replace=FALSE)
  del_list[[i]] <- del
}

for(i in 1:time){
  d[[i]] <- ipbridge_density(utas2_v_p,utas2_c_p,15,polarity2=rep(utas2_rightcand_c,2),del=del_list[[i]])
}


for(i in 1:length(d)){
  d[[i]] <- data.frame(d[[i]])
}

##
res_2 <- ooc(utas2_c_p, dims=2, min=10, lop=0.0001, polarity=rep(utas2_rightcand_c,2), iter=25, nv.method="svm.reg", cost=1)
d_clean <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]

d[[(time+1)]] <- data.frame(d_clean)

## Deriving the mean of each respondent ##
coor1 <- vector()
coor2 <- vector()
for(i in 1:nrow(utas2_c_p)){
  coor1 <- append(coor1, mean(unlist(sapply(d, function(x) x[i,"coord1D"])),na.rm=TRUE))
  coor2 <- append(coor2, mean(unlist(sapply(d, function(x) x[i,"coord2D"])),na.rm=TRUE))
}

d[[(time+2)]] <- cbind.data.frame(coor1, coor2)

save(d, file=paste0(projdir, "/Outputs/simulation/50s_2000t_ooc.RData"))

## 100 Outsamples (candidates) ##
d <- list()
del_list <- list()
time <- 2000
set.seed(19821983)
for(i in 1:time){
  del <- sample(1:nrow(utas2_v_p),100,replace=FALSE)
  del_list[[i]] <- del
}

for(i in 1:time){
  d[[i]] <- ipbridge_density(utas2_v_p,utas2_c_p,15,polarity2=rep(utas2_rightcand_c,2),del=del_list[[i]])
}


for(i in 1:length(d)){
  d[[i]] <- data.frame(d[[i]])
}

##
res_2 <- ooc(utas2_c_p, dims=2, min=10, lop=0.0001, polarity=rep(utas2_rightcand_c,2), iter=25, nv.method="svm.reg", cost=1)
d_clean <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]

d[[(time+1)]] <- data.frame(d_clean)

## Deriving the mean of each respondent ##
coor1 <- vector()
coor2 <- vector()
for(i in 1:nrow(utas2_c_p)){
  coor1 <- append(coor1, mean(unlist(sapply(d, function(x) x[i,"coord1D"])),na.rm=TRUE))
  coor2 <- append(coor2, mean(unlist(sapply(d, function(x) x[i,"coord2D"])),na.rm=TRUE))
}

d[[(time+2)]] <- cbind.data.frame(coor1, coor2)

save(d, file=paste0(projdir, "/Outputs/simulation/100s_2000t_ooc.RData"))

## 500 Outsamples (candidates) ##
d <- list()
del_list <- list()
time <- 2000
set.seed(19821983)
for(i in 1:time){
  del <- sample(1:nrow(utas2_v_p),500,replace=FALSE)
  del_list[[i]] <- del
}

for(i in 1:time){
  d[[i]] <- ipbridge_density(utas2_v_p,utas2_c_p,15,polarity2=rep(utas2_rightcand_c,2),del=del_list[[i]])
}


for(i in 1:length(d)){
  d[[i]] <- data.frame(d[[i]])
}

##
res_2 <- ooc(utas2_c_p, dims=2, min=10, lop=0.0001, polarity=rep(utas2_rightcand_c,2), iter=25, nv.method="svm.reg", cost=1)
d_clean <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]

d[[(time+1)]] <- data.frame(d_clean)

## Deriving the mean of each respondent ##
coor1 <- vector()
coor2 <- vector()
for(i in 1:nrow(utas2_c_p)){
  coor1 <- append(coor1, mean(unlist(sapply(d, function(x) x[i,"coord1D"])),na.rm=TRUE))
  coor2 <- append(coor2, mean(unlist(sapply(d, function(x) x[i,"coord2D"])),na.rm=TRUE))
}

d[[(time+2)]] <- cbind.data.frame(coor1, coor2)

save(d, file=paste0(projdir, "/Outputs/simulation/500s_2000t_ooc.RData"))

## 1000 Outsamples (candidates) ##
d <- list()
del_list <- list()
time <- 2000
set.seed(19821983)
for(i in 1:time){
  del <- sample(1:nrow(utas2_v_p),1000,replace=FALSE)
  del_list[[i]] <- del
}

for(i in 1:time){
  d[[i]] <- ipbridge_density(utas2_v_p,utas2_c_p,15,polarity2=rep(utas2_rightcand_c,2),del=del_list[[i]])
}


for(i in 1:length(d)){
  d[[i]] <- data.frame(d[[i]])
}

##
res_2 <- ooc(utas2_c_p, dims=2, min=10, lop=0.0001, polarity=rep(utas2_rightcand_c,2), iter=25, nv.method="svm.reg", cost=1)
d_clean <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]

d[[(time+1)]] <- data.frame(d_clean)

## Deriving the mean of each respondent ##
coor1 <- vector()
coor2 <- vector()
for(i in 1:nrow(utas2_c_p)){
  coor1 <- append(coor1, mean(unlist(sapply(d, function(x) x[i,"coord1D"])),na.rm=TRUE))
  coor2 <- append(coor2, mean(unlist(sapply(d, function(x) x[i,"coord2D"])),na.rm=TRUE))
}

d[[(time+2)]] <- cbind.data.frame(coor1, coor2)

save(d, file=paste0(projdir, "/Outputs/simulation/1000s_2000t_ooc.RData"))