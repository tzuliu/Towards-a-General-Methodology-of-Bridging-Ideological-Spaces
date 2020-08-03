## Rearranging UTAS 2009 (2)
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
lsv <- read.csv(paste0(projdir,"/Data/utas_data/utas_variable_list_utf-8.csv"),
                stringsAsFactors = FALSE, fileEncoding = "UTF-8")
utas1 <- readRDS(paste0(projdir,"/Outputs/application/utas09_ooc.rds"))

####################
## Rearrangements ##
####################

## Delete cases with more than 10% non-responses

utas1_tmp <- utas1[,15:37]

del_rep <- vector()
for(i in 1:nrow(utas1_tmp)){
  if(sum(is.na(utas1_tmp[i,]))/22 > 0.8){
    del_rep <- c(del_rep,i) 
  }
}

utas1 <- utas1[-del_rep,]

## voter data

utas1_v <- utas1[utas1$cv=="voter",]
utas1_v_p <- utas1_v[,15:37]

## identify the most right-wing voter

utas1_v_sub <- vector()

## ideological policy questions
detvs <- names(utas1)[-c(1:14)][names(utas1)[-c(1:14)] %in% lsv$qid[!is.na(lsv$q_right)]]
## hand-coded ideology codes
detis <- lsv$q_right[!is.na(lsv$q_right)][lsv$qid[!is.na(lsv$q_right)] %in% names(utas1)[-c(1:14)]]

## find consistently right-wing voters
for(i in 1:length(detis)){
  if(detis[i] < 0){
    #tmp_d <- utas2_v[,detvs[i]]-6
    utas1_v_sub <- cbind(utas1_v_sub, utas1_v[,detvs[i]]-6)#tmp_d)
  }else{
    utas1_v_sub <- cbind(utas1_v_sub, utas1_v[,detvs[i]])
  }
}
utas1_v_sub <- data.frame(utas1_v_sub)
colnames(utas1_v_sub) <- c(detvs)
tmp_v <- unlist(sapply(1:length(detvs), function(i) which(utas1_v_sub[,detvs[i]]*detis[i] >= 4)))
utas1_rightcand_v <- table(tmp_v)[which(as.vector(table(tmp_v))==max(table(tmp_v)))]
utas1_v[as.numeric(names(utas1_rightcand_v)),c(3:5)]
utas1[which(utas1$cv=="voter"),][as.numeric(names(utas1_rightcand_v)),c(3:4)]
# Location (row number) of the consistently right-wing voter
utas1_rightcand_v <- as.numeric(names(utas1_rightcand_v)[2])
utas1_rightcand_v

## canidate data 

utas1_c <- utas1[utas1$cv=="candidate",]
#utas1_c_new <- rbind(utas1_c, utas1_v[(nrow(utas1_v_p)-(29:0)),])
utas1_c_p <- utas1_c[,15:37]
#utas1_c_p_new <-  rbind(utas1_c_p, utas1_v_p[(nrow(utas1_v_p)-(29:0)),])

## identify the most right-wing candidate

utas1_c_sub <- vector()

## find consistently right-wing candidate
for(i in 1:length(detis)){
  if(detis[i] < 0){
    #tmp_d <- utas2_v[,detvs[i]]-6
    utas1_c_sub <- cbind(utas1_c_sub, utas1_c[,detvs[i]]-6)#tmp_d)
  }else{
    utas1_c_sub <- cbind(utas1_c_sub, utas1_c[,detvs[i]])
  }
}

utas1_c_sub <- data.frame(utas1_c_sub)
colnames(utas1_c_sub) <- c(detvs)

tmp_c <- unlist(sapply(1:length(detvs), function(i) which(utas1_c_sub[,detvs[i]]*detis[i] >= 4)))
utas1_rightcand_c <- table(tmp_c)[which(as.vector(table(tmp_c))==max(table(tmp_c)))]
utas1_c[as.numeric(names(utas1_rightcand_c)),c(3:5)]
# Location (row number) of the consistently right-wing candidate
utas1_rightcand_c <- as.numeric(names(utas1_rightcand_c)[2])
utas1_rightcand_c

###############
## Save Data ##
###############

# Remove unnecessary objects
rm(lsv, utas1, utas1_c_sub, utas1_tmp, utas1_v_sub, del_rep,
   detis, detvs, i, tmp_c, tmp_v)

## Save data
tmpdir <- projdir
rm(projdir)
save.image(paste0(tmpdir,"/Outputs/application/utas09_data.rda"))
