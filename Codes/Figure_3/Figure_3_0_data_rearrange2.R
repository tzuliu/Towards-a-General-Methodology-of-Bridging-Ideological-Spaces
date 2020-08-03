## Rearranging UTAS 2012 (2)
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
utas2 <- readRDS(paste0(projdir,"/Outputs/application/utas12_ooc.rds"))

####################
## Rearrangements ##
####################

## Delete cases with more than 10% non-responses

utas2_tmp <- utas2[,15:54]

del_rep <- vector()
for(i in 1:nrow(utas2_tmp)){
  if(sum(is.na(utas2_tmp[i,]))/40 > 0.9){
    del_rep <- c(del_rep,i) 
  }
}

utas2 <- utas2[-del_rep,]

## voter data

utas2_v <- utas2[utas2$cv=="voter",]
utas2_v_p <- utas2_v[,15:54]

## identify the most right-wing voter

utas2_v_sub <- vector()

## ideological policy questions
detvs <- names(utas2)[-c(1:5)][names(utas2)[-c(1:5)] %in% lsv$qid[!is.na(lsv$q_right)]]
## hand-coded ideology codes
detis <- lsv$q_right[!is.na(lsv$q_right)][lsv$qid[!is.na(lsv$q_right)] %in% names(utas2)[-c(1:5)]]

## find consistently right-wing voters
for(i in 1:length(detis)){
  if(detis[i] < 0){
    #tmp_d <- utas2_v[,detvs[i]]-6
    utas2_v_sub <- cbind(utas2_v_sub, utas2_v[,detvs[i]]-6)#tmp_d)
  }else{
    utas2_v_sub <- cbind(utas2_v_sub, utas2_v[,detvs[i]])
  }
}

utas2_v_sub <- data.frame(utas2_v_sub)
colnames(utas2_v_sub) <- c(detvs)

tmp_v <- unlist(sapply(2:length(detvs), function(i) which(utas2_v_sub[,detvs[i]]*detis[i] >= 4)))
tmp_0 <- which(utas2_v_sub[,detvs[1]]*detis[1] >= 9)
tmp_v <- c(tmp_v, tmp_0)
utas2_rightcand_v <- table(tmp_v)[which(as.vector(table(tmp_v))==max(table(tmp_v)))]
utas2_v[as.numeric(names(utas2_rightcand_v)),c(3:5)]
utas2[which(utas2$cv=="voter"),][as.numeric(names(utas2_rightcand_v)),c(3:4)]
# Location (row number) of the consistently right-wing voter
utas2_rightcand_v <- as.numeric(names(utas2_rightcand_v)[2])
utas2_rightcand_v

## candidate data 

utas2_c <- utas2[utas2$cv=="candidate",]
utas2_c_p <- utas2_c[,15:54]

## identify the most right-wing candidate

utas2_c_sub <- vector()

## find consistently right-wing cadidates
for(i in 1:length(detis)){
  if(detis[i] < 0){
    #tmp_d <- utas2_v[,detvs[i]]-6
    utas2_c_sub <- cbind(utas2_c_sub, utas2_c[,detvs[i]]-6)#tmp_d)
  }else{
    utas2_c_sub <- cbind(utas2_c_sub, utas2_c[,detvs[i]])
  }
}

utas2_c_sub <- data.frame(utas2_c_sub)
colnames(utas2_c_sub) <- c(detvs)

tmp_c <- unlist(sapply(2:length(detvs), function(i) which(utas2_c_sub[,detvs[i]]*detis[i] >= 4)))
tmp_c0 <- which(utas2_c_sub[,detvs[1]]*detis[1] >= 9)
tmp_c <- c(tmp_c, tmp_c0)
utas2_rightcand_c <- table(tmp_c)[which(as.vector(table(tmp_c))==max(table(tmp_c)))]
utas2_c[as.numeric(names(utas2_rightcand_c)),c(3:5)]
# Location (row number) of the consistently right-wing candidate
utas2_rightcand_c <- as.numeric(names(utas2_rightcand_c)[1])
utas2_rightcand_c

###############
## Save Data ##
###############

# Remove unnecessary objects
rm(lsv, utas2, utas2_c_sub, utas2_tmp, utas2_v_sub, del_rep,
   detis, detvs, i, tmp_c, tmp_c0, tmp_v, tmp_0)

## Save data
tmpdir <- projdir
rm(projdir)
save.image(paste0(tmpdir,"/Outputs/application/utas12_data.rda"))