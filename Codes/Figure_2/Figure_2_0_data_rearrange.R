## Rearranging Senator Data
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

## loading data
load(paste0(projdir,"/Data/senate_data/Formatted info votes matrices.rdata"))
load(paste0(projdir,"/Data/senate_data/Formatted Presidential Issues Survey Data.rdata"))

## reorganizing data
d <-as.data.frame(VOTES.ALL.SMALL)
colnames(d) <-paste0("policy",seq(1,ncol(d)))

## respondent's type
d$cv <-c(rep("senator",length(sen.party)),rep("voter",nrow(DAT)))

## party id
d$psup <-c(ifelse(sen.party==100,"Democrat",ifelse(sen.party==200,"Republican",NA)),ifelse(DAT$prezvote==1,"Republican",ifelse(DAT$prezvote==0,"Democrat",NA)))
d$psup <-factor(d$psup, levels=c("Democrat","Republican"))

## party vote
d$pltsup <-c(ifelse(sen.party==100,"Democrat",ifelse(sen.party==200,"Republican",NA)),ifelse(DAT$pid3i==3,"Republican",ifelse(DAT$pid3i==1,"Democrat","Independent")))
d$pltsup <-factor(d$pltsup, levels=c("Democrat","Independent","Republican"))

## vote matrix by respondent-type
dc <- d[which(d$cv=="senator"),grep("^policy",names(d))]
dv <- d[which(d$cv=="voter"),grep("^policy",names(d))]

## extra-variable for respondent's type
dcp <- d[which(d$cv=="senator"),"psup"]
dvp <- d[which(d$cv=="voter"),"psup"]

## polarity
dc_right <- 1
dv_right <- 3

## delet senators by the percentage of NA
del_rep <- vector()
for(i in 1:nrow(dc)){if(sum(is.na(dc[i,]))/27 > 0.5){del_rep <- c(del_rep,i) }}
dc <- dc[-del_rep,]
## fixing the length of party id
dcp <- dcp[-del_rep]
## delet voters by the percentage of NA
del_rep <- vector()
for(i in 1:nrow(dv)){if(sum(is.na(dv[i,]))/27 > 0.6){del_rep <- c(del_rep,i) }}
dv <- dv[-del_rep,]
## fixing the length of party id
dvp <- dvp[-del_rep]

## Remove Unnecessary Data
rm(d, DAT, INFO, sen.all.raw, VOTES.ALL.BIG, VOTES.ALL.SMALL, VOTES.RESP.BIG, VOTES.RESP.SMALL, VOTES.SEN.BIG, VOTES.SEN.SMALL,
   dc_right, del_rep, dv_right, i, issue.difficulty, qvote, sen.name, sen.party, senfips, senistate)

## Save.
tmpdir <- projdir
rm(projdir)
save.image(paste0(tmpdir,"/Outputs/application/senator_data.rda"))