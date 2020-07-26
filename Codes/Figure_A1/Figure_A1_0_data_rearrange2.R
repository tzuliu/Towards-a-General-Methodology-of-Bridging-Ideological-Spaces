## UTAS 09 Recode

## Load Data
lsv <- read.csv(paste0(projdir,"/Data/utas_data/utas_variable_list_utf-8.csv"),
                stringsAsFactors = FALSE, fileEncoding = "UTF-8")
utas1 <- readRDS(paste0(projdir,"/Outputs/application/utas09_ooc.rds"))

utas1_tmp <- utas1[,15:37]

del_rep <- vector()
for(i in 1:nrow(utas1_tmp)){
  if(sum(is.na(utas1_tmp[i,]))/22 > 0.8){
    del_rep <- c(del_rep,i) 
  }
}

utas1 <- utas1[-del_rep,]

utas1_v <- utas1[utas1$cv=="voter",]
utas1_v_sub <- vector()
utas1_v_p <- utas1_v[,15:37]
detvs <- names(utas1)[-c(1:14)][names(utas1)[-c(1:14)] %in% lsv$qid[!is.na(lsv$q_right)]]
detis <- lsv$q_right[!is.na(lsv$q_right)][lsv$qid[!is.na(lsv$q_right)] %in% names(utas1)[-c(1:14)]]

names(utas1)
lsv$qid[!is.na(lsv$q_right)]

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
#utas1[which(utas1$cv=="voter"),][as.numeric(names(utas1_rightcand_v)),c(3:4)]
utas1_rightcand_v <- as.numeric(names(utas1_rightcand_v)[2])

utas1_c <- utas1[utas1$cv=="candidate",]
utas1_c_sub <- vector()
#utas1_c_new <- rbind(utas1_c, utas1_v[(nrow(utas1_v_p)-(29:0)),])
utas1_c_p <- utas1_c[,15:37]
#utas1_c_p_new <-  rbind(utas1_c_p, utas1_v_p[(nrow(utas1_v_p)-(29:0)),])

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
utas1_rightcand_c <- as.numeric(names(utas1_rightcand_c)[2])

polar <- nrow(utas1_c_p) + utas1_rightcand_v

save.image(paste0(projdir,"/Outputs/application/utas09_data.rda"))
