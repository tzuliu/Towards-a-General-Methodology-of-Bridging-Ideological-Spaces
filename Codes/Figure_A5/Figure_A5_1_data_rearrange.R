## Validating Synthetic Anchors (Senator Data) Data Rearrangement
## (Ingroup=Senators, Outgroup=Voters)
## Author: Tzu-Ping Liu & Gento Kato
## Date: 08/03/2020
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

## Initiate data storage

d_new_x <- list()
d_new_y <- list()

d_mean_x <- vector()
d_mean_y <- vector()

dmean_x <- list()
dmean_y <- list()

## 10 Outsamples ##

load(paste0(projdir, "/Outputs/simulation/senator_sim_400_0010s_cand.rda"))

(tmp10_1h <- head(d[[1]]$coord1D))
(tmp10_1t <- tail(d[[1]]$coord1D))
(tmp10_2h <- head(d[[1]]$coord2D))
(tmp10_2t <- tail(d[[1]]$coord2D))

for(i in 1:400){
  d_new_x[[i]] <- na.omit(d[[i]][,1])
  d_new_y[[i]] <- na.omit(d[[i]][,2])
}

for(i in 1:length(d_new_x[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_x)){
    tmp_v[j] <- d_new_x[[j]][i]
  }
  d_mean_x[i] <- mean(tmp_v)
}

for(i in 1:length(d_new_y[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_y)){
    tmp_v[j] <- d_new_y[[j]][i]
  }
  d_mean_y[i] <- mean(tmp_v)
}

dmean_x[[1]] <- d_mean_x
dmean_y[[1]] <- d_mean_y

## 20 Outsamples ##

load(paste0(projdir, "/Outputs/simulation/senator_sim_400_0020s_cand.rda"))

(tmp20_1h <- head(d[[1]]$coord1D))
(tmp20_1t <- tail(d[[1]]$coord1D))
(tmp20_2h <- head(d[[1]]$coord2D))
(tmp20_2t <- tail(d[[1]]$coord2D))

for(i in 1:400){
  d_new_x[[i]] <- na.omit(d[[i]][,1])
  d_new_y[[i]] <- na.omit(d[[i]][,2])
}

for(i in 1:length(d_new_x[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_x)){
    tmp_v[j] <- d_new_x[[j]][i]
  }
  d_mean_x[i] <- mean(tmp_v)
}

for(i in 1:length(d_new_y[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_y)){
    tmp_v[j] <- d_new_y[[j]][i]
  }
  d_mean_y[i] <- mean(tmp_v)
}

dmean_x[[2]] <- d_mean_x
dmean_y[[2]] <- d_mean_y

## 50 Outsamples ##

load(paste0(projdir, "/Outputs/simulation/senator_sim_400_0050s_cand.rda"))

(tmp50_1h <- head(d[[1]]$coord1D))
(tmp50_1t <- tail(d[[1]]$coord1D))
(tmp50_2h <- head(d[[1]]$coord2D))
(tmp50_2t <- tail(d[[1]]$coord2D))

for(i in 1:400){
  d_new_x[[i]] <- na.omit(d[[i]][,1])
  d_new_y[[i]] <- na.omit(d[[i]][,2])
}

for(i in 1:length(d_new_x[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_x)){
    tmp_v[j] <- d_new_x[[j]][i]
  }
  d_mean_x[i] <- mean(tmp_v)
}

for(i in 1:length(d_new_y[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_y)){
    tmp_v[j] <- d_new_y[[j]][i]
  }
  d_mean_y[i] <- mean(tmp_v)
}

dmean_x[[3]] <- d_mean_x
dmean_y[[3]] <- d_mean_y

## 100 Outsamples ##

load(paste0(projdir, "/Outputs/simulation/senator_sim_400_0100s_cand.rda"))

(tmp100_1h <- head(d[[1]]$coord1D))
(tmp100_1t <- tail(d[[1]]$coord1D))
(tmp100_2h <- head(d[[1]]$coord2D))
(tmp100_2t <- tail(d[[1]]$coord2D))

for(i in 1:400){
  d_new_x[[i]] <- na.omit(d[[i]][,1])
  d_new_y[[i]] <- na.omit(d[[i]][,2])
}

for(i in 1:length(d_new_x[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_x)){
    tmp_v[j] <- d_new_x[[j]][i]
  }
  d_mean_x[i] <- mean(tmp_v)
}

for(i in 1:length(d_new_y[[1]])){
  tmp_v <- vector()
  for(j in 1:length(d_new_y)){
    tmp_v[j] <- d_new_y[[j]][i]
  }
  d_mean_y[i] <- mean(tmp_v)
}

dmean_x[[4]] <- d_mean_x
dmean_y[[4]] <- d_mean_y

## without Any Outsample ##

d <- readRDS(paste0(projdir, "/Outputs/simulation/senator_sim_400_clean_cand.rds"))

dmean_x[[5]] <- na.omit(d[,1])
dmean_y[[5]] <- na.omit(d[,2])

## Summarize & Save Data ##

names(dmean_x) <- c("10 outsample","20 outsample","50 outsample","100 outsample","Original")
names(dmean_y) <- c("10 outsample","20 outsample","50 outsample","100 outsample","Original")

dmean_var_x <- vector()
dmean_var_y <- vector()

for(i in 1:length(dmean_x)){
  tmp_d <- cbind.data.frame(dmean_x[[i]],names(dmean_x)[i])
  names(tmp_d) <- c("pos","samp")
  dmean_var_x <- rbind(dmean_var_x, tmp_d)
}

for(i in 1:length(dmean_y)){
  tmp_d <- cbind.data.frame(dmean_y[[i]],names(dmean_y)[i])
  names(tmp_d) <- c("pos","samp")
  dmean_var_y <- rbind(dmean_var_y, tmp_d)
}

save(dmean_var_x, file=paste0(projdir, "/Outputs/simulation/senator_sim_400_mean_dens_x.rda"))
save(dmean_var_y, file=paste0(projdir, "/Outputs/simulation/senator_sim_400_mean_dens_y.rda"))
