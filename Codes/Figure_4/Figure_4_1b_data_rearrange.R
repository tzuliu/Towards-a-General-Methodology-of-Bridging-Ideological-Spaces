## Rearrange Data

d_new_x <- list()
d_new_y <- list()

d_mean_x <- vector()
d_mean_y <- vector()

dmean_x <- list()
dmean_y <- list()

## 20 outsamples ##

load(paste0(projdir,"/Outputs/simulation/20s_2000t_ooc.RData"))

for(i in 1:2000){
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

## 50 outsamples ##

load(paste0(projdir,"/Outputs/simulation/50s_2000t_ooc.RData"))

for(i in 1:2000){
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

## 100 outsamples ## 

load(paste0(projdir,"/Outputs/simulation/100s_2000t_ooc.RData"))

for(i in 1:2000){
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

## 500 outsamples ##

load(paste0(projdir,"/Outputs/simulation/500s_2000t_ooc.RData"))

for(i in 1:2000){
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

## 1000 outsamples ##

load(paste0(projdir,"/Outputs/simulation/1000s_2000t_ooc.RData"))

for(i in 1:2000){
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

dmean_x[[5]] <- d_mean_x
dmean_y[[5]] <- d_mean_y

dmean_x[[6]] <- na.omit(d[[2001]][,1])
dmean_y[[6]] <- na.omit(d[[2001]][,2])

names(dmean_x) <- c("20 outsample","50 outsample","100 outsample","500 outsample","1000 outsample","Original")
names(dmean_y) <- c("20 outsample","50 outsample","100 outsample","500 outsample","1000 outsample","Original")

## Summarize data ##

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

save(dmean_var_x, file=paste0(projdir, "/Outputs/simulation/mean_dens_group_x.rda"))
save(dmean_var_y, file=paste0(projdir, "/Outputs/simulation/mean_dens_group_y.rda"))