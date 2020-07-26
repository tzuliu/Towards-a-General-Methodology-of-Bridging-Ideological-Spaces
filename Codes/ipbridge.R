library(MASS)
library(MCMCpack)
library(hitandrun)
# devtools::install_github("cran/oc")
# devtools::install_github("tzuliu/ooc")
library(ooc)
library(Matrix)
library(ggplot2)
library(statar)
library(reshape)
library(viridis)
library(foreach)
library(smacof)
library(doParallel)
library(dplyr)

## ipbridge function
ipbridge <-function(data1,data2,anchor=35,polarity1=c(1,1),polarity2=c(5,5),model="ooc"){
  ## extracting anchors and stiching them to the other data
  del <- sample(1:nrow(data1),anchor,replace=FALSE)
  bot <- data1[del,]
  data1 <- data1[-del,]
  data1 <- rbind(data1,bot)
  data2 <- rbind(data2,bot)
  ## ooc
  if(model=="ooc"){
    res_1 <- ooc(data1, dims=2, min=10, lop=0.0001, polarity=polarity1, iter=25, nv.method="svm.reg", cost=1)
    res_2 <- ooc(data2, dims=2, min=10, lop=0.0001, polarity=polarity2, iter=25, nv.method="svm.reg", cost=1)
    ## estimated IPs
    x_1 <- res_1$respondents[,grepl("coord", colnames(res_1$respondents))]
    x_2 <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]
  }else if(model=="oc"){
    res_1 <- oc(rollcall(data1), dims=2, minvotes=5, lop=0.025, polarity = polarity1)
    res_2 <- oc(rollcall(data2), dims=2, minvotes=5, lop=0.025, polarity = polarity2)
    ## estimated IPs
    x_1 <- res_1$legislators[,grepl("coord", colnames(res_1$legislators))]
    x_2 <- res_2$legislators[,grepl("coord", colnames(res_2$legislators))]
  }
  
  ########################
  ## Generating Anchors ##
  ########################
  anchors_1 <- x_1[nrow(x_1)-((anchor-1):0),]
  anchors_2 <- x_2[nrow(x_2)-((anchor-1):0),]
  ##########################################
  ## Estimating Procrustes Transformation ##
  ##########################################
  p <- procrustes(anchors_1, anchors_2, translation=TRUE)
  ##########################################
  ## Transforming All Points (Procrustes) ##
  ##########################################
  x_1_pro <- p$s*(x_1%*%p$R) + (matrix(rep(1,nrow(x_1)), ncol=1) %*% t(p$tt))
  ##########################################
  ## Estimating Regression Transformation ##
  ##########################################
  reg_x <- lm(anchors_2[,1] ~ anchors_1[,1] + anchors_1[,2])
  reg_y <- lm(anchors_2[,2] ~ anchors_1[,1] + anchors_1[,2])
  x_int <- reg_x$coef[1]; x_slo1 <- reg_x$coef[2]; x_slo2 <- reg_x$coef[3]
  y_int <- reg_y$coef[1]; y_slo1 <- reg_y$coef[2]; y_slo2 <- reg_y$coef[3]
  ##########################################
  ## Transforming All Points (Regression) ##
  ##########################################
  x_1_r_x <- x_int + x_slo1*x_1[,1] + x_slo2*x_1[,2]
  x_1_r_y <- y_int + y_slo1*x_1[,1] + y_slo2*x_1[,2]
  x_1_reg <- cbind(x_1_r_x, x_1_r_y)
  ##################
  ## Re-Fresh x_2 ##
  ##################
  x_2 <- x_2[1:(nrow(x_2)-anchor),]
  ## returning results
  list(x_1, x_1_pro, x_1_reg, x_2, del, anchors_1, anchors_2)
}
