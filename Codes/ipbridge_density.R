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
library(dplyr)
library(plyr)
library(data.table)

## ipbridge function
ipbridge_density <-function(data1,data2,anchor=35,polarity1=c(1,1),polarity2=c(5,5),model="ooc"){
  ## extracting anchors and stiching them to the other data
  if (anchor==0) {
    if (model=="ooc") {
      res_2 <- ooc(data2, dims=2, min=10, lop=0.0001, polarity=polarity2, iter=25, nv.method="svm.reg", cost=1)
      ## estimated IPs
      x_2 <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]
    } else if (model=="oc") {
      res_2 <- oc(rollcall(data2), dims=2, minvotes=5, lop=0.025, polarity = polarity2)
      ## estimated IPs
      x_2 <- res_2$legislators[,grepl("coord", colnames(res_2$legislators))]
    } 
  } else {
    del <- sample(1:nrow(data1),anchor,replace=FALSE)
    bot <- data1[del,]
    data1 <- data1[-del,]
    data1 <- rbind(data1,bot)
    data2 <- rbind(data2,bot)
    ## ooc
    if(model=="ooc"){
      res_2 <- ooc(data2, dims=2, min=10, lop=0.0001, polarity=polarity2, iter=25, nv.method="svm.reg", cost=1)
      ## estimated IPs
      x_2 <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]
    } else if (model=="oc") {
      res_2 <- oc(rollcall(data2), dims=2, minvotes=5, lop=0.025, polarity = polarity2)
      ## estimated IPs
      x_2 <- res_2$legislators[,grepl("coord", colnames(res_2$legislators))]
    }
    x_2 <- x_2[1:(nrow(x_2)-anchor),]
  }
  return(x_2)
}
