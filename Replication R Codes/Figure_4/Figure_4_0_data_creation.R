#
# ooc_montecarlo.r

rm(list=ls(all=TRUE))
#
library(MASS)
library(MCMCpack)
library(hitandrun)
library(ooc)
library(statar)
library(Matrix)
library(reshape)
library(ggplot2)
library(viridis)
library(foreach)
library(doParallel)
#
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
#
set.seed(1985)
#
#
#
#
# DEFINE MONTE CARLO FUNCTION
#
montecarlo.oc <- function(n=1200, q=20, ndim=2, utility.probs=c(0.33,0.33,0.33),
                          missing=0.1, error.respondents=c(0.1,1), error.issues=c(2,1)){
  #
  # %%%%%%%%%%%%%%%%%%%%%%%%%
  # %%%%%    BEGIN    %%%%%%%
  # %%%%% Monte Carlo %%%%%%%
  # %%%%%%%%%%%%%%%%%%%%%%%%%
  #
  heteroskedastic.respondents <- runif(n, error.respondents[1], error.respondents[2])
  heteroskedastic.issues <- rgamma(q, error.issues[1], error.issues[2])
  #
  correlations <- runif(n, -0.1, 0.7)
  knowledge <- xtile(correlations, 3)
  #
  # 1.) Generate respondent ideal points
  #
  mu <- rep(0,ndim)
  Sigma <- list()
  idealpoints <- matrix(NA, nrow=n, ncol=ndim)
  for (i in 1:n){
    Sigma[[i]] <- matrix(1, nrow=ndim, ncol=ndim)
    Sigma[[i]][lower.tri(Sigma[[i]])] <- runif(sum(lower.tri(Sigma[[i]])), correlations[i]-0.2, correlations[i]+0.2)
    Sigma[[i]][upper.tri(Sigma[[i]])] <- t(Sigma[[i]])[upper.tri(t(Sigma[[i]]))]
    Sigma[[i]] <- as.matrix(nearPD(Sigma[[i]])$mat)
    idealpoints[i,] <- mvrnorm(1, mu=mu, Sigma=Sigma[[i]])
  }
  #
  idealpoints[idealpoints > 2] <- 2
  idealpoints[idealpoints < -2] <- -2
  #
  # 2.) Generate normal vectors
  #
  normalvectors <- hypersphere.sample(ndim, q)
  #
  for (j in 1:q){
    if (normalvectors[j,1] < 0) normalvectors[j,] <- -1 * normalvectors[j,]
  }
  #
  # 3.) Project respondents on normal vectors
  #
  respondent.projections <- idealpoints %*% t(normalvectors)
  #
  # 4.) Generate outcome locations
  #
  outcome.locations <- apply(respondent.projections, 2, function(x){sort(runif(5,min(x),max(x)))})
  #
  # 5.) Define utility functions
  #
  linear.utility <- function(idealpoint, choices){
    tmp <- 1 - (3*abs(idealpoint - choices))
    return(tmp)}
  #
  normal.utility <- function(idealpoint, choices){
    tmp <- 5 * exp(-1 * ((idealpoint - choices)^2))
    return(tmp)}
  #
  quad.utility <- function(idealpoint, choices){
    tmp <- 1 - 2 * (idealpoint - choices)^2
    return(tmp)}
  #
  # 6.) Calculate utility and response probabilities
  #
  utility.fun <- sample(c("linear","normal","quadratic"), n, replace=TRUE, prob=utility.probs)
  #
  systematic.utility <- total.utility <- probmat <- list()
  for (j in 1:q){
    systematic.utility[[j]] <- matrix(NA, nrow=n, ncol=5)
    total.utility[[j]] <- matrix(NA, nrow=n, ncol=5)
    probmat[[j]] <- matrix(NA, nrow=n, ncol=5)
  }
  #
  for (i in 1:n){
    for (j in 1:q){
      #
      if(utility.fun[i]=="linear"){
        systematic.utility[[j]][i,] <- linear.utility(respondent.projections[i,j], outcome.locations[,j])
      }
      #
      if(utility.fun[i]=="normal"){
        systematic.utility[[j]][i,] <- normal.utility(respondent.projections[i,j], outcome.locations[,j])
      }
      #
      if(utility.fun[i]=="quadratic"){
        systematic.utility[[j]][i,] <- quad.utility(respondent.projections[i,j], outcome.locations[,j])
      }
      #
      total.utility[[j]][i,] <- systematic.utility[[j]][i,] / exp(heteroskedastic.respondents[i] * heteroskedastic.issues[j])
      probmat[[j]][i,] <- pnorm(total.utility[[j]][i,]) / sum(pnorm(total.utility[[j]][i,]))
    }}
  #
  for (j in 1:q){
    probmat[[j]][is.na(probmat[[j]])] <- 1
  }
  #
  # 7.) Generate perfect and error (probabilistic) responses
  #
  simulated.responses <- perfect.responses <- matrix(NA, nrow=n, ncol=q)
  #
  for (i in 1:n){
    for (j in 1:q){
      simulated.responses[i,j] <- sample(1:5, 1, prob=probmat[[j]][i,])
      # Note: perfect voting could of course also be simulated using the maximum of total.utility
      perfect.responses[i,j] <- which.max(systematic.utility[[j]][i,])
    }}
  #
  # 8.) Insert missing data at random
  #
  if(missing > 0){
    miss <- expand.grid(n=1:n, q=1:q)
    missing.selected <- as.matrix(miss)[sample(1:nrow(miss), (n*q*missing), replace=FALSE),]
    for (i in 1:nrow(missing.selected)){
      simulated.responses[missing.selected[i,1], missing.selected[i,2]] <- NA
      perfect.responses[missing.selected[i,1], missing.selected[i,2]] <- NA
    }
  }
  #
  # 9.) Organize output
  #
  correctvotes <- sum(diag(table(simulated.responses,perfect.responses)))
  totalvotes <- sum(table(simulated.responses,perfect.responses))
  #
  return(list(simulated.responses = simulated.responses,
              perfect.responses = perfect.responses,
              idealpoints = idealpoints,
              normalvectors = normalvectors,
              heteroskedastic.respondents = heteroskedastic.respondents,
              heteroskedastic.issues = heteroskedastic.issues,
              correlations = correlations,
              knowledge = knowledge,
              error = (1 - (correctvotes / totalvotes))
  ))
  #
}
#
# %%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%     END    %%%%%%%
# %%%%% Monte Carlo %%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@  RUN ORDERED  @@@@@@@@@@@@@
# @@@@@@@@  OPTIMAL CLASSIFICATION  @@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#
# utility.probs = c(linear, normal, quadratic)
# error.respondents = normal(mu, sigma)
# error.issues = gamma(shape, rate)
#
# Simulations randomize:
#
#		1.) Respondent utility functions
#		2.) Missingness
#		3.) Error rate
#
#
ntrials <- 400
#
# !!!!!!!!!!!!!!!!!!!!!!!!!
# Basic setup:
#sim <- montecarlo.oc(n=1500, q=40, ndim=2, utility.probs=runif(3,0,1), missing=0.1, error.respondents=sort(runif(2,0,0.5)), error.issues=c(runif(1,0,3), 0.5))
#res <- ooc(sim$perfect.responses, dims=2, min=10, lop=0.0001, polarity=rep(1,2), iter=25, nv.method="svm.reg", cost=1)
# !!!!!!!!!!!!!!!!!!!!!!!!!
#
#
#
###########
###########
###########
###########
fit_20 <- foreach(i=1:ntrials, .combine='rbind', .packages=c("ooc", "statar", "Matrix")) %dopar% {
  set.seed(i+1981)
  sim <- montecarlo.oc(n=2400, q=40, ndim=2, utility.probs=runif(3,0,1), missing=0.1, error.respondents=sort(runif(2,0,0.5)), error.issues=c(runif(1,0,3), 0.5))
  anchor <- 20
  issuescales <- sim$simulated.responses
  data2_memb <- sample(1:2400, 1000, replace=FALSE)
  data2 <- issuescales[data2_memb, ]
  data1 <- issuescales[-data2_memb, ]
  del <- sample(1:nrow(data1), anchor, replace=FALSE)
  bot <- data1[del,]
  data1_org <- data1[-del,]
  data1 <- rbind(data1_org,bot)
  data2_new <- rbind(data2,bot)
  is_new <- rbind(data1, data2)
  ip_2 <- sim$idealpoints[data2_memb,]
  ip_1 <- sim$idealpoints[-data2_memb,]
  ip_bot <- ip_1[del,]
  ip_org <- ip_1[-del,]
  ip_1 <- rbind(ip_org,ip_bot)
  ip <- rbind(ip_1,ip_2)
  ## ooc
  res <- ooc(is_new, dims=2, min=10, lop=0.0001, polarity=rep(1,2), iter=25, nv.method="svm.reg", cost=1)
  res_1 <- ooc(data1, dims=2, min=10, lop=0.0001, polarity=rep(1,2), iter=25, nv.method="svm.reg", cost=1)
  res_2 <- ooc(data2_new, dims=2, min=10, lop=0.0001, polarity=rep(1,2), iter=25, nv.method="svm.reg", cost=1)
  ## get positions
  x <- res$respondents[,grepl("coord", colnames(res$respondents))]
  x_1 <- res_1$respondents[,grepl("coord", colnames(res_1$respondents))]
  x_2 <- res_2$respondents[,grepl("coord", colnames(res_2$respondents))]
  ########################
  ## Generating Anchors ##
  ########################
  anchors_1 <- x_1[nrow(x_1)-((anchor-1):0),]
  anchors_2 <- x_2[nrow(x_2)-((anchor-1):0),]
  ##########################################
  ## Estimating Procrustes Transformation ##
  ##########################################
  p <- procrustes(anchors_1, anchors_2, translation=TRUE)
  ###############################
  ## P-Transforming All Points ##
  ###############################
  x_1_trans <- p$s*(x_1%*%p$R) + (matrix(rep(1,nrow(x_1)), ncol=1) %*% t(p$tt))
  ##########################################
  ## Estimating Regression Transformation ##
  ##########################################
  reg_x <- lm(anchors_2[,1] ~ anchors_1[,1])
  reg_y <- lm(anchors_2[,2] ~ anchors_1[,2])
  x_int <- reg_x$coef[1]; x_slo <- reg_x$coef[2]
  y_int <- reg_y$coef[1]; y_slo <- reg_y$coef[2]
  ###############################
  ## R-Transforming All Points ##
  ###############################
  x_1_r_x <- x_int + x_slo*x_1[,1]
  x_1_r_y <- y_int + y_slo*x_1[,2]
  x_1_r <- cbind(x_1_r_x, x_1_r_y)
  x_2 <- x_2[1:(nrow(x_2)-anchor),]
  ###############################
  ## Get New X's               ##
  ###############################
  x_p <- rbind(x_1_trans, x_2)
  x_r <- rbind(x_1_r, x_2)
  x_n <- rbind(x_1, x_2)
  #################
  ## Correlation ##
  #################
  corx <- cor(as.vector(dist(x)), as.vector(dist(ip)))
  corx_p <- cor(as.vector(dist(x_p)), as.vector(dist(ip)))
  corx_r <- cor(as.vector(dist(x_r)), as.vector(dist(ip)))
  corx_n <- cor(as.vector(dist(x_n)), as.vector(dist(ip)))
  cor_x_p <- cor(as.vector(dist(x)), as.vector(dist(x_p)))
  cor_x_r <- cor(as.vector(dist(x)), as.vector(dist(x_r)))
  cor_x_n <- cor(as.vector(dist(x)), as.vector(dist(x_n)))
  
  error <- sim$error
  c(corx, corx_p, corx_r, corx_n, cor_x_p, cor_x_r, cor_x_n, error)
}
save(fit_20, file="./fit_20.rda")

