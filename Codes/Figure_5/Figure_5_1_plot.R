## Comparing Performances through Generated Data: Plot
## Author: Tzu-Ping Liu & Gento Kato
## Date: 07/27/2020
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

# Packages 
library(ggplot2)
library(ggrepel)
require(grid)
require(statar)
require(reshape2)

## Load Data
load(paste0(projdir, "/Outputs/simulation/mcmc_corr_fit_20.rda"))

fit <- fit_20
colnames(fit) <- c("Cor_Joint", "Cor_Procrustes", "Cor_Regression", "Cor_Concatenation", "Joint_Procrustes", "Joint_Regression", "Joint_Concatenation", "Error")
fit <- data.frame(fit)
fit$Error <- factor(xtile(fit$Error, 3), labels=c("Low error", "Medium error", "High error"))

fit2 <- data.frame(cor=c(fit$Cor_Joint,fit$Cor_Procrustes,
                         fit$Cor_Regression,fit$Cor_Concatenation,
                         fit$Joint_Procrustes,fit$Joint_Regression,
                         fit$Joint_Concatenation),
                   err=as.character(fit$Error),
                   x=factor(c(rep("Real", nrow(fit)*4),
                              rep("Joint", nrow(fit)*3)),
                            levels=c("Real","Joint")),
                   y=factor(rep(c("Joint",rep(c("Procrustes","Regression","Concatenated"),2)),
                                each=nrow(fit)),
                            levels=c("Joint","Procrustes","Regression","Concatenated")),
                   stringsAsFactors = FALSE)

fit2$err[fit2$err=="Low error"] <- "Low"
fit2$err[fit2$err=="Medium error"] <- "Med."
fit2$err[fit2$err=="High error"] <- "High"
fit2$err <- factor(fit2$err, levels=c("Low","Med.","High"))

## Draw Plot
p <- ggplot(fit2, aes(x=err,y=cor,fill=err)) + 
  geom_boxplot() + 
  facet_grid(x~y) + 
  labs(y="Correlation",x="Error level",
       subtitle="Estimated ideal points") + 
  theme_classic() + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust=0.5))

grid.draw(p) 

## Save Plot
ggsave(paste0(projdir, "/Outputs/simulation/mcmc_corr_plot_20.pdf"),p, width=6, height=4)
ggsave(paste0(projdir, "/Outputs/simulation/mcmc_corr_plot_20.png"),p, width=6, height=4)