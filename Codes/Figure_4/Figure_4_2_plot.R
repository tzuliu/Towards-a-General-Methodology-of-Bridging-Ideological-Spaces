## Validating Synthetic Anchors (UTAS 2012) Export Plots
## (Ingroup=Candidates, Outgroup=Voters)
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

## Packages
library(ggplot2)
require(grid)

## Load Data
load(paste0(projdir, "/Outputs/simulation/utas12_sim_400_mean_dens_x.rda"))
load(paste0(projdir, "/Outputs/simulation/utas12_sim_400_mean_dens_y.rda"))

## Prepare Plotting Data
dx <- dmean_var_x[c(which(dmean_var_x$samp!="Original"),
                    rep(which(dmean_var_x$samp=="Original"),5)),]
dx$samp <- gsub(" outsample", "", as.character(dx$samp))
dx$out <- ifelse(dx$samp=="Original","Only candidates (N=1403)","With voters")
dx$out <- factor(dx$out, levels=c("With voters", "Only candidates (N=1403)"))
dx$samp[dx$out=="Only candidates (N=1403)"] <- dx$samp[dx$out=="With voters"]
dx$samp <- as.numeric(dx$samp)
dx$axis <- "X-axis\n(1st dimension)"

dy <- dmean_var_y[c(which(dmean_var_y$samp!="Original"),
                    rep(which(dmean_var_y$samp=="Original"),5)),]
dy$samp <- gsub(" outsample", "", as.character(dy$samp))
dy$out <- ifelse(dy$samp=="Original","Only candidates (N=1403)","With voters")
dy$out <- factor(dy$out, levels=c("With voters", "Only candidates (N=1403)"))
dy$samp[dy$out=="Only candidates (N=1403)"] <- dy$samp[dy$out=="With voters"]
dy$samp <- as.numeric(dy$samp)
dy$axis <- "Y-axis\n(2nd dimension)"

d <- rbind(dx,dy)
d$axis <- as.factor(d$axis)

## Draw Plot
p <- ggplot(d, aes(x=pos, group=out, color=out, linetype=out)) + 
  stat_density(geom="line", position="identity", size=0.5) + 
  facet_grid(axis~samp) + 
  labs(x="Ideological positions",
       subtitle="Extra samples (the number of voters)") +
  scale_color_discrete(name="Density") + 
  scale_linetype_discrete(name="Density") + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  theme_classic() + 
  theme(legend.position="bottom",
        plot.subtitle=element_text(hjust=0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

grid.draw(p)

## Save Plot
ggsave(paste0(projdir, "/Outputs/simulation/utas12_sim_400_outsample_density.pdf"), p, width=6, height=4)
ggsave(paste0(projdir, "/Outputs/simulation/utas12_sim_400_outsample_density.png"), p, width=6, height=4)