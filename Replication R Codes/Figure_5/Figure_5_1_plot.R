## Clear Workspace
rm(list = ls())

library(ggplot2)

## Load Data
load("./mean_dens_group_x.rda")
load("./mean_dens_group_y.rda")

## New Graph
dx <- dmean_var_x[c(which(dmean_var_x$samp!="Original"),
                    rep(which(dmean_var_x$samp=="Original"),5)),]
dx$samp <- gsub(" outsample", "", as.character(dx$samp))
dx$out <- ifelse(dx$samp=="Original","Only voters (N=1900)","With candidates")
dx$out <- factor(dx$out, levels=c("With candidates", "Only voters (N=1900)"))
dx$samp[dx$out=="Only voters (N=1900)"] <- dx$samp[dx$out=="With candidates"]
dx$samp <- as.numeric(dx$samp)
dx$axis <- "X-axis\n(1st dimension)"

dy <- dmean_var_y[c(which(dmean_var_y$samp!="Original"),
                    rep(which(dmean_var_y$samp=="Original"),5)),]
dy$samp <- gsub(" outsample", "", as.character(dy$samp))
dy$out <- ifelse(dy$samp=="Original","Only voters (N=1900)","With candidates")
dy$out <- factor(dy$out, levels=c("With candidates", "Only voters (N=1900)"))
dy$samp[dy$out=="Only voters (N=1900)"] <- dy$samp[dy$out=="With candidates"]
dy$samp <- as.numeric(dy$samp)
dy$axis <- "Y-axis\n(2nd dimension)"

d <- rbind(dx,dy)
d$axis <- as.factor(d$axis)

p <- ggplot(d, aes(x=pos, group=out, color=out, linetype=out)) + 
  stat_density(geom="line", position="identity", size=0.5) + 
  facet_grid(axis~samp) + 
  labs(x="Ideological positions",
       subtitle="Extra samples (the number of candidates)") +
  scale_color_discrete(name="Density") + 
  scale_linetype_discrete(name="Density") + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  theme_classic() + 
  theme(legend.position="bottom",
        plot.subtitle=element_text(hjust=0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

#ggsave("outsample_density_new.pdf", p, width=6, height=4)