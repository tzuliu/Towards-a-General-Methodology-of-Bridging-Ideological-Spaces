## Export Plots

library(ggplot2)
library(ggrepel)

## Load Data
load(paste0(projdir,"/Outputs/application/utas12_ip.rda"))

###########
## Joint ##
###########

ip_j$cv2 <- ifelse(ip_j$cv=="Candidate","Candidate\n","Voter\n")
pj <- ggplot(data=ip_j, 
             aes(x=coord1D, y=coord2D, color=party, shape=party)) + 
  geom_point(size=2, alpha=0.6) +
  # xlab("1st dimension") + ylab("2nd dimension") + 
  xlab(NULL) + ylab(NULL) + 
  ggtitle("Joint") +
  scale_color_brewer(name="", type="qual", palette=2) + 
  # scale_colour_manual(name="", values = c("#507AA6", "#DF585C"), 
  #                     labels=c("LDP","DPJ")) +
  scale_shape_discrete(name="") + 
  coord_cartesian(xlim=c(-1,1),ylim=c(-1,1)) + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  scale_y_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  facet_wrap(~cv2, ncol=1) +
  guides(color=guide_legend(ncol=1,nrow=3,byrow=TRUE),
         shape=guide_legend(ncol=1,nrow=3,byrow=TRUE)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom")
# pj

############
## C to V ##
############

p1 <- ggplot(data=ip_dt1, 
             aes(x=coord1D, y=coord2D, color=party, shape=party)) + 
  geom_point(size=2, alpha=0.6) +
  # xlab("1st dimension") + ylab("2nd dimension") + 
  xlab(NULL) + ylab(NULL) + 
  ggtitle("Transform candidates") +
  scale_color_brewer(name="", type="qual", palette=2) + 
  # scale_colour_manual(name="", values = c("#507AA6", "#DF585C"), 
  #                     labels=c("LDP","DPJ")) +
  scale_shape_discrete(name="Party") + 
  coord_cartesian(xlim=c(-1,1),ylim=c(-1,1)) + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  scale_y_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  facet_wrap(~cv2, ncol=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")
# p1

require(grid)
g1 <- ggplotGrob(p1)
# get the grobs that must be removed
rm_grobs <- g1$layout$name %in% c("strip-t-2-2", "strip-t-2-3", "panel-1-3", "panel-2-3")
# remove grobs
g1$grobs[rm_grobs] <- NULL
g1$layout <- g1$layout[!rm_grobs, ]
## move axis closer to panel
g1$layout[g1$layout$name == "axis-b-2-3", c("t", "b")] = c(10, 10)
# grid.newpage()
# grid.draw(g1)

############
## V to C ##
############

p2 <- ggplot(data=ip_dt2, 
             aes(x=coord1D, y=coord2D, color=party, shape=party)) + 
  geom_point(size=2, alpha=0.6) +
  # xlab("1st dimension") + ylab("2nd dimension") + 
  ggtitle("Transform voters") +
  xlab(NULL) + ylab(NULL) + 
  scale_color_brewer(name="", type="qual", palette=2) + 
  # scale_colour_manual(name="", values = c("#507AA6", "#DF585C"), 
  #                     labels=c("LDP","DPJ")) +
  scale_shape_discrete(name="Party") + 
  coord_cartesian(xlim=c(-1,1),ylim=c(-1,1)) + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  scale_y_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5"), position="right") + 
  facet_wrap(~cv2, ncol=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")
# p2

require(grid)
g2 <- ggplotGrob(p2)
# get the grobs that must be removed
rm_grobs <- g2$layout$name %in% c("strip-t-1-3", "strip-t-1-2", "panel-2-1", "panel-1-2")
# remove grobs
g2$grobs[rm_grobs] <- NULL
g2$layout <- g2$layout[!rm_grobs, ]
## move axis closer to panel
g2$layout[g2$layout$name == "axis-b-1-3", c("t", "b")] = c(10, 10)
# grid.newpage()
# grid.draw(g2)

########################
## Combine Everything ##
########################

require(gridExtra)

blank <- grid.rect(gp=gpar(col="white"))
pall <- arrangeGrob(g2, 
                    arrangeGrob(blank,pj,blank,ncol=1,heights=c(0.15,2,0.2)), 
                    g1, ncol=3, widths=c(1,0.55,1),
                    bottom = "1st dimension", 
                    left = "2nd dimension")

#+ fig.height=10, fig.height=6.5
grid.draw(pall)

#+
## Save plot
ggsave(paste0(projdir, "/Outputs/application/utas12_figure.pdf"), pall, width=10, height=6.5)
