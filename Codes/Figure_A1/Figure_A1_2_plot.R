## Export Plots for UTAS 2009 Ideal Point Analysis
## Author: Tzu-Ping Liu & Gento Kato
## Date: 07/25/2020
## Environment: R 4.0.2 on Ubuntu 20.04

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
library(ggrepel)
require(grid)
require(gridExtra)

## Load Data
load(paste0(projdir,"/Outputs/application/utas09_ip.rda"))

###############################
## Find Centroids & Distance ##
###############################

## Function to extract centroids
find_centroids = function(party, ip) {
  
  ip_sub <- ip[ip$party==party,]
  ip_sub_s <- ip_sub[ip_sub$cv=="Candidate",]
  ip_sub_v <- ip_sub[ip_sub$cv=="Voter",]
  ip_sub_s_x <- mean(ip_sub_s[,1])
  ip_sub_s_y <- mean(ip_sub_s[,2])
  ip_sub_v_x <- mean(ip_sub_v[,1])
  ip_sub_v_y <- mean(ip_sub_v[,2])
  
  return(data.frame(x=c(ip_sub_s_x, ip_sub_v_x),
                    y=c(ip_sub_s_y, ip_sub_v_y),
                    cv=c("Candidate","Voter"),
                    party=party))
  
} 

## Function to extract distances
find_distance = function(cent1) {
  
  cent1 <- as.data.frame(cent1)
  cent1_diff_c_x <- (cent1$x[1] - cent1$x[3])^2
  cent1_diff_c_y <- (cent1$y[1] - cent1$y[3])^2
  cent1_diff_v_x <- (cent1$x[2] - cent1$x[4])^2
  cent1_diff_v_y <- (cent1$y[2] - cent1$y[4])^2
  cent1_diff_c <- sqrt(cent1_diff_c_x + cent1_diff_c_y)
  cent1_diff_v <- sqrt(cent1_diff_v_x + cent1_diff_v_y)
  
  return(c(c=cent1_diff_c, v=cent1_diff_v))
  
}

## Joint 
cent_j = do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                 ip=ip_j))
dist_j = find_distance(cent_j)

## Candidate on Voter's Space
cent1o <- do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                  ip=ip_dt1))
dist1o = find_distance(cent1o)
cent1_pro <- do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                     ip=ip_dt1_pro))
dist1_pro = find_distance(cent1_pro)
cent1_reg <- do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                     ip=ip_dt1_reg))
dist1_reg = find_distance(cent1_reg)

## Voter on Candidate's Space
cent2o <- do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                  ip=ip_dt2))
dist2o = find_distance(cent2o)
cent2_pro <- do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                     ip=ip_dt2_pro))
dist2_pro = find_distance(cent2_pro)
cent2_reg <- do.call("rbind", lapply(c("LDP","DPJ"), find_centroids, 
                                     ip=ip_dt2_reg))
dist2_reg = find_distance(cent2_reg)

## Combine Centroids
cent1o$cv2 <- ifelse(cent1o$cv=="Candidate",
                     "Candidate\n(Untransformed)",
                     "Voter\n(Untransformed)")
cent1_pro$cv2 <- ifelse(cent1_pro$cv=="Candidate",
                        "Candidate\n(Procrustes)", # will not be used in figure 
                        "Voter\n(XProcrustes)")
cent1_reg$cv2 <- ifelse(cent1_reg$cv=="Candidate",
                        "Candidate\n(Regression)", # will not be used in figure 
                        "Voter\n(XRegression)")
cent1 <- rbind(cent1o,cent1_pro,cent1_reg)
cent1$cv2 <- factor(cent1$cv2, levels=unique(cent1$cv2)) 
cent2o$cv2 <- ifelse(cent2o$cv=="Candidate",
                     "Candidate\n(Untransformed)",
                     "Voter\n(Untransformed)")
cent2_pro$cv2 <- ifelse(cent2_pro$cv=="Candidate",
                        "Candidate\n(XProcrustes)", # will not be used in figure 
                        "Voter\n(Procrustes)")
cent2_reg$cv2 <- ifelse(cent2_reg$cv=="Candidate",
                        "Candidate\n(XRegression)", # will not be used in figure 
                        "Voter\n(Regression)")
cent2 <- rbind(cent2o,cent2_pro,cent2_reg)
cent2$cv2 <- factor(cent2$cv2, levels=unique(cent2$cv2)) 

## Combine Distances & Create Table

tabset <- data.frame(Group = c("\\texttt{Candidate}",rep("",6),
                               "\\texttt{Voter}", rep("",6)),
                     Type = rep(c("Joint",
                                  "Transform \\texttt{Voter}","","",
                                  "Transform \\texttt{Candidate}","",""),2),
                     Transformation = c("", rep(c("Untransformed",
                                                  "Procrustes","Regression"), 2)),
                     Distance = c(dist_j[1], 
                                  dist2o[1], dist2_pro[1], dist2_reg[1], 
                                  dist1o[1], dist1_pro[1], dist1_reg[1],
                                  dist_j[2], 
                                  dist2o[2], dist2_pro[2], dist2_reg[2], 
                                  dist1o[2], dist1_pro[2], dist1_reg[2])
)
tabset <- tabset[-c(3,4,13,14),]

hlines <- c(-1, 0, 5, nrow(tabset))

tabex <- print(xtable(tabset, align=c(rep("l",ncol(tabset)),"c"), digits = 3), 
               booktabs = TRUE, hline.after = hlines,
               sanitize.text.function = identity,
               file=paste0(projdir, "/Outputs/application/utas09_table.tex"),
               floating = F, include.rownames = F)
cat(tabex)

###########
## Joint ##
###########

ip_j$cv2 <- ifelse(ip_j$cv=="Candidate","Candidate\n","Voter\n")
cent_j$cv2 <- ifelse(cent_j$cv=="Candidate","Candidate\n","Voter\n")
pj <- ggplot(data=ip_j, 
             aes(x=coord1D, y=coord2D, color=party, shape=party)) + 
  geom_point(size=2, alpha=0.3) +
  geom_point(data=cent_j, aes(x=x,y=y), shape=4, color="black", size=3) + 
  # xlab("1st dimension") + ylab("2nd dimension") + 
  xlab(NULL) + ylab(NULL) + 
  ggtitle("Joint") +
  labs(caption = "X  Party Centroid") + 
  scale_color_brewer(name="", type="qual", palette=2) + 
  scale_shape_discrete(name="") + 
  coord_cartesian(xlim=c(-1,1),ylim=c(-1,1)) + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  scale_y_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  facet_wrap(~cv2, ncol=1) +
  guides(color=guide_legend(ncol=1,nrow=2,byrow=TRUE),
         shape=guide_legend(ncol=1,nrow=2,byrow=TRUE)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        plot.caption = element_text(hjust=0.5),
        strip.text = element_text(size=11))
pj

############
## C to V ##
############

ip_dt1$cv2 <- ifelse(ip_dt1$cv=="Candidate",
                     "Candidate\n(Untransformed)",
                     "Voter\n(Untransformed)")
ip_dt1_pro$cv2 <- ifelse(ip_dt1_pro$cv=="Candidate",
                        "Candidate\n(Procrustes)",
                        "Voter\n(XProcrustes)")
ip_dt1_reg$cv2 <- ifelse(ip_dt1_reg$cv=="Candidate",
                        "Candidate\n(Regression)",
                        "Voter\n(XRegression)")
ip_dt1 <- rbind(ip_dt1,ip_dt1_pro,ip_dt1_reg)
ip_dt1$cv2 <- factor(ip_dt1$cv2, levels=unique(ip_dt1$cv2)) 

p1 <- ggplot(data=ip_dt1, 
             aes(x=coord1D, y=coord2D, color=party, shape=party)) + 
  geom_point(size=2, alpha=0.3) +
  geom_point(data=cent1, aes(x=x,y=y), shape=4, color="black", size=3) + 
  # xlab("1st dimension") + ylab("2nd dimension") + 
  xlab(NULL) + ylab(NULL) + 
  ggtitle("Transform Candidate") +
  scale_color_brewer(name="", type="qual", palette=2) + 
  scale_shape_discrete(name="Party") + 
  coord_cartesian(xlim=c(-1,1),ylim=c(-1,1)) + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  scale_y_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  facet_wrap(~cv2, ncol=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        strip.text = element_text(size=11))
p1

g1 <- ggplotGrob(p1)
# get the grobs that must be removed
rm_grobs <- g1$layout$name %in% c("strip-t-2-2", "strip-t-2-3", "panel-1-3", "panel-2-3")
# remove grobs
g1$grobs[rm_grobs] <- NULL
g1$layout <- g1$layout[!rm_grobs, ]
## move axis closer to panel
g1$layout[g1$layout$name == "axis-b-2-3", c("t", "b")] = c(10, 10)
grid.newpage()
grid.draw(g1)

############
## V to C ##
############

ip_dt2$cv2 <- ifelse(ip_dt2$cv=="Candidate",
                     "Candidate\n(Untransformed)",
                     "Voter\n(Untransformed)")
ip_dt2_pro$cv2 <- ifelse(ip_dt2_pro$cv=="Candidate",
                        "Candidate\n(XProcrustes)",
                        "Voter\n(Procrustes)")
ip_dt2_reg$cv2 <- ifelse(ip_dt2_reg$cv=="Candidate",
                        "Candidate\n(XRegression)",
                        "Voter\n(Regression)")
ip_dt2 <- rbind(ip_dt2,ip_dt2_pro,ip_dt2_reg)
ip_dt2$cv2 <- factor(ip_dt2$cv2, levels=unique(ip_dt2$cv2)) 

p2 <- ggplot(data=ip_dt2, 
             aes(x=coord1D, y=coord2D, color=party, shape=party)) + 
  geom_point(size=2, alpha=0.3) +
  geom_point(data=cent2, aes(x=x,y=y), shape=4, color="black", size=3) + 
  # xlab("1st dimension") + ylab("2nd dimension") + 
  ggtitle("Transform Voter") +
  xlab(NULL) + ylab(NULL) + 
  scale_color_brewer(name="", type="qual", palette=2) + 
  scale_shape_discrete(name="Party") + 
  coord_cartesian(xlim=c(-1,1),ylim=c(-1,1)) + 
  scale_x_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5")) + 
  scale_y_continuous(breaks=c(-0.5,0,0.5),labels=c("-.5","0",".5"), position="right") + 
  facet_wrap(~cv2, ncol=2) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        strip.text = element_text(size=11))
p2

require(grid)
g2 <- ggplotGrob(p2)
# get the grobs that must be removed
rm_grobs <- g2$layout$name %in% c("strip-t-1-3", "strip-t-1-2", "panel-2-1", "panel-1-2")
# remove grobs
g2$grobs[rm_grobs] <- NULL
g2$layout <- g2$layout[!rm_grobs, ]
## move axis closer to panel
g2$layout[g2$layout$name == "axis-b-1-3", c("t", "b")] = c(10, 10)
grid.newpage()
grid.draw(g2)

########################
## Combine Everything ##
########################

blank <- grid.rect(gp=gpar(col="white"))
pall <- arrangeGrob(g2, 
                    arrangeGrob(blank,pj,blank,ncol=1,heights=c(0.11,2,0.2)), 
                    g1, ncol=3, widths=c(1,0.54,1),
                    bottom = "1st dimension", 
                    left = "2nd dimension")

grid.draw(pall)

## Save plot
ggsave(paste0(projdir,"/Outputs/application/utas09_figure.pdf"), 
       pall, width=8.6, height=6.5)
ggsave(paste0(projdir,"/Outputs/application/utas09_figure.png"), 
       pall, width=8.6, height=6.5)
