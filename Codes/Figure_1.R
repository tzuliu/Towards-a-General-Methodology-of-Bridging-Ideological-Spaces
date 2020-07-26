library(ggplot2)
library(ggrepel)

# Original Space
space <- data.frame(x = c(-20, 20, 20,-20),
                    y = c( 20, 20,-20,-20))
points <- data.frame(x = c(-15, 10, 15,-10),
                     y = c( 10, 15,-15,-10))
points$lab = paste0(c("A", "B", "C", "D"),
                    rep(" (", 4), points$x, rep(", ", 4), points$y, rep(")",4)) 

sp0 <-ggplot(points, aes(x=x,y=y)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_polygon(data=space, aes(x=x,y=y), alpha=0.4, color=NA) +
  geom_point() + 
  geom_text_repel(aes(label=lab)) + 
  coord_cartesian(xlim=c(-30,30), ylim=c(-30,30)) + 
  ggtitle("Original space") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=16),
        axis.title = element_blank(),
        axis.text = element_text(size=12))
# sp0

# Rotated Space 

rotangle = 30 # Rotate by ... degrees

space1 <- data.frame(x = space$x * cos(-rotangle * (pi/180)) - space$y * sin(-rotangle * (pi/180)),
                     y = space$x * sin(-rotangle * (pi/180)) + space$y * cos(-rotangle * (pi/180)))
points1 <- data.frame(x = points$x * cos(-rotangle * (pi/180)) - points$y * sin(-rotangle * (pi/180)),
                     y = points$x * sin(-rotangle * (pi/180)) + points$y * cos(-rotangle * (pi/180)))
points1$lab = paste0(c("A", "B", "C", "D"),
                    rep(" (", 4), round(points1$x,1), rep(", ", 4), round(points1$y,1), rep(")",4)) 

sp1 <-ggplot(points1, aes(x=x,y=y)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_polygon(data=space1, aes(x=x,y=y), alpha=0.4, color=NA) +
  geom_point() + 
  geom_text_repel(aes(label=lab)) + 
  coord_cartesian(xlim=c(-30,30), ylim=c(-30,30)) + 
  ggtitle("Rotation") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size=12))
# sp1

# Translated Space 

transcoord = c(10,-10) # Translation by (?,?)

space2 <- data.frame(x = space$x + transcoord[1],
                     y = space$y + transcoord[2])
points2 <- data.frame(x = points$x + transcoord[1],
                      y = points$y + transcoord[2])
points2$lab = paste0(c("A", "B", "C", "D"),
                     rep(" (", 4), round(points2$x,1), rep(", ", 4), round(points2$y,1), rep(")",4)) 

sp2 <-ggplot(points2, aes(x=x,y=y)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_polygon(data=space2, aes(x=x,y=y), alpha=0.4, color=NA) +
  geom_point() + 
  geom_text_repel(aes(label=lab)) + 
  coord_cartesian(xlim=c(-30,30), ylim=c(-30,30)) + 
  ggtitle("Translation") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size=12))
# sp2

# Scaled Space 

scaleset = 1.5 # Scaling by ...

space3 <- data.frame(x = space$x * scaleset,
                     y = space$y * scaleset)
points3 <- data.frame(x = points$x * scaleset,
                      y = points$y * scaleset)
points3$lab = paste0(c("A", "B", "C", "D"),
                     rep(" (", 4), round(points3$x,1), rep(", ", 4), round(points3$y,1), rep(")",4)) 

sp3 <-ggplot(points3, aes(x=x,y=y)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_polygon(data=space3, aes(x=x,y=y), alpha=0.4, color=NA) +
  geom_point() + 
  geom_text_repel(aes(label=lab)) + 
  coord_cartesian(xlim=c(-30,30), ylim=c(-30,30)) + 
  ggtitle("Scaling") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size=12))
# sp3

# Shearing

shearangle = 65 # Translation by (?,?)

space4 <- data.frame(x = space$x + (1/tan(shearangle* (pi/180))) * space$y,
                     y = space$y)
points4 <- data.frame(x = points$x + (1/tan(shearangle* (pi/180))) * points$y,
                      y = points$y)
points4$lab = paste0(c("A", "B", "C", "D"),
                     rep(" (", 4), round(points4$x,1), rep(", ", 4), round(points4$y,1), rep(")",4)) 

sp4 <-ggplot(points4, aes(x=x,y=y)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_polygon(data=space4, aes(x=x,y=y), alpha=0.4, color=NA) +
  geom_point() + 
  geom_text_repel(aes(label=lab)) + 
  coord_cartesian(xlim=c(-30,30), ylim=c(-30,30)) + 
  ggtitle("Shearing") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size=12))
# sp4

# Reflection

# Reflection by Y axis

space5 <- data.frame(x = -space$x,
                     y = space$y)
points5 <- data.frame(x = -points$x,
                      y = points$y)
points5$lab = paste0(c("A", "B", "C", "D"),
                     rep(" (", 4), round(points5$x,1), rep(", ", 4), round(points5$y,1), rep(")",4)) 

sp5 <-ggplot(points5, aes(x=x,y=y)) + 
  geom_vline(aes(xintercept=0), linetype=2) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_polygon(data=space5, aes(x=x,y=y), alpha=0.4, color=NA) +
  geom_point() + 
  geom_text_repel(aes(label=lab)) + 
  coord_cartesian(xlim=c(-30,30), ylim=c(-30,30)) + 
  ggtitle("Reflection") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size=12))
# sp5

## Annotation Plot

ant <- data.frame(y=c(0.45,0.3,0.2,0.1,0),
           x=c(-0.1,0,0,0,0),
           lab = c("Variations in the transformation of 2D ideological space",
                   "Rotation: rotate points by a specified degree.",
                   "Translation: move points by the same distance in a given direction.",
                   "Scaling: enlarge or shrink space by a specified scale factor.",
                   "Shearing: displace points by an amount proportional to its signed distance\n from the line that is parallel to that direction and goes through the origin."))
                   # paste0("Rotation: Rotate space clockwise by ", rotangle," degrees."),
                   # paste0("Translation: Shift all coordinates by (", transcoord[1], ", ", transcoord[2], ")."),
                   # paste0("Scaling: Expand space by ", scaleset*100, "%."),
                   # paste0("Shearing: Shear space by shear angle of ", shearangle, " degrees.")))
spant <- ggplot(ant, aes(x=x,y=y,label=lab)) + 
  geom_text(size=5, hjust=0, lineheight = .8) + 
  coord_cartesian(xlim=c(0,5), ylim=c(-0.15,0.55)) + 
  theme_void()
# spant

## Arrange Plots

library(gridExtra)
library(grid)

# p <- arrangeGrob(arrangeGrob(sp0, spant, ncol=2, widths=c(1,3)), 
#              arrangeGrob(sp1,sp2,sp3,sp4, ncol=4), nrow=2)
# grid.draw(p)

p <- arrangeGrob(arrangeGrob(sp1, sp0, sp5, ncol=3, widths=c(1,1,1)), 
                 arrangeGrob(sp2,sp3,sp4, ncol=3), nrow=2)

#+ fig.width=9.5, fig.height=6.5
grid.draw(p)

#+
ggsave(paste0(projdir, "/Outputs/illustration/transform_illustration.png"), p, width=9.5, height=6.5)

# Save individual figures
ggsave(paste0(projdir, "/Outputs/illustration/sp0.png"), sp0, width=3, height=3.2)
ggsave(paste0(projdir, "/Outputs/illustration/sp1.png"), sp1, width=3, height=3.2)
ggsave(paste0(projdir, "/Outputs/illustration/sp2.png"), sp2, width=3, height=3.2)
ggsave(paste0(projdir, "/Outputs/illustration/sp3.png"), sp3, width=3, height=3.2)
ggsave(paste0(projdir, "/Outputs/illustration/sp4.png"), sp4, width=3, height=3.2)
ggsave(paste0(projdir, "/Outputs/illustration/sp5.png"), sp5, width=3, height=3.2)
