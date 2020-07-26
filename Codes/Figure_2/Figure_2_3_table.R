## Load Data
load(paste0(projdir,"/Outputs/application/senator_ip.rda"))

require(dplyr)
require(xtable)

##########################################
## Calculate Congruence Between S and V ##
##########################################
################
## Reoublican ##
################
ip_t1_pro_rep <- ip_t1_pro %>% dplyr::filter(party == "Republican")
ip_t1_pro_rep_s <- ip_t1_pro_rep %>% dplyr::filter(cv == "Senator")
ip_t1_pro_rep_v <- ip_t1_pro_rep %>% dplyr::filter(cv == "Voter")
cent1_rep_s_x <- mean(ip_t1_pro_rep_s[,1]);cent1_rep_s_y <- mean(ip_t1_pro_rep_s[,2])
cent1_rep_v_x <- mean(ip_t1_pro_rep_v[,1]);cent1_rep_v_y <- mean(ip_t1_pro_rep_v[,2])

##############
## Democrat ##
##############
ip_t1_pro_dem <- ip_t1_pro %>% dplyr::filter(party == "Democrat")
ip_t1_pro_dem_s <- ip_t1_pro_dem %>% dplyr::filter(cv == "Senator")
ip_t1_pro_dem_v <- ip_t1_pro_dem %>% dplyr::filter(cv == "Voter")
cent1_dem_s_x <- mean(ip_t1_pro_dem_s[,1]);cent1_dem_s_y <- mean(ip_t1_pro_dem_s[,2])
cent1_dem_v_x <- mean(ip_t1_pro_dem_v[,1]);cent1_dem_v_y <- mean(ip_t1_pro_dem_v[,2])

cent1_diff_s_x <- (cent1_rep_s_x - cent1_dem_s_x)^2
cent1_diff_s_y <- (cent1_rep_s_y - cent1_dem_s_y)^2
cent1_diff_v_x <- (cent1_rep_v_x - cent1_dem_v_x)^2
cent1_diff_v_y <- (cent1_rep_v_y - cent1_dem_v_y)^2
cent1_diff_s <- sqrt(cent1_diff_s_x + cent1_diff_s_y)
cent1_diff_v <- sqrt(cent1_diff_v_x + cent1_diff_v_y)
######
cent1_rep_s_x;cent1_rep_s_y
cent1_dem_s_x;cent1_dem_s_y
cent1_diff_s
cent1_rep_v_x;cent1_rep_v_y
cent1_dem_v_x;cent1_dem_v_y
cent1_diff_v

##########################################
## Calculate Congruence Between S and V ##
##########################################
################
## Republican ##
################
ip_t2_pro_rep <- ip_t2_pro %>% dplyr::filter(party == "Republican")
ip_t2_pro_rep_s <- ip_t2_pro_rep %>% dplyr::filter(cv == "Senator")
ip_t2_pro_rep_v <- ip_t2_pro_rep %>% dplyr::filter(cv == "Voter")
cent2_rep_s_x <- mean(ip_t2_pro_rep_s[,1]);cent2_rep_s_y <- mean(ip_t2_pro_rep_s[,2])
cent2_rep_v_x <- mean(ip_t2_pro_rep_v[,1]);cent2_rep_v_y <- mean(ip_t2_pro_rep_v[,2])

##############
## Democrat ##
##############
ip_t2_pro_dem <- ip_t2_pro %>% dplyr::filter(party == "Democrat")
ip_t2_pro_dem_s <- ip_t2_pro_dem %>% dplyr::filter(cv == "Senator")
ip_t2_pro_dem_v <- ip_t2_pro_dem %>% dplyr::filter(cv == "Voter")
cent2_dem_s_x <- mean(ip_t2_pro_dem_s[,1]);cent2_dem_s_y <- mean(ip_t2_pro_dem_s[,2])
cent2_dem_v_x <- mean(ip_t2_pro_dem_v[,1]);cent2_dem_v_y <- mean(ip_t2_pro_dem_v[,2])

cent2_diff_s_x <- (cent2_rep_s_x - cent2_dem_s_x)^2
cent2_diff_s_y <- (cent2_rep_s_y - cent2_dem_s_y)^2
cent2_diff_v_x <- (cent2_rep_v_x - cent2_dem_v_x)^2
cent2_diff_v_y <- (cent2_rep_v_y - cent2_dem_v_y)^2
cent2_diff_s <- sqrt(cent2_diff_s_x + cent2_diff_s_y)
cent2_diff_v <- sqrt(cent2_diff_v_x + cent2_diff_v_y)
######
cent2_rep_s_x;cent2_rep_s_y
cent2_dem_s_x;cent2_dem_s_y
cent2_diff_s
cent2_rep_v_x;cent2_rep_v_y
cent2_dem_v_x;cent2_dem_v_y
cent2_diff_v

####################
## Creating Table ##
####################

tab <- data.frame(Partisan = rep(c("\\texttt{Republican}",
                                   "\\texttt{Democrat}"), 4),
                  Group = rep(c("\\texttt{Senator}",
                                "\\texttt{Voter}"), each=2),
                  Combination = rep(c("\\texttt{Senator} to \\texttt{Voter}",
                                      "\\texttt{Voter} to \\texttt{Senator}"),
                                    each=4),
                  Coordinates = paste0("$(", 
                                       sprintf(c(cent1_rep_s_x, cent1_dem_s_x, 
                                         cent1_rep_v_x, cent1_dem_v_x,
                                         cent2_rep_s_x, cent2_dem_s_x, 
                                         cent2_rep_v_x, cent2_dem_v_x),
                                         fmt = '%#.2f'), 
                                       "," ,
                                       sprintf(c(cent1_rep_s_y, cent1_dem_s_y, 
                                         cent1_rep_v_y, cent1_dem_v_y,
                                         cent2_rep_s_y, cent2_dem_s_y, 
                                         cent2_rep_v_y, cent2_dem_v_y),
                                         fmt = '%#.2f'),
                                       ")$"),
                  Distance = sprintf(rep(c(cent1_diff_s, cent1_diff_v,
                                   cent2_diff_s, cent2_diff_v), each=2),
                                   fmt = '%#.2f'))

hlines <- c(-1, 0, 4, nrow(tab))

print(xtable(tab), booktabs = TRUE, 
      hline.after = hlines, digits = 3,
      sanitize.text.function = identity,
      file=paste0(projdir, "/Outputs/application/senator_table.tex"),
      floating = F)

