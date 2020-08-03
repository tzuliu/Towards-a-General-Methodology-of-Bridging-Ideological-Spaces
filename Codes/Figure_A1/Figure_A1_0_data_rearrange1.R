## Rearranging UTAS 2009 (1)
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
require(haven)

# Variable List Data
lsv <- read.csv(paste0(projdir,"/Data/utas_data/utas_variable_list_utf-8.csv"),
                stringsAsFactors = FALSE, fileEncoding = "UTF-8")[-c(1,2),]

# 2009 Data
# candidates
c09 <- read.csv(paste0(projdir,"/Data/utas_data/2009UTASP20150910.csv"),
                stringsAsFactors = FALSE, fileEncoding="CP932")
# voters
v09 <- read_sav(paste0(projdir,"/Data/utas_data/2009_2010utas130816.sav"), 
                encoding="UTF-8")

#'
#' # Prepare 2009 Data
#'

## ID Data
id09 <- data.frame(id = c(c09$ID, v09$ID), 
                   cv = c(rep("candidate",nrow(c09)),
                          rep("voter", nrow(v09))))
id09$psup <- NA
id09$psup_short <- NA

psuplab <- c("Liberal Democratic Party",
             "Democratic Party of Japan",
             "Komei-to (Clean Government Party)",
             "Japanese Communist Party",
             "Social Democratic Party",
             "People's New Party",
             "Your Party","Other Parties/NA","Abstained")

psuplab_short <- c("LDP",
                   "DPJ",
                   "CGP (Komei)",
                   "JCP",
                   "SDP",
                   "PNP",
                   "YP","Other/NA","Abstained")


id09$psup[id09$cv=="candidate"] <- 
  ifelse(c09$PARTY%in%1, psuplab[1],
         ifelse(c09$PARTY%in%2, psuplab[2],
                ifelse(c09$PARTY%in%3, psuplab[3],
                       ifelse(c09$PARTY%in%4, psuplab[4],
                              ifelse(c09$PARTY%in%5, psuplab[5],
                                     ifelse(c09$PARTY%in%6, psuplab[6],
                                            ifelse(c09$PARTY%in%7, psuplab[7],
                                                   psuplab[8])))))))
id09$psup_short[id09$cv=="candidate"] <- 
  ifelse(c09$PARTY%in%1, psuplab_short[1],
         ifelse(c09$PARTY%in%2, psuplab_short[2],
                ifelse(c09$PARTY%in%3, psuplab_short[3],
                       ifelse(c09$PARTY%in%4, psuplab_short[4],
                              ifelse(c09$PARTY%in%5, psuplab_short[5],
                                     ifelse(c09$PARTY%in%6, psuplab_short[6],
                                            ifelse(c09$PARTY%in%7, psuplab_short[7],
                                                   psuplab_short[8])))))))

# For Party Membership for Politicians (PARTY)
# 326  1.  自民党 
# 330  2.  民主党 
# 51  3.  公明党 
# 171  4.  共産党 
# 37  5.  社民党 
# 18  6.  国民新党 
# 15  7.  みんなの党 
# 8  8.  新党日本 
# 4  9.  新党大地 
# 1  10.  改革クラブ 
# 343  11.  諸派 
# 70  12.  無所属 

## Abstained as 0
v09$Q010300[which(!v09$Q010100%in%4)] <- 0

id09$psup[id09$cv=="voter"] <- 
  ifelse(v09$Q010300%in%1, psuplab[1],
         ifelse(v09$Q010300%in%2, psuplab[2],
                ifelse(v09$Q010300%in%3, psuplab[3],
                       ifelse(v09$Q010300%in%4, psuplab[4],
                              ifelse(v09$Q010300%in%5, psuplab[5],
                                     ifelse(v09$Q010300%in%6, psuplab[6],
                                            ifelse(v09$Q010300%in%7, psuplab[7],
                                                   ifelse(v09$Q010300%in%c(0,90),psuplab[9],
                                                          psuplab[8]))))))))
id09$psup_short[id09$cv=="voter"] <- 
  ifelse(v09$Q010300%in%1, psuplab_short[1],
         ifelse(v09$Q010300%in%2, psuplab_short[2],
                ifelse(v09$Q010300%in%3, psuplab_short[3],
                       ifelse(v09$Q010300%in%4, psuplab_short[4],
                              ifelse(v09$Q010300%in%5, psuplab_short[5],
                                     ifelse(v09$Q010300%in%6, psuplab_short[6],
                                            ifelse(v09$Q010300%in%7, psuplab_short[7],
                                                   ifelse(v09$Q010300%in%c(0,90),psuplab_short[9],
                                                          psuplab_short[8]))))))))

# For PR Vote for Voters (Q010300)
# 295	0. 非該当（無投票）
# 507	1. 自民党
# 822	2. 民主党
# 149	3. 公明党
# 100	4. 共産党
# 57	5. 社民党
# 30	6. 国民新党
# 62	7. みんなの党
# 16	8. その他の政党
# 16	90. 白票・無効票など（投票所で棄権した）
# 31	999. 無回答

# Make Party Support Variables Factor
id09$psup <- factor(id09$psup, levels=psuplab)
table(id09$psup, useNA="always")
id09$psup_short <- factor(id09$psup_short, levels=psuplab_short)
table(id09$psup_short, useNA="always")

# Long-Term Party Leaning (not voted party) Variable for Voters
id09$pltsup <- NA
id09$pltsup_short <- NA

pltsuplab <- ifelse(psuplab=="Abstained","Independent (Muto-Ha)",
                    psuplab)
pltsuplab_short <- ifelse(psuplab_short=="Abstained","Independent",
                          psuplab_short)

id09$pltsup[id09$cv=="voter"] <- 
  ifelse(v09$Q014100%in%1, pltsuplab[1],
         ifelse(v09$Q014100%in%2, pltsuplab[2],
                ifelse(v09$Q014100%in%3, pltsuplab[3],
                       ifelse(v09$Q014100%in%4, pltsuplab[4],
                              ifelse(v09$Q014100%in%5, pltsuplab[5],
                                     ifelse(v09$Q014100%in%6, pltsuplab[6],
                                            ifelse(v09$Q014100%in%7, pltsuplab[7],
                                                   ifelse(v09$Q014100%in%9,pltsuplab[9],
                                                          pltsuplab[8]))))))))
id09$pltsup_short[id09$cv=="voter"] <- 
  ifelse(v09$Q014100%in%1, pltsuplab_short[1],
         ifelse(v09$Q014100%in%2, pltsuplab_short[2],
                ifelse(v09$Q014100%in%3, pltsuplab_short[3],
                       ifelse(v09$Q014100%in%4, pltsuplab_short[4],
                              ifelse(v09$Q014100%in%5, pltsuplab_short[5],
                                     ifelse(v09$Q014100%in%6, pltsuplab_short[6],
                                            ifelse(v09$Q014100%in%7, pltsuplab_short[7],
                                                   ifelse(v09$Q014100%in%9,pltsuplab_short[9],
                                                          pltsuplab_short[8]))))))))

# Make Long-Term Party Leaning Variables Factor
id09$pltsup <- factor(id09$pltsup, levels=pltsuplab)
table(id09$pltsup, useNA="always")
id09$pltsup_short <- factor(id09$pltsup_short, levels=pltsuplab_short)
table(id09$pltsup_short, useNA="always")

## Party/Koen-kai Membership
id09$pmem <- NA
id09$pmem[which(id09$cv=="voter")] <- v09$Q015001
table(id09$pmem, useNA="always")

## Demographic variables

# Gender (Female=1, Male=0)
id09$female <- NA
id09$female[which(id09$cv=="candidate")] <- ifelse(c09$SEX%in%2,1, 
                                                   ifelse(c09$SEX%in%1, 0, NA))
id09$female[which(id09$cv=="voter")] <- ifelse(!v09$Q014600%in%c(1,2),NA,
                                               ifelse(v09$Q014600==2,1,0))
table(id09$female, useNA="always")

# Age Cohort (Ordered Factor, No Raw Age Variable)
id09$agecat <- NA
id09$agecat[which(id09$cv=="voter")] <- 
  ifelse(v09$Q014700%in%1, "20s",
         ifelse(v09$Q014700%in%2, "30s",
                ifelse(v09$Q014700%in%3, "40s",
                       ifelse(v09$Q014700%in%4, "50s",
                              ifelse(v09$Q014700%in%5, "60s",
                                     ifelse(v09$Q014700%in%6, "70s/over",NA))))))
id09$agecat <- as.factor(id09$agecat)
table(id09$agecat, useNA="always")                                               

# Education
# Raw Categories (Not necessarily in the order of level)
id09$edu <- NA
id09$edu[which(id09$cv=="voter")] <- 
  ifelse(v09$Q014800%in%1, "Elementary/JHS", 
         ifelse(v09$Q014800%in%2, "Senior High School",
                ifelse(v09$Q014800%in%3, "Vocational School",
                       ifelse(v09$Q014800%in%4, "Junior College",
                              ifelse(v09$Q014800%in%5, "University",
                                     ifelse(v09$Q014800%in%6, "Graduate School",
                                            ifelse(v09$Q014800%in%7, "Others", NA)))))))
id09$edu <- factor(id09$edu, levels=c("Elementary/JHS","Senior High School",
                                      "Vocational School","Junior College",
                                      "University", "Graduate School", "Others"))
table(id09$edu, useNA="always")
# 4 Categories, Ordered (Others to NA)
id09$edu4 <- NA
id09$edu4 <- 
  ifelse(id09$edu%in%c("Elementary/JHS"), "<=JHS",
         ifelse(id09$edu%in%c("Senior High School"), "SHS",
                ifelse(id09$edu%in%c("Vocational School", "Junior College"), ">SHS & <University",
                       ifelse(id09$edu%in%c("University","Graduate School"), ">=University", NA))))
id09$edu4 <- factor(id09$edu4, levels=c("<=JHS","SHS",">SHS & <University",">=University"))
table(id09$edu4, useNA="always")

# Jobs (nominal categories)
id09$job <- NA
id09$job[which(id09$cv=="voter")] <- 
  ifelse(v09$Q015100%in%1, "Company Employee",
         ifelse(v09$Q015100%in%2, "Public Servant",
                ifelse(v09$Q015100%in%3, "Self-Employed",
                       ifelse(v09$Q015100%in%4, "Agriculture/Fishery",
                              ifelse(v09$Q015100%in%5, "Part-Timer",
                                     ifelse(v09$Q015100%in%6, "Homemaker",
                                            ifelse(v09$Q015100%in%7, "Student",
                                                   ifelse(v09$Q015100%in%8, "Unemployed",
                                                          ifelse(v09$Q015100%in%9, "Others", NA)))))))))
id09$job <- factor(id09$job,
                   levels=c("Company Employee",
                            "Public Servant",
                            "Self-Employed",
                            "Agriculture/Fishery",
                            "Part-Timer",
                            "Homemaker",
                            "Student",
                            "Unemployed",
                            "Others"))
table(id09$job)

# Residential Prefecture (See code book for prefecture names)
id09$pref <- NA
id09$pref[which(id09$cv=="candidate")] <- as.numeric(c09$PREFEC) # PR only candidates have missing values
id09$pref[which(id09$cv=="voter")] <- as.numeric(v09$PREF)

# House Electoral District (Numbering within prefecture)
id09$hdist <- NA
id09$hdist[which(id09$cv=="candidate")] <- as.numeric(c09$DISTRICT) # PR only candidates have missing values
id09$hdist[which(id09$cv=="voter")] <- as.numeric(v09$HORDIST)

## Policy Data

# Variables
lsv09 <- lsv[complete.cases(lsv[,c("cand09","voter09")]),]
nrow(lsv09) # 23 Variables

# Candidate
c09tr <- as.data.frame(c09[,lsv09$cand09])
c09tr <- sapply(1:nrow(lsv09), function(k) ifelse(c09tr[,k]%in%c(lsv09$min[k]:lsv09$max[k]),
                                                  c09tr[,k],NA))
colnames(c09tr) <- lsv09$qid

# Voter
v09tr <- as.data.frame(v09[,lsv09$voter09])
v09tr <- sapply(1:nrow(lsv09), function(k) ifelse(v09tr[,k]%in%c(lsv09$min[k]:lsv09$max[k]),
                                                  v09tr[,k],NA))
colnames(v09tr) <- lsv09$qid

## Combine Everyghing
d09 <- cbind(id09, rbind(c09tr,v09tr))
head(d09)

## Save Data
saveRDS(d09, paste0(projdir,"/Outputs/application/utas09_ooc.rds"))
