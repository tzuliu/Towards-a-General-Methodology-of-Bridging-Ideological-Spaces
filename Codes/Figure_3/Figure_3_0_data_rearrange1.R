## Recode UTAS 12 Data

## Packages
require(haven)

# Variable List Data
lsv <- read.csv(paste0(projdir,"/Data/utas_data/utas_variable_list_utf-8.csv"),
                stringsAsFactors = FALSE, fileEncoding = "UTF-8")[-c(1,2),]

# 2012 Data
# candidates
c12 <- read.csv(paste0(projdir,"/Data/utas_data/2012UTASP20150910.csv"),
                stringsAsFactors = FALSE, fileEncoding="CP932")
# voters
v12 <- read_sav(paste0(projdir,"/Data/utas_data/2012-2013UTASV131129.sav"),
                encoding="CP932")

#'
#' # Prepare 2012 Data
#'

## ID Data
id12 <- data.frame(id = c(c12$ID, v12$ID), 
                   cv = c(rep("candidate",nrow(c12)),
                          rep("voter", nrow(v12))))
id12$psup <- NA
id12$psup_short <- NA

psuplab <- c("Liberal Democratic Party",
             "Democratic Party of Japan",
             "Tomorrow Party of Japan",
             "Komei-to (Clean Government Party)",
             "Japan Restoration Party",
             "Japanese Communist Party",
             "Your Party",
             "Social Democratic Party",
             "Other Parties/NA","Abstained")

psuplab_short <- c("LDP",
                   "DPJ",
                   "TPJ",
                   "CGP (Komei)",
                   "JRP",
                   "JCP",
                   "YP",
                   "SDP",
                   "Other/NA","Abstained")


id12$psup[id12$cv=="candidate"] <- 
  ifelse(c12$PARTY%in%1, psuplab[2],
         ifelse(c12$PARTY%in%2, psuplab[1],
                ifelse(c12$PARTY%in%3, psuplab[3],
                       ifelse(c12$PARTY%in%4, psuplab[4],
                              ifelse(c12$PARTY%in%5, psuplab[5],
                                     ifelse(c12$PARTY%in%6, psuplab[6],
                                            ifelse(c12$PARTY%in%7, psuplab[7],
                                                   ifelse(c12$PARTY%in%8, psuplab[8],
                                                          psuplab[9]))))))))
id12$psup_short[id12$cv=="candidate"] <- 
  ifelse(c12$PARTY%in%1, psuplab_short[2],
         ifelse(c12$PARTY%in%2, psuplab_short[1],
                ifelse(c12$PARTY%in%3, psuplab_short[3],
                       ifelse(c12$PARTY%in%4, psuplab_short[4],
                              ifelse(c12$PARTY%in%5, psuplab_short[5],
                                     ifelse(c12$PARTY%in%6, psuplab_short[6],
                                            ifelse(c12$PARTY%in%7, psuplab_short[7],
                                                   ifelse(c12$PARTY%in%8, psuplab_short[8],
                                                          psuplab_short[9]))))))))

# For Party Membership for Politicians (PARTY)
# 1.  民主党 
# 2.  自民党 
# 3.  未来の党 
# 4.  公明党 
# 5.  日本維新の会 
# 6.  共産党 
# 7.  みんなの党 
# 8.  社民党 
# 9.  新党大地 
# 10.  国民新党 
# 11.  新党日本 
# 12.  新党改革 
# 13.  諸派 
# 14.  無所属 

## Abstained as 66
v12$Q010200[which(!v12$Q010100%in%2)] <- 66

id12$psup[id12$cv=="voter"] <- 
  ifelse(v12$Q010200%in%1, psuplab[2],
         ifelse(v12$Q010200%in%2, psuplab[1],
                ifelse(v12$Q010200%in%3, psuplab[3],
                       ifelse(v12$Q010200%in%4, psuplab[4],
                              ifelse(v12$Q010200%in%5, psuplab[5],
                                     ifelse(v12$Q010200%in%6, psuplab[6],
                                            ifelse(v12$Q010200%in%7, psuplab[7],
                                                   ifelse(v12$Q010200%in%8, psuplab[8],
                                                          ifelse(v12$Q010200%in%c(66,90),psuplab[10],
                                                                 psuplab[9])))))))))
id12$psup_short[id12$cv=="voter"] <- 
  ifelse(v12$Q010200%in%1, psuplab_short[2],
         ifelse(v12$Q010200%in%2, psuplab_short[1],
                ifelse(v12$Q010200%in%3, psuplab_short[3],
                       ifelse(v12$Q010200%in%4, psuplab_short[4],
                              ifelse(v12$Q010200%in%5, psuplab_short[5],
                                     ifelse(v12$Q010200%in%6, psuplab_short[6],
                                            ifelse(v12$Q010200%in%7, psuplab_short[7],
                                                   ifelse(v12$Q010200%in%8, psuplab_short[8],
                                                          ifelse(v12$Q010200%in%c(66,90),psuplab_short[10],
                                                                 psuplab_short[9])))))))))

# For PR Vote for Voters (Q010200)
# 238	1. 民主党
# 482	2. 自民党
# 79	3. 日本未来の党
# 163	4. 公明党
# 280	5. 日本維新の会
# 68	6. 共産党
# 119	7. みんなの党
# 30	8. 社民党
# 10	9. 新党大地
# 0	10. 国民新党
# 2	11. 新党改革
# 1	12. その他の政党
# 401	66. 非該当（無投票）
# 21	90. 白票・無効票など（投票所で棄権した）
# 6	99. 無回答

# Make Party Support Variables Factor
id12$psup <- factor(id12$psup, levels=psuplab)
table(id12$psup, useNA="always")
id12$psup_short <- factor(id12$psup_short, levels=psuplab_short)
table(id12$psup_short, useNA="always")

# Long-Term Party Leaning (not voted party) Variable for Voters
id12$pltsup <- NA
id12$pltsup_short <- NA

pltsuplab <- ifelse(psuplab=="Abstained","Independent (Muto-Ha)",
                    psuplab)
pltsuplab_short <- ifelse(psuplab_short=="Abstained","Independent",
                          psuplab_short)

id12$pltsup[id12$cv=="voter"] <- 
  ifelse(v12$Q013700%in%1, pltsuplab[2],
         ifelse(v12$Q013700%in%2, pltsuplab[1],
                ifelse(v12$Q013700%in%3, pltsuplab[3],
                       ifelse(v12$Q013700%in%4, pltsuplab[4],
                              ifelse(v12$Q013700%in%5, pltsuplab[5],
                                     ifelse(v12$Q013700%in%6, pltsuplab[6],
                                            ifelse(v12$Q013700%in%7, pltsuplab[7],
                                                   ifelse(v12$Q013700%in%8, pltsuplab[8],
                                                          ifelse(v12$Q013700%in%14,pltsuplab[10],
                                                                 pltsuplab[9])))))))))
id12$pltsup_short[id12$cv=="voter"] <- 
  ifelse(v12$Q013700%in%1, pltsuplab_short[2],
         ifelse(v12$Q013700%in%2, pltsuplab_short[1],
                ifelse(v12$Q013700%in%3, pltsuplab_short[3],
                       ifelse(v12$Q013700%in%4, pltsuplab_short[4],
                              ifelse(v12$Q013700%in%5, pltsuplab_short[5],
                                     ifelse(v12$Q013700%in%6, pltsuplab_short[6],
                                            ifelse(v12$Q013700%in%7, pltsuplab_short[7],
                                                   ifelse(v12$Q013700%in%8, pltsuplab_short[8],
                                                          ifelse(v12$Q013700%in%14,pltsuplab_short[10],
                                                                 pltsuplab_short[9])))))))))

# Make Long-Term Party Leaning Variables Factor
id12$pltsup <- factor(id12$pltsup, levels=pltsuplab)
table(id12$pltsup, useNA="always")
id12$pltsup_short <- factor(id12$pltsup_short, levels=pltsuplab_short)
table(id12$pltsup_short, useNA="always")

## Party/Koen-kai Membership
id12$pmem <- NA
id12$pmem[which(id12$cv=="voter")] <- ifelse(v12$Q014401%in%1,1,0)
table(id12$pmem, useNA="always")

## Demographic variables

# Gender (Female=1, Male=0)
id12$female <- NA
id12$female[which(id12$cv=="candidate")] <- ifelse(c12$SEX%in%2,1, 
                                                   ifelse(c12$SEX%in%1, 0, NA))
id12$female[which(id12$cv=="voter")] <- ifelse(!v12$Q014100%in%c(1,2),NA,
                                               ifelse(v12$Q014100==2,1,0))
table(id12$female, useNA="always")

# Age Cohort (Ordered Factor, No Raw Age Variable)
id12$agecat <- NA
id12$agecat[which(id12$cv=="voter")] <- 
  ifelse(v12$Q014200%in%1, "20s",
         ifelse(v12$Q014200%in%2, "30s",
                ifelse(v12$Q014200%in%3, "40s",
                       ifelse(v12$Q014200%in%4, "50s",
                              ifelse(v12$Q014200%in%5, "60s",
                                     ifelse(v12$Q014200%in%6, "70s/over",NA))))))
id12$agecat <- as.factor(id12$agecat)
table(id12$agecat, useNA="always")                                               

# Education
# Raw Categories (Not necessarily in the order of level)
id12$edu <- NA
id12$edu[which(id12$cv=="voter")] <- 
  ifelse(v12$Q014300%in%1, "Elementary/JHS", 
         ifelse(v12$Q014300%in%2, "Senior High School",
                ifelse(v12$Q014300%in%3, "Vocational School",
                       ifelse(v12$Q014300%in%4, "Junior College",
                              ifelse(v12$Q014300%in%5, "University",
                                     ifelse(v12$Q014300%in%6, "Graduate School",
                                            ifelse(v12$Q014300%in%7, "Others", NA)))))))
id12$edu <- factor(id12$edu, levels=c("Elementary/JHS","Senior High School",
                                      "Vocational School","Junior College",
                                      "University", "Graduate School", "Others"))
table(id12$edu, useNA="always")
# 4 Categories, Ordered (Others to NA)
id12$edu4 <- NA
id12$edu4 <- 
  ifelse(id12$edu%in%c("Elementary/JHS"), "<=JHS",
         ifelse(id12$edu%in%c("Senior High School"), "SHS",
                ifelse(id12$edu%in%c("Vocational School", "Junior College"), ">SHS & <University",
                       ifelse(id12$edu%in%c("University","Graduate School"), ">=University", NA))))
id12$edu4 <- factor(id12$edu4, levels=c("<=JHS","SHS",">SHS & <University",">=University"))
table(id12$edu4, useNA="always")

# Jobs (nominal categories)
id12$job <- NA
id12$job[which(id12$cv=="voter")] <- 
  ifelse(v12$Q014500%in%1, "Company Employee",
         ifelse(v12$Q014500%in%2, "Public Servant",
                ifelse(v12$Q014500%in%3, "Self-Employed",
                       ifelse(v12$Q014500%in%4, "Agriculture/Fishery",
                              ifelse(v12$Q014500%in%5, "Part-Timer",
                                     ifelse(v12$Q014500%in%6, "Homemaker",
                                            ifelse(v12$Q014500%in%7, "Student",
                                                   ifelse(v12$Q014500%in%8, "Unemployed",
                                                          ifelse(v12$Q014500%in%9, "Others", NA)))))))))
id12$job <- factor(id12$job,
                   levels=c("Company Employee",
                            "Public Servant",
                            "Self-Employed",
                            "Agriculture/Fishery",
                            "Part-Timer",
                            "Homemaker",
                            "Student",
                            "Unemployed",
                            "Others"))
table(id12$job)

# Residential Prefecture (See code book for prefecture names)
id12$pref <- NA
id12$pref[which(id12$cv=="candidate")] <- as.numeric(ifelse(c12$PREFEC%in%seq(1,47),c12$PREFEC,NA)) # PR only candidates have missing values
id12$pref[which(id12$cv=="voter")] <- as.numeric(v12$PREFEC)

# House Electoral District (Numbering within prefecture)
id12$hdist <- NA
id12$hdist[which(id12$cv=="candidate")] <- as.numeric(ifelse(c12$DISTRICT%in%seq(1,25),c12$DISTRICT,NA)) # PR only candidates have missing values
id12$hdist[which(id12$cv=="voter")] <- as.numeric(v12$HRDIST)

## Policy Data

# Variables
lsv12 <- lsv[complete.cases(lsv[,c("cand12","voter12")]),]
nrow(lsv12) # 23 Variables

# Candidate
c12tr <- as.data.frame(c12[,lsv12$cand12])
c12tr <- sapply(1:nrow(lsv12), function(k) ifelse(c12tr[,k]%in%c(lsv12$min[k]:lsv12$max[k]),
                                                  c12tr[,k],NA))
colnames(c12tr) <- lsv12$qid

# Voter
v12tr <- as.data.frame(v12[,lsv12$voter12])
v12tr <- sapply(1:nrow(lsv12), function(k) ifelse(v12tr[,k]%in%c(lsv12$min[k]:lsv12$max[k]),
                                                  v12tr[,k],NA))
colnames(v12tr) <- lsv12$qid

## Combine Everyghing

d12 <- cbind(id12, rbind(c12tr,v12tr))
head(d12)

## Save Data
saveRDS(d12, paste0(projdir,"/Outputs/application/utas12_ooc.rds"))