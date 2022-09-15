library(readr)
library(reshape2)
library(mixtools)
library(jsonlite)

## vzdelani ####
load("ciselniky/ciselnikKraje.RData")
vzdelani <- read_csv("data_csu/sldb2021_vzdelani.csv")
vzdelani_obce<-vzdelani[vzdelani$uzemi_kod>500000,]
vzdelani_obce$uzemi_txt2<-paste(vzdelani_obce$uzemi_txt,vzdelani_obce$uzemi_kod)
vzdelani_obce<-vzdelani_obce[,c("idhod","hodnota","uzemi_kod","vzdelani_kod","vzdelani_txt","uzemi_txt","uzemi_txt2")]
vzdelani_obce$vzdelani_txt[is.na(vzdelani_obce$vzdelani_txt)]<-"Celkem"
vzdelani_obce$vzdelani_kod[is.na(vzdelani_obce$vzdelani_kod)]<-"000"
vzdelani_obce$vzdelani_txt<-as.factor(vzdelani_obce$vzdelani_txt)
vzdelani_obce$vzdelani_kod<-as.factor(vzdelani_obce$vzdelani_kod)

jmena<-vzdelani_obce[,c("uzemi_kod","uzemi_txt")]
jmena2<-jmena[!duplicated(jmena),]
vzdelani3<-dcast(vzdelani_obce,uzemi_kod~vzdelani_txt,value.var="hodnota")
vzdelani3<-merge(vzdelani3,jmena2,by="uzemi_kod")
vzdelani3<-vzdelani3[,c(10,1,2,9,5,6,8,7,4,3)]

x<-levels(as.factor(ciselnik_okrsky$cis_obec))
y<-vzdelani3$uzemi_kod
setdiff(x,y)
setdiff(y,x)          
vzdelani4<-vzdelani3[y %in% x,]


#workdir <- "/Users/User/Desktop/MFF/sfg_volby/reseni_0922/"

# Nahrani dat
obce <- read.table("reseni2021/pscoco.csv", sep=";", header=T);
okrsky <- read.table("reseni2021/psp17okrs.csv", sep=",", header=T);


z<-levels(as.factor(obce$OBEC))
setdiff(z,x)

# EM multinom podle vzdelani

set.seed(1)
n.strat <- 7 # počet strat
matice<-as.matrix(vzdelani4[,c(3:9)])
em.out2 <- multmixEM(matice, k=n.strat)

obce2<-cbind(vzdelani4,em.out2$posterior)
obce3<-cbind(vzdelani4[,c(1,2)],em.out2$posterior)

pocet<-sum(okrsky[okrsky$CIS_OBEC=="999997","PLATNE_HLASY"])
zahranici<-rep(NA,ncol(obce3))
obce3_vzdelani<-rbind(obce3,zahranici)
obce3_vzdelani[nrow(obce3_vzdelani),1]<-"Zahraničí"
obce3_vzdelani[nrow(obce3_vzdelani),2:9]<-c(999997,em.out2$posterior[vzdelani4$uzemi_txt=="Praha 1",])
# Ulozit
save(obce3_vzdelani, file="ciselniky/ciselnik_vzdelani.RData")

## VEK ####
load("ciselniky/ciselnikKraje.RData")
vek <-read_csv("data_csu/sldb2021_vek10_pohlavi_new.csv",
               locale = locale(encoding = "ISO-8859-2"))
vek_obce<-vek[vek$uzemi_kod>500000,]
vek_obce$vek_txt[is.na(vek_obce$vek_txt)]<-"Celkem"
vek_obce$pohlavi_txt[is.na(vek_obce$pohlavi_txt)]<-"Celkem"
vek_obce$vek_kod[is.na(vek_obce$vek_kod)]<-"1100000000"
vek_obce$pohlavi_kod[is.na(vek_obce$pohlavi_kod)]<-"0"

vek_obce$pohlavi_txt[vek_obce$pohlavi_txt=="mu\u009e"]<-"muž"
vek_obce$pohlavi_txt[vek_obce$pohlavi_txt=="\u009eena"]<-"žena"

vek_obce$pohlavi_vek_txt<-paste(vek_obce$vek_txt,vek_obce$pohlavi_txt)

vek_obce<-vek_obce[,c("idhod","hodnota","uzemi_kod","pohlavi_kod","pohlavi_txt","vek_kod","vek_txt","pohlavi_vek_txt")]


vek3<-dcast(vek_obce,uzemi_kod~pohlavi_vek_txt,value.var="hodnota")
vek3<-merge(vek3,jmena2,by="uzemi_kod")
vek3<-vek3[,c(38,1:7,11:34,8:10,35:37)]
vek4<-vek3[y %in% x,]

sloupce <- colnames(vek4)[!grepl('Celkem', colnames(vek4))]
sloupce<-sloupce[3:24]

set.seed(1)
n.strat <- 7 # počet strat
matice_vek<-as.matrix(vek4[,sloupce])
em.out_vek <- multmixEM(matice_vek, k=n.strat)

obce3_vek<-cbind(vek4[,c(1,2)],em.out_vek$posterior)
#zahranici2<-c("Zahraničí",999997,pocet,em.out_vek$posterior[vek4$uzemi_txt=="Praha 1",])
zahranici<-rep(NA,ncol(obce3_vek))
obce3_vek<-rbind(obce3_vek,zahranici)
obce3_vek[nrow(obce3_vek),1]<-"Zahraničí"
obce3_vek[nrow(obce3_vek),2:9]<-c(999997,em.out2$posterior[vek4$uzemi_txt=="Praha 1",])

# Ulozit
save(obce3_vek, file="ciselniky/ciselnik_vek.RData")

