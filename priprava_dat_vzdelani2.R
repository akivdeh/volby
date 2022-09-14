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
hist(log(vzdelani3$Celkem,10))



x<-levels(as.factor(ciselnik_okrsky$cis_obec))
y<-vzdelani3$uzemi_kod
setdiff(x,y)
setdiff(y,x)          
vzdelani4<-vzdelani3[y %in% x,]


## workdir <- "/Users/User/Desktop/MFF/sfg_volby/reseni_0922/"

# Nahrani dat
obce <- read.table("reseni2021/pscoco.csv", sep=";", header=T);
okrsky <- read.table("reseni2021/psp17okrs.csv", sep=",", header=T);


z<-levels(as.factor(obce$OBEC))
setdiff(z,x)

# EM multinom podle vzdelani

set.seed(1)
n.strat <- 3 # počet strat
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
# save(obce3_vzdelani, file="ciselniky/ciselnik_vzdelani.RData")

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

vek4_male<-vek4[vek4$'Celkem Celkem'<200,]
vek4_stredni<-vek4[vek4$'Celkem Celkem'>=200 & vek4$'Celkem Celkem'<1000,]
vek4_velke<-vek4[vek4$'Celkem Celkem'>=1000,]


sloupce <- colnames(vek4)[!grepl('Celkem', colnames(vek4))]
sloupce2 <- colnames(vek4)[grepl('Celkem', colnames(vek4))]
sloupce<-sloupce[3:24]
# vek5<-vek4[,sloupce2]
# vek5<-vek5[,1:11]/vek5[,12]
# 

matice_vek_male<-as.matrix(vek4_male[,sloupce])
matice_vek_stredni<-as.matrix(vek4_stredni[,sloupce])
matice_vek_velke<-as.matrix(vek4_velke[,sloupce])


# set.seed(1)
# km.res <- kmeans(vek5, 2, nstart = 25)
# matice_vek_norm<-matice_vek/vek4[,37]

set.seed(1)
n.strat <- 3 # počet strat
em.out_vek_male <- multmixEM(matice_vek_male, k=n.strat)
em.out_vek_stredni <- multmixEM(matice_vek_stredni, k=n.strat)
em.out_vek_velke <- multmixEM(matice_vek_velke, k=n.strat)


vek5_male<-cbind(vek4_male[,1:2],em.out_vek_male$posterior)
vek5_stredni<-cbind(vek4_stredni[,1:2],em.out_vek_stredni$posterior)
vek5_velke<-cbind(vek4_velke[,1:2],em.out_vek_velke$posterior)


vek6<-merge(vek4[,1:2],vek5_male,by=c("uzemi_txt","uzemi_kod"),all.x = T)
vek6<-merge(vek6,vek5_stredni,by=c("uzemi_txt","uzemi_kod"),all.x = T)
vek6<-merge(vek6,vek5_velke,by=c("uzemi_txt","uzemi_kod"),all.x = T)
vek6[is.na(vek6)] <- 0

names(vek6)<-c("uzemi_txt","uzemi_kod","comp.1","comp.2","comp.3","comp.4",
               "comp.5","comp.6","comp.7","comp.8","comp.9")

#obce3_vek<-cbind(vek4[,c(1,2)],em.out_vek$posterior)
#zahranici2<-c("Zahraničí",999997,pocet,em.out_vek$posterior[vek4$uzemi_txt=="Praha 1",])
zahranici<-rep(NA,ncol(vek6))
obce3_vek<-rbind(vek6,zahranici)
obce3_vek[nrow(obce3_vek),1]<-"Zahraničí"
obce3_vek[nrow(obce3_vek),2:11]<-c(999997,vek6[vek6$uzemi_txt=="Praha 1",3:11])

# Ulozit
 save(obce3_vek, file="ciselniky/ciselnik_vek2.RData")

