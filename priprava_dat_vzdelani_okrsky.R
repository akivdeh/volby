rm(list=ls());

library(readr)
library(reshape2)
library(mixtools)
library(jsonlite)
library(readxl)

## vzdelani ####
load("ciselniky/ciselnik17.RData");
okrsky <-ciselnik_okrsky
load("ciselniky/ciselnikKraje.RData")
data <- read_excel("data_csu/VO_vek_vzdel_op.xlsx")


data$momc<-!is.na(data$momc)
#data$id<-paste0(data$obec,data$cislo)
 

stat_mesta<-c("Pardubice","Liberec","Praha","Plzeň","Opava","Ústí","Ostrava","Brno")
stat_mesta_cisla<-unique(data$obec[data$momc==TRUE])
# 555134 563889 554782 554791 505927 554804 554821 582786
# pardub Libere Praha  Plzen  Opava  UstinL Ostava Brno
names(stat_mesta_cisla)<-stat_mesta

obce<-setdiff(unique(data$obec),stat_mesta_cisla)

data$cis_obec<-ifelse(data$momc,data$momc,data$obec)


#stat_mesta_cisla["Praha"]
ciselnik_okrsky2<-ciselnik_okrsky[,c("cis_obec","nazevobce","cis_okrsek","vel_okrsek")]
ciselnik_okrsky2<-ciselnik_okrsky2[!(ciselnik_okrsky2$cis_obec %in% obce),]
ciselnik_okrsky2$mesto_cislo<-NA
for (mesto in stat_mesta){
  ciselnik_okrsky2$mesto_cislo[grepl(mesto,ciselnik_okrsky2$nazevobce)]<-stat_mesta_cisla[mesto] 
}

ciselnik_okrsky2$mesto_cislo[is.na(ciselnik_okrsky2$mesto_cislo)& ciselnik_okrsky2$cis_obec<555000]<-stat_mesta_cisla["Ostrava"]
ciselnik_okrsky2$mesto_cislo[is.na(ciselnik_okrsky2$mesto_cislo)]<-stat_mesta_cisla["Opava"]

  
# ciselnik_okrsky2$mesto[grepl("",ciselnik_okrsky2$nazevobce)]<-"Brno"
# 
# #ciselnik_okrsky2$kod<-
# sum(data$momc)
# 

# ostrava
# poruba nova bela vitkovice stara bela marianky 
# petrkovice lhotka hostalkovice nova proskovice
# michalkovice radvanice

# opava
#  

# for (mesto in stat_mesta){
# print(mesto)
# print(range(ciselnik_okrsky2$cis_okrsek[ciselnik_okrsky2$mesto_cislo==stat_mesta_cisla[mesto]]))
# }
# 
# range(ciselnik_okrsky2$cis_okrsek[ciselnik_okrsky2$nazevobce=="Praha 4"])
# max(ciselnik_okrsky2$cis_okrsek)

ciselnik_okrsky2$cis_okrsek<-ciselnik_okrsky2$cis_okrsek+10^6
ciselnik_okrsky2$cis_okrsek2<-floor(ciselnik_okrsky2$cis_okrsek/1000)

kontrola<-ciselnik_okrsky2[,c("nazevobce","cis_okrsek2")]


ciselnik_okrsky2$id_obec<-paste0(ciselnik_okrsky2$mesto_cislo,ciselnik_okrsky2$cis_okrsek2)

spojka<-ciselnik_okrsky2[,c("cis_obec","id_obec")]
spojka<-unique(spojka)
colnames(spojka)[1]<-"cis_obec2"

data$cislo2<-(data$cislo+10^6)*data$momc
data$cislo2<-floor(data$cislo2/1000)*data$momc
data$id_obec<-ifelse(data$momc,paste0(data$obec,data$cislo2),0)


data2<-merge(data,spojka,by=c("id_obec"),all.x = T)


data2$cis_obec3<-ifelse(is.na(data2$cis_obec2),data2$obec,data2$cis_obec2)

# sum(!is.na(data2$cis_obec2))
# sum(is.na(data2$cis_obec3))
# 
# sum(data2$cis_obec!=data2$obec)
# sum(data2$cis_obec %in% stat_mesta_cisla)

indx_vek<-grepl("vek",colnames(data2))
name_vek<-colnames(data2)[indx_vek]

indx_vzdelani<-grepl("vzdel",colnames(data2))
name_vzdelani<-colnames(data2)[indx_vzdelani]

data3_vek<-data2[,c("cis_obec3","cislo","pocet_obyvatel",name_vek)]
data3_vzdelani<-data2[,c("cis_obec3","cislo","pocet_obyvatel",name_vzdelani)]
colnames(data3_vzdelani)[1]<-"cis_obec"
colnames(data3_vzdelani)[2]<-"cis_okrsek"
colnames(data3_vzdelani)[3]<-"vel_okrsek"
data3_vzdelani$id<-paste0(data3_vzdelani$cis_obec,data3_vzdelani$cis_okrsek)
colnames(data3_vek)[1]<-"cis_obec"
colnames(data3_vek)[2]<-"cis_okrsek"
colnames(data3_vek)[3]<-"vel_okrsek"
data3_vek$id<-paste0(data3_vek$cis_obec,data3_vek$cis_okrsek)


#### vzdelani

set.seed(1)
n.strat <- 7 # počet strat
matice<-as.matrix(data3_vzdelani[,name_vzdelani])
em.out2 <- multmixEM(matice, k=n.strat)

# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# number of iterations= 132

vzdel2<-cbind(data3_vzdelani,em.out2$posterior)
vzdel3<-cbind(data3_vzdelani[,setdiff(colnames(data3_vzdelani),name_vzdelani)],em.out2$posterior)

zahranici<-okrsky[okrsky$cis_obec=="999997",]
zahranici<-zahranici[,c(1:3,11,4:10)]

# p1<-em.out2$posterior[data3_vzdelani$cis_obec=="500054",]
# p1<-p1[1,]

p1mat<-em.out2$posterior[vzdel3$cis_obec=="500054",]
p1<-apply(p1mat,MARGIN=2,FUN=mean)


vzdel3<-rbind(vzdel3,zahranici)
vzdel3[vzdel3$cis_obec=="999997",5:11]<-matrix(rep(p1,107),ncol=7,byrow=T)
ciselnik_vzdelani_okr<-vzdel3

# Ulozit
save(ciselnik_vzdelani_okr, file="ciselniky/ciselnik_vzdelani_okrsky.RData")


#### vek


set.seed(1)
n.strat <- 7 # počet strat
matice<-as.matrix(data3_vek[,name_vek])
em.out2 <- multmixEM(matice, k=n.strat)

# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# Restarting due to numerical problem.
# number of iterations= 229 


vek2<-cbind(data3_vek,em.out2$posterior)
vek3<-cbind(data3_vek[,setdiff(colnames(data3_vek),name_vek)],em.out2$posterior)


zahranici<-okrsky[okrsky$cis_obec=="999997",]
zahranici<-zahranici[,c(1:3,11,4:10)]

# zahranici<-ciselnik_okrsky[okrsky$cis_obec=="999997",]
# zahranici<-zahranici[,c(1:3,11,4:10)]

p1mat<-em.out2$posterior[data3_vek$cis_obec=="500054",]
p1<-apply(p1mat,MARGIN=2,FUN=mean)

vek3<-rbind(vek3,zahranici)
vek3[vzdel3$cis_obec=="999997",5:11]<-matrix(rep(p1,107),ncol=7,byrow=T)
ciselnik_vek_okr<-vek3

# Ulozit
save(ciselnik_vek_okr, file="ciselniky/ciselnik_vek_okrsky.RData")




