
### Priprava dat, spustit jednou pred vsim pro vytvoreni dat

# Pozn.: - zahraničí je kraj==9900, cis_obec==999997 a jeden záznam má výskyt v roce 2017 pouze jako opakovane==1

rm(list=ls());

#workdir <- "/Users/admin/Desktop/reseni_0922/"
#workdir <- "/Users/User/Desktop/MFF/sfg_volby/reseni_0922/"

# Nahrani dat
obce <- read.table("reseni2021/pscoco.csv", sep=";", header=T);
okrsky <- read.table("reseni2021/psp17okrs.csv", sep=",", header=T);

# Manipulace s daty
okrsky[is.na(okrsky)] <- 0;
names(okrsky) <- tolower(names(okrsky));

### Pouzit jen posledni zaznam pro dane id
okrsky_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, okrsky, FUN = max)
okrsky <- merge(okrsky, okrsky_posledni, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.y=T)

# Strata pomocí mixed multinomial model
library(mixtools)
set.seed(1)
#n.strat <- 7 # počet strat

temp_strany <- colnames(okrsky)[grepl('kstrana', colnames(okrsky))]
temp <- okrsky[rowSums(okrsky[, temp_strany]) > 0, ] # odstranit záznamy bez hlasů
temp_id <- temp[, c("cis_obec", "cis_okrsek")]
temp_vysledky <- temp[, temp_strany]
poradi.stran <- order((-1)*colSums(temp_vysledky)) # seřadit strany podle celkového výsledku
temp_vysledky <- temp_vysledky[, 1:11] # vezmeme 11 nejsilnějších stran


maxn<-14
loglikelihood<-numeric(maxn)

for(k in 3:maxn){
  
  n.strat <- k # počet strat
  em.out <- multmixEM(as.matrix(temp_vysledky), k=n.strat)
  loglikelihood[k]<-em.out$loglik
}
# 
# em.out <- multmixEM(as.matrix(temp_vysledky), k=n.strat)
# temp = cbind(temp_id, em.out$posterior)

data<-cbind(1:maxn,loglikelihood)

plot(data[3:maxn,])
abline(v=7)

temp_vysledky2<-temp_vysledky/apply(temp_vysledky,MARGIN=1,FUN=sum)
maxnn<-10
totwss<-numeric(maxnn)

for(k in 2:maxnn){
  km.res <- kmeans(temp_vysledky2, k, nstart = 25,iter.max=50)
  totwss[k]<-km.res$tot.withinss
  
}



data2<-cbind(1:maxnn,totwss)
plot(data2[2:maxnn,])

okrsky <- merge(okrsky, temp, by=c("cis_obec", "cis_okrsek"), all.y=T)

# Subset a názvy
temp_comp <- colnames(okrsky)[grepl('comp', colnames(okrsky))]
ciselnik_okrsky <- subset(okrsky, select=c("cis_obec", "cis_okrsek", "zapsani_volici", temp_comp))
names(ciselnik_okrsky) <- c("cis_obec", "cis_okrsek", "vel_okrsek", temp_comp)
ciselnik_okrsky$id <- paste0(ciselnik_okrsky$cis_obec, ciselnik_okrsky$cis_okrsek)

# Ulozit
save(ciselnik_okrsky, file=paste0(workdir, "reseni2021/ciselnik.RData"))

