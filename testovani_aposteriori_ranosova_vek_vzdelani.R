
#Sys.setlocale(category = 'LC_CTYPE','en_US.UTF-8')

rm(list=ls());

library(reshape2)

### Set up

start <- 5
stop <- 100

vstup_vse<-read.table("pscr_2021.csv", sep=",", header = T)
n<-dim(vstup_vse)[1]
names(vstup_vse) <- tolower(names(vstup_vse));

### Spustit predikci
vysledky_vse = data.frame(strana=character(), aktualne=numeric(), predikce_strata=numeric(),predikce_vzdelani=numeric(), predikce_vek=numeric(), procent_secteno=numeric())

procent_secteno<-15
for (procent_secteno in seq(start, stop, by=5)) {
  ######## 
  ######## Multinom 7 clusteru
  
  # Nacti
  #            load("/Users/admin/Desktop/reseni_0922/testovani/ciselnik17.RData")
  load("ciselniky/ciselnik17.RData")
  # Nacti
  #vstup <- read.table(paste0(paste0("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/secteno_upravene/secteno", as.character(procent_secteno)), ".csv"), sep=",", header = T)
  
  #vstup1 <- read.table(paste0(paste0("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/secteno_upravene/secteno", as.character(procent_secteno)), ".csv"), sep=",", header = T)
  #pocet_sectenych<-min(800*procent_secteno/5,n)
  pocet_sectenych<-min(round(procent_secteno*n/100),n)
  vstup<-vstup_vse[1:pocet_sectenych,]
  names(vstup) <- tolower(names(vstup))
  vstup[is.na(vstup)] <- 0
  
  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.x=T)
  
  # data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
  # indx_cluster <- grepl('comp', colnames(data))
  # name_cluster <- colnames(data)[indx_cluster]
  # indx_kstrana <- grepl('kstrana_', colnames(data))
  # name_kstrana <- colnames(data)[indx_kstrana]
  
  ### Join ciselnik
  data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
  indx_cluster <- grepl('comp', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kstrana <- grepl('kstrana_', colnames(data))
  name_kstrana <- colnames(data)[indx_kstrana]
  
  # Přiřadíme rovnoměrně cluster neznámým okrskům
  m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]
  print("Neznámých: ")
  print(sum(is.na(data$vel_okrsek)))
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kstrana][is.na(data[, indx_kstrana])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[is.na(data$vel_okrsek), 'vel_okrsek'] = data[is.na(data$vel_okrsek), 'zapsani_volici']
  
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kstrana), c("id", "secteno", "vel_okrsek", "zapsani_volici")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek', 'zapsani_volici'), name_kstrana))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru (rozlišujeme expected spočtené na základě číselníku okrsků a observed na základě počtu zapsaných voličů)
  data_melted$f_expected = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e_expected = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  data_melted$e_observed = data_melted$cluster_weight*data_melted$zapsani_volici*data_melted$secteno
  
  data_melted[is.na(data_melted$e_observed), 'e_observed'] = 0
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kstrana] = data_melted[, name_kstrana]*data_melted$cluster_weight
  
  # Agregace
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kstrana)], sum)
  vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
  #f_observed odhadneme na základě f_observed, e_observed, e_expected
  vysledky_clustery["f_observed"] = vysledky_clustery["f_expected"]*vysledky_clustery['e_observed']/vysledky_clustery['e_expected']
  
  
  # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
  vysledky_clustery[vysledky_clustery$e_observed==0, 'e_observed'] = 1
  
  vysledky_clustery$vaha = (vysledky_clustery$f_observed/vysledky_clustery$e_observed)#*(volicu_celkem/volicu_secteno)# konstantu zanedbáme
  
  # Navážit a sečíst
  pom = colSums(vysledky_clustery[, name_kstrana]*vysledky_clustery$vaha)
  vysledky = data.frame(pom, row.names=name_kstrana)
  vysledky["predikce_raw"] = vysledky/sum(vysledky)
  vysledky["strana"] = rownames(vysledky)
  rownames(vysledky) = c()
  
  # Současný stav
  vysledky["aktualne_hlasu"] = colSums(data[, name_kstrana])
  vysledky["aktualne"] = vysledky["aktualne_hlasu"]/sum(vysledky["aktualne_hlasu"])
  
  n_start = 14000
  n_stop = 15000
  alpha = (nrow(vstup_posledni) - n_start)/(n_stop-n_start)
  vysledky["predikce"] = vysledky["predikce_raw"]
  if (alpha > 1) { # věříme aktuálním výsledkům
    vysledky["predikce"] = vysledky["aktualne"]
  } else if (alpha > 0) { # blížíme se k aktuálním výsledkům
    vysledky["predikce"] = (1-alpha)*vysledky["predikce_raw"] + alpha*vysledky["aktualne"]
  }
  
  vysledky = vysledky[, c("strana", "aktualne", "predikce")]
  names(vysledky) = c("strana", "aktualne", "predikce_strata")
  
  vysledky_strata = vysledky

  ######## 
  ######## Multinom 7 clusteru vek
  
  # Nacti
  #load("/Users/admin/Desktop/reseni_0922/testovani/ciselnik17.RData")
  load("ciselniky/ciselnik_vek.RData")
  names(obce3_vek)[2]<-names(vstup_vse)[1]
  # 
  # ciselnik_vek<-ciselnik_okrsky[,c("cis_obec","cis_okrsek","vel_okrsek","id")]
  # ciselnik_vek<-merge(ciselnik_vek,obce3_vek,by="cis_obec")
  # 
  # 
  ciselnik_vek<-vstup_vse[,c("cis_obec","cis_okrsek")]
  ciselnik_vek<-merge(ciselnik_vek,obce3_vek,by="cis_obec")
  
  #names(ciselnik_vek)[3]<-"vel_okrsek"
  
  vel_okrsku<-ciselnik_okrsky[,c(1:3)]
  ciselnik_vek<-merge(ciselnik_vek,vel_okrsku,by=c("cis_obec","cis_okrsek"),all.x=T)
  
  pocet_sectenych<-min(round(procent_secteno*n/100),n)
  vstup<-vstup_vse[1:pocet_sectenych,]
  names(vstup) <- tolower(names(vstup))
  vstup[is.na(vstup)] <- 0

  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.x=T)

  ### Join ciselnik
  data <- merge(ciselnik_vek, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
  indx_cluster <- grepl('comp', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kstrana <- grepl('kstrana_', colnames(data))
  name_kstrana <- colnames(data)[indx_kstrana]
  # 
  # # Přiřadíme rovnoměrně cluster neznámým okrskům
  # m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  # data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]
  # print("Neznámých: ")
  # print(sum(is.na(data$vel_okrsek)))
  #data[is.na(data$vel_okrsek), name_cluster]<-obce3_vek[obce3_vek$cis_obec==data[is.na(data$vel_okrsek),"cis_obec"],3:9]
  
   
   
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kstrana][is.na(data[, indx_kstrana])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))

    ### Pozorovaným okrskům aktualizuj velikost obce
  data[is.na(data$vel_okrsek), 'vel_okrsek'] = data[is.na(data$vel_okrsek), 'zapsani_volici']
  data$id=paste0(data$cis_obec,data$cis_okrsek)
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kstrana), c("id", "secteno", "vel_okrsek", "zapsani_volici")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek', 'zapsani_volici'), name_kstrana))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru (rozlišujeme expected spočtené na základě číselníku okrsků a observed na základě počtu zapsaných voličů)
  data_melted$f_expected = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e_expected = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  data_melted$e_observed = data_melted$cluster_weight*data_melted$zapsani_volici*data_melted$secteno
  
  data_melted[is.na(data_melted$e_observed), 'e_observed'] = 0
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kstrana] = data_melted[, name_kstrana]*data_melted$cluster_weight
  
  # Agregace
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kstrana)], sum)
  vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
  #f_observed odhadneme na základě f_observed, e_observed, e_expected
  vysledky_clustery["f_observed"] = vysledky_clustery["f_expected"]*vysledky_clustery['e_observed']/vysledky_clustery['e_expected']
  
  
  # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
  vysledky_clustery[vysledky_clustery$e_observed==0, 'e_observed'] = 1
  
  vysledky_clustery$vaha = (vysledky_clustery$f_observed/vysledky_clustery$e_observed)#*(volicu_celkem/volicu_secteno)# konstantu zanedbáme
  
  # Navážit a sečíst
  pom = colSums(vysledky_clustery[, name_kstrana]*vysledky_clustery$vaha)
  vysledky = data.frame(pom, row.names=name_kstrana)
  vysledky["predikce_raw"] = vysledky/sum(vysledky)
  vysledky["strana"] = rownames(vysledky)
  rownames(vysledky) = c()
  
  # Současný stav
  vysledky["aktualne_hlasu"] = colSums(data[, name_kstrana])
  vysledky["aktualne"] = vysledky["aktualne_hlasu"]/sum(vysledky["aktualne_hlasu"])
  
  n_start = 14000
  n_stop = 15000
  alpha = (nrow(vstup_posledni) - n_start)/(n_stop-n_start)
  vysledky["predikce"] = vysledky["predikce_raw"]
  if (alpha > 1) { # věříme aktuálním výsledkům
    vysledky["predikce"] = vysledky["aktualne"]
  } else if (alpha > 0) { # blížíme se k aktuálním výsledkům
    vysledky["predikce"] = (1-alpha)*vysledky["predikce_raw"] + alpha*vysledky["aktualne"]
  }
  
  vysledky = vysledky[, c("strana", "predikce")]
  names(vysledky) = c("strana", "predikce_vek")
  
  vysledky_vek = vysledky
  ######## 
  ######## Multinom 7 clusteru vzdelani

  # Nacti
  #            load("/Users/admin/Desktop/reseni_0922/testovani/ciselnik17.RData")
  load("ciselniky/ciselnik_vzdelani.RData")
  names(obce3_vzdelani)[2]<-names(vstup_vse)[1]
  
  ciselnik_vzdelani<-vstup_vse[,c("cis_obec","cis_okrsek")]
  ciselnik_vzdelani<-merge(ciselnik_vzdelani,obce3_vzdelani,by="cis_obec")

  vel_okrsku<-ciselnik_okrsky[,c(1:3)]
  ciselnik_vzdelani<-merge(ciselnik_vzdelani,vel_okrsku,by=c("cis_obec","cis_okrsek"),all.x=T)
   
   
   
  pocet_sectenych<-min(round(procent_secteno*n/100),n)
  vstup<-vstup_vse[1:pocet_sectenych,]
  names(vstup) <- tolower(names(vstup))
  vstup[is.na(vstup)] <- 0
  
  
  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.x=T)
  
  ### Join ciselnik
  data <- merge(ciselnik_vzdelani, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
  indx_cluster <- grepl('comp', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kstrana <- grepl('kstrana_', colnames(data))
  name_kstrana <- colnames(data)[indx_kstrana]
  # 
  # # Přiřadíme rovnoměrně cluster neznámým okrskům
  # m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  # data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]
  # print("Neznámých: ")
  # print(sum(is.na(data$vel_okrsek)))
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kstrana][is.na(data[, indx_kstrana])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[is.na(data$vel_okrsek), 'vel_okrsek'] = data[is.na(data$vel_okrsek), 'zapsani_volici']
  data$id=paste0(data$cis_obec,data$cis_okrsek)
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kstrana), c("id", "secteno", "vel_okrsek", "zapsani_volici")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek', 'zapsani_volici'), name_kstrana))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru (rozlišujeme expected spočtené na základě číselníku okrsků a observed na základě počtu zapsaných voličů)
  data_melted$f_expected = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e_expected = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  data_melted$e_observed = data_melted$cluster_weight*data_melted$zapsani_volici*data_melted$secteno
  
  data_melted[is.na(data_melted$e_observed), 'e_observed'] = 0
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kstrana] = data_melted[, name_kstrana]*data_melted$cluster_weight
  
  # Agregace
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kstrana)], sum)
  vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
  #f_observed odhadneme na základě f_observed, e_observed, e_expected
  vysledky_clustery["f_observed"] = vysledky_clustery["f_expected"]*vysledky_clustery['e_observed']/vysledky_clustery['e_expected']
  
  
  # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
  vysledky_clustery[vysledky_clustery$e_observed==0, 'e_observed'] = 1
  
  vysledky_clustery$vaha = (vysledky_clustery$f_observed/vysledky_clustery$e_observed)#*(volicu_celkem/volicu_secteno)# konstantu zanedbáme
  
  # Navážit a sečíst
  pom = colSums(vysledky_clustery[, name_kstrana]*vysledky_clustery$vaha)
  vysledky = data.frame(pom, row.names=name_kstrana)
  vysledky["predikce_raw"] = vysledky/sum(vysledky)
  vysledky["strana"] = rownames(vysledky)
  rownames(vysledky) = c()
  
  # Současný stav
  vysledky["aktualne_hlasu"] = colSums(data[, name_kstrana])
  vysledky["aktualne"] = vysledky["aktualne_hlasu"]/sum(vysledky["aktualne_hlasu"])
  
  n_start = 14000
  n_stop = 15000
  alpha = (nrow(vstup_posledni) - n_start)/(n_stop-n_start)
  vysledky["predikce"] = vysledky["predikce_raw"]
  if (alpha > 1) { # věříme aktuálním výsledkům
    vysledky["predikce"] = vysledky["aktualne"]
  } else if (alpha > 0) { # blížíme se k aktuálním výsledkům
    vysledky["predikce"] = (1-alpha)*vysledky["predikce_raw"] + alpha*vysledky["aktualne"]
  }
  
  vysledky = vysledky[, c("strana", "predikce")]
  names(vysledky) = c("strana", "predikce_vzdelani")
  
  vysledky_vzdelani = vysledky  
  
  
  ####### Merge
  vysledky = merge(vysledky_strata,merge(vysledky_vzdelani, vysledky_vek, by="strana"),by="strana")
  
  
  vysledky["procent_secteno"] = procent_secteno
  vysledky_vse = rbind(vysledky_vse, vysledky)

  
}



library(ggplot2)
library(scales)

# velke_strany <- c("kstrana_13", "kstrana_17", "kstrana_20",
#                   "kstrana_4", "kstrana_5", "kstrana_18",
#                   "kstrana_12", "kstrana_8", "kstrana_1")
velke_strany<-c("kstrana_13", "kstrana_17", "kstrana_20", "kstrana_4")
male_strany<-c("kstrana_5", "kstrana_18","kstrana_12")
strany<-c(velke_strany,male_strany)  


nazvy_strany <- list()

nazvy_strany[["kstrana_13"]] = "SPOLU" #velke strany
nazvy_strany[["kstrana_17"]] = "PirSTAN"
nazvy_strany[["kstrana_20"]] = "ANO"
nazvy_strany[["kstrana_4"]] = "SPD"

nazvy_strany[["kstrana_5"]] = "CSSD" #male strany (boj o 3-5 procent)
nazvy_strany[["kstrana_18"]] = "KSCM"
nazvy_strany[["kstrana_12"]] = "Prisaha"
nazvy_strany[["kstrana_8"]] = "Trikolora"
#nazvy_strany[["kstrana_1"]] = "Zeleni"

pruzkum <- list()

pruzkum[["kstrana_13"]] = 0.214 #velke strany
pruzkum[["kstrana_17"]] = 0.174
pruzkum[["kstrana_20"]] = 0.273
pruzkum[["kstrana_4"]]  = 0.123

pruzkum[["kstrana_5"]] = 0.044 #male strany (boj o 3-5 procent)
pruzkum[["kstrana_18"]] =0.065
pruzkum[["kstrana_12"]] = 0.057
pruzkum[["kstrana_8"]] = 0.018
#pruzkum[["kstrana_1"]] = 0.016


plot_list = list()

### Vývoj

for(strana in strany){
  
  vysledky_plot = vysledky_vse[vysledky_vse$strana==strana, ]
  
  plot <- ggplot(data=vysledky_plot[vysledky_plot$procent_secteno>=5, ])+
    geom_line(aes(x=procent_secteno/100, y=aktualne, color="Aktualne")) + 
    geom_line(aes(x=procent_secteno/100, y=predikce_strata, color="Predikce volby 2017"),size=1.2) +
    geom_line(aes(x=procent_secteno/100, y=predikce_vek, color="Predikce vek")) + 
    geom_line(aes(x=procent_secteno/100, y=predikce_vzdelani, color="Predikce vzdelani")) + 
    ggtitle(nazvy_strany[[strana]]) +
    geom_hline(yintercept = vysledky_plot[20,2],linetype = 3)+ #tabulka_strany$pruzkum[k]
    #geom_hline(yintercept = pruzkum[[strana]],linetype = 2)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
    scale_color_hue(h = c(0, 215) + 15,limits=c("Aktualne", "Predikce vek","Predikce vzdelani","Predikce volby 2017")) +
    #geom_hline(yintercept = 0.05) + #opravit
    xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
    theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))
    if (strana %in% male_strany){
    plot<-plot+ geom_hline(yintercept = 0.05)
    }
    if (strana=="kstrana_8"){
      plot<-plot+ geom_hline(yintercept = 0.03)
    }
  
  plot_list[[strana]] <- plot
}
plot_list[["kstrana_8"]]

# 
# par(mfrow=c(2,2))
# show_col(hue_pal(h=c(0,180)+15)(3))
# show_col(hue_pal(h=c(90,270)+15)(3))
# show_col(hue_pal(h=c(180,360)+15)(3))
# 
# show_col(hue_pal(h=c(270,360)+15)(2))
# show_col(hue_pal()(1))
# 
# par(mfrow=c(1,2))
# show_col(hue_pal(h=c(0,360)+15)(5))
# show_col(hue_pal(h=c(0,216)+15)(4))
# 
# show_col(hue_pal(h=c(180,360)+15)(3))
# 
# show_col(hue_pal(h=c(270,360)+15)(2))
# show_col(hue_pal()(1))

# 
# show_col(hue_pal()(5)[1:4])
# show_col(hue_pal(h = c(0, 214)+15)(4))


### Souboj ANO/SPOLU


vysledky_souboj = vysledky_vse[vysledky_vse$strana=="kstrana_13", c("procent_secteno", "predikce_strata","predikce_vek", "predikce_vzdelani", "aktualne")]
names(vysledky_souboj) = c("procent_secteno", "predikce_strata_spolu","predikce_vek_spolu","predikce_vzdelani_spolu", "aktualne_spolu")

vysledky_souboj = merge(vysledky_souboj, vysledky_vse[vysledky_vse$strana=="kstrana_20", c("procent_secteno", "predikce_strata","predikce_vek", "predikce_vzdelani", "aktualne")])
names(vysledky_souboj) = c("procent_secteno", "predikce_strata_spolu","predikce_vek_spolu","predikce_vzdelani_spolu", "aktualne_spolu","predikce_strata_ano","predikce_vek_ano","predikce_vzdelani_ano", "aktualne_ano")

vysledky_souboj["predikce_strata"] = vysledky_souboj["predikce_strata_ano"] - vysledky_souboj["predikce_strata_spolu"]
vysledky_souboj["predikce_vek"] = vysledky_souboj["predikce_vek_ano"] - vysledky_souboj["predikce_vek_spolu"]
vysledky_souboj["predikce_vzdelani"] = vysledky_souboj["predikce_vzdelani_ano"] - vysledky_souboj["predikce_vzdelani_spolu"]
vysledky_souboj["aktualne"] = vysledky_souboj["aktualne_ano"] - vysledky_souboj["aktualne_spolu"]

plot_list[["souboj"]] <- ggplot(data=vysledky_souboj[vysledky_souboj$procent_secteno>=5, ])+
  geom_line(aes(x=procent_secteno/100, y=predikce_strata, color="Predikce volby 2017"),size=1.2) +
  geom_line(aes(x=procent_secteno/100, y=predikce_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=predikce_vzdelani, color="Predikce vzdelani")) + 
  geom_line(aes(x=procent_secteno/100, y=aktualne, color="Aktualne")) + 
  ggtitle("Rozdil obdrzenych procent pro ANO a SPOLU") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
scale_color_hue(h = c(0, 215) + 15,limits=c("Aktualne", "Predikce vek","Predikce vzdelani","Predikce volby 2017")) +
  geom_hline(yintercept = 0) + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["souboj"]]

### Většina ve sněmovně

vysledky_vetsina = vysledky_vse
vysledky_vetsina["koalice"] = (vysledky_vetsina$strana=="kstrana_13") + (vysledky_vetsina$strana=="kstrana_17")

vysledky_vetsina["predikce_strata_adj"] = vysledky_vetsina$predikce_strata*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["predikce_vek_adj"] = vysledky_vetsina$predikce_vek*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["predikce_vzdelani_adj"] = vysledky_vetsina$predikce_vzdelani*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["aktualne_adj"] = vysledky_vetsina$aktualne*(vysledky_vetsina$aktualne>=0.05)

vetsina = aggregate(cbind(predikce_strata_adj,predikce_vek_adj,predikce_vzdelani_adj, aktualne_adj) ~ koalice + procent_secteno,vysledky_vetsina,  sum)
vetsina_celkem = aggregate(cbind(predikce_strata_adj,predikce_vek_adj,predikce_vzdelani_adj, aktualne_adj) ~ procent_secteno,vetsina,  sum)
names(vetsina_celkem) = c("procent_secteno", "predikce_strata_celkem","predikce_vek_celkem","predikce_vzdelani_celkem", "aktualne_celkem")
vetsina = merge(vetsina, vetsina_celkem, by="procent_secteno")
vetsina["pomer_predikce_strata"] = vetsina["predikce_strata_adj"]/vetsina["predikce_strata_celkem"]
vetsina["pomer_predikce_vek"] = vetsina["predikce_vek_adj"]/vetsina["predikce_vek_celkem"]
vetsina["pomer_predikce_vzdelani"] = vetsina["predikce_vzdelani_adj"]/vetsina["predikce_vzdelani_celkem"]
vetsina["pomer_aktualne"] = vetsina["aktualne_adj"]/vetsina["aktualne_celkem"]

plot_list[["vetsina"]] <- ggplot(data=vetsina[(vetsina$koalice==1) & (vetsina$procent_secteno>=5), ])+
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_strata, color="Predikce volby 2017"), size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_vzdelani, color="Predikce vzdelani")) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_aktualne, color="Aktualne")) + 
  ggtitle("Koalice proti zbytku: SPOLU+PirSTAN") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
  geom_hline(yintercept = 0.5) + 
scale_color_hue(h = c(0, 215) + 15,limits=c("Aktualne", "Predikce vek","Predikce vzdelani","Predikce volby 2017")) +
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["vetsina"]]

### Bývalá vladní většina ve sněmovně (ANO, CSSD, KSCM)


vysledky_vlada = vysledky_vse
vysledky_vlada["koalice"] = (vysledky_vlada$strana=="kstrana_20") + (vysledky_vlada$strana=="kstrana_5")+ (vysledky_vlada$strana=="kstrana_18")

vysledky_vlada["predikce_strata_adj"] = vysledky_vlada$predikce_strata*(vysledky_vlada$predikce_strata>=0.05)
vysledky_vlada["predikce_vek_adj"] = vysledky_vlada$predikce_vek*(vysledky_vlada$predikce_strata>=0.05)
vysledky_vlada["predikce_vzdelani_adj"] = vysledky_vlada$predikce_vzdelani*(vysledky_vlada$predikce_strata>=0.05)
vysledky_vlada["aktualne_adj"] = vysledky_vlada$aktualne*(vysledky_vlada$aktualne>=0.05)

vlada = aggregate(cbind(predikce_strata_adj,predikce_vek_adj,predikce_vzdelani_adj, aktualne_adj) ~ koalice + procent_secteno,vysledky_vlada,  sum)
vlada_celkem = aggregate(cbind(predikce_strata_adj,predikce_vek_adj,predikce_vzdelani_adj, aktualne_adj) ~ procent_secteno,vlada,  sum)
names(vlada_celkem) = c("procent_secteno", "predikce_strata_celkem","predikce_vek_celkem","predikce_vzdelani_celkem", "aktualne_celkem")
vlada = merge(vlada, vlada_celkem, by="procent_secteno")
vlada["pomer_predikce_strata"] = vlada["predikce_strata_adj"]/vlada["predikce_strata_celkem"]
vlada["pomer_predikce_vek"] = vlada["predikce_vek_adj"]/vlada["predikce_vek_celkem"]
vlada["pomer_predikce_vzdelani"] = vlada["predikce_vzdelani_adj"]/vlada["predikce_vzdelani_celkem"]
vlada["pomer_aktualne"] = vlada["aktualne_adj"]/vlada["aktualne_celkem"]

plot_list[["vlada"]] <- ggplot(data=vlada[(vlada$koalice==1) & (vlada$procent_secteno>=5), ])+
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_strata, color="Predikce volby 2017"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_vzdelani, color="Predikce vzdelani")) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_aktualne, color="Aktualne")) + 
  ggtitle("Strany minulé vládní většiny: ANO, ČSSD, KSČM") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
scale_color_hue(h = c(0, 215) + 15,limits=c("Aktualne", "Predikce vek","Predikce vzdelani","Predikce volby 2017")) +
  geom_hline(yintercept = 0.5) + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))


### Rychlost konvergence


vysledky_rychlost= vysledky_vse

vysledky_rychlost["vysledek"]=rep(vysledky_rychlost[vysledky_rychlost$procent_secteno==100,"aktualne"],times=20)
vysledky_rychlost["rozdil_strata"] = vysledky_rychlost$predikce_strata-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_vek"] =  vysledky_rychlost$predikce_vek-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_vzdelani"] =  vysledky_rychlost$predikce_vzdelani-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_aktualne"] =  vysledky_rychlost$aktualne-vysledky_rychlost$vysledek

normF<-function(x){ norm(matrix(x,nrow=1),type="F")}

rychlost = aggregate(cbind(rozdil_strata,rozdil_vek,rozdil_vzdelani,rozdil_aktualne) ~ procent_secteno,vysledky_rychlost,  FUN=normF)

plot_list[["rychlost"]] <- ggplot(data=rychlost)+
  geom_line(aes(x=procent_secteno/100, y=rozdil_strata, color="Predikce volby 2017"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_aktualne, color="Aktualne")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vzdelani, color="Predikce vzdelani")) + 
  ggtitle("Vzdalenost vysledku pro ruzne metody") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + geom_hline(yintercept = 0) + 
scale_color_hue(h = c(0, 215) + 15,limits=c("Aktualne", "Predikce vek","Predikce vzdelani","Predikce volby 2017")) +
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))


plot_list[["rychlost"]] 

### Rychlost konvergence - prumer


vysledky_rychlost2= vysledky_rychlost
vysledky_rychlost2[,2:11]=abs(vysledky_rychlost2[,2:11])

rychlost2 = aggregate(cbind(rozdil_strata,rozdil_vek,rozdil_vzdelani,rozdil_aktualne) ~ procent_secteno,vysledky_rychlost2,  FUN=mean)

plot_list[["rychlost2"]] <- ggplot(data=rychlost2)+
  geom_line(aes(x=procent_secteno/100, y=rozdil_strata, color="Predikce volby 2017"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vzdelani, color="Predikce vzdelani")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_aktualne, color="Aktualne")) + 
  ggtitle("Průměrná chyba pro ruzne metody") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  scale_color_hue(h = c(0, 215) + 15,limits=c("Aktualne", "Predikce vek","Predikce vzdelani","Predikce volby 2017")) +
  xlab("Procent okrsku secteno") + ylab("Vzdalenost") + labs(color = "") + geom_hline(yintercept = 0) + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["rychlost2"]]


# pdf("obrazky/pokus.pdf", encoding="ISOLatin2.enc",width=5,height = 5)
# plot_list[["rychlost2"]]
# dev.off()




#pdf("/Users/admin/Documents/volebni_predikce/testovani/test_aposteriori_upravene.pdf", encoding="ISOLatin2.enc")
pdf("obrazky/test_aposteriori_upravene_ran_VV_final.pdf", encoding="ISOLatin2.enc",width=5,height = 5)
plot_list
dev.off()


save(vysledky_vse, vysledky_souboj, vetsina,vlada,rychlost,rychlost2, file = "vysledky/aposteriori_analyza_data_upravene_ran_VV2.RData")
