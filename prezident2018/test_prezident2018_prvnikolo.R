#Sys.setlocale(category = 'LC_CTYPE','en_US.UTF-8')

rm(list=ls());

library(reshape2)
#workdir <- "/Users/User/Desktop/MFF/sfg_volby/reseni_0922/"
#workdir <- "reseni_2021/"

###### Set up #####

start <- 5
stop <- 100

vstup_vse<-read.table("prezident2018/kolo_1.csv", sep=",", header = T)
vstup_vse[is.na(vstup_vse)] <- 0
names(vstup_vse) <- tolower(names(vstup_vse));

vstup_vse$id<- paste0(vstup_vse$cis_obec, vstup_vse$cis_okrsek)
n<-dim(vstup_vse)[1]

jmena<-c("Milos Zeman", "Jiri Drahos","Pavel Fischer",
         "Michal Horacek","Marek Hilser", "Mirek Topolanek",
         "Jiri Hynek", "Petr Hannig","Vratislav Kulhanek")
kandidati<-c("kandidat_7","kandidat_9","kandidat_3",
             "kandidat_2","kandidat_8","kandidat_1",
             "kandidat_4","kandidat_5","kandidat_6")
tabulka_kandidati<-as.data.frame(cbind(jmena,kandidati))


###### Spustit predikci ###### 
vysledky_vse = data.frame(kandidat=character(), aktualne=numeric(), predikce_strata=numeric(),predikce_kraje=numeric(), predikce_vek=numeric(),predikce_vzdelani=numeric(), procent_secteno=numeric())

procent_secteno<-15

for (procent_secteno in seq(start, stop, by=5)) {
  ######## 
  ######## Multinom 7 clusteru ######
  
  # Nacti
  load("ciselniky/ciselnik17.RData")
  # Nacti
  pocet_sectenych<-min(round(procent_secteno*n/100),n)
  vstup<-vstup_vse[1:pocet_sectenych,]
  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek+id, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac","id"), all.x=T)
  
  ### Join ciselnik
  data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek","id"), all=T);
  indx_cluster <- grepl('comp', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kandidat <- grepl('kandidat_', colnames(data))
  name_kandidat <- colnames(data)[indx_kandidat]
  
  # Přiřadíme rovnoměrně cluster neznámým okrskům
  m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]
  print("Neznámých: ")
  print(sum(is.na(data$vel_okrsek)))
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kandidat][is.na(data[, indx_kandidat])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[is.na(data$vel_okrsek), 'vel_okrsek'] = data[is.na(data$vel_okrsek), 'zapsani_volici']
  
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kandidat), c("id", "secteno", "vel_okrsek", "zapsani_volici")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek', 'zapsani_volici'), name_kandidat))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru (rozlišujeme expected spočtené na základě číselníku okrsků a observed na základě počtu zapsaných voličů)
  data_melted$f_expected = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e_expected = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  
  data_melted$e_observed = data_melted$cluster_weight*data_melted$zapsani_volici*data_melted$secteno
  data_melted[is.na(data_melted$e_observed), 'e_observed'] = 0
  #data_melted$f_observed = ifelse(data_melted$secteno==1,data_melted$e_observed,data_melted$f)
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kandidat] = data_melted[, name_kandidat]*data_melted$cluster_weight
  
  # Agregace
  #vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "f_observed", "e_expected", "e_observed", name_kstrana)], sum)
  
  
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kandidat)], sum)
  #vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
  #f_observed odhadneme na základě f_observed, e_observed, e_expected
  vysledky_clustery["f_observed"] = vysledky_clustery["f_expected"]*vysledky_clustery['e_observed']/vysledky_clustery['e_expected']
  
  
  # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
  vysledky_clustery[vysledky_clustery$e_observed==0, 'e_observed'] = 1
  
  vysledky_clustery$vaha = (vysledky_clustery$f_observed/vysledky_clustery$e_observed)#*(volicu_celkem/volicu_secteno)# konstantu zanedbáme
  
  # Navážit a sečíst
  pom = colSums(vysledky_clustery[, name_kandidat]*vysledky_clustery$vaha)
  vysledky = data.frame(pom, row.names=name_kandidat)
  vysledky["predikce_raw"] = vysledky/sum(vysledky)
  vysledky["kandidat"] = rownames(vysledky)
  rownames(vysledky) = c()
  
  # Současný stav
  vysledky["aktualne_hlasu"] = colSums(data[, name_kandidat])
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
  
  vysledky = vysledky[, c("kandidat", "aktualne", "predikce")]
  names(vysledky) = c("kandidat", "aktualne", "predikce_strata")
  
  vysledky_strata = vysledky
  
  ######## Kraje #####
  
  #            load("/Users/admin/Desktop/reseni_0922/testovani/ciselnikKraje.RData")
  load("ciselniky/ciselnikKraje.RData")
  # Nacti
  #vstup <- read.table(paste0(paste0("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/secteno_upravene/secteno", as.character(procent_secteno)), ".csv"), sep=",", header = T)
  
  
  data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek","id"), all=T);
  indx_cluster <- grepl('cluster', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kandidat <- grepl('kandidat_', colnames(data))
  name_kandidat <- colnames(data)[indx_kandidat]
  
  # Přiřadíme rovnoměrně cluster neznámým okrskům
  m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  data[is.na(data$kraj), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$kraj))), ]
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kandidat][is.na(data[, indx_kandidat])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[!is.na(data$zapsani_volici), 'vel_okrsek'] = data[!is.na(data$zapsani_volici), 'zapsani_volici']
  
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kandidat), c("id", "secteno", "vel_okrsek")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek'), name_kandidat))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru
  data_melted$f = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kandidat] = data_melted[, name_kandidat]*data_melted$cluster_weight
  
  # Agregovat
  volicu_celkem <- sum(data$vel_okrsek)
  volicu_secteno <- sum(data$vel_okrsek*data$secteno)
  
  if (volicu_secteno == 0) {
    print("Nejsou sectene zadne hlasy.")
  } else {
    vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", name_kandidat, "f", "e")], sum)
    
    # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
    vysledky_clustery[vysledky_clustery$e==0, 'e'] = 1
    
    vysledky_clustery$vaha = (vysledky_clustery$f/vysledky_clustery$e)*(volicu_celkem/volicu_secteno)
    
    # Navážit a sečíst
    pom = colSums(vysledky_clustery[, name_kandidat]*vysledky_clustery$vaha)
    vysledky = data.frame(pom, row.names=name_kandidat)
    vysledky["predikce_raw"] = vysledky/sum(vysledky)
    vysledky["kandidat"] = rownames(vysledky)
    rownames(vysledky) = c()
    
    # Současný stav
    vysledky["aktualne_hlasu"] = colSums(data[, name_kandidat])
    vysledky["aktualne"] = vysledky["aktualne_hlasu"]/sum(vysledky["aktualne_hlasu"])
    
    # Od n_stop počtu okrsků se blížíme k aktuální hodnotě, od n_stop jí věříme naplno
    n_start = 14000
    n_stop = 15000
    alpha = (nrow(vstup_posledni) - n_start)/(n_stop-n_start)
    vysledky["predikce"] = vysledky["predikce_raw"]
    if (alpha > 1) { # věříme aktuálním výsledkům
      vysledky["predikce"] = vysledky["aktualne"]
    } else if (alpha > 0) { # blížíme se k aktuálním výsledkům
      vysledky["predikce"] = (1-alpha)*vysledky["predikce_raw"] + alpha*vysledky["aktualne"]
    }
    
    vysledky = vysledky[, c("kandidat", "predikce")]
    names(vysledky) = c("kandidat", "predikce_kraje")
  }
  
  vysledky_kraje = vysledky

  ######## Multinom 7 clusteru vek   ######## 
  
  # Nacti
  #load("/Users/admin/Desktop/reseni_0922/testovani/ciselnik17.RData")
  load("ciselniky/ciselnik17.RData")
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
  
  # pocet_sectenych<-min(round(procent_secteno*n/100),n)
  # vstup<-vstup_vse[1:pocet_sectenych,]
  # names(vstup) <- tolower(names(vstup))
  # vstup[is.na(vstup)] <- 0
  
  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.x=T)
  
  ### Join ciselnik
  data <- merge(ciselnik_vek, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
  indx_cluster <- grepl('comp', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kandidat <- grepl('kandidat_', colnames(data))
  name_kandidat <- colnames(data)[indx_kandidat]
  # 
  # # Přiřadíme rovnoměrně cluster neznámým okrskům
  # m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  # data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]
  # print("Neznámých: ")
  # print(sum(is.na(data$vel_okrsek)))
  #data[is.na(data$vel_okrsek), name_cluster]<-obce3_vek[obce3_vek$cis_obec==data[is.na(data$vel_okrsek),"cis_obec"],3:9]
  
  
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kandidat][is.na(data[, indx_kandidat])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[is.na(data$vel_okrsek), 'vel_okrsek'] = data[is.na(data$vel_okrsek), 'zapsani_volici']
  data$id=paste0(data$cis_obec,data$cis_okrsek)
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kandidat), c("id", "secteno", "vel_okrsek", "zapsani_volici")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek', 'zapsani_volici'), name_kandidat))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru (rozlišujeme expected spočtené na základě číselníku okrsků a observed na základě počtu zapsaných voličů)
  data_melted$f_expected = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e_expected = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  data_melted$e_observed = data_melted$cluster_weight*data_melted$zapsani_volici*data_melted$secteno
  
  data_melted[is.na(data_melted$e_observed), 'e_observed'] = 0
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kandidat] = data_melted[, name_kandidat]*data_melted$cluster_weight
  
  # Agregace
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kandidat)], sum)
  vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
  #f_observed odhadneme na základě f_observed, e_observed, e_expected
  vysledky_clustery["f_observed"] = vysledky_clustery["f_expected"]*vysledky_clustery['e_observed']/vysledky_clustery['e_expected']
  
  
  # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
  vysledky_clustery[vysledky_clustery$e_observed==0, 'e_observed'] = 1
  
  vysledky_clustery$vaha = (vysledky_clustery$f_observed/vysledky_clustery$e_observed)#*(volicu_celkem/volicu_secteno)# konstantu zanedbáme
  
  # Navážit a sečíst
  pom = colSums(vysledky_clustery[, name_kandidat]*vysledky_clustery$vaha)
  vysledky = data.frame(pom, row.names=name_kandidat)
  vysledky["predikce_raw"] = vysledky/sum(vysledky)
  vysledky["kandidat"] = rownames(vysledky)
  rownames(vysledky) = c()
  
  # Současný stav
  vysledky["aktualne_hlasu"] = colSums(data[, name_kandidat])
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
  
  vysledky = vysledky[, c("kandidat", "predikce")]
  names(vysledky) = c("kandidat", "predikce_vek")
  
  vysledky_vek = vysledky

  ######## Multinom 7 clusteru vzdelani   ######## 
  
  # Nacti
  #            load("/Users/admin/Desktop/reseni_0922/testovani/ciselnik17.RData")
  load("ciselniky/ciselnik_vzdelani.RData")
  names(obce3_vzdelani)[2]<-names(vstup_vse)[1]
  
  ciselnik_vzdelani<-vstup_vse[,c("cis_obec","cis_okrsek")]
  ciselnik_vzdelani<-merge(ciselnik_vzdelani,obce3_vzdelani,by="cis_obec")
  
  vel_okrsku<-ciselnik_okrsky[,c(1:3)]
  ciselnik_vzdelani<-merge(ciselnik_vzdelani,vel_okrsku,by=c("cis_obec","cis_okrsek"),all.x=T)
  
  # pocet_sectenych<-min(round(procent_secteno*n/100),n)
  # vstup<-vstup_vse[1:pocet_sectenych,]
  # names(vstup) <- tolower(names(vstup))
  # vstup[is.na(vstup)] <- 0
  
  
  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.x=T)
  
  ### Join ciselnik
  data <- merge(ciselnik_vzdelani, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
  indx_cluster <- grepl('comp', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kandidat <- grepl('kandidat_', colnames(data))
  name_kandidat <- colnames(data)[indx_kandidat]
  # 
  # # Přiřadíme rovnoměrně cluster neznámým okrskům
  # m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  # data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]
  # print("Neznámých: ")
  # print(sum(is.na(data$vel_okrsek)))
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kandidat][is.na(data[, indx_kandidat])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[is.na(data$vel_okrsek), 'vel_okrsek'] = data[is.na(data$vel_okrsek), 'zapsani_volici']
  data$id=paste0(data$cis_obec,data$cis_okrsek)
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kandidat), c("id", "secteno", "vel_okrsek", "zapsani_volici")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek', 'zapsani_volici'), name_kandidat))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru (rozlišujeme expected spočtené na základě číselníku okrsků a observed na základě počtu zapsaných voličů)
  data_melted$f_expected = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e_expected = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  data_melted$e_observed = data_melted$cluster_weight*data_melted$zapsani_volici*data_melted$secteno
  
  data_melted[is.na(data_melted$e_observed), 'e_observed'] = 0
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kandidat] = data_melted[, name_kandidat]*data_melted$cluster_weight
  
  # Agregace
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kandidat)], sum)
  vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
  #f_observed odhadneme na základě f_observed, e_observed, e_expected
  vysledky_clustery["f_observed"] = vysledky_clustery["f_expected"]*vysledky_clustery['e_observed']/vysledky_clustery['e_expected']
  
  
  # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
  vysledky_clustery[vysledky_clustery$e_observed==0, 'e_observed'] = 1
  
  vysledky_clustery$vaha = (vysledky_clustery$f_observed/vysledky_clustery$e_observed)#*(volicu_celkem/volicu_secteno)# konstantu zanedbáme
  
  # Navážit a sečíst
  pom = colSums(vysledky_clustery[, name_kandidat]*vysledky_clustery$vaha)
  vysledky = data.frame(pom, row.names=name_kandidat)
  vysledky["predikce_raw"] = vysledky/sum(vysledky)
  vysledky["kandidat"] = rownames(vysledky)
  rownames(vysledky) = c()
  
  # Současný stav
  vysledky["aktualne_hlasu"] = colSums(data[, name_kandidat])
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
  
  vysledky = vysledky[, c("kandidat", "predikce")]
  names(vysledky) = c("kandidat", "predikce_vzdelani")
  
  vysledky_vzdelani = vysledky  
  
  
  ####### Merge
  #vysledky = merge(merge(vysledky_strata, vysledky_kraje, by="kandidat"),vysledky_naivni,by="strana")
  vysledky1 = merge(vysledky_strata, vysledky_kraje, by="kandidat")
  vysledky2 = merge(vysledky_vek, vysledky_vzdelani, by="kandidat")
  vysledky = merge(vysledky1, vysledky2, by="kandidat")
  vysledky["procent_secteno"] = procent_secteno
  vysledky_vse = rbind(vysledky_vse, vysledky)
  
  
  
}




##### Zobrazení výsledků  #####
library(colorspace)
library(ggplot2)
library(scales)
 


plot_list = list()

### Vývoj tabulka_strany$barvy[tabulka_strany$strany21==strana]
#k<-1
for(k in 1:length(tabulka_kandidati$kandidati)){
  kandidat<-tabulka_kandidati$kandidati[k]
  # (barva<-barvicky[k])
  # print(barva)
  # print(k)
  vysledky_plot = vysledky_vse[vysledky_vse$kandidat==kandidat, ]
  
  plot <- ggplot(data=vysledky_plot[vysledky_plot$procent_secteno>=5, ])+
    geom_line(aes(x=procent_secteno/100, y=aktualne, color="Aktualne")) + 
    geom_line(aes(x=procent_secteno/100, y=predikce_strata, color="Predikce PSP volby 2017"),size=1.2) +
    geom_line(aes(x=procent_secteno/100, y=predikce_kraje, color="Predikce kraje")) + 
    geom_line(aes(x=procent_secteno/100, y=predikce_vek, color="Predikce vek")) +
    geom_line(aes(x=procent_secteno/100, y=predikce_vzdelani, color="Predikce vzdelani")) +
    ggtitle(tabulka_kandidati$jmena[k]) +
    geom_hline(yintercept = vysledky_plot[20,2],linetype = 3)+ #tabulka_strany$pruzkum[k]
    scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
    #geom_hline(yintercept = 0.05) + #opravit tabulka_strany$barvy[tabulka_strany$strany21==strana]
    xlab("Procent okrsku secteno") + ylab("") + labs(color = "") +
    scale_color_hue(limits=c("Aktualne","Predikce vek", "Predikce kraje","Predikce PSP volby 2017","Predikce vzdelani")) +
    theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))
  plot_list[[kandidat]] <- plot
}
plot_list[["kandidat_5"]]

# plot_list = list()
# 
# ### Vývoj
# 
# plot_list[["kstrana_1"]]
# #x<-vysledky_vse[vysledky_vse$strana=="kstrana_13", ]



### Souboj Zeman (7)/Drahos (9)
indx_predikce <- grepl('predikce', colnames(vysledky_vse))
name_predikce <- colnames(vysledky_vse)[indx_predikce]


vysledky_souboj = vysledky_vse[vysledky_vse$kandidat=="kandidat_7", c("procent_secteno", name_predikce, "aktualne")]
names(vysledky_souboj) = c("procent_secteno",paste(name_predikce,"Zeman"), "aktualne Zeman")

vysledky_souboj = merge(vysledky_souboj, vysledky_vse[vysledky_vse$kandidat=="kandidat_9", c("procent_secteno", name_predikce, "aktualne")])
names(vysledky_souboj) = c("procent_secteno",paste(name_predikce,"Zeman"), "aktualne Zeman",paste(name_predikce,"Drahos"), "aktualne Drahos")

vysledky_souboj["predikce_strata"] = vysledky_souboj["predikce_strata Zeman"] - vysledky_souboj["predikce_strata Drahos"]
vysledky_souboj["predikce_kraje"] = vysledky_souboj["predikce_kraje Zeman"] - vysledky_souboj["predikce_kraje Drahos"]
vysledky_souboj["predikce_vek"] = vysledky_souboj["predikce_vek Zeman"] - vysledky_souboj["predikce_vek Drahos"]
vysledky_souboj["predikce_vzdelani"] = vysledky_souboj["predikce_vzdelani Zeman"] - vysledky_souboj["predikce_vzdelani Drahos"]
vysledky_souboj["aktualne"] = vysledky_souboj["aktualne Zeman"] - vysledky_souboj["aktualne Drahos"]

plot_list[["souboj"]] <- ggplot(data=vysledky_souboj[vysledky_souboj$procent_secteno>=5, ])+
  geom_line(aes(x=procent_secteno/100, y=predikce_strata, color="Predikce PSP volby 2017"),size=1.2) +
  geom_line(aes(x=procent_secteno/100, y=predikce_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=predikce_vek, color="Predikce vek")) +
  geom_line(aes(x=procent_secteno/100, y=predikce_vek, color="Predikce vzdelani")) + 
  geom_line(aes(x=procent_secteno/100, y=aktualne, color="Aktualne")) + 
  ggtitle("Rozdil obdrzenych procent pro Zemana a Drahose") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
  geom_hline(yintercept = 0.12) + 
  scale_color_hue(limits=c("Aktualne","Predikce vek", "Predikce kraje","Predikce PSP volby 2017","Predikce vzdelani")) +
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

### Souboj Zeman (7)/Antizeman (1+2+3) WIP MA TO SMYSL???
# 
# vysledky_vetsina = vysledky_vse
# vysledky_vetsina["koalice"] = (vysledky_vetsina$strana=="kstrana_13") + (vysledky_vetsina$strana=="kstrana_17")
# 
# vysledky_vetsina["predikce_strata_adj"] = vysledky_vetsina$predikce_strata*(vysledky_vetsina$predikce_strata>=0.05)
# vysledky_vetsina["predikce_kraje_adj"] = vysledky_vetsina$predikce_kraje*(vysledky_vetsina$predikce_strata>=0.05)
# vysledky_vetsina["predikce_naivni_okrsky_adj"] = vysledky_vetsina$predikce_naivni_okrsky*(vysledky_vetsina$predikce_strata>=0.05)
# vysledky_vetsina["predikce_naivni_procenta_adj"] = vysledky_vetsina$predikce_naivni_procenta*(vysledky_vetsina$predikce_strata>=0.05)
# vysledky_vetsina["aktualne_adj"] = vysledky_vetsina$aktualne*(vysledky_vetsina$aktualne>=0.05)
# 
# vetsina = aggregate(cbind(predikce_strata_adj,predikce_kraje_adj,predikce_naivni_okrsky_adj,predikce_naivni_procenta_adj, aktualne_adj) ~ koalice + procent_secteno,vysledky_vetsina,  sum)
# vetsina_celkem = aggregate(cbind(predikce_strata_adj,predikce_kraje_adj,predikce_naivni_okrsky_adj,predikce_naivni_procenta_adj,predikce_kraje_adj, aktualne_adj) ~ procent_secteno,vetsina,  sum)
# names(vetsina_celkem) = c("procent_secteno", "predikce_strata_celkem","predikce_kraje_celkem","predikce_naivni_okrsky_celkem","predikce_naivni_procenta_celkem", "aktualne_celkem")
# vetsina = merge(vetsina, vetsina_celkem, by="procent_secteno")
# vetsina["pomer_predikce_strata"] = vetsina["predikce_strata_adj"]/vetsina["predikce_strata_celkem"]
# vetsina["pomer_predikce_kraje"] = vetsina["predikce_kraje_adj"]/vetsina["predikce_kraje_celkem"]
# vetsina["pomer_predikce_naivni_okrsky"] = vetsina["predikce_naivni_okrsky_adj"]/vetsina["predikce_naivni_okrsky_celkem"]
# vetsina["pomer_predikce_naivni_procenta"] = vetsina["predikce_naivni_procenta_adj"]/vetsina["predikce_naivni_procenta_celkem"]
# vetsina["pomer_aktualne"] = vetsina["aktualne_adj"]/vetsina["aktualne_celkem"]
# 
# plot_list[["vetsina"]] <- ggplot(data=vetsina[(vetsina$koalice==1) & (vetsina$procent_secteno>=5), ])+
#   geom_line(aes(x=procent_secteno/100, y=pomer_predikce_strata, color="Predikce strata (volby 2017)"),size=1.2) + 
#   geom_line(aes(x=procent_secteno/100, y=pomer_predikce_kraje, color="Predikce strata (kraje)")) + 
#   geom_line(aes(x=procent_secteno/100, y=pomer_predikce_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype = 2) + 
#   geom_line(aes(x=procent_secteno/100, y=pomer_predikce_naivni_procenta, color="Predikce naivni (procenta)"),linetype = 2) + 
#   geom_line(aes(x=procent_secteno/100, y=pomer_aktualne, color="Aktualne")) + 
#   ggtitle("Koalice proti zbytku: SPOLU+PirStan") +
#   scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
#   scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
#   xlab("Procent okrsku secteno") + ylab("Pomer hlasu") + labs(color = "") + 
#   scale_color_hue(limits=c("Aktualne","Predikce naivni (okrsky)", "Predikce strata (kraje)","Predikce strata (volby 2017)","Predikce naivni (procenta)")) +
#   geom_hline(yintercept = 0.5) + 
#   theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))


### Rychlost konvergence - l^2 


vysledky_rychlost= vysledky_vse #[vysledky_vse$strana %in% strany21,]

vysledky_rychlost["vysledek"]=rep(vysledky_rychlost[vysledky_rychlost$procent_secteno==100,"aktualne"],times=20)
vysledky_rychlost["rozdil_strata"] = vysledky_rychlost$predikce_strata-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_kraje"] =  vysledky_rychlost$predikce_kraje-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_vek"] = vysledky_rychlost$predikce_vek-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_vzdelani"] =  vysledky_rychlost$predikce_vzdelani-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_aktualne"] =  vysledky_rychlost$aktualne-vysledky_rychlost$vysledek

normF<-function(x){ norm(matrix(x,nrow=1),type="F")}

rychlost = aggregate(cbind(rozdil_strata,rozdil_kraje,rozdil_vek,rozdil_vzdelani,rozdil_aktualne) ~ procent_secteno,vysledky_rychlost,  FUN=normF)

plot_list[["rychlost"]] <- ggplot(data=rychlost)+
  geom_line(aes(x=procent_secteno/100, y=rozdil_strata, color="Predikce PSP volby 2017"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vzdelani, color="Predikce vzdelani")) +
  geom_line(aes(x=procent_secteno/100, y=rozdil_aktualne, color="Aktualne")) + 
  ggtitle("Vzdalenost vysledku pro ruzne metody") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  scale_color_hue(limits=c("Aktualne","Predikce vek", "Predikce kraje","Predikce PSP volby 2017","Predikce vzdelani")) +
  xlab("Procent okrsku secteno") + ylab("Vzdalenost") + labs(color = "") + geom_hline(yintercept = 0) +
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["rychlost"]] 


### Rychlost konvergence - prumer


vysledky_rychlost2= vysledky_rychlost #[vysledky_vse$strana %in% strany21,]
vysledky_rychlost2[,2:13]=abs(vysledky_rychlost2[,2:13])

rychlost2 = aggregate(cbind(rozdil_strata,rozdil_kraje,rozdil_vek,rozdil_vzdelani,rozdil_aktualne) ~ procent_secteno,vysledky_rychlost2,  FUN=mean)

plot_list[["rychlost2"]] <- ggplot(data=rychlost2)+
  geom_line(aes(x=procent_secteno/100, y=rozdil_strata, color="Predikce PSP volby 2017"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vek, color="Predikce vek")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_vzdelani, color="Predikce vzdelani")) +
  geom_line(aes(x=procent_secteno/100, y=rozdil_aktualne, color="Aktualne")) + 
  ggtitle("Prumerna chyba pro ruzne metody") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("Vzdalenost") + labs(color = "") + geom_hline(yintercept = 0) +
  scale_color_hue(limits=c("Aktualne","Predikce vek", "Predikce kraje","Predikce PSP volby 2017","Predikce vzdelani")) +
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["rychlost"]]


#cairo_pdf('/Users/admin/Documents/volebni_predikce/testovani/test_aposteriori.pdf', family="DejaVu Sans")
#pdf("/Users/admin/Documents/volebni_predikce/testovani/test_aposteriori_upravene.pdf", encoding="ISOLatin2.enc")
pdf("prezident2018/test_prez2018_all.pdf", encoding="ISOLatin2.enc",width=6.5,height = 5)
plot_list
dev.off()


save(vysledky_vse, vysledky_souboj,rychlost,rychlost2, file = "prezident2018/data_prez2018_all.RData")

#load("aposteriori_analyza_data.RData")
