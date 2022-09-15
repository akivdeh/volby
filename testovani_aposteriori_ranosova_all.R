
#Sys.setlocale(category = 'LC_CTYPE','en_US.UTF-8')

rm(list=ls());

library(reshape2)
workdir <- "/Users/User/Desktop/MFF/sfg_volby/reseni_0922/"

###### Set up #####

start <- 5
stop <- 100

vstup_vse<-read.table("pscr_2021.csv", sep=",", header = T)
old <- read.table(paste0(workdir, "psp17okrs.csv"), sep=",", header=T);

vstup_vse[is.na(vstup_vse)] <- 0
old[is.na(old)] <- 0
names(vstup_vse) <- tolower(names(vstup_vse));
names(old) <- tolower(names(old));

vstup_vse$id<- paste0(vstup_vse$cis_obec, vstup_vse$cis_okrsek)
old$id<-paste0(old$cis_obec, old$cis_okrsek)


n<-dim(vstup_vse)[1]

old$kstrana_spolu<-old$kstrana_1+old$kstrana_20+old$kstrana_24
old$kstrana_pirstan<-old$kstrana_15+old$kstrana_7

strany21<-c("kstrana_20","kstrana_13","kstrana_17",
            "kstrana_4","kstrana_5","kstrana_18",
            "kstrana_12","kstrana_8")
strany17<-c("kstrana_21","kstrana_spolu","kstrana_pirstan",
            "kstrana_29","kstrana_4","kstrana_8",
            "kstrana_21","kstrana_12")
jmena<-c("ANO","SPOLU","PirStan",
         "SPD","CSSD","KSCM",
         "Prisaha","Trikolora")
tabulka_strany<-as.data.frame(cbind(jmena,strany17,strany21))



### Pouzit jen posledni zaznam pro dane id
okrsky_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, old, FUN = max)
okrsky <- merge(old, okrsky_posledni, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.y=T)

temp_strany <- colnames(okrsky)[grepl('kstrana', colnames(okrsky))]
temp <- okrsky[rowSums(okrsky[, temp_strany]) > 0, ] # odstranit záznamy bez hlasů
temp_id <- temp[, c("cis_obec", "cis_okrsek")]
temp_vysledky <- temp[, temp_strany]
celkove_vysledky<-colSums(temp_vysledky)
celkove_vysledky_procenta<-celkove_vysledky/sum(celkove_vysledky[1:31])
tabulka_strany$vysledky<-celkove_vysledky[tabulka_strany$strany17]
tabulka_strany$vysledkyproc<-celkove_vysledky_procenta[tabulka_strany$strany17]


###### Spustit predikci ###### 
vysledky_vse = data.frame(strana=character(), aktualne=numeric(), predikce_strata=numeric(),predikce_kraje=numeric(), predikce_naivni_okrsky=numeric(),predikce_naivni_procenta=numeric(), procent_secteno=numeric())

procent_secteno<-15

for (procent_secteno in seq(start, stop, by=5)) {
  ######## 
  ######## Multinom 7 clusteru ######
  
  # Nacti
  #            load("/Users/admin/Desktop/reseni_0922/testovani/ciselnik17.RData")
  load("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/ciselnik17.RData")
  # Nacti
  #vstup <- read.table(paste0(paste0("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/secteno_upravene/secteno", as.character(procent_secteno)), ".csv"), sep=",", header = T)
  
  #vstup1 <- read.table(paste0(paste0("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/secteno_upravene/secteno", as.character(procent_secteno)), ".csv"), sep=",", header = T)
  #pocet_sectenych<-min(800*procent_secteno/5,n)
  pocet_sectenych<-min(round(procent_secteno*n/100),n)
  vstup<-vstup_vse[1:pocet_sectenych,]
  # Zpracuj
  vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek+id, vstup, FUN = max)
  vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac","id"), all.x=T)
  
  ### Join ciselnik
  data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek","id"), all=T);
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
  #data_melted$f_observed = ifelse(data_melted$secteno==1,data_melted$e_observed,data_melted$f)
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kstrana] = data_melted[, name_kstrana]*data_melted$cluster_weight
  
  # Agregace
  #vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "f_observed", "e_expected", "e_observed", name_kstrana)], sum)
  
  
  vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", "f_expected", "e_expected", "e_observed", name_kstrana)], sum)
  #vysledky_clustery[vysledky_clustery["e_expected"]==0, 'e_expected'] = 1 #můžu dát libovolnou hodnotu, jde o situaci, kdy se v clusteru nenašly hlasy, tedy je jedno, jakou váhou pak tyto žádné hlasy budu vážit
  
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
  
  ######## Kraje #####
  
  #            load("/Users/admin/Desktop/reseni_0922/testovani/ciselnikKraje.RData")
  load("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/ciselnikKraje.RData")
  # Nacti
  #vstup <- read.table(paste0(paste0("/Users/User/Desktop/MFF/sfg_volby/reseni_0922/testovani/secteno_upravene/secteno", as.character(procent_secteno)), ".csv"), sep=",", header = T)

  
  data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek","id"), all=T);
  indx_cluster <- grepl('cluster', colnames(data))
  name_cluster <- colnames(data)[indx_cluster]
  indx_kstrana <- grepl('kstrana_', colnames(data))
  name_kstrana <- colnames(data)[indx_kstrana]
  
  # Přiřadíme rovnoměrně cluster neznámým okrskům
  m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
  data[is.na(data$kraj), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$kraj))), ]
  
  ### Nepozorovaným okrskům dosaď nuly
  data[, indx_kstrana][is.na(data[, indx_kstrana])] <- 0
  data$secteno <- as.numeric(!is.na(data$platne_hlasy))
  
  ### Pozorovaným okrskům aktualizuj velikost obce
  data[!is.na(data$zapsani_volici), 'vel_okrsek'] = data[!is.na(data$zapsani_volici), 'zapsani_volici']
  
  ### Drop data
  data = subset(data, select=c(c(name_cluster, name_kstrana), c("id", "secteno", "vel_okrsek")))
  
  ### Vypocet vah
  
  # Pro kazdy okrsek dame do radku vsechny vahy pro clustery
  data_melted = melt(data, id.vars=c(c("id", "secteno", 'vel_okrsek'), name_kstrana))
  names(data_melted)[names(data_melted)=="variable"] = "cluster_name"
  names(data_melted)[names(data_melted)=="value"] = "cluster_weight"
  
  # f=velikost clusteru, e=velikost již sečtené části clusteru
  data_melted$f = data_melted$cluster_weight*data_melted$vel_okrsek
  data_melted$e = data_melted$cluster_weight*data_melted$vel_okrsek*data_melted$secteno
  
  # Navážit výsledky z okrsku váhou clusterů
  data_melted[, name_kstrana] = data_melted[, name_kstrana]*data_melted$cluster_weight
  
  # Agregovat
  volicu_celkem <- sum(data$vel_okrsek)
  volicu_secteno <- sum(data$vel_okrsek*data$secteno)
  
  if (volicu_secteno == 0) {
    print("Nejsou sectene zadne hlasy.")
  } else {
    vysledky_clustery = aggregate(. ~ cluster_name, data_melted[, c("cluster_name", name_kstrana, "f", "e")], sum)
    
    # Vahy kde jsem delil nulou (zadna pozorovani v clusteru) nastavim na f (stejne pak nepribudou zadne hlasy z tohoto clusteru)
    vysledky_clustery[vysledky_clustery$e==0, 'e'] = 1
    
    vysledky_clustery$vaha = (vysledky_clustery$f/vysledky_clustery$e)*(volicu_celkem/volicu_secteno)
    
    # Navážit a sečíst
    pom = colSums(vysledky_clustery[, name_kstrana]*vysledky_clustery$vaha)
    vysledky = data.frame(pom, row.names=name_kstrana)
    vysledky["predikce_raw"] = vysledky/sum(vysledky)
    vysledky["strana"] = rownames(vysledky)
    rownames(vysledky) = c()
    
    # Současný stav
    vysledky["aktualne_hlasu"] = colSums(data[, name_kstrana])
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
    
    vysledky = vysledky[, c("strana", "predikce")]
    names(vysledky) = c("strana", "predikce_kraje")
  }
  
  vysledky_kraje = vysledky
  ###### Naivni ######
  # Zpracuj
  vysledky21<-colSums(vstup_posledni[,name_kstrana])
  procenta21<-vysledky21/sum(vysledky21)
  
  aktualni<-procenta21
  vysledky_okrsky<-procenta21
  vysledky_procenta<-procenta21
    
  vysledky21<-vysledky21[tabulka_strany$strany21]
  procenta21<-procenta21[tabulka_strany$strany21]
  #celkem21<-sum(vstup_posledni[,"platne_hlasy"])
  # names(vysledky21)<-tabulka_strany$jmena
  # names(procenta21)<-tabulka_strany$jmena
  
  vysledky17_okrsky<-colSums(okrsky[okrsky$id %in% vstup_posledni$id,temp_strany])
  procenta17_okrsky<-vysledky17_okrsky/sum(vysledky17_okrsky[1:31])
  vysledky17_okrsky<-vysledky17_okrsky[tabulka_strany$strany17]
  procenta17_okrsky<-procenta17_okrsky[tabulka_strany$strany17]
  # names(vysledky17_okrsky)<-tabulka_strany$jmena
  # names(procenta17_okrsky)<-tabulka_strany$jmena
  #celkem17<-sum(old[,"platne_hlasy"])
  # 
  # indx_kstrana <- grepl('kstrana_', colnames(data))
  # name_kstrana <- colnames(data)[indx_kstrana]
  
  old1<-old[1:round(procent_secteno*(length(old$cis_obec))/100),]
  
  okrsky_posledni1 <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, old1, FUN = max)
  okrsky1 <- merge(old1, okrsky_posledni1, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.y=T)
  
  
  
  vysledky17_procenta<-colSums(okrsky1[,temp_strany])
  procenta17_procenta<-vysledky17_procenta/sum(vysledky17_procenta[1:31])
  vysledky17_procenta<-vysledky17_procenta[tabulka_strany$strany17]
  procenta17_procenta<-procenta17_procenta[tabulka_strany$strany17]
  # names(vysledky17_procenta)<-tabulka_strany$jmena
  # names(procenta17_procenta)<-tabulka_strany$jmena  
  
  vys_okrsky<-vysledky21/vysledky17_okrsky*tabulka_strany$vysledky
  odhadcelku<-sum(vys_okrsky)*(1/sum(procenta21))
  vys_okrsky2<-vys_okrsky/odhadcelku
  # vyspred_p<-vys/celkem17
  
  vys_procenta<-procenta21/procenta17_procenta*tabulka_strany$vysledkyproc
  # vyspred2_p<-vys2/celkem17
  #vysakt_p<-procenta21
  
  
  vysledky_okrsky[tabulka_strany$strany21]<-vys_okrsky2
  vysledky_procenta[tabulka_strany$strany21]<-vys_procenta

  vysledky<-data.frame(strana=names(aktualni), aktualne=aktualni,
                       predikce_raw=vysledky_okrsky, predikce_raw2=vysledky_procenta)
  rownames(vysledky)<-c()
  n_start = 14000
  n_stop = 15000
  alpha = (nrow(vstup_posledni) - n_start)/(n_stop-n_start)
  vysledky["predikce"] = vysledky["predikce_raw"]
  vysledky["predikce2"] = vysledky["predikce_raw2"]
  if (alpha > 1) { # věříme aktuálním výsledkům
    vysledky["predikce"] = vysledky["aktualne"]
    vysledky["predikce2"] = vysledky["aktualne"]
    
  } else if (alpha > 0) { # blížíme se k aktuálním výsledkům
    vysledky["predikce"] = (1-alpha)*vysledky["predikce_raw"] + alpha*vysledky["aktualne"]
    vysledky["predikce2"] = (1-alpha)*vysledky["predikce_raw2"] + alpha*vysledky["aktualne"]
  }
  
  
  
  vysledky_naivni = vysledky[, c("strana", "predikce","predikce2")]
  names(vysledky_naivni) = c("strana", "predikce_naivni_okrsky","predikce_naivni_procenta")
  
  
  
  # 
  # 
  # docasne<-vysledky_strata
  # docasne$predikce_strata=docasne$aktualne
  # names(docasne)=c("strana","predikce_naivni_okrsky","predikce_naivni_procenta")
  # 
  # docasne[docasne$strana %in% vysledky$strana,]=vysledky
  # vysledky_naivni=docasne
  
  ####### Merge
  vysledky = merge(merge(vysledky_strata, vysledky_kraje, by="strana"),vysledky_naivni,by="strana")
  vysledky["procent_secteno"] = procent_secteno
  vysledky_vse = rbind(vysledky_vse, vysledky)
 
 
  
}




##### Zobrazení výsledků  #####
library(colorspace)
library(ggplot2)
library(scales)

tabulka_strany$velke<-(tabulka_strany$strany21 %in%
                         c("kstrana_13", "kstrana_17", "kstrana_20", "kstrana_4"))


# # velke_strany <- c("kstrana_13", "kstrana_17", "kstrana_20",
# #                   "kstrana_4", "kstrana_5", "kstrana_18",
# #                   "kstrana_12", "kstrana_8", "kstrana_1")
# velke_strany<-c("kstrana_13", "kstrana_17", "kstrana_20", "kstrana_4")
# male_strany<-c("kstrana_5", "kstrana_18","kstrana_12", "kstrana_8", "kstrana_1")
# strany<-c(velke_strany,male_strany)  
# 
# 
# nazvy_strany <- list()
# 
# nazvy_strany[["kstrana_13"]] = "SPOLU" #velke strany
# nazvy_strany[["kstrana_17"]] = "PirSTAN"
# nazvy_strany[["kstrana_20"]] = "ANO"
# nazvy_strany[["kstrana_4"]] = "SPD"
# 
# nazvy_strany[["kstrana_5"]] = "CSSD" #male strany (boj o 3-5 procent)
# nazvy_strany[["kstrana_18"]] = "KSCM"
# nazvy_strany[["kstrana_12"]] = "Prisaha"
# nazvy_strany[["kstrana_8"]] = "Trikolora"
# nazvy_strany[["kstrana_1"]] = "Zeleni"
# 
# pruzkum <- list()
# 
# pruzkum[["kstrana_13"]] = 0.214 #velke strany
# pruzkum[["kstrana_17"]] = 0.174
# pruzkum[["kstrana_20"]] = 0.273
# pruzkum[["kstrana_4"]]  = 0.123
# 
# pruzkum[["kstrana_5"]] = 0.044 #male strany (boj o 3-5 procent)
# pruzkum[["kstrana_18"]] =0.065
# pruzkum[["kstrana_12"]] = 0.057
# pruzkum[["kstrana_8"]] = 0.018
# pruzkum[["kstrana_1"]] = 0.016
# 
# 
tabulka_strany$pruzkum<-c(0.273,0.214,0.174,0.123,
                          0.044,0.065,0.057,0.018)
tabulka_strany$barvy<-c("Teal", "Blues 3","YlGn","YlOrBr",
                        "Oranges","Red-Yellow","Blues 2",
                        "Plasma")


plot_list = list()

### Vývoj tabulka_strany$barvy[tabulka_strany$strany21==strana]

for(k in 1:length(tabulka_strany$strany21)){
  strana<-tabulka_strany$strany21[k]
  # (barva<-barvicky[k])
  # print(barva)
  # print(k)
  vysledky_plot = vysledky_vse[vysledky_vse$strana==strana, ]
  
  plot <- ggplot(data=vysledky_plot[vysledky_plot$procent_secteno>=5, ])+
    geom_line(aes(x=procent_secteno/100, y=aktualne, color="Aktualne")) + 
    geom_line(aes(x=procent_secteno/100, y=predikce_strata, color="Predikce strata"),size=1.2) +
    geom_line(aes(x=procent_secteno/100, y=predikce_kraje, color="Predikce kraje")) + 
    geom_line(aes(x=procent_secteno/100, y=predikce_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype = 2) +
    geom_line(aes(x=procent_secteno/100, y=predikce_naivni_procenta, color="Predikce naivni (procenta)"),linetype = 2) +
    ggtitle(tabulka_strany$jmena[k]) +
    geom_hline(yintercept = vysledky_plot[20,2],linetype = 3)+ #tabulka_strany$pruzkum[k]
    scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
    #geom_hline(yintercept = 0.05) + #opravit tabulka_strany$barvy[tabulka_strany$strany21==strana]
    xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
    theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))
  
  if (!tabulka_strany$velke[k]){
    plot<-plot+ geom_hline(yintercept = 0.05)
  }
  plot_list[[strana]] <- plot
}
plot_list[["kstrana_13"]]

# 
# plot_list[["kstrana_20"]] <-  plot_list[["kstrana_20"]]+
#   scale_color_discrete_sequential(palette = "Teal", nmax=7,order=7:1)
# plot_list[["kstrana_13"]] <-  plot_list[["kstrana_13"]]+
#   scale_color_discrete_sequential(palette = "Blues 3", nmax=7,order=7:1)
# plot_list[["kstrana_17"]] <-  plot_list[["kstrana_17"]]+
#   scale_color_discrete_sequential(palette = "YlGn", nmax=7,order=7:1)
# plot_list[["kstrana_4"]] <-  plot_list[["kstrana_4"]]+
#   scale_color_discrete_sequential(palette = "YlGn", nmax=7,order=7:1)
# 
# plot_list[["kstrana_5"]] <-  plot_list[["kstrana_5"]]+
#   scale_color_discrete_sequential(palette = "Oranges", nmax=7,order=7:1)
# plot_list[["kstrana_18"]] <-  plot_list[["kstrana_18"]]+
#   scale_color_discrete_sequential(palette = "Red-Yellow", nmax=7,order=7:1)
# plot_list[["kstrana_12"]] <-  plot_list[["kstrana_12"]]+
#   scale_color_discrete_sequential(palette = "Blues 2", nmax=7,order=7:1)
# plot_list[["kstrana_8"]] <-  plot_list[["kstrana_8"]]+
#   scale_color_discrete_sequential(palette = "Plasma", nmax=7,order=7:1)
# 

# plot_list = list()
# 
# ### Vývoj
# 
# plot_list[["kstrana_1"]]
# #x<-vysledky_vse[vysledky_vse$strana=="kstrana_13", ]



### Souboj ANO/SPOLU


vysledky_souboj = vysledky_vse[vysledky_vse$strana=="kstrana_13", c("procent_secteno", "predikce_strata","predikce_kraje","predikce_naivni_okrsky","predikce_naivni_procenta", "aktualne")]
names(vysledky_souboj) = c("procent_secteno", "predikce__strata_spolu","predikce_kraje_spolu","predikce_naivni_okrsky_spolu","predikce_naivni_procenta_spolu", "aktualne_spolu")

vysledky_souboj = merge(vysledky_souboj, vysledky_vse[vysledky_vse$strana=="kstrana_20", c("procent_secteno", "predikce_strata","predikce_kraje","predikce_naivni_okrsky","predikce_naivni_procenta", "aktualne")])
names(vysledky_souboj) = c("procent_secteno", "predikce_strata_spolu","predikce_kraje_spolu","predikce_naivni_okrsky_spolu","predikce_naivni_procenta_spolu", "aktualne_spolu",
                           "predikce_strata_ano","predikce_kraje_ano","predikce_naivni_okrsky_ano","predikce_naivni_procenta_ano", "aktualne_ano")

vysledky_souboj["predikce_strata"] = vysledky_souboj["predikce_strata_ano"] - vysledky_souboj["predikce_strata_spolu"]
vysledky_souboj["predikce_kraje"] = vysledky_souboj["predikce_kraje_ano"] - vysledky_souboj["predikce_kraje_spolu"]
vysledky_souboj["predikce_naivni_okrsky"] = vysledky_souboj["predikce_naivni_okrsky_ano"] - vysledky_souboj["predikce_naivni_okrsky_spolu"]
vysledky_souboj["predikce_naivni_procenta"] = vysledky_souboj["predikce_naivni_procenta_ano"] - vysledky_souboj["predikce_naivni_procenta_spolu"]
vysledky_souboj["aktualne"] = vysledky_souboj["aktualne_ano"] - vysledky_souboj["aktualne_spolu"]

plot_list[["souboj"]] <- ggplot(data=vysledky_souboj[vysledky_souboj$procent_secteno>=5, ])+
  geom_line(aes(x=procent_secteno/100, y=predikce_strata, color="Predikce strata"),size=1.2) +
  geom_line(aes(x=procent_secteno/100, y=predikce_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=predikce_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype = 2) +
  geom_line(aes(x=procent_secteno/100, y=predikce_naivni_procenta, color="Predikce naivni (procenta)"),linetype = 2) + 
  geom_line(aes(x=procent_secteno/100, y=aktualne, color="Aktualne")) + 
  ggtitle("Rozdil obdrzenych procent pro ANO a SPOLU") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("") + labs(color = "") + 
  geom_hline(yintercept = 0) + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))


### Většina ve sněmovně

vysledky_vetsina = vysledky_vse
vysledky_vetsina["koalice"] = (vysledky_vetsina$strana=="kstrana_13") + (vysledky_vetsina$strana=="kstrana_17")

vysledky_vetsina["predikce_strata_adj"] = vysledky_vetsina$predikce_strata*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["predikce_kraje_adj"] = vysledky_vetsina$predikce_kraje*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["predikce_naivni_okrsky_adj"] = vysledky_vetsina$predikce_naivni_okrsky*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["predikce_naivni_procenta_adj"] = vysledky_vetsina$predikce_naivni_procenta*(vysledky_vetsina$predikce_strata>=0.05)
vysledky_vetsina["aktualne_adj"] = vysledky_vetsina$aktualne*(vysledky_vetsina$aktualne>=0.05)

vetsina = aggregate(cbind(predikce_strata_adj,predikce_kraje_adj,predikce_naivni_okrsky_adj,predikce_naivni_procenta_adj, aktualne_adj) ~ koalice + procent_secteno,vysledky_vetsina,  sum)
vetsina_celkem = aggregate(cbind(predikce_strata_adj,predikce_kraje_adj,predikce_naivni_okrsky_adj,predikce_naivni_procenta_adj,predikce_kraje_adj, aktualne_adj) ~ procent_secteno,vetsina,  sum)
names(vetsina_celkem) = c("procent_secteno", "predikce_strata_celkem","predikce_kraje_celkem","predikce_naivni_okrsky_celkem","predikce_naivni_procenta_celkem", "aktualne_celkem")
vetsina = merge(vetsina, vetsina_celkem, by="procent_secteno")
vetsina["pomer_predikce_strata"] = vetsina["predikce_strata_adj"]/vetsina["predikce_strata_celkem"]
vetsina["pomer_predikce_kraje"] = vetsina["predikce_kraje_adj"]/vetsina["predikce_kraje_celkem"]
vetsina["pomer_predikce_naivni_okrsky"] = vetsina["predikce_naivni_okrsky_adj"]/vetsina["predikce_naivni_okrsky_celkem"]
vetsina["pomer_predikce_naivni_procenta"] = vetsina["predikce_naivni_procenta_adj"]/vetsina["predikce_naivni_procenta_celkem"]
vetsina["pomer_aktualne"] = vetsina["aktualne_adj"]/vetsina["aktualne_celkem"]

plot_list[["vetsina"]] <- ggplot(data=vetsina[(vetsina$koalice==1) & (vetsina$procent_secteno>=5), ])+
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_strata, color="Predikce strata"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype = 2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_naivni_procenta, color="Predikce naivni (procenta)"),linetype = 2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_aktualne, color="Aktualne")) + 
  ggtitle("Koalice proti zbytku: pomer hlasu pro SPOLU+PirSTAN ve snemovne") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("Pomer hlasu") + labs(color = "") + 
  geom_hline(yintercept = 0.5) + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))


### Bývalá vladní většina ve sněmovně (ANO, CSSD, KSCM)


vysledky_vlada = vysledky_vse
vysledky_vlada["koalice"] = (vysledky_vlada$strana=="kstrana_20") + (vysledky_vlada$strana=="kstrana_5")+ (vysledky_vlada$strana=="kstrana_18")

vysledky_vlada["predikce_strata_adj"] = vysledky_vlada$predikce_strata*(vysledky_vlada$predikce_strata>=0.05)
vysledky_vlada["predikce_kraje_adj"] = vysledky_vlada$predikce_kraje*(vysledky_vlada$predikce_strata>=0.05)
vysledky_vlada["predikce_naivni_okrsky_adj"] = vysledky_vlada$predikce_naivni_okrsky*(vysledky_vlada$predikce_strata>=0.05)
vysledky_vlada["predikce_naivni_procenta_adj"] = vysledky_vlada$predikce_naivni_procenta*(vysledky_vlada$predikce_strata>=0.05)

vysledky_vlada["aktualne_adj"] = vysledky_vlada$aktualne*(vysledky_vlada$aktualne>=0.05)

vlada = aggregate(cbind(predikce_strata_adj,predikce_kraje_adj,predikce_naivni_okrsky_adj,predikce_naivni_procenta_adj, aktualne_adj) ~ koalice + procent_secteno,vysledky_vlada,  sum)
vlada_celkem = aggregate(cbind(predikce_strata_adj,predikce_kraje_adj,predikce_naivni_okrsky_adj,predikce_naivni_procenta_adj, aktualne_adj) ~ procent_secteno,vlada,  sum)
names(vlada_celkem) = c("procent_secteno", "predikce_strata_celkem","predikce_kraje_celkem" ,"predikce_naivni_okrsky_celkem" ,"predikce_naivni_procenta_celkem", "aktualne_celkem")
vlada = merge(vlada, vlada_celkem, by="procent_secteno")
vlada["pomer_predikce_strata"] = vlada["predikce_strata_adj"]/vlada["predikce_strata_celkem"]
vlada["pomer_predikce_kraje"] = vlada["predikce_kraje_adj"]/vlada["predikce_kraje_celkem"]
vlada["pomer_predikce_naivni_okrsky"] = vlada["predikce_naivni_okrsky_adj"]/vlada["predikce_naivni_okrsky_celkem"]
vlada["pomer_predikce_naivni_procenta"] = vlada["predikce_naivni_procenta_adj"]/vlada["predikce_naivni_procenta_celkem"]
vlada["pomer_aktualne"] = vlada["aktualne_adj"]/vlada["aktualne_celkem"]

plot_list[["vlada"]] <- ggplot(data=vlada[(vlada$koalice==1) & (vlada$procent_secteno>=5), ])+
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_strata, color="Predikce strata"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype=2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_predikce_naivni_procenta, color="Predikce naivni (procenta)"),linetype=2) + 
  geom_line(aes(x=procent_secteno/100, y=pomer_aktualne, color="Aktualne")) + 
  ggtitle("Strany minulé vládní většiny: ANO, ČSSD, KSČM") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("Pomer hlasu") + labs(color = "") + 
  geom_hline(yintercept = 0.5) + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

### Rychlost konvergence - l^2 


vysledky_rychlost= vysledky_vse[vysledky_vse$strana %in% strany21,]

vysledky_rychlost["vysledek"]=rep(vysledky_rychlost[vysledky_rychlost$procent_secteno==100,"aktualne"],times=20)
vysledky_rychlost["rozdil_strata"] = vysledky_rychlost$predikce_strata-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_kraje"] =  vysledky_rychlost$predikce_kraje-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_naivni_okrsky"] = vysledky_rychlost$predikce_naivni_okrsky-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_naivni_procenta"] =  vysledky_rychlost$predikce_naivni_procenta-vysledky_rychlost$vysledek
vysledky_rychlost["rozdil_aktualne"] =  vysledky_rychlost$aktualne-vysledky_rychlost$vysledek

normF<-function(x){ norm(matrix(x,nrow=1),type="F")}

rychlost = aggregate(cbind(rozdil_strata,rozdil_kraje,rozdil_naivni_okrsky,rozdil_naivni_procenta,rozdil_aktualne) ~ procent_secteno,vysledky_rychlost,  FUN=normF)

plot_list[["rychlost"]] <- ggplot(data=rychlost)+
  geom_line(aes(x=procent_secteno/100, y=rozdil_strata, color="Predikce strata"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype=2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_naivni_procenta, color="Predikce naivni (procenta)"),linetype=2) +
  geom_line(aes(x=procent_secteno/100, y=rozdil_aktualne, color="Aktualni vysledky"),) + 
  ggtitle("Vzdalenost vysledku pro ruzne metody") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("Vzdalenost") + labs(color = "") + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["rychlost"]] 


### Rychlost konvergence - prumer


vysledky_rychlost2= vysledky_vse[vysledky_vse$strana %in% strany21,]

vysledky_rychlost2["vysledek"]=rep(vysledky_rychlost[vysledky_rychlost$procent_secteno==100,"aktualne"],times=20)
vysledky_rychlost2["rozdil_strata"] = abs(vysledky_rychlost$predikce_strata-vysledky_rychlost$vysledek)
vysledky_rychlost2["rozdil_kraje"] =  abs(vysledky_rychlost$predikce_kraje-vysledky_rychlost$vysledek)
vysledky_rychlost2["rozdil_naivni_okrsky"] = abs(vysledky_rychlost$predikce_naivni_okrsky-vysledky_rychlost$vysledek)
vysledky_rychlost2["rozdil_naivni_procenta"] =  abs(vysledky_rychlost$predikce_naivni_procenta-vysledky_rychlost$vysledek)
vysledky_rychlost2["rozdil_aktualne"] =  abs(vysledky_rychlost$aktualne-vysledky_rychlost$vysledek)

rychlost2 = aggregate(cbind(rozdil_strata,rozdil_kraje,rozdil_naivni_okrsky,rozdil_naivni_procenta,rozdil_aktualne) ~ procent_secteno,vysledky_rychlost2,  FUN=mean)

plot_list[["rychlost2"]] <- ggplot(data=rychlost2)+
  geom_line(aes(x=procent_secteno/100, y=rozdil_strata, color="Predikce strata"),size=1.2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_kraje, color="Predikce kraje")) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_naivni_okrsky, color="Predikce naivni (okrsky)"),linetype=2) + 
  geom_line(aes(x=procent_secteno/100, y=rozdil_naivni_procenta, color="Predikce naivni (procenta)"),linetype=2) +
  geom_line(aes(x=procent_secteno/100, y=rozdil_aktualne, color="Aktualni vysledky"),) + 
  ggtitle("Průměrná chyba pro ruzne metody") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 0.1L)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1L), breaks = seq(0.05, 1, by=0.1)) +
  xlab("Procent okrsku secteno") + ylab("Vzdalenost") + labs(color = "") + 
  theme(legend.position = "bottom")+guides(color=guide_legend(nrow=2,byrow=FALSE))

plot_list[["rychlost2"]]


#cairo_pdf('/Users/admin/Documents/volebni_predikce/testovani/test_aposteriori.pdf', family="DejaVu Sans")
#pdf("/Users/admin/Documents/volebni_predikce/testovani/test_aposteriori_upravene.pdf", encoding="ISOLatin2.enc")
pdf("obrazky/test_aposteriori_upravene_ran_all_new.pdf", encoding="ISOLatin2.enc")
plot_list
dev.off()


save(vysledky_vse, vysledky_souboj, vetsina,vlada,rychlost,rychlost2, file = "vysledky/aposteriori_analyza_data_upravene_ran2.RData")

#load("aposteriori_analyza_data.RData")
