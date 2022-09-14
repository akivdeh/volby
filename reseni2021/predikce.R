
##############################
# Model pro predikci vysledku#
# Ondrej Tybl, Zari 2021    ##
# tybl@karlin.mff.cuni.cz   ##
##############################

library(reshape2)
library(RJSONIO)

rm(list=ls());

#workdir <- "/Users/admin/Desktop/reseni_0922/"
workdir <- "/Users/User/Desktop/MFF/sfg_volby/reseni_0922/"

### Nahrani dat

load(paste0(workdir, "ciselnik.RData"))

f <- file(paste0(workdir, "psp17okrs.txt"))
#f <- file(paste0(workdir, "psp17okrs.txt"))
#f <- file("stdin")
#open(f, mode = "r")


vstup_raw = readLines(f)
for (i in 1:length(vstup_raw)) {
  vstup_raw[i] = paste0(vstup_raw[i], ",POM")
}

vstup <- strsplit(vstup_raw, ",")
vstup <- do.call(rbind, vstup)
vstup <- as.data.frame(vstup)

names(vstup) <- gsub('"', '', strsplit(vstup_raw[1], ',')[[1]])
names(vstup) <- tolower(names(vstup))
vstup <- subset(vstup, select = -c(pom, datum_cas_zprac))
vstup <- vstup[2:nrow(vstup), ]
vstup[] <- lapply(vstup, function(x) as.numeric(as.character(x)))
vstup[is.na(vstup)] <- 0

### Pouzit jen posledni zaznam pro dane id
vstup_posledni <- aggregate(poradi_zprac ~ cis_obec + cis_okrsek, vstup, FUN = max)
vstup_posledni <- merge(vstup_posledni, vstup, by=c("cis_obec", "cis_okrsek", "poradi_zprac"), all.x=T)

### Join ciselnik
data <- merge(ciselnik_okrsky, vstup_posledni, by=c("cis_obec", "cis_okrsek"), all=T);
indx_cluster <- grepl('comp', colnames(data)) #clustery
name_cluster <- colnames(data)[indx_cluster]
indx_kstrana <- grepl('kstrana_', colnames(data)) #strany
name_kstrana <- colnames(data)[indx_kstrana]

# Přiřadíme rovnoměrně cluster neznámým okrskům
m = matrix(rep(1/sum(indx_cluster), sum(indx_cluster)), nrow = 1)
data[is.na(data$vel_okrsek), indx_cluster] <- m[rep(1:1, times=sum(is.na(data$vel_okrsek))), ]

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

vysledky = vysledky[, c("strana", "aktualne", "predikce")]

toJSON(vysledky)

