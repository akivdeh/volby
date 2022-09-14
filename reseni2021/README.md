staré

Upozornění: Před spuštěním je potřeba správně nastavit proměnou workdir ve skriptech predikce.R i priprava_dat.R

Slozka „reseni“ obsahuje nasledujici soubory:


predikce.R - skript vlastního řešení, po spuštění loaduje ciselnik.RData (pomocná data generovaná pomocí priprava_dat.R) a soubor na stdin ve stejném formátu jako je psp17okrs_test.txt (zde jsou uloženy průběžné výsledky ve formátu, který nám dodal p. Cibulka - vzniklo z jsonů, které jsou veřejně dostupné při sčítání voleb, lze jednoduše odkomentovat v kódu, aby se načítalo z txt), v produkci místo tohoto csv očekáváme csv ve stejném formátu s průběžnými výsledky. Na stdout je vypsána tabulka key=strana (ve formátu kstrana_X, který dědíme přímo od publikovaných dat), value=aktualne (aktuální procentní zisk strany v průběžném sčítání), value=predikce (aktuální predikce konečného výsledku)

priprava_dat.R - generuje soubor ciselnik.RData na základě historických dat (v současné chvíli používá pscoco.csv a psp17okrs.csv, tyto soubory se budou měnit), tento skript stačí spustit jednou, a sice právě pro generování ciselnik.RData, pak již netřeba aktualizovat
ciselnik.RData - data, která používá skript predikce.R, generováno pomocí priprava_dat.R

pscoco.csv - historická data
psp17okrs.csv - historická data

psp17okrs_test.csv - testovací data, dostupné výsledky z voleb 2017 k 17:00 v den sčítání, v produkci se očekává csv se stejným formátem: tj. všechny sečtené okrsky do daného časového okamžiku, místo názvů stran používáme kódování kstrana_X, které se používalo jak v roce 2017, tak bude v roce 2021 (ačkoliv budou jiné strany v jiném pořadí), skript nijak nevyužívá názvy stran (tj. kódování se může měnit, pouze se předpokládá, že názvy obsahují „kstrana“)
