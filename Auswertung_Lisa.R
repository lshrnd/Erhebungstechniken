library(readxl)
Daten1 = read_excel("~/GitHub/Erhebungstechniken/Daten_Lisa.xlsx", skip = 3)
Daten2 = read_excel("~/GitHub/Erhebungstechniken/dataJK.xlsx", skip = 3)
Daten3 = read_excel("~/GitHub/Erhebungstechniken/Erhebung_Philipp.xlsx", skip = 3)

Daten = rbind(Daten1[1:46], Daten2[1:46], Daten3[1:46]) # alle Daten vereinen und Meta-Daten entfernen

names(Daten) = c("ToA" , "SwDown" , "SwUp" , "TendenzDown" , "TendenzUp" , 
                 "TendenzDown1" , "TendenzDown3" ,"TendenzDown5" , "TendenzDownM5" , "AnzahlDownIA" , 
                 "TendenzUp1" , "TendenzUp3" , "TendenzUp5" , "TendenzUpM5" , "AnzahlUpIA" ,
                 "AnstrengungDown1" , "AnstrengungDown3" ,"AnstrengungDown5" , "AnstrengungDownM5" , 
                 "AnstrengungUp1" ,"AnstrengungUp3" , "AnstrengungUp5" , "AnstrengungUpM5" ,
                 "KampagneBool" , "ChangeBereitschaftAllg1" ,"ChangeVerhaltenPrax1" , "ChangeBereitschaftAllg0" ,
                 "ZeitFuß" , "ZeitFahrrad" , "ZeitArbeit" , "FrequenzSport" ,"DauerSport" , "Alter" , "Geschlecht" , 
                 "Gewicht" ,"Koerpergroeße" , "Schuhgroeße" , "Studiengang" ,"Abschluss" , "Haupttaetigkeit" ,
                 "BeeintrBool", "BeeintrValue" , "UnwohlBool" , "UnwohlValue" , 
                 "AndereFaktoren" , "Anmerkungen") 

summary(Daten) # Ueberblick

n = length(Daten$ToA)

sum(Daten$Geschlecht == "m")
sum(Daten$Geschlecht == "w")
sum(Daten$Geschlecht == "d")

Daten$AndereFaktoren
Daten$Anmerkungen

BMI = Daten$Gewicht / (Daten$Koerpergroeße/100)^2
cor(BMI, Daten$AnzahlUpIA, use = "complete.obs")
cor(BMI, Daten$AnzahlDownIA, use = "complete.obs")



library(viridis)
################################################################################
#   TODO Variablen:
#         
#         SPALTEN SINNVOLL UMBENNENEN, EINGABEN EINHEITLICH MACHEN # CHECK
#         - siehe nie/-1, NA/"NA", etc
#         - Ziel: einfacher fuers mit arbeiten
#
#         Erstelle VarSport # CHECK
#         - kombiniere alle Angaben zu Sporttaetigkeiten in einer Variable.
#           also wie lang * wie oft * wie viel laufen/radfahren/arbeiten
#         - am besten klassifizieren fuer bessere Grafiken
#
#         Klassifiziere Anstrengung fuer einfachere Interpretation
#         - z.B. gar nicht anstrengend, etwas anstrengend, anstrengend, sehr anstrengend
#
#
#   TODO PLOTS: 
#
#         Tendenz T/A je Stockwerk ~ VarSport
#         - x Achse: Anzahl Stockwerke
#         - y Achse: Tendenz Treppe/Aufzug
#         - 1 Balken pro Stockwerk geteilt nach VarSport
#           oben Treppe, unten Aufzug, Laenge der Balken symbolisiert avg Tendenz
#
#         Anstrengung Treppen je Stockwerk ~ VarSport # CHECK
#         - x Achse: Anzahl Stockwerke
#         - y Achse: Anstrengung Treppe
#         - 1 Balken pro Stockwerk geteilt nach VarSport
#           oben Treppe, unten Aufzug, Laenge der Balken symbolisiert avg Anstrengung
#
#
#   TODO Analyse:
#
#         praktisches Tlaufen ~ theoretisches TLaufen
#         - muessen die Menschen mehr T laufen als sie wollen?
#         - berechne avg T die gelaufen werden muessen und vergleiche: 
#           - wie hoch ist die avg Anstrengung für diese Treppen
#           - wv % wuerden ab dieser Menge an Stockwerken immer A nehmen
#
#         Up vs Down - was ist anstrengender
#         - für jede Anzahl Stockwerke vergleichen
#         - steigt die Anstrengung mit den Stockwerken bei Up und Down gleich viel?
# 
################################################################################
1
par(mfrow = c(1,1))

#### VarSport erstellen ####
  FrSportMonth = c(1:n)
  for(i in 1:n){
  if(Daten$FrequenzSport[i] == 1){
    FrSportMonth[i] = 0
  }
  if(Daten$FrequenzSport[i] == 2){
    FrSportMonth[i] = 0.5
  }
  if(Daten$FrequenzSport[i] == 3){
    FrSportMonth[i] = 1
  }
  if(Daten$FrequenzSport[i] == 4){
    FrSportMonth[i] = 2.5
  }
  if(Daten$FrequenzSport[i] == 5){
    FrSportMonth[i] = 4
  }
  if(Daten$FrequenzSport[i] == 6){
    FrSportMonth[i] = 10
  }
  if(Daten$FrequenzSport[i] == 7){
    FrSportMonth[i] = 20
  }
  if(Daten$FrequenzSport[i] == 8){
    FrSportMonth[i] = 30
  }
} # FrequenzSport auf Monat hochrechnen

  VarSport = FrSportMonth * Daten$DauerSport / 60 + 30 * (Daten$ZeitFuß + Daten$ZeitFahrrad + Daten$ZeitArbeit) 
## Stunden Sport im Monat + Stunden Bewegung im Monat
  VarSport = VarSport / 30 
## Stunden Bewegung am Tag
  
# Kategorien: <2 , 2-<5, 5-<9, >9

#### ToA zu Int #### 
ToAInt = c(1:n)
  for(i in 1:n){
    if(Daten$ToA[i] == "T"){ ToAInt[i] = 1}
    if(Daten$ToA[i] == "A"){ ToAInt[i] = -1}
  }
ToAInt  
mean(ToAInt)


######################## GRAFIKEN ##########################
#### GRAFIK: Anstrengung pro Stockwerke nach Oben ~ VarSport ####

d11 = mean(Daten$AnstrengungUp1[which(VarSport < 2)])
d12 = mean(Daten$AnstrengungUp1[which(VarSport >= 2 & VarSport < 5)])
d13 = mean(Daten$AnstrengungUp1[which(VarSport >= 5 & VarSport <9)])
d14 = mean(Daten$AnstrengungUp1[which(VarSport >= 9)])

d21 = mean(Daten$AnstrengungUp3[which(VarSport < 2)])
d22 = mean(Daten$AnstrengungUp3[which(VarSport >= 2 & VarSport < 5)])
d23 = mean(Daten$AnstrengungUp3[which(VarSport >= 5 & VarSport <9)])
d24 = mean(Daten$AnstrengungUp3[which(VarSport >= 9)])

d31 = mean(Daten$AnstrengungUp5[which(VarSport < 2)])
d32 = mean(Daten$AnstrengungUp5[which(VarSport >= 2 & VarSport < 5)])
d33 = mean(Daten$AnstrengungUp5[which(VarSport >= 5 & VarSport <9)])
d34 = mean(Daten$AnstrengungUp5[which(VarSport >= 9)])

d41 = mean(Daten$AnstrengungUpM5[which(VarSport < 2)])
d42 = mean(Daten$AnstrengungUpM5[which(VarSport >= 2 & VarSport < 5)])
d43 = mean(Daten$AnstrengungUpM5[which(VarSport >= 5 & VarSport <9)])
d44 = mean(Daten$AnstrengungUpM5[which(VarSport >= 9)])

kategorien <- c("1", "3", "5", "M5")
balken_höhen <- matrix(c(d11, d21, d31, d41,
                         d12, d22, d32, d42,
                         d13, d23, d33, d43,
                         d14, d24, d34, d44), ncol = 4, byrow = TRUE)

barplot(balken_höhen, 
        beside = TRUE,  # Balken nebeneinander platzieren
        names.arg = kategorien,  # Kategorien auf der X-Achse
        col = viridis(4),  # Farben der Balken
        ylim = c(0, 35),  # Y-Achsenbegrenzung anpassen
        xlab = "Anzahl der Stockwerke",  # Beschriftung der X-Achse
        ylab = "Anstrengung")  # Beschriftung der Y-Achse
        #main = "Anstrengung Up")  # Haupttitel

legend("topleft", legend = c("<2h", "2h-5h", "5h-9h", ">9h"), 
       fill = viridis(4), title = "Fitness")


#### GRAFIK: Anstrengung pro Stockwerke nach Unten ~ VarSport ####

d11 = mean(Daten$AnstrengungDown1[which(VarSport < 2)])
d12 = mean(Daten$AnstrengungDown1[which(VarSport >= 2 & VarSport < 5)])
d13 = mean(Daten$AnstrengungDown1[which(VarSport >= 5 & VarSport <9)])
d14 = mean(Daten$AnstrengungDown1[which(VarSport >= 9)])

d21 = mean(Daten$AnstrengungDown3[which(VarSport < 2)])
d22 = mean(Daten$AnstrengungDown3[which(VarSport >= 2 & VarSport < 5)])
d23 = mean(Daten$AnstrengungDown3[which(VarSport >= 5 & VarSport <9)])
d24 = mean(Daten$AnstrengungDown3[which(VarSport >= 9)])

d31 = mean(Daten$AnstrengungDown5[which(VarSport < 2)])
d32 = mean(Daten$AnstrengungDown5[which(VarSport >= 2 & VarSport < 5)])
d33 = mean(Daten$AnstrengungDown5[which(VarSport >= 5 & VarSport <9)])
d34 = mean(Daten$AnstrengungDown5[which(VarSport >= 9)])

d41 = mean(Daten$AnstrengungDownM5[which(VarSport < 2)])
d42 = mean(Daten$AnstrengungDownM5[which(VarSport >= 2 & VarSport < 5)])
d43 = mean(Daten$AnstrengungDownM5[which(VarSport >= 5 & VarSport <9)])
d44 = mean(Daten$AnstrengungDownM5[which(VarSport >= 9)])

kategorien <- c("1", "3", "5", "M5")
balken_höhen <- matrix(c(d11, d21, d31, d41,
                         d12, d22, d32, d42,
                         d13, d23, d33, d43,
                         d14, d24, d34, d44), ncol = 4, byrow = TRUE)

barplot(balken_höhen, 
        beside = TRUE,  # Balken nebeneinander platzieren
        names.arg = kategorien,  # Kategorien auf der X-Achse
        col = viridis(4),  # Farben der Balken
        ylim = c(0, 25),  # Y-Achsenbegrenzung anpassen
        xlab = "Anzahl der Stockwerke",  # Beschriftung der X-Achse
        ylab = "Anstrengung")  # Beschriftung der Y-Achse
        #main = "Anstrengung Down")  # Haupttitel

legend("topleft", legend = c("<2h", "2h-5h", "5h-9h", ">9h"), 
       fill = viridis(4), title = "Fitness")

#### GRAFIK: Tendenz pro Stockwerke nach Oben ~ VarSport ####

d11 = mean(Daten$TendenzUp1[which(VarSport < 2)])
d12 = mean(Daten$TendenzUp1[which(VarSport >= 2 & VarSport < 5)])
d13 = mean(Daten$TendenzUp1[which(VarSport >= 5 & VarSport <9)])
d14 = mean(Daten$TendenzUp1[which(VarSport >= 9)])

d21 = mean(Daten$TendenzUp3[which(VarSport < 2)])
d22 = mean(Daten$TendenzUp3[which(VarSport >= 2 & VarSport < 5)])
d23 = mean(Daten$TendenzUp3[which(VarSport >= 5 & VarSport <9)])
d24 = mean(Daten$TendenzUp3[which(VarSport >= 9)])

d31 = mean(Daten$TendenzUp5[which(VarSport < 2)])
d32 = mean(Daten$TendenzUp5[which(VarSport >= 2 & VarSport < 5)])
d33 = mean(Daten$TendenzUp5[which(VarSport >= 5 & VarSport <9)])
d34 = mean(Daten$TendenzUp5[which(VarSport >= 9)])

d41 = mean(Daten$TendenzUpM5[which(VarSport < 2)])
d42 = mean(Daten$TendenzUpM5[which(VarSport >= 2 & VarSport < 5)])
d43 = mean(Daten$TendenzUpM5[which(VarSport >= 5 & VarSport <9)])
d44 = mean(Daten$TendenzUpM5[which(VarSport >= 9)])

kategorien <- c("1", "3", "5", "M5")
balken_höhen <- matrix(c(d11, d21, d31, d41,
                         d12, d22, d32, d42,
                         d13, d23, d33, d43,
                         d14, d24, d34, d44), ncol = 4, byrow = TRUE)

barplot(balken_höhen, 
        beside = TRUE,  # Balken nebeneinander platzieren
        names.arg = kategorien,  # Kategorien auf der X-Achse
        col = viridis(4),  # Farben der Balken
        ylim = c(-20,20),  # Y-Achsenbegrenzung anpassen
        xlab = "Anzahl der Stockwerke",  # Beschriftung der X-Achse
        ylab = "Tendenz")  # Beschriftung der Y-Achse
        #main = "Tendenz Up")  # Haupttitel

legend("topleft", legend = c("<2h", "2h-5h", "5h-9h", ">9h"), 
       fill = viridis(4), title = "Fitness")


#### GRAFIK: Tendenz pro Stockwerke nach Unten ~ VarSport ####

d11 = mean(Daten$TendenzDown1[which(VarSport < 2)])
d12 = mean(Daten$TendenzDown1[which(VarSport >= 2 & VarSport < 5)])
d13 = mean(Daten$TendenzDown1[which(VarSport >= 5 & VarSport <9)])
d14 = mean(Daten$TendenzDown1[which(VarSport >= 9)])

d21 = mean(Daten$TendenzDown3[which(VarSport < 2)])
d22 = mean(Daten$TendenzDown3[which(VarSport >= 2 & VarSport < 5)])
d23 = mean(Daten$TendenzDown3[which(VarSport >= 5 & VarSport <9)])
d24 = mean(Daten$TendenzDown3[which(VarSport >= 9)])

d31 = mean(Daten$TendenzDown5[which(VarSport < 2)])
d32 = mean(Daten$TendenzDown5[which(VarSport >= 2 & VarSport < 5)])
d33 = mean(Daten$TendenzDown5[which(VarSport >= 5 & VarSport <9)])
d34 = mean(Daten$TendenzDown5[which(VarSport >= 9)])

d41 = mean(Daten$TendenzDownM5[which(VarSport < 2)])
d42 = mean(Daten$TendenzDownM5[which(VarSport >= 2 & VarSport < 5)])
d43 = mean(Daten$TendenzDownM5[which(VarSport >= 5 & VarSport <9)])
d44 = mean(Daten$TendenzDownM5[which(VarSport >= 9)])

kategorien <- c("1", "3", "5", "M5")
balken_höhen <- matrix(c(d11, d21, d31, d41,
                         d12, d22, d32, d42,
                         d13, d23, d33, d43,
                         d14, d24, d34, d44), ncol = 4, byrow = TRUE)

barplot(balken_höhen, 
        beside = TRUE,  # Balken nebeneinander platzieren
        names.arg = kategorien,  # Kategorien auf der X-Achse
        col = viridis(4),  # Farben der Balken
        ylim = c(-20,20),  # Y-Achsenbegrenzung anpassen
        xlab = "Anzahl der Stockwerke",  # Beschriftung der X-Achse
        ylab = "Tendenz")  # Beschriftung der Y-Achse
        #main = "Tendenz Down")  # Haupttitel

legend("topleft", legend = c("<2h", "2h-5h", "5h-9h", ">9h"), 
       fill = viridis(4), title = "Fitness")

#### GRAFIK: Boxplot der Anzahlen der gelaufenen Stockwerke im Alltag, oben und unten ####
boxplot(Daten$SwDown, Daten$SwUp, names = c("nach unten", "nach oben"), xlab = "Anzahl der Stockwerke", horizontal = T)

#### GRAFIK: BMI zu Max gelaufenen Stockwerken ####
plot(BMI, Daten$AnzahlUpIA, col = "blue", xlab = "Body-Mass-Index", ylab = "Anzahl der Stockwerke")
plot(BMI, Daten$AnzahlDownIA, col = "blue", xlab = "Body-Mass-Index", ylab = "Anzahl der Stockwerke")


#### Beeintraechtigungen und unwohlsein ####
par(mfrow = c(1,2))
barplot(height = c( sum(Daten$BeeintrBool),sum(Daten$UnwohlBool) ), ylim = c(0,8) ,
        names = c("körperlicher\nBeschwerden","Unwohlsein"), col = plasma(2), xlab = "Anzahl Betroffener")

barplot(height = c( mean(Daten$BeeintrValue[Daten$BeeintrBool == 1]), mean(Daten$UnwohlValue[Daten$UnwohlBool == 1]) ), ylim = c(0,30),
        names = c("körperlicher\nBeschwerden","Unwohlsein"), col = plasma(2), xlab = "Durchschnittliche Ausprägung (max.: 38)")
