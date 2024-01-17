library(readxl)
Daten1 = read_excel("~/GitHub/Erhebungstechniken/Daten_Lisa.xlsx", skip = 3)
Daten2 = read_excel("~/GitHub/Erhebungstechniken/dataJK.xlsx", skip = 3)


Daten = rbind(Daten1[,1:46], Daten2[1:46]) # alle Daten vereinen und Meta-Daten entfernen

summary(Daten) # Ueberblick

################################################################################
#   TODO Variablen:
#
#         Erstelle VarSport 
#         - kombiniere alle Angaben zu Sporttaetigkeiten in einer Variable.
#           also wie lang * wie oft * wie viel laufen/radfahren/arbeiten
#         - am besten klassifizieren fuer bessere Grafiken
#
#         Klassifiziere Anstrengung für einfachere Interpretation
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
#         Anstrengung Treppen je Stockwerk ~ VarSport
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
################################################################################
