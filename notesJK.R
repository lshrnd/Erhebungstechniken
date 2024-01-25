## notizen Jorrit

attributes(Daten)

# pref short for preference
# or use trend instead?
# D = down
# U = up

  # Vorschlag fuer Namen
names = c("pref", "amountDown", "amountUp", "prefD", "prefU",
          "prefD1", "prefD3", "prefD5", "prefDMore", "alwaysElevD",
          "prefU1", "prefU3", "prefU5", "prefUMore", "alwaysElevU",
          "effortD1", "effortD3", "effortD5", "effortDMore",
          "effortU1", "effortU3", "effortU5", "effortUMore",
          "campKnown", "attChangeK", "actChangeK", "attChangeN",
          "foot", "bike", "work", "freqSport", "timeSport",
          "age", "sex", "weight", "height", "shoeSize",
          "subject", "degree", "job",
          "unable", "unability", "unwellSS", "unwelnessSS", # SS = small space
          "other", "feedback")

### .RData machen (mit obigen names!)
daten = Daten
names(daten) = names
save(daten, file = "Datensatz.RData")
rm(list = ls())
load("Datensatz.RData")
###

### Sport Variable
sport = daten[c("foot", "bike", "work", "freqSport", "timeSport")]

## Idee: gewichtete summe:
##
## varSport =  w(f) * f + w(b) * b + w(work) * work 
##             + w(sport) * c(freqSport) * timeSport
##
##    wobei w(.) das zugehoerige gewicht ## TODO: was sinnvoll?
##      und c(freqSport) ein Koeffizient der timeSport so transformiert,
##          das die angaben eine etwa einheitliche Zeiteinheit haben
##          bestenfalls: Minuten / Tag
##          (d.h. 1x pro Woche gibt z.B: c(1xproWoche) = c(5) = 1/7)
##

sport$freqSport


