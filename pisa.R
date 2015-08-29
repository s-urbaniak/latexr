# ---- pisa

load("pisa.rda")
#in dem Datensatz befinden sich Daten von 4979 Personen. Da nicht für alle Personnen auf allen Variablen die für diese Arbeit relevant sind (mig,sprache,hisced,buecher) gültige Werte vorligen endscheide ich mich diese Personen aus dan Analysen auszuschliessen. 

pisa$hisced[pisa$hisced=="None"] <- NA # None bei hisced auf fehlend setzen


no.mis <- is.na(pisa$mig)==FALSE & is.na(pisa$sprache)==FALSE & is.na(pisa$hisced)==FALSE & is.na(pisa$buecher)==FALSE 

pisa.nm <- pisa[no.mis,] #Datensatz mit Personen ohne fehlende Werte bei allen relewanten Variablen
# in die Analysn gehen 1106 Personen nicht ein. Weitere Berechnungen werden mit 3873 Fällen vorgenommen.

#DESKRIPTIVE STATISTIK - Häufigkeiten

#Häfigkeitsverteilung für nominale/kategoriale Variablen (Migrationshintergrund,Sprachgebrauch zuhause, hisced, Anzahl der Bücher zuhause)

#Migrationshintergrund

#häufigkeiten in einer Tabelle
absolut<-table(pisa.nm$mig)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ,prozent,kumuliert)
tabelle1_1<-round(heufigkeiten,2)
 
#Heufigkeitsttabele für Sprachgebrauch zu Hause
absolut<-table(pisa.nm$sprache)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ,prozent,kumuliert)
tabelle1_2<-round(heufigkeiten,2)

#Die meisten Jugentliche benutzten zuhause Deutsch als Familiensprache- ca.92%. 8% der Jungen und Mädschen spechen bei sich zuahuse eine andere Sprache.
#hierzu ist interesant die Frage wie sich die Nutzung der nichtdeutchen Sprache in abhängigkeit von Migrationshintergrund verteilt:

absolut<-table(pisa.nm$mig, pisa.nm$sprache)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,prozent)
tabelle2<-round(heufigkeiten,2)

#unter den Jugentlichen ohne Migrationshintergrung geben 1,5 % an, eine andere Sprache zuhausese zu benutzen als deutsch. 

#es erscheint immer noch die Kategorie "None".In der Stichprobe gibt nur 3 Personen die der Kategorie "ISCED 1" zugeordnet worden sind würden "ISCED 1" und  "ISCED 2" zusammengefasst. Die variable enthelt jetzt 5 susprägungen von 1 - Level 1 bis 5 - Level 5 
library(plyr)
pisa.nm$hisced.r <- revalue(pisa.nm$hisced, c("None" = "ISCED 1+2",
                                              "ISCED 1" = "ISCED 1+2",
                                              "ISCED 2" = "ISCED 1+2" ))


absolut<-table(pisa.nm$hisced.r)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ)
tabelle3_1<-round(heufigkeiten,2)

pisa.nm$hisced.v <- as.numeric(pisa.nm$hisced.r)

#Häufigkeiten Büchervariable

absolut<-table(pisa.nm$buecher)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ)
tabelle3_2<-round(heufigkeiten,2)

tabelle3<-rbind(tabelle3_1, tabelle3_2)

df<-data.frame(
	c("X"," "," "," "," ","X"," "," "," "," "," "),
	c(" ","X"," "," "," "," "," ","X"," "," "," "),
	c(" "," ","X"," "," "," "," ","X"," "," "," "),
	c(" "," "," "," ","X"," "," "," "," ","X"," "),
	c(" "," "," "," ","X"," "," "," "," "," ","X"),
	c(" "," "," "," ","X"," "," ","X"," "," "," ")
)
colnames(df) <- c("Min.", "1 Qu.", "Md", "3 Qu.", "Max.", "Mo")
tabelle3<-cbind(tabelle3, df)

t4<-rbind(t(rbind(
  tapply(pisa.nm$lesen, pisa.nm$mig, min),
  tapply(pisa.nm$lesen, pisa.nm$mig, quantile, probs=0.25),
  tapply(pisa.nm$lesen, pisa.nm$mig, mean),
  tapply(pisa.nm$lesen, pisa.nm$mig, median),
  tapply(pisa.nm$lesen, pisa.nm$mig, quantile, probs=0.75),
  tapply(pisa.nm$lesen, pisa.nm$mig, max),
  tapply(pisa.nm$lesen, pisa.nm$mig, IQR),
  tapply(pisa.nm$lesen, pisa.nm$mig, sd),
  tapply(pisa.nm$lesen, pisa.nm$mig, var)
)),
c(
  min(pisa.nm$lesen),
  quantile(pisa.nm$lesen, probs=0.25),
  mean(pisa.nm$lesen),
  median(pisa.nm$lesen),
  quantile(pisa.nm$lesen, probs=0.75),
  max(pisa.nm$lesen),
  IQR(pisa.nm$lesen),
  sd(pisa.nm$lesen),
  var(pisa.nm$lesen)
))

colnames(t4) <- c("Min", "1 Qu.", "Mean", "Md", "3 Qu.", "Max", "Qdiff", "SD", "Var")
rownames(t4)[4] <- "Gesamt"
t4<-round(t4,1)
tabelle4<-t4

model1<-lm(pisa.nm$lesen ~ pisa.nm$mig)
model2<-lm(pisa.nm$lesen ~pisa.nm$mig + pisa.nm$sprache)
model3<-lm(pisa.nm$lesen ~pisa.nm$mig + pisa.nm$sprache + pisa.nm$hisced.r)
model4<-lm(pisa.nm$lesen ~pisa.nm$mig + pisa.nm$sprache + pisa.nm$hisced.r + pisa.nm$buecher)
