# ---- pisa

load("pisa.rda")
#in dem Datensatz befinden sich Daten von 4979 Personen. Da nicht für alle Personnen auf allen Variablen die für diese Arbeit relevant sind (mig,sprache,hisced,buecher) gültige Werte vorligen endscheide ich mich diese Personen aus dan Analysen auszuschliessen. 

#table(pisa$hisced)
pisa$hisced[pisa$hisced=="None"] <- NA # None bei hisced auf fehlend setzen
no.mis <- is.na(pisa$mig)==FALSE & is.na(pisa$sprache)==FALSE & is.na(pisa$hisced)==FALSE & is.na(pisa$buecher)==FALSE 
#table(no.mis)

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

tbl1<-round(heufigkeiten,2)

#die meisten Jugentliche haben keinen Migrationshintergrund (ca.88 %). Der Anteil der Schülerinen und Schüller die selbst nicht in Deutschland geboren sind (erste Generation) und Jugentlichen von denen nur die Eltern zugewandert sind (zweite Generation)  beträgt endsprechend 4 und 8 %. Das Heisst 12% der Jugentlichen hat Migrationshintergrund. Die Gruppe der Jugentlichen mit Migrationshintergrund, die  in deutschland geboren sind ist fast doppelt so gross wie deren, die selbst auch zugewandert sind. 

#Heufigkeitsttabele für Sprachgebrauch zu Hause
absolut<-table(pisa.nm$sprache)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ,prozent,kumuliert)

tbl2<-round(heufigkeiten,2)

#Die meisten Jugentliche benutzten zuhause Deutsch als Familiensprache- ca.92%. 8% der Jungen und Mädschen spechen bei sich zuahuse eine andere Sprache.
#hierzu ist interesant die Frage wie sich die Nutzung der nichtdeutchen Sprache in abhängigkeit von Migrationshintergrund verteilt:

#table(pisa.nm$mig, pisa.nm$sprache) #sprachgebrauch nach Migrationshintergrund
absolut<-table(pisa.nm$mig, pisa.nm$sprache)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,prozent)
#round(heufigkeiten,2)

#unter den Jugentlichen ohne Migrationshintergrung geben 1,5 % an, eine andere Sprache zuhausese zu benutzen als deutsch. Es lässt sich damit zu ekrären, dass in dem Datensatz jugentliche mit nur einem im Ausland geborenen Elternteil zu den Jugentlichen ohne Migraionshintergrund gerechnet werden. In der Gruppe der Jugentlichen deren Eltern zugewandert sind, sie selbst aber in Deutschland geboren sind sprechen fast gleich soviele Deutsch zuhause wie eine andere Sprache. Unter den Jugentlichen deren Geburtsland nicht Deutschland ist, sprechen die meisten ein andere Sprache als Deutsch. 

#Heufigkeitsttabele für Bildungsviveu der Eltern (hisced)

absolut<-table(pisa.nm$hisced)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ,prozent,kumuliert)
#round(heufigkeiten,2)

#es erscheint immer noch die Kategorie "None". In der Stichprobe gibt nur 3 Personen die der Kategorie "ISCED 1" zugeordnet worden sind würden "ISCED 1" und  "ISCED 2" zusammengefasst. Die variable enthelt jetzt 5 susprägungen von 1 - Level 1 bis 5 - Level 5 
library(plyr)
pisa.nm$hisced.r <- revalue(pisa.nm$hisced, c("ISCED 1" = "ISCED 1+2",
                                              "ISCED 2" = "ISCED 1+2"))

absolut<-table(pisa.nm$hisced.r)
relativ<-prop.table(absolut)
prozent<-100*relativ
kumuliert<-cumsum(prozent)
heufigkeiten<-cbind(absolut,relativ,prozent,kumuliert)

#[-1,] ist das komplemaentere dataset zu zeile 1 -> "alles ausser zeile"
tbl3<-round(heufigkeiten,2)[-1,]
