# Utfordring 2.3
# Oppgave 1.

# Fjerner alt fra Global Envirement så ingenting kan klusse til koden senere.
rm(list = ls())

# Laster inn nødvendige pakker.
library(sf)
library(plyr)
library(dplyr)
library(readr)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(ggtextures)

# Setter lokalet til no-No for å få norsk språk (for å få øæå).
Sys.setlocale(locale="no_NO")

# Setter arbeidsplassen.
setwd("~/")

# Laster ned csv filen "union_unempl.csv". 
# Leser dataen inn i "union".
union <- read.csv("union_unempl.csv") 
view(union)

# Endrer navnet på en enkel observasjon.
union$country <- gsub("United Kingdom", "UK", union$country) 
view(union)

# Endrer kolonne navn på kolonne 1.
colnames(union)[1] <- "region" 
# En annen måte å endre navn på kunne vært med å bruke:
# names(union)[names(union) == "country"] <- "region"

# Laster inn map_data fra nettet som inneholder plasseringen til stort sett alle europeiske land. 
mapdata <- map_data("world") 
view(mapdata)

# Slår sammen datasettene mapdata og union om til et datasett kallt mapdata ved
# bruk av left_join.  
mapdata <- left_join(mapdata, union, by = "region")
view(mapdata)

# Fjerner NA observasjoner fra datasettet.
mapdata1 <- mapdata %>% filter(!is.na(mapdata$unempl)) 
# mapdata1 <- mapdata %>% drop_na(unempl) # Raskere måte å fjerne Na på.

# Plotter første kart over Europa (plot 1) som viser arbeidsledighetsrate i 
# ulike land.
map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), color = "black") +
  scale_fill_gradient(name = "% unempl", 
                      low = "yellow", 
                      high = "red",
                      na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map1

# Det vi kan se på kartet som viser arbeidsledighet i Europa er at det er 
# større arbeidsledighet i sør-Europa enn i nord. Det skyldes blant annet at 
# Nord-Norge har større andel offentlige ansatte som ikke rammes av            # permitteringer, som f.eks under corona-pandemien. En annen grunn til lav     # arbeidsledighet oppe i nord er at NAV kjører flere tiltak gjennom kjappe kurs # og sertifikater for å få folk raskt utdannet og ut i arbeidsmarkedet. De     # jobber også med å gi ungdom mer langsiktig utdannelse, gjennom videregående  # skole og fagbrev. Myndighetene kan påvirke arbeidsledigheten gjennom         # stabiliseringspolitikk. Ved høy ledighet kan de stimulere økonomien gjennom  # en ekspansiv finanspolitikk altså øke offentlige utgifter eller reduserer    # skatter og avgifter for å stimulere økonomien under en lavkonjunktur, for    # eksempel ved å øke offentlig forbruk eller ved å gi skattelette.



# Utfordring 2.3
# Oppgave 2.
# Fjerner NA observasjoner fra datasettet.
# Plot 2.
mapdata2 <- mapdata %>% filter(!is.na(mapdata$density)) 

# Plotter et andre plot som viser density i Europa.
map2 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), color = "black") +
  scale_fill_gradient(name = "% density", 
                      low = "yellow", 
                      high = "red",
                      na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map2

# Nå har vi sett på arbeidsledighetsraten i de ulike landene i Europa og nå    # skal vi se på fagforeningsdensitet. 
# En fagforening er en sammenslutning av arbeidstakere innen samme yrkesgruppe. # Deres fremste oppgaver er å kjempe for bedre lønn og gode arbeidsvilkår.
# Det vi kan se på kartet er at det er stor fagforeningsdensitet i nord europa, # spesielt i island. I den europeiske unionen (EU) er fagforeninger sterkere i # offentligheten enn i privat sektor.

# Vi kan se en klar sammenheng mellom arbeidsledighet (map1) og                # fagforeningsdensitet (map2). Altså at land med gode fagforeninger har lavere # arbeidsledighet en land uten. 



# Plot 3
mapdata3 <- mapdata2

# Lager en ny kolonne kallt excess_coverage.
mapdata3$Excess_coverage <- mapdata3$coverage - mapdata3$density

# Plotter et tredje plot som viser excess_coverage i Europa.
map3 <- ggplot(mapdata3, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Excess_coverage), color = "black") +
  scale_fill_gradient(name = "% Excess coverage", 
                      low = "yellow", 
                      high = "red",
                      na.value = "gray50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map3

# Kartet viser hvor stor andel av arbeidskraften som dekkes av kollektive      # forhandlinger. Landene som er dekket med grå har NA verdier, altså manglende # verdier. "(De er med i kartet kun pga at det så vissuelt dårlig ut å fjerne  # dem helt"). 
# Vi kan altså se at Norge har manglende verider og er grå. Kollektive         # forhandlinger er forhandlingsprosesser mellom arbeidsgivere og en gruppe     # arbeidstakere som tar sikte på avtaler for å regulere arbeidslønn,           # arbeidsvilkår, fordeler osv. for arbeidstakere. I Norge er det stor kollektiv # forhandling mellom arbeidsgivere og arbeidstakere.

# Plot 4
mapdata4 <- mapdata2

# Plotter et fjerde plot som viser koordinering av lønnsfastsettelse i Europa.
map4 <- ggplot(mapdata4, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), color = "black") +
  scale_fill_brewer(name = "Coord", 
                    palette = "Set3") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map4

# På kartet kan vi se koordinering av lønnsfastsettelse. 1. Det grønne viser   # fragmenterte lønnsforhandlinger, 2. gult viser noe koordinering, 3. lys lilla # viser prosedyreretningslinjer for forhandling, 4. oransje viser ikke-bindene # nasjonale normer og 5. blå viser bindene-nasjonale normer. 1. Fragmentere    # betyr å brekke opp, splitte eller dele i små stykker, som vil si at          # fragmenterte lønnsforhandlinger er lønnsforhandlinger delt opp i mindre      # stykker. 2. Betyr noe kordinasjon, 3. prosedyreretningslinjer for forhandling # , 4. ikke bindende nasjonale normer og 5. bindene nasjonale normer.

# Man kan si det slik at tallene sier noe om lønnsforhanlding, fra 1-5, hvor 1 # er "litt lønnsforhandling" til 5 som er stor lønnsforhandling.



# Utfordring 2.3
# Oppgave 3.
# Diskuteres under plottene.