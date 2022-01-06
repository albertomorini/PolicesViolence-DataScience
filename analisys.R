setwd("/Volumes/Archive/uni/DataScience/policesViolence")
library(dplyr)
library(ggplot2)
library(waffle)
library(plotly)
library(usmap)
library(calendR)
library(treemap)
library(networkD3)

policesViolence <- read.csv("fatalEncountersDotOrg.csv")
policesViolence <- as_tibble(policesViolence)
#phase: DATA CLEANING
#alcune osservazioni del dataset sono un po' errate probabilmente per errori di battitura o sviste, quindi si è deciso di normalizzarle con valori nulli o arrotondarli a valori simili

policesViolence$Age[which(policesViolence$Age == "")]="NA"
policesViolence$Age[which(policesViolence$Age < 1)]="NA" #eg. (0.25 non è un'età)
policesViolence$Age=as.numeric(policesViolence$Age) #per togliere i range d'età (eg. 18-25 lo assumo come NA)

policesViolence$Gender[which(policesViolence$Gender=="")]="NA"

policesViolence$Race[which(policesViolence$Race=="")]="Race unspecified"
policesViolence$Race[which(policesViolence$Race=="Christopher Anthony Alexander")]="Race unspecified"
policesViolence$Race[which(policesViolence$Race=="European-American/European-American/White")]="European-American/White"
policesViolence$Race[which(policesViolence$Race=="european-American/White")]="European-American/White"
policesViolence$Race[which(policesViolence$Race=="African-American/Black African-American/Black Not imputed")]="African-American/Black"

policesViolence$Location.of.death..city.[which(policesViolence$Location.of.death..city.=="")]="NA"

policesViolence$Foreknowledge.of.mental.illness..INTERNAL.USE..NOT.FOR.ANALYSIS[which(policesViolence$Foreknowledge.of.mental.illness..INTERNAL.USE..NOT.FOR.ANALYSIS=="")]="NA"

policesViolence$date<-format(as.Date(policesViolence$Date.of.injury.resulting.in.death..month.day.year., '%m/%d/%Y'), "%Y/%m/%d")
policesViolence <- policesViolence %>%
  filter(format(as.Date(date, "%Y/%m/%d"),"%Y")<2022)


############################################
############################################
ageAndGender <- policesViolence %>%
  group_by(Age, Gender) %>%
  summarise (n = n()) %>%
  arrange (n) 

ggplot(ageAndGender, aes(x = Age, y = n, fill = Gender)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#cf91ea", "#226696", "red", "green"))+
  theme_minimal()+
  theme(
    legend.text = element_text(size=11)
  )+
  geom_vline(aes(xintercept = mean(as.numeric(Age))), color="red")+
  ggtitle("Età & Genere","Vittime raggruppate per età e sesso")



sumGender <- ageAndGender %>%
  group_by(Gender) %>%
  summarise ( totali = sum(n)) %>%
  arrange(-totali)

sumGender<-sumGender %>%
  mutate(percentuale=round(totali/sum(sumGender$totali)*100, digits=2))
  

ggplot(sumGender, aes(x="", y=totali, fill=paste(Gender, "-",percentuale,"%"))) +
  geom_bar(stat="identity", width = 1)+
  coord_polar("y", start=0)+
  labs(x=NULL, y=NULL, fill="Gender")+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=11),
        axis.ticks = element_blank())+
  guides(fill = guide_legend(title = "Genere"))+
  scale_fill_manual(values=c("#cf91ea","#226696","red","green"))+
  ggtitle("Per genere","Totale delle vittime suddivise per sesso")
  
  




genderRace <- policesViolence %>%
  group_by(Gender, Race) %>%
  summarise(n=n()) %>%
  arrange(-n)

# basic treemap
treemap(genderRace,
        index=c("Gender","Race"),
        vSize="n",
        type="index"
)

byRace <- policesViolence%>%
  group_by(Race) %>%
  summarise(n=n()) %>%
  arrange(-n)
colnames(byRace) <- c("Race","n")
names(byRace$n) = paste0(byRace$Race, "-", byRace$n, " [",round(byRace$n/sum(byRace$n)*100),"%]")
waffle(round(byRace$n/sum(byRace$n)*100), rows=length(byRace$Race), colors = c("#0b827c", "#707070", "black", "#BD0026","#d8ce15","#15b8d8","pink"), legend_pos = "bottom", title = "Etnia in percentuale")




yearAndRace <- policesViolence %>%
  mutate(anno=format(as.Date(date, "%Y/%m/%d"), "%Y")) %>%
  group_by(Race, anno) %>%
  summarise(n=n())


ggplot(yearAndRace, aes(x = anno, y = n, group=Race, colour=Race))+
  geom_line(size=1.2)+
  scale_color_manual(values=c("black", "#d8ce15", "#0b827c", "#BD0026","pink","#15b8d8","#707070"))+
  labs(x = "Anno",
       y = "Numero vittime",
       colour = "Race")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle=70)
  )+
  ggtitle("Anni & Etnie", "Le vittime divise per etnia negli anni")


############################################


############################################

#PER ANNO, osservazioni memorizzate come mese/giorno/anno -> splittiamo
#anche qui non tutte le osservazioni hanno valore, ad ogni modo la conversione da m/d/Y -> Y/m/d elimina quelle non definite

groupedYears <- policesViolence %>%
  mutate(anno=format(as.Date(date, "%Y/%m/%d"),"%Y")) %>%
  group_by(anno) %>%
  summarise (n=n())

ggplot(groupedYears, aes(x = anno, y = n, group=1)) +
  geom_line(size=1.2, color="#352c2e") +
  geom_point() +
  annotate(geom="text", x="2020/03", y=1600, label="Breonna Taylor", color="#BD0026",size=4.5)+
  annotate(geom="point", x="2020/03", y=1570, color="#BD0026")+
  annotate(geom="text", x="2020/05", y=1500, label="George Floyd", color="#BD0026", size=4.5)+
  annotate(geom="point", x="2020/05", y=1470, color="#BD0026") +
  ylab("Numero di vittime")+
  ggtitle("Negli anni", "Dal 2000 al 2021")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle=70),
    axis.text.y = element_text(angle=320)
  )+
  geom_vline(aes(xintercept="2020"), color="#2681e2", size=0.5)
  
  


#BLM ha cambiato la situazione? 
#Non divido per razza, poiché il movimento mirava più alla violenza della polizia in generale (tant'è vero che in molti video durante la protesta vi sono persone caucasiche aggredite)
#boh dividiamo anche, più linee: una complessiva, poi per razza

#prendiamo un'anno prima della protesta fino ad oggi, ottobre non lo calcoliamo, non era finito come mese
blmYears <- policesViolence%>%
  mutate(AnnoMese = format(as.Date(date, "%Y/%m/%d"),"%Y/%m"))%>%
  filter(AnnoMese > "2019/02", AnnoMese < "2021/10")%>% 
  group_by(AnnoMese) %>%
  summarise(n=n())

ggplot(blmYears, aes(x=AnnoMese, y=n)) +
  geom_segment( aes(x=AnnoMese, xend=AnnoMese, y=0, yend=n), color="grey") +
  geom_point( color="#BD0026", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(angle=90)
  ) +
  annotate(geom="text", x="2020/05", y=230, label="'Inizio' delle proteste")+
  annotate(geom="point", x="2020/05", y=215, size=10, shape=21, fill="transparent")+
  geom_hline(aes(yintercept=mean(n)), color="#2681e2", size=0.7)+
  xlab("mese/anno") +
  ylab("numero di vittime")+
  ggtitle("BLM effect", "Prima e dopo le proteste BLM")






#quali sono i giorni più sfortunati?
dayInTheYear <- policesViolence %>%
  filter(format(as.Date(date, "%Y/%m/%d"),"%Y")==2020) %>%
  mutate(day = format(as.Date(date, "%Y/%m/%d"), "%j")) %>%
  group_by(day) %>%
  summarise(n=n()) %>%
  arrange(-n)

calendR(
        orientation = "portrait", # Equivalent to: orientation = "p"
        months.pos = 0,
        year = 2020,# Year
        start = "M",
        special.days = as.numeric(head(dayInTheYear$day,50)),
        special.col =  "#BD0026",
        low.col = "#a1a2af",
        subtitle = "I 50 giorni con più vittime nel 2020",
        title= "50 giorni del 2020"
        )


############################################
############################################


groupedByState <- policesViolence %>%
  group_by(State) %>%
  summarise(n=n()) %>%
  arrange(-n)

colnames(groupedByState) <- c("state","n")

plot_usmap(data = groupedByState, values = "n", color = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Numero di vittime", label = scales::comma) + 
  labs(title = "Heatmap degli stati", subtitle = "Il numero di vittime diviso per stato") +
  theme(legend.position = "right")




########################################################
########################################################


usaMapInter <- policesViolence
usaMapInter <- usaMapInter %>%
  plot_ly(
    lat= ~policesViolence$Latitude,
    lon= ~policesViolence$Longitude,
    marker= list(color="#BD0026"),
    type = 'scattermapbox')
usaMapInter <- usaMapInter %>%
  layout(
    mapbox=list(
      style='open-street-map',
      zoom=2.5,
      center=list(lon=-88, lat=34)
    )
  )
usaMapInter






############################################
############################################




#cambiarlo con razza / stati?

RaceWeaponIllness <- policesViolence %>%
  group_by(Race, Highest.level.of.force, Foreknowledge.of.mental.illness..INTERNAL.USE..NOT.FOR.ANALYSIS) %>%
  summarise (n=n()) %>%
  arrange(-n)


links<- data.frame(
  source= c(RaceWeaponIllness$Race, RaceWeaponIllness$Highest.level.of.force),
  target= c(RaceWeaponIllness$Highest.level.of.force, RaceWeaponIllness$Foreknowledge.of.mental.illness..INTERNAL.USE..NOT.FOR.ANALYSIS),
  value= RaceWeaponIllness$n
)
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
sankeyRWI <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=TRUE, fontSize = 13, nodeWidth = 8,
                   )

sankeyRWI

