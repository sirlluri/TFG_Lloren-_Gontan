---
title: "TFG"
author: "Llorenç Gontan"
date: '2024-05-02'
output: html_document
---

#Carreguem llibreries

library(sf)
library(readxl)
library(Hmisc)
library(dplyr)
library(corrplot)
library(ggplot2)
library(car)
library(carData)
library("boot")
library(sankey)
library(networkD3)
library(tidyverse)

dades <- read_excel("C:/Users/llore/OneDrive/Escriptori/UNI/4t any/TFG/Dades quantitatives/dades_senceres.xlsx")

names(dades)


names(dades)[names(dades) == "llengua_materna"] <- "llg_casa"
names(dades)[names(dades) == "llengua_amics"] <- "llg_amic_escola"
names(dades)[names(dades) == "llg_amic_escola"] <- "llg_amic_no_escola"
names(dades)[names(dades) == "llengua_desconegut"] <- "llg_desconegut"
names(dades)[names(dades) == "llengua_extraescolar"] <- "llg_extraesc"
names(dades)[names(dades) == "llengua_tv"] <- "llg_tv"
names(dades)[names(dades) == "llengua_XXSS"] <- "llg_XXSS"
names(dades)[names(dades) == "llengua_lectura"] <- "llg_lectura"
names(dades)[names(dades) == "llengua_companys_classe"] <- "llg_companys_classe"
names(dades)[names(dades) == "llengua_mestre_classe"] <- "llg_mestre_classe"
names(dades)[names(dades) == "llengua_companys_pati"] <- "llg_companys_pati"
names(dades)[names(dades) == "llengua_mestres_pati"] <- "llg_mestre_pati"
names(dades)[names(dades) == "XXSS amb prof"] <- "coment_XXSS"
names(dades)[names(dades) == "extraescolars prof"] <- "coment_extraesc"
Dades <- dades[,-1]


#recodificació Dades
Dades$llenguacasa <- NA
Dades$llenguacasa[Dades$llg_casa == "catala"]<-1
Dades$llenguacasa[Dades$llg_casa == "castella"]<-2
Dades$llenguacasa[Dades$llg_casa == "catala_castella"]<-3

Dades$llenguanoescola <- NA
Dades$llenguanoescola[Dades$llg_amic_no_escola == "catala"]<-1
Dades$llenguanoescola[Dades$llg_amic_no_escola == "castella"]<-2
Dades$llenguanoescola[Dades$llg_amic_no_escola == "catala_castella"]<-3

Dades$llenguadesconegut <- NA
Dades$llenguadesconegut[Dades$llg_desconegut == "catala"]<-1
Dades$llenguadesconegut[Dades$llg_desconegut == "castella"]<-2
Dades$llenguadesconegut[Dades$llg_desconegut == "catala_castella"]<-3

Dades$llenguaextraescolar <- NA
Dades$llenguaextraescolar[Dades$llg_extraesc== "catala"]<-1
Dades$llenguaextraescolar[Dades$llg_extraesc== "castella"]<-2
Dades$llenguaextraescolar[Dades$llg_extraesc== "catala_castella"]<-3
Dades$llenguaextraescolar[Dades$llg_extraesc== "altra"]<-4

Dades$llenguatv <- NA
Dades$llenguatv[Dades$llg_tv == "catala"]<-1
Dades$llenguatv[Dades$llg_tv == "castella"]<-2
Dades$llenguatv[Dades$llg_tv == "catala_castella"]<-3

Dades$llenguaXXSS <- NA
Dades$llenguaXXSS[Dades$llg_XXSS == "catala"]<-1
Dades$llenguaXXSS[Dades$llg_XXSS == "castella"]<-2
Dades$llenguaXXSS[Dades$llg_XXSS == "catala_castella"]<-3
Dades$llenguaXXSS[Dades$llg_XXSS == "altra"]<-4

Dades$llengualectura <- NA
Dades$llengualectura[Dades$llg_lectura == "catala"]<-1
Dades$llengualectura[Dades$llg_lectura == "castella"]<-2
Dades$llengualectura[Dades$llg_lectura == "catala_castella"]<-3
Dades$llengualectura[Dades$llg_lectura == "altra"]<-4

Dades$llenguacompanyclasse <- NA
Dades$llenguacompanyclasse[Dades$llg_companys_classe == "catala"]<-1
Dades$llenguacompanyclasse[Dades$llg_companys_classe == "mes_catala"]<-1
Dades$llenguacompanyclasse[Dades$llg_companys_classe == "castella"]<-3
Dades$llenguacompanyclasse[Dades$llg_companys_classe == "mes_castella"]<-3
Dades$llenguacompanyclasse[Dades$llg_companys_classe == "catala_castella"]<-2
Dades$llenguacompanyclasse[Dades$llg_companys_classe == "altra"]<-4

Dades$llenguamestreclasse <- NA
Dades$llenguamestreclasse[Dades$llg_mestre_classe == "catala"]<-1
Dades$llenguamestreclasse[Dades$llg_mestre_classe == "mes_catala"]<-1
Dades$llenguamestreclasse[Dades$llg_mestre_classe == "castella"]<-3
Dades$llenguamestreclasse[Dades$llg_mestre_classe == "mes_castella"]<-3
Dades$llenguamestreclasse[Dades$llg_mestre_classe == "catala_castella"]<-2
Dades$llenguamestreclasse[Dades$llg_mestre_classe == "altra"]<-4

Dades$llenguacompanyspati <- NA
Dades$llenguacompanyspati[Dades$llg_companys_pati == "catala"]<-1
Dades$llenguacompanyspati[Dades$llg_companys_pati == "mes_catala"]<-1
Dades$llenguacompanyspati[Dades$llg_companys_pati == "castella"]<-3
Dades$llenguacompanyspati[Dades$llg_companys_pati == "mes_castella"]<-3
Dades$llenguacompanyspati[Dades$llg_companys_pati == "catala_castella"]<-2
Dades$llenguacompanyspati[Dades$llg_companys_pati == "altra"]<-4

Dades$llenguamestrepati <- NA
Dades$llenguamestrepati[Dades$llg_mestre_pati == "catala"]<-1
Dades$llenguamestrepati[Dades$llg_mestre_pati == "mes_catala"]<-1
Dades$llenguamestrepati[Dades$llg_mestre_pati == "castella"]<-3
Dades$llenguamestrepati[Dades$llg_mestre_pati == "mes_castella"]<-3
Dades$llenguamestrepati[Dades$llg_mestre_pati == "catala_castella"]<-2
Dades$llenguamestrepati[Dades$llg_mestre_pati == "altra"]<-4

Dades1 <- Dades[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]
Dades1 <- Dades1[,-4]

#Les 4 ultimes però amb més difència



names(Dades1)[names(Dades1) == "llenguacasa"] <- "llg_casa"
names(Dades1)[names(Dades1) == "llenguanoescola"] <- "llg_no_escola"
names(Dades1)[names(Dades1) == "llenguadesconegut"] <- "llg_desconegut"
names(Dades1)[names(Dades1) == "llenguaextraescolar"] <- "llg_extraesc"
names(Dades1)[names(Dades1) == "llenguatv"] <- "llg_tv"
names(Dades1)[names(Dades1) == "llenguaXXSS"] <- "llg_XXSS"
names(Dades1)[names(Dades1) == "llengualectura"] <- "llg_lectura"
names(Dades1)[names(Dades1) == "llenguacompanyclasse"] <- "llg_companys_classe"
names(Dades1)[names(Dades1) == "llenguamestreclasse"] <- "llg_mestre_classe"
names(Dades1)[names(Dades1) == "llenguacompanyspati"] <- "llg_companys_pati"
names(Dades1)[names(Dades1) == "llenguamestrepati"] <- "llg_mestre_pati"



#Dades de context

#llengua materna per escoles

Dades <- Dades %>%
  mutate(llg_casa = factor(llg_casa, levels = c('altra', 'castella', "catala_castella", 'catala')))

llengues_maternes <- Dades %>%
  ggplot(aes(x = escola, fill = llg_casa)) + 
  geom_bar() +
  labs(x = "escola", y = "recompte") +                       
  ggtitle("Llengua materna") +
  ylim(c(0, 51))
llengues_maternes

#llengua amistat
llengua_amic <- Dades %>%
  ggplot(aes(x = llg_casa, fill = llg_amic_no_escola)) + 
  geom_bar() +
  labs(x = "llengua materna", y = "recompte") +                       
  ggtitle("Llengua amics") +
  ylim(c(0, 51))
llengua_amic

#llengua desconegut

llengua_desconegut <- Dades %>%
  ggplot(aes(x = llg_casa, fill = llg_desconegut)) + 
  geom_bar() +
  labs(x = "llengua materna", y = "recompte") +                       
  ggtitle("Llengua desconegut") +
  ylim(c(0, 51))
llengua_desconegut
#llengua extraescolar


llengua_extraescolar<- Dades %>%
  ggplot(aes(x = llg_casa, fill = llg_extraesc)) + 
  geom_bar() +
  labs(x = "llengua materna", y = "recompte") +                       
  ggtitle("Llengua extraescolar") +
  ylim(c(0, 51))

llengua_extraescolar

#llengua OCI
#llengua xxss

llengua_XXSS<- Dades %>%
  ggplot(aes(x = llg_casa, fill = llg_XXSS)) + 
  geom_bar() +
  labs(x = "llengua materna", y = "recompte") +                       
  ggtitle("Llengua xarxes socials") +
  ylim(c(0, 51))

llengua_XXSS
#llengua tv
llengua_TV<- Dades %>%
  ggplot(aes(x = llg_casa, fill = llg_tv)) + 
  geom_bar() +
  labs(x = "llengua materna", y = "recompte") +                       
  ggtitle("Llengua televisió") +
  ylim(c(0, 51))

llengua_TV
#llengua lectura
llengua_lectura<- Dades %>%
  ggplot(aes(x = llg_casa, fill = llg_lectura)) + 
  geom_bar() +
  labs(x = "llengua materna", y = "recompte") +                       
  ggtitle("Llengua lectura") +
  ylim(c(0, 51))

llengua_lectura

#SANKEYPLOT

Dades2 <- Dades1 %>% select(llg_companys_classe, llg_companys_pati) 
Dades2 <- Dades2 [complete.cases(Dades2), ]
Dades2$llg_companys_classe<- ordered(Dades2$llg_companys_classe, levels=c(1,2,3,4), labels=c("Català classe","Català i castellà classe"," Castellà classe","Altra classe"))
Dades2$llg_companys_pati<- ordered(Dades2$llg_companys_pati, levels=c(1,2,3,4), labels=c("Català pati","Català i castellà pati"," Castellà pati","Altra pati"))

links <- Dades2 %>% 
  count(llg_companys_classe, llg_companys_pati) %>% 
  rename(source = llg_companys_pati,
         target = llg_companys_classe)
links <- 
  links %>%
  group_by(target) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

nodes  

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
node_color <- 'd3.scaleOrdinal() .domain(["Catala classe","Catala i castellà classe"," Castellà classe"]) .range(["red", "blue", "green"])'
p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDtarget",
  Target = "IDsource",
  Value = "percent",
  NodeID = "name",
  units = "%",
  fontSize = 12,
  colourScale = node_color,
  nodeWidth = 30,
  iterations = 0)

p



#########################################
#Sankey Hipòtesi 1
Dades3 <- Dades1 %>% select(llg_mestre_classe, llg_mestre_pati) 
Dades3 <- Dades3 [complete.cases(Dades3), ]
Dades3$llg_mestre_classe<- ordered(Dades3$llg_mestre_classe, levels=c(1,2,3,4), labels=c("Català classe","Català i castellà classe"," Castellà classe","Altra classe"))
Dades3$llg_mestre_pati<- ordered(Dades3$llg_mestre_pati, levels=c(1,2,3,4), labels=c("Català pati","Català i castellà pati"," Castellà pati","Altra pati"))

links1 <- Dades3 %>% 
  count(llg_mestre_classe, llg_mestre_pati) %>% 
  rename(source = llg_mestre_pati,
         target = llg_mestre_classe)
links1 <- 
  links1 %>%
  group_by(target) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

nodes1 <- data.frame(
  name=c(as.character(links1$source), as.character(links1$target)) %>% 
    unique()
)

nodes1  

links1$IDsource <- match(links1$source, nodes1$name)-1 
links1$IDtarget <- match(links1$target, nodes1$name)-1
node_colo1r <- 'd3.scaleOrdinal() .domain(["Català classe","Català i castellà classe"," Castellà classe"]) .range(["red", "blue", "green"])'
p1 <- sankeyNetwork(
  Links = links1,
  Nodes = nodes1,
  Source = "IDtarget",
  Target = "IDsource",
  Value = "percent",
  NodeID = "name",
  units = "%",
  fontSize = 12,
  colourScale = node_color,
  nodeWidth = 30,
  iterations = 0)

p1


###############################################
#Taules contingència Hipòtesi 2
#filtre de dades

espriu <- dades %>%
  filter(escola == 'espriu')
espriu_2 <- Dades %>%
  filter(escola == 'espriu_2')
educem <- Dades %>%
  filter(escola == 'educem')
llobet <- Dades %>%
  filter(escola == 'llobet')

#Creació de taules i noms
tespriu <- tespriu [complete.cases(tespriu), ]
tespriu<- prop.table(table(espriu$llg_casa, espriu$llg_companys_classe),margin = 1)*100

tespriu_2<- prop.table(table(espriu_2$llg_casa, espriu_2$llg_companys_classe),margin = 1)*100
tespriu_2 <- tespriu_2 [complete.cases(tespriu_2), ]

teducem<- prop.table(table(educem$llg_casa, educem$llg_companys_classe),margin = 1)*100
teducem <- teducem [complete.cases(teducem), ]

tllobet<- prop.table(table(llobet$llg_casa, llobet$llg_companys_classe),margin = 1)*100
tllobet <- tllobet [complete.cases(tllobet), ]

#taules
tespriu
tespriu_2
teducem
tllobet
#mosaics

########################
#Taules de contingència
#hipòtesi 3

tpespriu <- tpespriu [complete.cases(tpespriu), ]
tpespriu<- prop.table(table(espriu$llg_casa, espriu$llg_companys_pati),margin = 2)*100


tpespriu_2<- prop.table(table(espriu_2$llg_casa, espriu_2$llg_companys_pati),margin = 1)*100
tpespriu_2 <- tpespriu_2 [complete.cases(tpespriu_2), ]

tpeducem<- prop.table(table(educem$llg_casa, educem$llg_companys_pati),margin = 1)*100
tpeducem <- tpeducem [complete.cases(tpeducem), ]

tpllobet<- prop.table(table(llobet$llg_casa, llobet$llg_companys_pati),margin = 1)*100
tpllobet <- tpllobet [complete.cases(tpllobet), ]

#taules
tpespriu
tpespriu_2
tpeducem
tpllobet



#regressions hipotesi 4
#Cat-cast/cast/altres = 1
#regresio simple XXSS
Dades4 <- Dades1 %>% select(llg_XXSS, llg_companys_pati) 
Dades4$llg_companys_pati1 <- NA
Dades4$llg_companys_pati1[Dades1$llg_companys_pati == "1"]<-0
Dades4$llg_companys_pati1[Dades1$llg_companys_pati == "2"]<-1
Dades4$llg_companys_pati1[Dades1$llg_companys_pati == "3"]<-1
Dades4$llg_companys_pati1[Dades1$llg_companys_pati == "4"]<-1

Dades4$XXSS1 <- NA
Dades4$XXSS1[Dades1$llg_XXSS == "1"]<-0
Dades4$XXSS1[Dades1$llg_XXSS == "2"]<-1
Dades4$XXSS1[Dades1$llg_XXSS == "3"]<-1
Dades4$XXSS1[Dades1$llg_XXSS == "4"]<-1

regresion2 <- lm(llg_companys_pati1 ~ XXSS1, data = Dades4)
summary(regresion2)
plot(Dades4$llg_XXSS, Dades4$llg_companys_pati1, xlab='llg_xxss', ylab='llg_companys_pati')
abline(regresion1)

#regresio simple lectura
Dades4$llg_lectura <- NA
Dades4$llg_lectura[Dades1$llg_lectura == "1"]<-0
Dades4$llg_lectura[Dades1$llg_lectura == "2"]<-1
Dades4$llg_lectura[Dades1$llg_lectura == "3"]<-1
Dades4$llg_lectura[Dades1$llg_lectura == "4"]<-1

regresion6 <- lm(llg_companys_pati1 ~ llg_lectura, data = Dades4)
summary(regresion6)
plot(Dades4$llg_lectura, Dades4$llg_companys_pati1, xlab='llg_lectura', ylab='llg_companys_pati', title= "Lectura")
abline(regresion6)

#regresio simple tv
Dades4$llg_tv <- NA
Dades4$llg_tv[Dades1$llg_tv == "1"]<-0
Dades4$llg_tv[Dades1$llg_tv == "2"]<-1
Dades4$llg_tv[Dades1$llg_tv == "3"]<-1
Dades4$llg_tv[Dades1$llg_tv == "4"]<-1

regresion7 <- lm(llg_companys_pati1 ~ llg_tv, data = Dades4)
summary(regresion7)

regresion4 <- lm(llg_companys_pati1 ~ llg_t + llg_tv + llg_lectura, data = Dades4)
summary(regresion4)
plot(Dades4$llg_XXSS, Dades4$llg_companys_pati1, xlab='llg_xxss', ylab='llg_companys_pati')
abline(regresion1)

#########
#llengua materna amb XXSS
Dades4$llg_casa <- NA
Dades4$llg_casa[Dades1$llg_casa == "1"]<-0
Dades4$llg_casa[Dades1$llg_casa == "2"]<-1
Dades4$llg_casa[Dades1$llg_casa == "3"]<-1
Dades4$llg_casa[Dades1$llg_casa == "4"]<-1

regresion8 <- lm(llg_casa ~ XXSS1, data = Dades4)
summary(regresion5)
Dades4$comen_XXSS <- NA
Dades4$comen_XXSS[dades$coment_XXSS == "0"]<-0
Dades4$comen_XXSS[dades$coment_XXSS == "1"]<-1

regresion9 <- lm(llg_companys_pati1 ~ comen_XXSS, data = Dades4)
summary(regresion9)

Dades4$comen_extraesc <- NA
Dades4$comen_extraesc[dades$coment_extraesc == "0"]<-0
Dades4$comen_extraesc[dades$coment_extraesc == "1"]<-1

regresion10 <- lm(llg_companys_pati1 ~ comen_extraesc, data = Dades4)
summary(regresion10)
