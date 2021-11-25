library(tidyverse)
library(ggplot2)
library(dplyr)

temporel3<-read.csv("temporel3.csv",header=T,sep=";")
temporel<-read.csv("temporel.csv",header=T,sep=";")

# Classement des pays les plus émetteurs de CO2 (t/hab) par ordre décroissant en 2016

CO2tot_2016<-temporel3 %>% filter(Annee=="2016")

graphgen<-CO2tot_2016 %>% 
  ggplot(aes(fct_rev(fct_reorder(Country.Name,
                                 TOTAL)),TOTAL))+
  geom_bar(stat = 'identity',color="pink")+ 
  labs(x='country',hjust=0.9)+
  theme(axis.text.x = element_text(face="bold", color="#993333", size=9, angle=90))+
  labs(y='CO2 (t/hab)',hjust=0.9)+
labs(title='Classement des pays les plus émetteurs de CO2 par ordre décroissant en 2016')

graphgen


# Evolution des émissions de CO2 (t/hab) selon le pays depuis 1970
temporel%>% filter(Country.Name==c("United States","France","Russia","China",
                                    "Germany","Korea","Saudi Arabia")) %>% 
  ggplot()+
  aes(x=Annee,y=TOTAL,color=Country.Name)+
  geom_line(size=1)+
  scale_x_continuous()+
  ylim(0,31)+
  ggtitle('Évolution  des émissions de CO2 par pays') +  
  xlab("année") + ylab("émissions de CO2 t/ hab") 

