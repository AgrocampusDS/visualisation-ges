library(tidyverse)
library(ggplot2)
library(dplyr)

temporel<-read.csv("temporel.csv",header=T,sep=";")
GES<-read.csv("GES1.csv", header = T, sep=";")


# graphique 1 : proportions mondiales d'émmissions de CO2 par pays 

GES %>% 
  mutate(tot = CO2_2016+CH4_2016) %>% 
  mutate(prop = round(((tot/sum(tot)) * 100),1)) %>%
  arrange(desc(prop)) %>%
  slice(1:10) %>%
  ggplot() +
  aes(x=reorder(Country,(-prop)), y=prop) +
  geom_bar(stat="identity", fill ="steelblue") +
  ggtitle("10 pays les plus émetteurs de gazs à effets de serre") +
  xlab("") +
  ylab("Part d'émmission de gazs à effets de serre (%)") +
  geom_text(aes(label=prop), vjust=1.6, color="white", size=3.5)+
  theme_minimal() 
  


# Classement des pays les plus émetteurs de CO2 (t/hab) par ordre décroissant en 2016

CO2tot_2016<-temporel %>% filter(Annee=="2016")

graphgen<-CO2tot_2016 %>% 
  ggplot(aes(fct_rev(fct_reorder(Country.Name,
                                 TOTAL)),TOTAL))+
  geom_bar(stat = 'identity',color="pink")+ 
  labs(x='country',hjust=0.9)+
  theme(axis.text.x = element_text(face="bold", color="#993333", size=9, angle=90))+
  labs(y='CO2 (t/hab)',hjust=0.9)
labs(title='Classement des pays les plus émetteurs de CO2 par ordre décroissant en 2016')

graphgen


# Evolution des émissions de CO2 (t/hab) selon le pays depuis 1970
temporel %>% filter(Country.Name==c("United States","France","Russia","China",
                                    "Germany","Korea","Saudi Arabia")) %>% 
  ggplot()+
  aes(x=Annee,y=TOTAL,color=Country.Name)+
  geom_line(size=1)+
  scale_x_continuous()+
  ylim(0,31)+
  ggtitle('Évolution  des émissions de CO2 par pays') +  
  xlab("année") + ylab("émissions de CO2 t/ hab") 


# Graph 2 harmonisé : pays les plus émetteurs de CO2 (t/hab) par ordre décroissant en 2016

CO2tot_2016<-temporel %>% filter(Annee=="2016")

graphgen<-CO2tot_2016 %>% 
  ggplot()+
  aes(x = reorder(Country.Name,(-TOTAL)), y = TOTAL) +
  geom_bar(stat = 'identity',fill ="steelblue")+ 
  xlab("") +
  ylab("Part d'émmission de gazs à effets de serre (%)") +
  geom_text(aes(label=round(TOTAL,1)), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x = element_text(face="bold", color="#993333", size=9, angle=90))
  #labs(y='CO2 (t/hab)',hjust=0.9)
#labs(title='Classement des pays les plus émetteurs de CO2 par ordre décroissant en 2016')

graphgen

# Evolution des émissions de CO2 (t/hab) selon le pays depuis 1970

temporel %>% 
  filter(Country.Name %in% c("United States","France","Russia","China","Germany")) %>% 
  ggplot()+
  aes(x=Annee,y=TOTAL,color=Country.Name)+
  coord_cartesian(expand = FALSE, clip = "off") +
  geom_line(size=1) +
  annotate("rect", 
           xmin = 1989, xmax = 1990, 
           ymin = 0, ymax = 32, 
           alpha = 0.2,
           fill = "#2596be") +
  geom_segment(x = 2000, y = 2, xend = 2018, yend = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "both"),
               colour = "red") +
  annotate("text", x = 2010, y = 3.5, 
           label = "Croissance du \n secteur manufacturier",
           fontface = 3,
           colour = "red") +
  annotate("text", x = 1996, y = 30, 
           label = "Fin de la guerre froide",
           fontface = 3,
           colour = "#2596be") +
  labs("text", x = 2020, y = -2, 
           label = "Source : Banque Mondiale (2018)",
           fontface = 3) +
  ggtitle('Évolution  des émissions de gazs à effet de serre par pays depuis 1970') +  
  xlab("") + 
  ylab("émissions de gazs à effet de serre en tonnes par habitants") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title=element_blank())