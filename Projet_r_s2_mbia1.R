#########################################################################################
################################ PROJET R MBA BDIA 2021 S2 ##############################
#########################################################################################

################################
# Chargement des library 

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(DT)
library(data.table)
library(plotly)
library(rAmCharts)
library(scatterD3)
library(rnaturalearth)
library(mapview)
library(rmdformats)



#Importer les fichiers :

caracteristiques<-read.csv2(file ="C:/Users/Tony PC/Desktop/COURS MBA BDIA1/r programmation/Projet_R_MBDIA_2021_S2/caracteristiques-2017.csv",
                            sep = ",")

usagers<-read.csv2(file ="C:/Users/Tony PC/Desktop/COURS MBA BDIA1/r programmation/Projet_R_MBDIA_2021_S2/usagers-2017.csv",
                   sep = ",")

depart_region <-read.csv2(file ="C:/Users/Tony PC/Desktop/COURS MBA BDIA1/r programmation/Projet_R_MBDIA_2021_S2/departments_regions_france.csv",
                          sep = ";", encoding = 'UTF-8', na.strings = "NULL",
                          stringsAsFactors = FALSE)

str(caracteristiques)



############################ I/ Pre-processing



# Nombre de personnes impliquées par accident 

Nbre_accident<- usagers %>% group_by(Num_Acc) %>% count()



# Répartition par sexe

r_sexe<- usagers %>% group_by(sexe) %>% count() %>%
  rename(label= sexe, value=  n)

r_sexe$label<- as.character(r_sexe$label)

r_sexe[r_sexe$label=='1',1]<- 'Masculin'

r_sexe[r_sexe$label=='2',1]<- 'Feminin'



# Répartition par année de naissance

an<- usagers %>% group_by(an_nais) %>% count()

an$an_nais<- as.character(an$an_nais)



# Variable calcule l'âge de l'accidenté

usagers <- usagers %>% mutate(age = year(Sys.Date())-an_nais)



# Répartition des accidents par département en affichant le nom du département/region 

depart_region[depart_region$departmentName == 'CORSE',][1] <- '20'

caracteristiques <- caracteristiques %>%
  mutate(dep = as.character(ifelse(nchar(dep)==2, substr(dep, 1,1), dep))) %>%
  mutate(dep = as.character(ifelse(dep < 970, substr(dep, 1,2), dep)))

dep_name<- caracteristiques %>% left_join(depart_region, by=c("dep"="departmentCode")) %>%
  group_by(dep, departmentName) %>%
  summarise(nb = n())



# Join Region w/ caract

region_name<- caracteristiques %>% left_join(depart_region, by=c("dep"="departmentCode")) %>%
  group_by(regionName) %>%
  summarise(nb = n())

# Variable date 
caracteristiques <- caracteristiques %>%
  mutate(date_acc = as.Date(paste0(an,'-' ,mois,'-' ,jour), "%y-%m-%d"))



#Age Moyen/Median

age_moyen_median <- usagers %>%
  group_by(sexe) %>%
  summarise(age_moyen = round(mean(age, na.rm = TRUE)),
            age_median = round(median(age, na.rm = TRUE)))

age_moyen_median$sexe <- as.character(age_moyen_median$sexe)



# groupes d'âge

usagers <- usagers %>% mutate(groupe_age = cut(age, breaks = c(0,25,40,65,+Inf), right = FALSE))



# Répartition par semaine

rep_semaine <- caracteristiques %>% mutate(semaine = isoweek(date_acc)) %>%
  group_by(semaine) %>%
  count()



# Répartition par jour de la semaine

rep_jour_semaine <- rep_jour_semaine[order(rep_jour_semaine$n, decreasing = TRUE),]
rep_jour_semaine <- caracteristiques %>% 
  group_by(weekdays(date_acc)) %>%
  count()

rep_jour_semaine <- rep_jour_semaine[order(rep_jour_semaine$n,
                                           decreasing = TRUE),]



# Variable classe_heure 

caracteristiques <- caracteristiques %>%
  mutate(classe_heure = cut(hrmn, breaks = c(0,630,1200,1800,2400), 
                            labels = c("nuit", "matin", "apres-midi", "soirée"), right = FALSE))



# Typologie par classe d'heures
carac_classe_heure_group <- caracteristiques %>%
  group_by(classe_heure) %>% count()



# Variables en séparant heures et minutes

caracteristiques <- caracteristiques %>%
  mutate(heure = substr(sprintf("%04d", caracteristiques$hrmn),1,2),
         minutes = substr(sprintf("%04d", caracteristiques$hrmn),3,4))



#Jointure entre l'objet usagers et caracteristiques8

usa_cara <- usagers %>% full_join(caracteristiques)



# Departement par typologie 

usa_cara <- usa_cara %>% 
  mutate(grav2 = recode(grav, "1" = "Indemne", "2" = "Tué", 
                        "3" = "Blessé_hospitalisé", "4" = "Blessé_léger"))

stack_plot_grav <- usa_cara %>%
  group_by(dep, grav2) %>%
  count() %>%
  spread(grav2, n)

depart <- depart_region %>% select(departmentCode,departmentName)
dep_blesse <- merge(stack_plot_grav,
                    depart,by.x = "dep", by.y = 'departmentCode')
dep_blesse <- dep_blesse %>% select(-dep)



# Nombre blessés par typologie et par classe_heure 

classe<-usa_cara %>%
  group_by(grav2, classe_heure) %>%
  count() %>%
  spread(grav2, n)



# Jointure dep pour carte

result_dep <- usa_cara %>% left_join(depart_region, by=c("dep"="departmentCode")) %>%
  group_by(dep, departmentName) %>%
  summarise(nb_accident = n_distinct(Num_Acc), nb_moyen = round(n()/n_distinct(Num_Acc),2),
            age_moyen = round(mean(age, na.rm = TRUE)), age_min = min(age, na.rm = TRUE),
            age_max = max(age, na.rm = TRUE)) %>%
  left_join(usa_cara %>%
              group_by(dep, grav2) %>%
              count() %>%
              spread(grav2, n))



# Pre-processing pour carte

dep1<- result_dep %>% select(departmentName,nb_accident)

france <- ne_states(country = "France", returnclass = "sf") %>% 
  filter(!name %in% c("Guyane franÃ§aise", "Martinique", "Guadeloupe", "La RÃ©union", "Mayotte"))

france2 <- france %>% select(iso_3166_2, name, geometry) %>%
  mutate(Code_dep = gsub("FR-","",iso_3166_2))

data_dep <- france2 %>% 
  left_join(dep1, 
            by = c("Code_dep"="dep")) 

data_dep$nb_accident[is.na(data_dep$nb_accident)]<- 0

data_dep <- data_dep %>%
  mutate(bulle = paste(name, '<BR>', nb_accident))



# Évolution du nombre d'accident par mois

mois_accident <- caracteristiques %>% 
  group_by(mois) %>% count()

mois_accident$mois[which(mois_accident$mois=="1")] = "2017-01-01"
mois_accident$mois[which(mois_accident$mois=="2")] = "2017-02-01"
mois_accident$mois[which(mois_accident$mois=="3")] = "2017-03-01"
mois_accident$mois[which(mois_accident$mois=="4")] = "2017-04-01"
mois_accident$mois[which(mois_accident$mois=="5")] = "2017-05-01"
mois_accident$mois[which(mois_accident$mois=="6")] = "2017-06-01"
mois_accident$mois[which(mois_accident$mois=="7")] = "2017-07-01"
mois_accident$mois[which(mois_accident$mois=="8")] = "2017-08-01"
mois_accident$mois[which(mois_accident$mois=="9")] = "2017-09-01"
mois_accident$mois[which(mois_accident$mois=="10")] = "2017-10-01"
mois_accident$mois[which(mois_accident$mois=="11")] = "2017-11-01"
mois_accident$mois[which(mois_accident$mois=="12")] = "2017-12-01"

mois_accident$mois<- as.POSIXlt(mois_accident$mois, "GMT")



############################ II/ Création des graphiques et analyse



##### Graphe 1: Evolution de nombre d'accident en 2017

amTimeSeries(mois_accident, 'mois', c('n'), bullet = 'round',
             linewidth = c(3, 1), main = "Evolution de nombre d'accident en 2017")



# Nombre d'accidents distinct avec l'objet usagers et avec l'objet caracteristiques

usagers %>% summarise(n_distinct(Num_Acc))
length(unique(usagers$Num_Acc))

caracteristiques %>% summarise(n_distinct(Num_Acc))



#### Graphe 2

fig2 <- plot_ly(Nbre_accident, x = ~Num_Acc, y = ~n, type = 'scatter', mode = 'markers',
                marker = list(size = ~n, opacity = 0.5))%>% 
  layout(title = "Nombre de personnes impliquées lors d'un accident",
         xaxis = list(title= "Id accident"),
         yaxis = list(title= "Nombre de blessés"))

fig2



#### Graphe 3

amPie(data
      = r_sexe, inner_radius = 45, 
      depth = 15, show_values = TRUE) %>%
  amOptions(legend = TRUE, main = "Nombre de personnes impliquées par sexe  
            lors d'un accident",
            mainColor = "#4814ed", mainSize = 15, creditsPosition = "bottom-right")



#### Graphe 4 

fig11 <- plot_ly(an, x = ~an_nais, y = ~n, type = 'bar', name = 'Nbre',
                 marker = list(color = "#5233ff"))
fig11<- fig11 %>% layout(title = "Nombre d'accidentés par année de naissance", 
                         plot_bgcolor='#e5ecf6',
                         xaxis = list(title = "Années",
                                      zerolinecolor = '#ffff', 
                                      zerolinewidth = 2, 
                                      gridcolor = 'ffff',
                                      tickangle = -45),
                         yaxis = list(title = "Nombre de'accidentés",
                                      zerolinecolor = '#ffff', 
                                      zerolinewidth = 2, 
                                      gridcolor = 'ffff'))

fig11



#### Graphe 5

fig_age <- plot_ly(usagers, x = ~age,marker = list(color = "#b833ff")) %>%
  layout(title ="Répartition par âge des accidentés",
         plot_bgcolor='#e5ecf6',
         xaxis = list(title ="Âge",
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(title = "Nombre d'accidentés",
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'))

fig_age



# Âge moyen par accident / âge moyen et médiane par sexe

usagers %>%
  group_by(Num_Acc) %>%
  summarise(age_moyen = round(mean(age)))

usagers %>%
  group_by(sexe) %>%
  summarise(age_moyen = round(mean(age, na.rm = TRUE)),
            age_median = round(median(age, na.rm = TRUE)))



#### Graphe 6

fig111 <- plot_ly(age_moyen_median, x = ~sexe,
                  y= ~age_moyen, type = 'bar', name='Age Moyen')
fig222 <- plot_ly(age_moyen_median, x = ~sexe,
                  y= ~age_median, type = 'bar',name='Age Median')
fig6 <- subplot(fig111, fig222, shareY = TRUE)
fig6 <- fig6 %>% layout(title = "Age moyen et médian par sexe des accidentés",
                        plot_bgcolor='#e5ecf6',
                        xaxis = list(title ="Sexe ( 1 = Homme , 2 = Femme )",
                                     zerolinecolor = '#ffff',
                                     zerolinewidth = 2,
                                     gridcolor = 'ffff'),
                        yaxis = list(title = "Age",
                                     zerolinecolor = '#ffff',
                                     zerolinewidth = 2,
                                     gridcolor = 'ffff'))

fig6  



#### Graphe 7

fig_classe_age <- plot_ly(usagers, labels = ~groupe_age, type = 'pie') %>%
  layout(title ="Répartition par groupe d'âge des accidentés")

fig_classe_age



#### Graphe 8 (Départment Name)

fig8 <-  plot_ly(x = dep_name$departmentName, y = dep_name$nb, type = 'bar',
                 marker = list(color = "#b833ff"))%>%
  layout(title = "Nombre d'accidentés par Département",
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff',
           tickangle = -45), 
         yaxis = list(title= "Nombre d'accidentés", 
                      zerolinecolor = '#ffff', 
                      zerolinewidth = 2, 
                      gridcolor = 'ffff'))

print(fig8)



#### Graphe 9 (Region Name)

fig9 <-  plot_ly(x = region_name$regionName, y = region_name$nb, type = 'bar',
                 marker = list(color = "#b833ff")) %>%
  layout(title = "Nombre d'accidentés par Région",
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#08ff00', 
           zerolinewidth = 2, 
           gridcolor = 'ffff',
           tickangle = -45), 
         yaxis = list(title= "Nombre de'accidentés", 
                      zerolinecolor = '#ffff', 
                      zerolinewidth = 2, 
                      gridcolor = 'ffff'))

fig9



# Répartition par semaine et par jour

caracteristiques %>% mutate(semaine = isoweek(date_acc)) %>%
  group_by(semaine) %>%
  count()

caracteristiques %>% 
  group_by(weekdays(date_acc)) %>%
  count()



#### Graphe 10

fig_rep_semaine <- plot_ly(rep_semaine, x = ~semaine,y=~n,
                           type='bar',marker = list(color = "#b833ff")) %>%
  layout(title ="Répartition par semaine des accidents",
         plot_bgcolor='#e5ecf6',
         xaxis = list(title ="Semaine",
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(title = "Nombre d'accidentés",
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'))

fig_rep_semaine



#### Graphe 11

amBarplot(x="weekdays(date_acc)",y="n",data= rep_jour_semaine,
          depth = 15,
          horiz = TRUE, main='Répartition par jour des accidents')



#### Graphe 12

fig_donut_heure <- carac_classe_heure_group %>%
  plot_ly(labels = ~classe_heure, values= ~n) %>%
  add_pie(hole=0.5) %>%
  layout(title="Répartition des accidents par classe d'heures")

fig_donut_heure



#### Graphe 13

fig_type  <- plot_ly(dep_blesse, x = ~departmentName, y = ~Indemne,
                     type = 'bar', name = 'Indemne') %>%
  add_trace(y = ~Blessé_léger, name = 'Blessé léger') %>% 
  add_trace(y = ~Blessé_hospitalisé,name = 'Blessé hospitalisé')%>%
  add_trace(y = ~Tué, name = 'Tué') %>% 
  layout(barmode='stack',
         plot_bgcolor='#e5ecf6',
         title = "Typologie de l'état des accidentés par département",
         xaxis = list(title ="",
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff',
                      tickangle = -45),
         yaxis = list(title = "Nombre d'accidentés",
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'))

fig_type



#### Graphe 14

fig14 <- plot_ly(classe, x = ~classe_heure, y = ~Blessé_hospitalisé, type = 'bar', 
                 name = 'Blessé hospitalisé')%>% 
  add_trace(y = ~Blessé_léger, name = 'Blessé léger')%>%
  add_trace(y = ~Indemne, name = 'Idemne')%>% 
  add_trace(y = ~Tué, name = 'Tué')%>% 
  layout(title = "Nombre d'accidentés par typologie et par classe d'heure",
         plot_bgcolor='#e5ecf6',
         xaxis = list(title = "",
                      zerolinecolor = '#08ff00', 
                      zerolinewidth = 2, 
                      gridcolor = 'ffff',
                      tickangle = -45),
         yaxis = list(title = "Nombre d'accidentés",
                      zerolinecolor = '#ffff', 
                      zerolinewidth = 2, 
                      gridcolor = 'ffff'))

fig14



#### Graphe 15 : Carte géographique

mapview(data_dep, zcol = "nb_accident",legend = TRUE)



# Save pour markdown

save(mois_accident, Nbre_accident, r_sexe, an, usagers, age_moyen_median,fig_classe_age,
     dep_name, region_name, rep_semaine, rep_jour_semaine, carac_classe_heure_group,
     stack_plot_grav, dep_blesse, classe,
     data_dep,
     file = "C:/Users/Tony PC/Desktop/COURS MBA BDIA1/r programmation/Projet_R_MBDIA_2021_S2/Donnees_Projet_Sécurité_Routière.Rdata")


