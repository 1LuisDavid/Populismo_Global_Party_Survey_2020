####### GRÁFICAS SOBRE POPULISMO -------

Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

#### Librerías  ------

library(readr)
library(janitor)
library(tidyverse)
library(ggthemes)  
library(patchwork)
library(ggrepel)



#### Datos  ------

pop <- read_csv("01_DATOS/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")
pop <- clean_names(pop)
pop <- pop %>% select(2, 4, 8, 12, 13, 14, 16, 20:27, 29:32, 36, 37, 38, 
                      39:44, 58, 60, 108:111, 116, 117, 118, 127, 107:110)
bd <- 
  pop %>% rename(País = iso,
                 Partido = partyname,
                 Categorías_de_valores = type_values,
                 Populismo = type_populism,
                 Categorías_de_valores_populistas = type_populist_values,
                 Categorías_asientos_en_cámara = type_partysize_seat,
                 Planes_y_programas = v3,
                 Valores_ideológicos = v4_scale,
                 Categorías_de_valores_ideológicos = v4_ord,
                 Importancia_a_la_economía = v5,
                 Escala_conservador_liberal = v6_scale,
                 Binario_conservador_liberal = v6_bin,
                 Categorías_conservador_liberal = v6_ord,
                 Retórica_populista = v8_scale,
                 Retórica_populista_bin = v8_bin,
                 Retórica_populista_ord = v8_ord,
                 Importancia_de_la_retórica = v9,
                 Nacionalismo = v13,
                 Derechos_de_las_mujeres = v14,
                 Derechos_de_minorías = v15,
                 Valores_democrático_liberales = v16,
                 Clientelismo = v17,
                 Voluntad_del_pueblo = v18,
                 Decisión_del_pueblo = v19,
                 Retórica_hacia_políticos = v20,
                 Controles_y_balances_al_Ejecutivo = v21,
                 Ano_de_elecciones = elec_year,
                 Porcentaje_asientos_en_cámara = party_per_seats,
                 Tipo_de_régimen_fh = fh_regime,
                 Derechos_políticos_fh = fh_political_rights,
                 Libertades_civiles_fh = fh_civil_liberties,
                 Tipo_de_régimen_Vdem = v_dem_regime)




####### Gráfica de características retóricas de MORENA   -----

# Hay que obtener los valores promedios globales de cada variable de los partidos populistas y pluralistas
# Con estos valores se harán las líneas promedio en la gráfica
promedios_pop <-
  na.omit(lineas_pop) %>% 
  mutate(Planes_y_programas = mean(Planes_y_programas),
         Valores_ideológicos = mean(Valores_ideológicos),
         Importancia_a_la_economía = mean(Importancia_a_la_economía),
         Escala_conservador_liberal = mean(Escala_conservador_liberal),
         Retórica_populista = mean(Retórica_populista),
         Importancia_de_la_retórica = mean(Importancia_de_la_retórica),
         Nacionalismo = mean(Nacionalismo),
         Derechos_de_las_mujeres = mean(Derechos_de_las_mujeres),
         Derechos_de_minorías = mean(Derechos_de_minorías),
         Valores_democrático_liberales = mean(Valores_democrático_liberales),
         Clientelismo = mean(Clientelismo),
         Voluntad_del_pueblo = mean(Voluntad_del_pueblo),
         Decisión_del_pueblo = mean(Decisión_del_pueblo),
         Retórica_hacia_políticos = mean(Retórica_hacia_políticos),
         Controles_y_balances_al_Ejecutivo = mean(Controles_y_balances_al_Ejecutivo)) %>% 
  pivot_longer(3:17) 



promedios_plur <-
  na.omit(lineas_plur) %>% 
  mutate(Planes_y_programas = mean(Planes_y_programas),
         Valores_ideológicos = mean(Valores_ideológicos),
         Importancia_a_la_economía = mean(Importancia_a_la_economía),
         Escala_conservador_liberal = mean(Escala_conservador_liberal),
         Retórica_populista = mean(Retórica_populista),
         Importancia_de_la_retórica = mean(Importancia_de_la_retórica),
         Nacionalismo = mean(Nacionalismo),
         Derechos_de_las_mujeres = mean(Derechos_de_las_mujeres),
         Derechos_de_minorías = mean(Derechos_de_minorías),
         Valores_democrático_liberales = mean(Valores_democrático_liberales),
         Clientelismo = mean(Clientelismo),
         Voluntad_del_pueblo = mean(Voluntad_del_pueblo),
         Decisión_del_pueblo = mean(Decisión_del_pueblo),
         Retórica_hacia_políticos = mean(Retórica_hacia_políticos),
         Controles_y_balances_al_Ejecutivo = mean(Controles_y_balances_al_Ejecutivo)) %>% 
  pivot_longer(3:17) 

# Aquí se obtienen los valores de MORENA para graficar su línea y se asiga a MORENA todos los tipos de régimen para usar en la última gráfica 
morena_linea <-
  lineas_pop %>%
  filter(Partido == "Movimiento Regeneracion Nacional") %>%
  pivot_longer(3:17)

morena_linea_2 <-
  lineas_pop %>%
  filter(Partido == "Movimiento Regeneracion Nacional") %>% pivot_longer(3:17)

morena_linea_0 <-
  lineas_pop %>%
  filter(Partido == "Movimiento Regeneracion Nacional") %>% pivot_longer(3:17) %>%
  mutate(Tipo_de_régimen_Vdem = ifelse(Partido == "Movimiento Regeneracion Nacional", 0, NA))

morena_linea_1 <-
  lineas_pop %>%
  filter(Partido == "Movimiento Regeneracion Nacional") %>% pivot_longer(3:17) %>%
  mutate(Tipo_de_régimen_Vdem = ifelse(Partido == "Movimiento Regeneracion Nacional", 1, NA))


morena_linea_3 <-
  lineas_pop %>%
  filter(Partido == "Movimiento Regeneracion Nacional") %>% pivot_longer(3:17) %>%
  mutate(Tipo_de_régimen_Vdem = ifelse(Partido == "Movimiento Regeneracion Nacional", 3, NA))

morena_linea_completo <-
  rbind.data.frame(morena_linea_0, morena_linea_1, morena_linea_2, morena_linea_3)



# Ahora hay que encontrar a los partidos populistas y pluralistas relevantes y
# no marginales (con más del 3% de asientos en sus respectivas cámaras legislativas).


partidos_relevantes <-
  bd %>% 
  filter(Categorías_asientos_en_cámara >= 2) %>%
  select(38, 2, 5,
         Planes_y_programas,
         Valores_ideológicos, 
         Importancia_a_la_economía, 
         Escala_conservador_liberal,
         Retórica_populista,
         Importancia_de_la_retórica,
         Nacionalismo,
         Derechos_de_las_mujeres,
         Derechos_de_minorías, 
         Valores_democrático_liberales,
         Clientelismo, 
         Voluntad_del_pueblo,
         Decisión_del_pueblo,
         Retórica_hacia_políticos,
         Controles_y_balances_al_Ejecutivo)

# Hay que saber cuáles osn populistas o pluralistas
partidos_relevantes <-
  partidos_relevantes %>% mutate(pop_o_plur = 
                                   ifelse(Populismo >=3, "Populista", "Pluralista"))

# Hay que obtener los promedios
promedios_pop_y_plur <-
  na.omit(partidos_relevantes) %>% group_by(Tipo_de_régimen_Vdem, pop_o_plur) %>% 
  summarise(Populismo = mean(Populismo),
            Planes_y_programas = mean(Planes_y_programas),
            Valores_ideológicos = mean(Valores_ideológicos),
            Importancia_a_la_economía = mean(Importancia_a_la_economía),
            Escala_conservador_liberal = mean(Escala_conservador_liberal),
            Retórica_populista = mean(Retórica_populista),
            Importancia_de_la_retórica = mean(Importancia_de_la_retórica),
            Escala_conservador_liberal = mean(Escala_conservador_liberal),
            Retórica_populista = mean(Retórica_populista),
            Importancia_de_la_retórica = mean(Importancia_de_la_retórica),
            Nacionalismo = mean(Nacionalismo),
            Derechos_de_las_mujeres = mean(Derechos_de_las_mujeres),
            Derechos_de_minorías = mean(Derechos_de_minorías),
            Valores_democrático_liberales = mean(Valores_democrático_liberales),
            Clientelismo = mean(Clientelismo),
            Voluntad_del_pueblo = mean(Voluntad_del_pueblo),
            Decisión_del_pueblo = mean(Decisión_del_pueblo),
            Retórica_hacia_políticos = mean(Retórica_hacia_políticos),
            Controles_y_balances_al_Ejecutivo = mean(Controles_y_balances_al_Ejecutivo)) %>% 
  ungroup()


promedios_pop_y_plur <-
  promedios_pop_y_plur %>% rename(Partido = pop_o_plur) %>% 
  mutate(pop_o_plur = 
           ifelse(Populismo >=3, "Populista", "Pluralista"))


# Se unen promedios y relevantes
partidos_relev_completo <-
  rbind.data.frame(partidos_relevantes, promedios_pop_y_plur)

# Se etiquetan para resaltarlos en la gráfica
partidos_relev_completo<-
  partidos_relev_completo %>% 
  mutate(color_realce = ifelse(Partido == "Pluralista", "realce promedio plur", 
                               ifelse(Partido == "Populista", "realce promedio populi", " ")))

# Se crea una variable que solo tiene valores para los valores promedio, que se encuentran en la parte baja del data frame
agrupados <-
  c(rep(NA, 449), "Pluralista 0",
    "Populista 0",
    "Pluralista 1",
    "Populista 1",
    "Pluralista 2",
    "Populista 2", 
    "Pluralista 3",
    "Populista 3" )

#Se agrega al data frame la variable recién creada a manera de etiqueta
partidos_relev_completo <-
  cbind.data.frame(partidos_relev_completo, agrupados)

# Se convierte el df en tibble
partidos_relev_completo <-
  as_tibble(partidos_relev_completo)

# Se duplica el data frame pero sin las variables creadas para poder hacer varios realces
partidos_relev_completo_2 <-
  partidos_relev_completo %>% select(-Populismo, -agrupados) 



# Comienza la gráfica

na.omit(partidos_relev_completo_2) %>% pivot_longer(3:17) %>% 
  
  ggplot(aes(x = name, y=value)) +
  
  geom_line(aes(group = Partido, color = pop_o_plur))+
  
  # Esta es la línea de valores promedio de los partidos populistas
  geom_line(data = promedios_pop, aes(group = Partido, color = "Valores promedio\nde partidos\npopulistas"), 
            size = 1.5)+
  
  # Esta es la línea de valores promedio de los partidos pluralistas
  geom_line(data = promedios_plur, aes(group = Partido, color = "Valores promedio\nde partidos\npluralistas"), 
            size = 1.5)+
  
  # Esta es la línea de valores promedio de MORENA
  geom_line(data = morena_linea_completo, 
            aes(group = Partido, color = "Morena"),
            size = 1.5)+
  
  scale_color_manual(values = c("#d9060c", "#2683A1", "#0B465F", "#50F2B7", "#D8FDF2"),
                     labels = c("Morena", "Partidos\npluralistas", "Partidos\npopulistas",
                                "Valores promedio\nde partidos\npluralistas",
                                "Valores promedio\nde partidos\npopulistas"))+
  
  theme_fivethirtyeight()+
  
  #Aquí se colocan las variables en el orden deseado y se les asigna una etiqueta
  scale_x_discrete(limits=c("Retórica_populista",
                            "Importancia_de_la_retórica",   
                            "Retórica_hacia_políticos",
                            "Clientelismo",
                            "Controles_y_balances_al_Ejecutivo",
                            "Planes_y_programas",               
                            "Valores_democrático_liberales",    
                            "Importancia_a_la_economía",
                            
                            
                            "Derechos_de_las_mujeres",          
                            "Derechos_de_minorías",             
                            "Escala_conservador_liberal",       
                            "Nacionalismo",                     
                            "Voluntad_del_pueblo",              
                            "Valores_ideológicos",
                            "Decisión_del_pueblo"),
                   
                   labels=c("Retórica\npopulista",
                            "Importancia\notorgada a la\nretórica",
                            "Retórica usada\nhacia los\npolíticos",
                            "Clientelismo",
                            "A favor o en contra\nde los controles y\nbalances hacia el\npoder ejecutivo",
                            "Formalidad en los\nplanes y\nprogramas del\npartido",
                            "Valores de la\ndemocracia liberal",
                            "Importancia\notorgada\na la economía",
                            "Derechos de\nlas mujeres",
                            "Derechos de\nlas minorías",
                            "Escala sobre\ncobservadurismo y\nliberalismo",
                            "Importancia hacia el\nnacionalismo o hacia\nel multilaterismo",
                            "Voluntad del\npueblo",
                            "Cuestiones\neconómicas",
                            "Decisiones importantes\nen el pueblo o\nen los políticos"))+
  
  
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(angle = 90, size = 12, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "grey75"),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 14))+
  
  labs(x = " ", y = " ",
       title="Características del discurso populista o pluralista de los partidos políticos en comparación con MORENA",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020",
       color = " ") +
  
  ggsave("G5 Características del discurso populista o pluralista de los partidos políticos.png", width = 15, height = 10, dpi = 200)


###### Gráfica con facetas por tipo de régimen    ------

#Solo se hace un vector de nombres que será usado para modificar los títulos de cada faceta
labels <- c("Autocracia\nabsoluta", "Autocracia\nelectoral", "Democracia\nelectoral", "Democracia\nliberal")
names(labels) <- c(0, 1, 2, 3)


na.omit(partidos_relev_completo_2) %>% pivot_longer(3:17) %>% 
  
  ggplot(aes(x = name, y=value)) +
  geom_line(aes(group = Partido, color = pop_o_plur))+
  
  geom_line(data = na.omit(partidos_relev_completo) %>% select(-Populismo) %>%  pivot_longer(3:17), 
            aes(group = agrupados, color = color_realce), size = 1.5)+
  
  geom_line(data = morena_linea_completo, 
            aes(group = Partido, color = "Morena"), size = 1.5)+
  
  scale_color_manual(values = c("#d9060c", "#2683A1", "#0B465F", "#50F2B7", "#D8FDF2"),
                     labels = c("Morena", "Partidos\npluralistas", "Partidos\npopulistas",
                                "Valores promedio\nde partidos\npluralistas",
                                "Valores promedio\nde partidos\npopulistas"))+
  
  #Aquí se usa el vector creado para cambiar el nombre las factas
  facet_grid(Tipo_de_régimen_Vdem ~., labeller = labeller(Tipo_de_régimen_Vdem = labels))+
  
  
  theme_fivethirtyeight()+
  
  scale_x_discrete(limits=c("Retórica_populista",
                            "Importancia_de_la_retórica",   
                            "Retórica_hacia_políticos",
                            "Clientelismo",
                            "Controles_y_balances_al_Ejecutivo",
                            "Planes_y_programas",               
                            "Valores_democrático_liberales",    
                            "Importancia_a_la_economía",
                            
                            
                            "Derechos_de_las_mujeres",          
                            "Derechos_de_minorías",             
                            "Escala_conservador_liberal",       
                            "Nacionalismo",                     
                            "Voluntad_del_pueblo",              
                            "Valores_ideológicos",
                            "Decisión_del_pueblo"),
                   
                   labels=c("Retórica\npopulista",
                            "Importancia\notorgada a la\nretórica",
                            "Retórica usada\nhacia los\npolíticos",
                            "Clientelismo",
                            "A favor o en contra\nde los controles y\nbalances hacia el\npoder ejecutivo",
                            "Formalidad en los\nplanes y\nprogramas del\npartido",
                            "Valores de la\ndemocracia liberal",
                            "Importancia\notorgada\na la economía",
                            "Derechos de\nlas mujeres",
                            "Derechos de\nlas minorías",
                            "Escala sobre\ncobservadurismo y\nliberalismo",
                            "Importancia hacia el\nnacionalismo o hacia\nel multilaterismo",
                            "Voluntad del\npueblo",
                            "Cuestiones\neconómicas",
                            "Decisiones importantes\nen el pueblo o\nen los políticos"))+
  
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(angle = 90, size = 12, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "grey75"),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 14),
        strip.text.y = element_text(size = 15))+
  
  
  labs(x = " ", y = " ",
       title="Características del discurso populista o pluralista de los partidos políticos en comparación con MORENA",
       subtitle = "Por tipo de régimen",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020",
       color = " ") +
  
  ggsave("G6 Características del discurso populista o pluralista de los partidos políticos por tipo de régimen.png", width = 16, height = 11, dpi = 200)




#####  Análisis de clusters   ------

cluster <- na.omit(partidos_relevantes)

cluster <- cluster %>% filter(Retórica_populista >= 5)

cluster <- cluster %>% select(Valores_democrático_liberales, 
                              Controles_y_balances_al_Ejecutivo, 
                              Retórica_hacia_políticos, Voluntad_del_pueblo)




cluster_scale <- scale(cluster) # escalar 

set.seed(180) #establecer valor aleatorio para replicabilidad

optimo <- kmeans(cluster_scale, centers = 1)$betweenss

for(i in 2:10) optimo[i] <- kmeans(cluster_scale, centers = i)$betweenss

plot(1:10, optimo, type = "b", xlab = "número de clusters", ylab = "suma de cuadrados inter grupos")

part_pop_clusters <- kmeans(cluster_scale, centers = 3) # Realizamos clustering
names(part_pop_clusters) # contenido del objeto

head(part_pop_clusters$cluster) # asignación observaciones a clusters
part_pop_clusters$totss # inercia total
part_pop_clusters$betweenss # inercia ínter grupos
part_pop_clusters$withinss # inercia intra grupos
part_pop_clusters$tot.withinss # inercia intra grupos (total)

plot(cluster$Voluntad_del_pueblo, cluster$Controles_y_balances_al_Ejecutivo, 
     col = part_pop_clusters$cluster ,xlab = "Voluntad del pueblo", ylab = "Balances al Ejecutivo" )

aggregate(cluster ,by = list(part_pop_clusters$cluster), mean)

cluster_optimo <-
na.omit(partidos_relevantes) %>% filter(Retórica_populista >= 5)
cluster_optimo$num_cluster <- as.vector(part_pop_clusters$cluster)


cluster_optimo %>% count(num_cluster)


# Gráficas de cluster
 

c1 <-
  ggplot(cluster_optimo %>% filter(num_cluster == 1))+
  
  geom_point( aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                  color = Valores_democrático_liberales, 
                  size = Retórica_hacia_políticos), 
              alpha = .5)+
  
  geom_text_repel(data = cluster_optimo %>% filter(Partido == "Labour Party" |
                                                     Partido == "The Left" |
                                                     Partido == "Conservative Party"),
                  aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                                     label = Partido), 
                  # color = "#d9060c",
                  nudge_y = 3.5)+
  
  
  
  geom_label( fill = "#F0F0F0",
              x = 0.5, y = 6.5, 
              label="Valores promedio del Cluster 1:
                                \n•Valores de la democracia liberal: 3.4\n•Retórica usada hacia los políticos: 5.0\n•Voluntad del pueblo: 3.9\n•A favor o en contra de los controles y\nbalances hacia el poder ejecutivo: 3.7",
              size = 3.55, color="black")+
  
  
  scale_color_gradient(low = "#0B465F", high = "#50F2B7", limits = c(0, 10))+
  scale_size(guide = F)+
  # scale_radius(limits = c(0, 10), range = c(0, 10))+
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))+
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))+
  
  theme_void()+
  
  
  theme(plot.background = element_rect(fill = "#F0F0F0", color = F),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.title.x = element_text(inherit.blank = F),
        axis.title.y = element_text(inherit.blank = F, angle = 90),
        axis.text.x = element_text(angle = 0, size = 12, vjust = 0, hjust = .5),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "#F0F0F0", color = F),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 12, hjust = 1),
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right")+
  
  labs(x = "Voluntad del pueblo", y = "A favor o en contra\nde los controles y\nbalances hacia el\npoder ejecutivo",
       title="Cluster 1: Partidos populistas que favorecen las normas y principios de la democracia liberal",
       subtitle = "Tienen una retórica que favorece los controles y balances hacia el poder ejecutivo ",
       color = "Valores de la\ndemocracia liberal",
       size = " ")



c2 <-
  ggplot(cluster_optimo %>% filter(num_cluster == 2))+
  
  geom_point( aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                  color = Valores_democrático_liberales, 
                  size = Retórica_hacia_políticos), 
              alpha = .5)+
  geom_point(data = morena, aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                                color = Valores_democrático_liberales,
                                size = Retórica_hacia_políticos), 
             shape = 21, colour = "#d9060c", size = 8, stroke = 1)+
    
  geom_text_repel(data = morena, aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                                     label = Partido), 
                  color = "#d9060c",
                  nudge_y = 1)+
  
  geom_text_repel(data = cluster_optimo %>% filter(Partido == "Republican Party" |
                                                     Partido == "Communist Party of Greece" |
                                                     Partido == "Vox" |
                                                     Partido == "United Socialist Party of Venezuela/Great Patriotic Pole"),
                  aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                      label = Partido), 
                  # color = "#d9060c",
                  nudge_x = 1.5)+
  
  
  
  geom_label( fill = "#F0F0F0",
              x= 7.5, y=5, 
              label="Valores promedio del Cluster 2:
                                \n•Valores de la democracia liberal: 7.3\n•Retórica usada hacia los políticos: 7.9\n•Voluntad del pueblo: 2.7\n•A favor o en contra de los controles y\nbalances hacia el poder ejecutivo: 5.7",
              size=4, color="black")+
  
  
  
  scale_color_gradient(low = "#0B465F", high = "#50F2B7", limits = c(0, 10),guide = F)+
  # scale_size(guide = F)+
  scale_radius(limits = c(0, 10), range = c(0, 10))+
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))+
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))+
  
  theme_void()+
  
  
  theme(plot.background = element_rect(fill = "#F0F0F0", color = F),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(angle = 0, size = 12, vjust = 0, hjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(inherit.blank = F),
        axis.title.y = element_text(inherit.blank = F, angle = 90),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "#F0F0F0", color = F),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 12, hjust = 1),
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right")+
  
  labs(x = "Voluntad del pueblo", y = "A favor o en contra\nde los controles y\nbalances hacia el\npoder ejecutivo",
       title="Cluster 2: Partidos populistas contra los grupos políticos y contra la democracia liberal",
       subtitle = "Tienen una retórica que expresa que los políticos se deben limitar a seguir la voluntad del pueblo",
       color = "Valores de la democracia liberal",
       size = "Retórica hacia\nlos políticos")




c3 <- 
  ggplot(cluster_optimo %>% filter(num_cluster == 3))+
  
  geom_point( aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                  color = Valores_democrático_liberales, 
                  size = Retórica_hacia_políticos), 
              alpha = .5)+
  
  geom_text_repel(data = cluster_optimo %>% filter(Partido == "Israel is our Home" |
                                                     Partido == "Party of Communists" |
                                                     Partido == "Shas" |
                                                     Partido == "Malaysian Islamic Party") ,
                  aes(x = Voluntad_del_pueblo, y = Controles_y_balances_al_Ejecutivo, 
                      label = Partido), 
                  # color = "#d9060c",
                  nudge_y = -4)+
  
  
  geom_label( fill = "#F0F0F0",
              x = 1, y = 5, 
              label="Valores promedio del Cluster 3:
                                \n•Valores de la democracia liberal: 7.2\n•Retórica usada hacia los políticos: 5.3\n•Voluntad del pueblo: 6.7\n•A favor o en contra de los controles y\nbalances hacia el poder ejecutivo: 7.5",
              size = 4, color="black")+
  
  
  
  scale_color_gradient(low = "#0B465F", high = "#50F2B7", limits = c(0, 10), guide = F)+
  scale_size(guide = F)+
  # scale_radius(limits = c(0, 10), range = c(0, 10))+
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))+
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))+
  
  theme_void()+
  
  
  theme(plot.background = element_rect(fill = "#F0F0F0", color = F),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(angle = 0, size = 12, vjust = 0, hjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(inherit.blank = F),
        axis.title.y = element_text(inherit.blank = F, angle = 90),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "grey75", color = F),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 12),
        legend.position = c(.05, .05),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left")+
  
  labs(x = "Voluntad del pueblo", y = "A favor o en contra\nde los controles y\nbalances hacia el\npoder ejecutivo",
       title="Cluster 3: Partidos populistas en contra de los controles hacia el poder ejecutivo ",
       subtitle = "Tienen una retórica que se opone fuertemente a los principios de la democracia liberal",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020",
       color = "Valores de la democracia liberal",
       size = "El tamaño de cada punto refleja la escala\nde la variable Retórica hacia los políticos")



#Patchwork, juntar las gráficas

c1/c2/c3 + plot_annotation(
  title = "Análsis de clusters de partidos políticos populistas",
  subtitle = "Cada círculo representa un partido",
  theme = theme(plot.title = element_text(size = 18, face = "bold"),
                plot.subtitle = element_text(color = "black", size = 14))
  )+

ggsave("G7 Clusters.png", width = 15, height = 12, dpi = 200)







