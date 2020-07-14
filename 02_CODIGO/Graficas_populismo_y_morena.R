####### GRÁFICAS SOBRE POPULISMO -------

Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

#### Librerías  ------

library(readr)
library(janitor)
library(tidyverse)
library(ggthemes)  



#### Datos  ------

pop <- read_csv("DATOS/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")
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



######## Gráfica sobre el Número de partidos populistas por país -------
#  Promedio de partidos populistas en países que tienen al menos un partido populista no marginal
bd %>%
  filter(Populismo >= 3 &
           Categorías_asientos_en_cámara >= 2) %>%
  group_by(País, Tipo_de_régimen_Vdem) %>%
  count(País) %>% arrange(-n) %>% ungroup() %>% count(n) %>% 
  mutate(total_partidos = n*nn) %>% 
  summarise(promedio_partidos_por_pais = sum(total_partidos)/sum(nn))



# Primero se preparan los datos
bd %>%
  # Este es el filtro para separar a los partidos populistas con más del 3% de asientos en sus congresos
  filter(Populismo >= 3 & 
           Categorías_asientos_en_cámara >= 2) %>% 
  group_by(País) %>% count(País) %>% arrange(-n) %>% ungroup() %>%
  count(n) %>%
  # Se grafican los datos
  ggplot(aes(x = n, y = nn))+
  geom_col(fill = "#0B465F")+
  coord_flip()+
  
  theme_fivethirtyeight()+
  
  scale_x_continuous(limits = c(0, 9), breaks = seq(1, 8, 1))+
  
  geom_text(aes( y = nn, label = nn),
            position = position_stack(), vjust = .5, hjust = 1.5,
            col = "grey80", size = 6)+
  
  
  labs(x = "Número de partidos populistas por país", y = "Número de países con al menos un partido populista",
       title="Número de partidos populistas por país",
       subtitle = "Total de países con al menos un partido populista: 87\nPromedio de partidos populistas por cada país que tiene al menos un partido populista: 2.72",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020")+
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(angle = 0, size = 14),
        axis.text.y = element_text(angle = 0, size = 16),
        axis.ticks = element_blank(),
        axis.title = element_text(size=14)) +
  
  ggsave("G1 número de partidos populistas por país.png", width = 14, height = 10, dpi = 200)


####### Gráfica sobre número de países con al menos un partido populista    ------

#primero obtener el número de países totales por tipo de régimen
y <-
  bd %>%
  select(País, Tipo_de_régimen_Vdem) %>% 
  group_by(País) %>% 
  summarise(Tipo_de_régimen_Vdem = max(Tipo_de_régimen_Vdem)) %>% ungroup() %>% 
  count(Tipo_de_régimen_Vdem)


#Después, hay que obtener el número de países con partidos populistas por régimen
x <-
  bd %>% 
  filter(Populismo >= 3 &
           Categorías_asientos_en_cámara >= 2) %>% 
  group_by(País, Tipo_de_régimen_Vdem) %>% 
  count(País) %>% arrange(-n) %>% ungroup() %>% 
  count(Tipo_de_régimen_Vdem)


#Unir los dataframes
Países_con_pop <- merge(x, y, by = "Tipo_de_régimen_Vdem", sort = F, all = T)


#Renombrar variables y obtener porcentajes
Países_con_pop <- 
  Países_con_pop %>% rename(Países_c_pp = n.x,
                            Total_países = n.y) %>% 
  mutate(Países_sin_pp = (Total_países - Países_c_pp),
         porcentaje_c_pp = (Países_c_pp / Total_países),
         porcentaje_s_pp = (Países_sin_pp / Total_países)) %>% na.omit()


#Graficar
Países_con_pop %>% select(1,2,4) %>% 
  #Se suma un uno a la variable Tipo de régimen para que al momento de graficar se comience en el valor 1 y no en el 0
  mutate(Tipo_de_régimen_Vdem = (Tipo_de_régimen_Vdem+1)) %>% 
  #Se pivotearon los datos, desde la variable número dos hasta la tres
  pivot_longer(2:3) %>% 
  
  ggplot(aes(x = Tipo_de_régimen_Vdem, y = value, fill = name))+
  geom_col()+
  coord_flip()+
  
  theme_fivethirtyeight()+
  
  geom_text(aes( y = value, label = value),
            position = position_stack(), vjust = .5, hjust = 1.1,
            col = "grey60", size = 6)+
  
  labs(x = " ", y = "Número de países",
       title="Número de países con al menos un partido populista por tipo de régimen",
       subtitle = "Número de países con al menos un partido populista: 83\nNúmero de países sin un solo partido populista: 74",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020",
       fill = " ")+
  
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))+
  
  #Aquí se modificaron los nombres de las variables discretas del eje x, a cada factor se le asignó una etiqueta
  scale_x_discrete(limits=c(1,2,3,4),
                   labels=c("Autocracia\nabsoluta", "Autocracia\nelectoral",
                            "Democracia\nelectoral", "Democracia\nliberal"))+
  
  
  #Se buscaron y se asignaron los colores de la paleta de manera manual al igual que en el resto de las gráficas
  scale_fill_manual(values = c("#0B465F", "#50F2B7"),
                    labels = c("Países con al menos\nun partido populista", "Países sin\npartido populista"))+
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(vjust = 0.3, hjust = .5, size = 14),
        axis.text.y = element_text(size = 16),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "grey75"),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 14),
        axis.title = element_text(size=14))+
  
  ggsave("G2 número de países con al menos un partido populista.png", width = 14, height = 10, dpi = 200)


#Aquí se identifican los 3 países con partidos populistas que no pudieron ser clasificados por tipo de régimen
bd %>% 
  filter(Populismo >= 3 &
           Categorías_asientos_en_cámara >= 2) %>% 
  select(País, Tipo_de_régimen_Vdem) %>% 
  group_by(País) %>% 
  summarise(Tipo_de_régimen_Vdem = max(Tipo_de_régimen_Vdem)) %>% ungroup() %>% 
  arrange(Tipo_de_régimen_Vdem) %>% tail()


#######   Gráfica sobre el % de países con al menos un partido populista    ------


Países_con_pop %>% select(1,2,4) %>% 
  mutate(Tipo_de_régimen_Vdem = (Tipo_de_régimen_Vdem+1)) %>%
  mutate(pct_c_pp = (Países_c_pp/(Países_c_pp+Países_sin_pp)),
         pct_sin_pp = (Países_sin_pp/(Países_c_pp+Países_sin_pp))) %>% 
  select(1,4,5) %>% 
  pivot_longer(2:3) %>% 
  
  ggplot(aes(x = Tipo_de_régimen_Vdem, y = value, fill = name))+
  geom_col()+
  coord_flip()+
  
  theme_fivethirtyeight()+
  
  geom_text(aes( y = value, label = round(value, digits = 2)),
            position = position_stack(), vjust = .5, hjust = 1.1,
            col = "grey60", size = 6)+
  
  labs(x = " ", y = " ",
       title="Porcentaje de países con al menos un partido populista",
       subtitle = "Por tipo de régimen",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020",
       fill = " ")+
  
  scale_x_discrete(limits=c(1,2,3,4),
                   labels=c("Autocracia\nabsoluta", "Autocracia\nelectoral",
                            "Democracia\nelectoral", "Democracia\nliberal"))+
  
  
  
  scale_fill_manual(values = c("#0B465F", "#50F2B7"),
                    labels = c("Porcentaje de\npaíses con al menos\nun partido populista", 
                               "Porcentaje de\npaíses sin\npartido populista"))+
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(vjust = 0.3, hjust = .5, size = 14),
        axis.text.y = element_text(size = 16),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "grey75"),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 14),
        axis.title = element_text(size=12))+
  
  ggsave("G3 Porcentaje de países con al menos un partido populista.png", width = 14, height = 10, dpi = 200)


#######   Gráfica sobre el número de partidos pluralistas y populistas    ------


bd %>% mutate(con_partido_populista = ifelse(Populismo >= 3, "Con populismo", "Sin populismo")) %>% 
  #Se filtran todos los partidos no marginales, con más del 3% de aisentos en sus respectivas cámaras legislativas
  filter(Categorías_asientos_en_cámara >= 2) %>%
  group_by( Tipo_de_régimen_Vdem, con_partido_populista) %>% 
  count() %>% na.omit() %>% ungroup() %>% 
  mutate(Tipo_de_régimen_Vdem = (Tipo_de_régimen_Vdem+1)) %>% 
  
  ggplot(aes(x = Tipo_de_régimen_Vdem, y = n, fill = con_partido_populista))+
  geom_col()+
  coord_flip()+
  
  labs(x = " ", y = "Número de partidos",
       title="Número de partidos no marginales tanto pluralistas como populistas por tipo de régimen",
       subtitle = "Número de partidos pluralistas: 192\nNúmero de partidos populistas: 233",
       caption = "Elaborado por @1LuisDavid con datos de Global Party Survey 2020",
       fill = " ")+
  
  theme_fivethirtyeight()+
  
  geom_text(aes( y = n, label = n),
            position = position_stack(), vjust = .5, hjust = 1.1,
            col = "grey60", size = 6)+
  
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 30))+
  
  scale_x_discrete(limits=c(1,2,3,4),
                   labels=c("Autocracia\nabsoluta", "Autocracia\nelectoral",
                            "Democracia\nelectoral", "Democracia\nliberal"))+
  
  scale_fill_manual(values = c("#0B465F", "#50F2B7"),
                    labels = c("Partidos\npopulistas", "Partidos\npluralistas"))+
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "black", size = 14),
        plot.caption = element_text(face = "italic", hjust = 1, size = 11),
        axis.text.x = element_text(vjust = 0.3, hjust = .5, size = 14),
        axis.text.y = element_text(size = 16),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "grey75"),
        legend.key = element_rect(fill = "grey75", color = NA),
        legend.text = element_text(size = 14),
        axis.title = element_text(size=12))+
  
  ggsave("G4 Número de partidos tanto pluralistas como populistas.png", width = 14, height = 10, dpi = 200)


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











