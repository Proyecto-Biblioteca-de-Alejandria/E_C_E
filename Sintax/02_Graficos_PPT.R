##############################################
##     Encuesta Continua de Empleo [CR]     ##
## Creado: 4-Oct-2023
## Modif : 25-Abr-2024
# Objetivo: Gráficos ECE - PPT

rm(list=ls())

# Librerías
library(tidyverse)
library(data.table)
library(viridis)

# Funciones de ayuda y datos
# source(paste0(getwd(),"/Sintax/00_funciones_ayuda.R"))
# source(paste0(getwd(),"/Sintax/01_data"))

# Data (ECE_data_resumen.csv viene de: 01_data.R)
ECE_data <- fread(paste0(getwd(),"//Output/ECE_data_resumen.csv"))

#### Gráficos ####

#### Rama de Actividad ####


##### Personas Ocupadas #### 
data_plot <- ECE_data |> 
  filter(var %in% c("Condicion_actividad"),
         Trimestre %in% c("2020-01","2021-01", "2022-01",
                          "2023-01", "2023-02", "2023-03", "2023-04", "2024-01", "2024-02"),
         categoria %in% c("1.Ocupado")) |> 
  mutate(n=round((n/1000),1))


p_ocup <- data_plot |> 
  ggplot(aes(x=Trimestre, y=n, group=categoria,
             label=n)) + 
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=2, colour="#C4C4C4")+
  geom_text(size=4, nudge_y = 10, colour="#741306")+
  labs(title = "Personas Ocupadas",
       x="",
       y="Miles de personas",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  facet_wrap(~categoria)+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = 11),
        strip.background=element_rect(fill = "#C4C4C4", linetype = 0))

p_ocup

#### Rama de Actividad ####

rama_top_10 <- ECE_data |> 
  filter(var %in% c("Rama_actividad"),
         Trimestre=="2024-02") |>
  arrange(-n) |> 
  head(12) |> 
  select(categoria) 


data_plot <- ECE_data |> 
  filter(var %in% c("Rama_actividad"),
         Trimestre %in% c("2020-02","2021-02", "2022-02",
                          "2023-01", "2023-02", "2023-03", "2023-04", "2024-01", "2024-02"),
         categoria %in% rama_top_10$categoria) |> 
  mutate(n=round((n/1000),1))


rama_act <- data_plot |> 
  ggplot(aes(x=Trimestre, y=n, group=categoria,
             label=n)) + 
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_text(size=4, nudge_y = 10, colour="#741306")+
  labs(title = "Ocupados por Rama de actividad",
       x="",
       y="Miles de personas",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  facet_wrap(~categoria)+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = 11),
        strip.background=element_rect(fill = "#C4C4C4", linetype = 0))

rama_act




#### Informalidad del empleo ####

data_plot <- ECE_data |> 
  filter(var %in% c("Subempleado"),
         categoria=="2.Sí") |> 
  mutate(lag_1=lag(prop,n=1),
         var_t1=round((prop-lag_1),1),
         lag_4=lag(prop,n=4),
         var_t4=(prop-lag_4)) 


sub_emp <- data_plot |> 
  ggplot(aes(x=Trimestre, y=prop, group=1, 
             label=prop)) + 
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_text(size=5, nudge_y = 0.8, colour="#741306")+
  # geom_text(nudge_y=0.8, size=5)+
  annotate("text", 
           x = tail(data_plot$Trimestre, n=1), 
           y = tail(data_plot$prop, n=1)-1.5, 
           label = paste0("ΔT-1: ",tail(data_plot$var_t1, n=1),"pp"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(data_plot$Trimestre, n=1), 
           y = tail(data_plot$prop, n=1)-3.5 , 
           label = paste0("ΔT-4: ",tail(data_plot$var_t4, n=1),"pp"),
           size=4,
           colour="#741306")+
  scale_y_continuous(limits = c(0, 30))+
  # scale_color_viridis(option = "magma") +
  labs(title = "Sub Empleo",
       x="",
       y="%",
       colour="",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "none")

sub_emp

#### Desempleo ####
data_plot <- ECE_data |> 
  filter(var=="Condicion_actividad",
         categoria=="2.Desempleado") |> 
  mutate(lag_1=lag(prop,n=1),
         var_t1=round((prop-lag_1),2),
         lag_4=lag(prop,n=4),
         var_t4=round((prop-lag_4),2)) 

desem <- data_plot |> 
  ggplot(aes(x=Trimestre, y=prop, group=1, 
             label=prop)) + 
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=3, colour="#C4C4C4")+
  geom_text(size=5, nudge_y = 0.8, colour="#741306")+
  # geom_text(nudge_y=0.8)+
  annotate("text", 
           x = tail(data_plot$Trimestre, n=1), 
           y = tail(data_plot$prop, n=1)-1.5, 
           label = paste0("ΔT-1: ",tail(data_plot$var_t1, n=1),"pp"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(data_plot$Trimestre, n=1), 
           y = tail(data_plot$prop, n=1)-3.5 , 
           label = paste0("ΔT-4: ",tail(data_plot$var_t4, n=1),"pp"),
           size=4,
           colour="#741306")+
  scale_y_continuous(limits = c(0, 20))+
  # scale_color_viridis(option = "magma")+
  labs(title = "Desempleo",
       x="",
       y="%",
       colour="",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "none")

desem


#### PEI por Educación ####
asis_univ <-ECE_data |> 
  filter(var=="Educacion_asiste",
         categoria=="3.Parauniversitaria o universitaria",
         Trimestre>="2020-01") |> 
  mutate(n=round((n/1000),1),
         lag_1=lag(n),
         var_t1=round(100*(n/lag_1-1),1),
         lag_4=lag(n,n=4),
         var_t4=round(100*(n/lag_4-1),1))

g_asis_univ <- asis_univ |> 
  ggplot(aes(x=Trimestre, y=n ,group=1, colour=n))+
  # geom_line()+
  # geom_text(aes(label=n), size=3, nudge_y=10)+
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=3, colour="#C4C4C4")+
  geom_text(aes(label=n), size=5, nudge_y = 5, colour="#741306")+
  annotate("text", 
           x = tail(asis_univ$Trimestre, n=1), 
           y = tail(asis_univ$n, n=1)-5, 
           label = paste0("ΔT-1: ",tail(asis_univ$var_t1, n=1),"%"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(asis_univ$Trimestre, n=1), 
           y = tail(asis_univ$n, n=1)-15 , 
           label = paste0("ΔT-4: ",tail(asis_univ$var_t4, n=1),"%"),
           size=4,
           colour="#741306")+
  scale_color_viridis(option = "magma")+
  scale_y_continuous(limits = c(200, 330))+
  # geom_col(alpha=0.7)+
  labs(
    title = "Estudiantes Universitarios (Miles)",
    x="",
    y="Miles de estudiantes",
    caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "none")

g_asis_univ


#### No trabaja por estudiar ####
pei_educ <-ECE_data |> 
  filter(Trimestre>="2020-01",
         var=="Motivo_nobusco",
         categoria=="11.Asiste a centro de enseñanza") |> 
  arrange(Trimestre) |> 
  mutate(n=round((n/1000),1),
         lag_1=lag(n,n=1),
         var_t1=round(100*(n/lag_1-1),1),
         lag_4=lag(n,n=4),
         var_t4=round(100*(n/lag_4-1),1))

g_pei_educ <- pei_educ |> 
  ggplot(aes(x=Trimestre, y=n ,group=1, colour=n))+
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=3, colour="#C4C4C4")+
  geom_text(aes(label=n), size=5, nudge_y = 1.5, colour="#741306")+
  annotate("text", 
           x = tail(pei_educ$Trimestre, n=1), 
           y = tail(pei_educ$n, n=1)-1, 
           label = paste0("ΔT-1: ",tail(pei_educ$var_t1, n=1),"%"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(pei_educ$Trimestre, n=1), 
           y = tail(pei_educ$n, n=1)-3 , 
           label = paste0("ΔT-4: ",tail(pei_educ$var_t4, n=1),"%"),
           size=4,
           colour="#741306")+
  scale_color_viridis(option = "magma")+
  scale_y_continuous(limits = c(0, 30))+
  labs(title = "No busca Trabajo por Estudiar",
       x="",
       y="Miles de personas",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "none")

g_pei_educ


#### Ninis ####
ninis <-ECE_data |> 
  filter(Trimestre>="2020-01",
         var=="Joven_nini",
         categoria=="Sí") |> 
  arrange(Trimestre) |> 
  mutate(n=round((n/1000),1),
         lag_1=lag(n,n=1),
         var_t1=round(100*(n/lag_1-1),1),
         lag_4=lag(n,n=4),
         var_t4=round(100*(n/lag_4-1),1))

g_ninis <- ninis |> 
ggplot(aes(x=Trimestre, y=n ,group=1, colour=n))+
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=3, colour="#C4C4C4")+
  geom_text(aes(label=n), size=5, nudge_y = 1.5, colour="#741306")+
  annotate("text", 
           x = tail(ninis$Trimestre, n=1), 
           y = tail(ninis$n, n=1)-5, 
           label = paste0("ΔT-1: ",tail(ninis$var_t1, n=1),"%"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(ninis$Trimestre, n=1), 
           y = tail(ninis$n, n=1)-10, 
           label = paste0("ΔT-4: ",tail(ninis$var_t4, n=1),"%"),
           size=4,
           colour="#741306")+
  # scale_color_viridis(option = "magma")+
  # scale_y_continuous(limits = c(100, 225))+
  labs(title = "Jóvenes Nini",
       x="",
       y="Miles de personas",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "none")

g_ninis

## No estudia por trabajo 
table(ECE_data$var)
no_edu <- ECE_data |> 
  filter(Trimestre>="2020-01",
         var %in% c("numerador_no_edu","denominador_no_edu"),
         categoria==1) |>
  pivot_wider(id_cols = Trimestre,
              names_from = var,
              values_from =n) |>
  mutate(relacion=round(100*numerador_no_edu/denominador_no_edu,2)) |> 
  arrange(Trimestre) |> 
  mutate(# Porcentuales
         lag_1=lag(relacion,n=1),
         var_t1=round(100*(relacion/lag_1-1),1),
         lag_4=lag(relacion,n=4),
         var_t4=round(100*(relacion/lag_4-1),1),
         #Nominales
         lag_nom_1=lag(numerador_no_edu,n=1),
         var_nom_t1=round((100*(numerador_no_edu-lag_nom_1)/lag_nom_1),1),
         lag_nom_4=lag(numerador_no_edu,n=4),
         var_nom_t4=round((100*(numerador_no_edu-lag_nom_4)/lag_nom_4),1),
         numerador_no_edu = round(numerador_no_edu/1000,2))

g_no_edu_1 <- no_edu |> 
  ggplot(aes(x=Trimestre, y=relacion ,group=1))+
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=3, colour="#C4C4C4")+
  geom_text(aes(label=relacion), size=5, nudge_y = 1.5, colour="#741306")+
  annotate("text", 
           x = tail(no_edu$Trimestre, n=1), 
           y = tail(no_edu$relacion, n=1)-2, 
           label = paste0("ΔT-1: ",tail(no_edu$var_t1, n=1),"%"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(no_edu$Trimestre, n=1), 
           y = tail(no_edu$relacion, n=1)-4, 
           label = paste0("ΔT-4: ",tail(no_edu$var_t4, n=1),"%"),
           size=4,
           colour="#741306")+
  scale_y_continuous(limits = c(50,90))+
  labs(title = "No estudian por trabajar o interés en hacerlo",
       x="",
       y="%",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "none")

g_no_edu_1


g_no_edu_2 <- no_edu |> 
  ggplot(aes(x=Trimestre, y=numerador_no_edu ,group=1))+
  geom_line(linewidth=1.2, colour="#C4C4C4")+
  geom_point(size=3, colour="#C4C4C4")+
  geom_text(aes(label=numerador_no_edu), size=5, nudge_y = 5, colour="#741306")+
  annotate("text", 
           x = tail(no_edu$Trimestre, n=1), 
           y = tail(no_edu$numerador_no_edu, n=1)-10, 
           label = paste0("ΔT-1: ",tail(no_edu$var_nom_t1, n=1),"%"),
           size=4,
           colour="#741306")+
  annotate("text", 
           x = tail(no_edu$Trimestre, n=1), 
           y = tail(no_edu$numerador_no_edu, n=1)-20, 
           label = paste0("ΔT-4: ",tail(no_edu$var_nom_t4, n=1),"%"),
           size=4,
           colour="#741306")+
  scale_y_continuous(limits = c(500,700))+
  labs(title = "Personas que no estudian por trabajar o interés en hacerlo",
       x="",
       y="Miles de personas",
       caption = "[CR] INEC - Encuesta Continua de Empleo")+
  theme_minimal() +
  theme(legend.position = "none")

g_no_edu_2

