
rm(list=ls())

# Librerías
library(tidyverse)
# library(data.table)
library(openxlsx2)
library(viridis)

#### DATA ####

# ECE

files <- list.files(paste0(getwd(),"//Data/BD_Matrícula_Estatal/"))

for (file in files) {
  row <- read_xlsx(paste0(getwd(),"//Data/BD_Matrícula_Estatal/", file))
}

# Mercado POtencial
mercado_potencial_CR <- read_xlsx(paste0(getwd(), "/Data/Mercado_potencial_CR.xlsx")) |> 
  pivot_wider(id_cols = Trimestre, 
              names_from = Universidad,
              values_from =Potencial,
              names_prefix ="potencial_")

# Enrollment
df_enrollment <- read_xlsx(paste0(getwd(), "//Data/Enrollment_CR.xlsx"))

# Criterios de MAtching
# Cuatri	  Meses clases	ECE-Mathch
# 2020-1	   Ene-Abr	     2019 -4
# 2020-2	   May-Ago	     2020 -1
# 2020-3	   Sept-Dic      2020 -2


# Preparación

df_enrollment <- df_enrollment |> 
  rowwise() |> 
  mutate(enrollment_total=round(((UAM+Latina)/1000),2)) |> 
  ungroup() |> 
  mutate(anio=str_sub(Semestre,1,4),
         anio_lag=as.numeric(anio)-1,
         ece_match=case_when(str_detect(Semestre, "-1")~ str_c(anio_lag, "-04"),
                             str_detect(Semestre, "-2")~ str_c(anio, "-01"),
                             str_detect(Semestre, "-3")~ str_c(anio, "-02"),
                             TRUE~NA_character_),
         anio=NULL,
         anio_lag=NULL) 

# 0. Evolución proporción educación
evol_educacion <- ECE_data |> 
  filter(var=="Educacion_asiste",
         # categoria=="3.Parauniversitaria o universitaria",
         Trimestre>="2020-01") |> 
  mutate(n=round((n/1000),2)) |> 
  select(Trimestre,categoria, n) |> 
  pivot_wider(id_cols =Trimestre,
              names_from = categoria,
              values_from = n) |> 
  rowwise() |> 
  mutate(Otra_educ = (`4.Enseñanza especial`+ `5.Educación abierta en institutos para presentar exámenes ante el MEP`+`6.Otro tipo de formación no formal`)) |> 
  select(Trimestre, `1.Escuela`, `2.colegio`, `3.Parauniversitaria o universitaria`, Otra_educ) |> 
  rename(`3.Universitaria`=`3.Parauniversitaria o universitaria`,
         `4.Otra_educ`=Otra_educ)

names(evol_educacion)

g_0 <- evol_educacion |> 
  ggplot(aes(x=Trimestre, group=1))+
  # geom_line(aes(y=`1.Escuela`), linewidth=1.2, colour="#FDCECE")+
  # geom_point(aes(y=`1.Escuela`), size=2, colour="#FDCECE")+
  # geom_text(aes(y=`1.Escuela`, label=`1.Escuela`), size=5, nudge_y = 5, colour="#FDCECE")+
  geom_line(aes(y=`2.colegio`), linewidth=1.2, colour="#A5A5A5")+
  geom_point(aes(y=`2.colegio`), size=2, colour="#A5A5A5")+
  geom_text(aes(y=`2.colegio`, label=`2.colegio`), size=5, nudge_y = 5, colour="#A5A5A5")+
  geom_line(aes(y=`3.Universitaria`), linewidth=1.2, colour="#741306")+
  geom_point(aes(y=`3.Universitaria`), size=2, colour="#741306")+
  geom_text(aes(y=`3.Universitaria`, label=`3.Universitaria`), size=5, nudge_y = 5, colour="#741306")+
  geom_line(aes(y=`4.Otra_educ`), linewidth=1.2, colour="#907373")+
  geom_point(aes(y=`4.Otra_educ`), size=2, colour="#907373")+
  geom_text(aes(y=`4.Otra_educ`, label=`4.Otra_educ`), size=5, nudge_y = 5, colour="#907373")+
  labs(
    title = "Evolución de asistencia educativa",
    x="Trimestre",
    y="Miles de estudiantes",
    caption = "Fuente: ECE")+
  theme_minimal()+
  theme(legend.position = "none")

g_0


# 1. Asistencia Universitaria vs Enrollment total
ece_mercado_real <- ECE_data |> 
  filter(var=="Educacion_asiste",
         categoria=="3.Parauniversitaria o universitaria",
         Trimestre>="2020-01") |> 
  mutate(n=round((n/1000),2)) |> 
  select(Trimestre, n) 
  

enroll_vs_ece_total = df_enrollment |> 
  left_join(ece_mercado_real, by = join_by("ece_match"=="Trimestre")) |> 
  mutate(ratio_ocupacion=round((enrollment_total/n)*100,2))

names(enroll_vs_ece_total)

# Plot 1 - Asistencia Universitaria vs Enrollment total

g_1 <- enroll_vs_ece_total |> 
  ggplot(aes(x=Semestre, y=n ,group=1))+
  # geom_line()+
  # geom_text(aes(label=n), size=3, nudge_y=10)+
  geom_line(aes(y=enrollment_total), linewidth=1.2, colour="#741306")+
  geom_point(size=2, colour="#741306")+
  geom_text(aes(y=enrollment_total, label=enrollment_total), size=5, nudge_y = 5, colour="#741306")+
  geom_line(aes(y=n), linewidth=1.2, colour="#9A9A9A")+
  geom_point(size=2, colour="#9A9A9A")+
  geom_text(aes(label=n), size=5, nudge_y = 5, colour="#9A9A9A")+
  labs(
    title = "Total Enrollment vs Asistencia Universitaria",
    x="Cuatrimestre",
    y="Miles de estudiantes",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_1

g_2 <- enroll_vs_ece_total |> 
  ggplot(aes(x=Semestre, y=n ,group=1))+
  geom_line(aes(y=ratio_ocupacion), linewidth=1.2, colour="#741306")+
  geom_point(size=3, colour="#741306")+
  geom_text(aes(y=ratio_ocupacion, label=ratio_ocupacion), size=5, nudge_y = 0.5, colour="#741306")+
  scale_y_continuous(limits = c(0, 15))+
    labs(
    title = "Ocupación del mercado Total",
    x="Cuatrimestre",
    y="%",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_2

# 2. Mercado Potencial vs Enrollment Universidad
enroll_vs_potencial <-   df_enrollment |> 
  left_join(mercado_potencial_CR, by = join_by("ece_match"=="Trimestre")) |>   
  mutate(Latina          =round(Latina/1000,2),
         potencial_LATINA=round(potencial_LATINA/1000,2),
         UAM             =round(UAM/1000,2),
         potencial_UAM   =round(potencial_UAM/1000,2),
         ratio_latina=round((Latina/potencial_LATINA)*100,2),
         ratio_uam   =round((UAM/potencial_UAM      )*100,2),)

names(enroll_vs_potencial)

# names(enroll_vs_potencial)
g_3 <- enroll_vs_potencial |> 
  ggplot(aes(x=Semestre, group=1))+
    geom_line(aes(y=Latina), linewidth=1.2, colour="#741306")+
    geom_point(aes(y=Latina), size=2, colour="#741306")+
    geom_text(aes(y=Latina, label=Latina), size=5, nudge_y = 5, colour="#741306")+
      geom_line(aes(y=potencial_LATINA), linewidth=1.2, colour="#9A9A9A")+
      geom_point(aes(y=potencial_LATINA),size=2, colour="#9A9A9A")+
      geom_text(aes(y=potencial_LATINA, label=potencial_LATINA), size=5, nudge_y = 5, colour="#9A9A9A")+
  labs(
    title = "Universidad Latina",
    subtitle = "Enrollment vs Mercado Potencial",
    x="Cuatrimestre",
    y="Miles de estudiantes",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_3

# Ratio UAM
g_4 <- enroll_vs_potencial |> 
  ggplot(aes(x=Semestre, group=1))+
  geom_line(aes(y=ratio_latina), linewidth=1.2, colour="#741306")+
  geom_point(aes(y=ratio_latina), size=2, colour="#741306")+
  geom_text(aes(y=ratio_latina, label=ratio_latina), size=5, nudge_y = 0.6, colour="#741306")+
  scale_y_continuous(limits = c(0, 30))+
  labs(
    title = "Universidad Latina",
    subtitle = "% Ocupación Mercado Potencial",
    x="Cuatrimestre",
    y="%",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_4

# LATINA
g_5 <- enroll_vs_potencial |> 
  ggplot(aes(x=Semestre, group=1))+
  geom_line(aes(y=UAM), linewidth=1.2, colour="#741306")+
  geom_point(aes(y=UAM), size=2, colour="#741306")+
  geom_text(aes(y=UAM, label=UAM), size=5, nudge_y = 5, colour="#741306")+
  geom_line(aes(y=potencial_UAM), linewidth=1.2, colour="#9A9A9A")+
  geom_point(aes(y=potencial_UAM),size=2, colour="#9A9A9A")+
  geom_text(aes(y=potencial_UAM, label=potencial_UAM), size=5, nudge_y = 5, colour="#9A9A9A")+
  labs(
    title = "Universidad Americana",
    subtitle = "Enrollment vs Mercado Potencial",
    x="Cuatrimestre",
    y="Miles de estudiantes",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_5

# Ratio LATINA
g_6 <- enroll_vs_potencial |> 
  ggplot(aes(x=Semestre, group=1))+
  geom_line(aes(y=ratio_uam), linewidth=1.2, colour="#741306")+
  geom_point(aes(y=ratio_uam), size=2, colour="#741306")+
  geom_text(aes(y=ratio_uam, label=ratio_uam), size=5, nudge_y = 0.6, colour="#741306")+
  scale_y_continuous(limits = c(0, 20))+
  labs(
    title = "Universidad Americana",
    subtitle = "% Ocupación Mercado Potencial",
    x="Cuatrimestre",
    y="%",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_4


# UAM
g_3 <- enroll_vs_potencial |> 
  ggplot(aes(x=Semestre, group=1))+
    geom_line(aes(y=UAM), linewidth=1.2, colour="#741306")+
    geom_point(aes(y=UAM), size=2, colour="#741306")+
    geom_text(aes(y=UAM, label=UAM), size=5, nudge_y = 5, colour="#741306")+
      geom_line(aes(y=potencial_UAM), linewidth=1.2, colour="#9A9A9A")+
      geom_point(aes(y=potencial_UAM),size=2, colour="#9A9A9A")+
      geom_text(aes(y=potencial_UAM, label=potencial_UAM), size=5, nudge_y = 5, colour="#9A9A9A")+
  labs(
    title = "Universidad Americana",
    subtitle = "Enrollment vs Mercado Potencial",
    x="Cuatrimestre",
    y="Miles de estudiantes",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_3

# Ratio UAM
g_4 <- enroll_vs_potencial |> 
  ggplot(aes(x=Semestre, group=1))+
  geom_line(aes(y=ratio_uam), linewidth=1.2, colour="#741306")+
  geom_point(aes(y=ratio_uam), size=2, colour="#741306")+
  geom_text(aes(y=ratio_uam, label=ratio_uam), size=5, nudge_y = 0.6, colour="#741306")+
  scale_y_continuous(limits = c(0, 20))+
  labs(
    title = "Universidad Americana",
    subtitle = "% Ocupación Mercado Potencial",
    x="Cuatrimestre",
    y="%",
    caption = "Fuente: ECE y Enrollment")+
  theme_minimal()+
  theme(legend.position = "none")

g_6
