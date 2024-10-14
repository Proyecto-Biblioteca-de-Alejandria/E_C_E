
rm(list=ls())

# Librerías
library(tidyverse)
library(data.table)
library(openxlsx2)
library(viridis)

#### DATA ####

# files <- list.files(paste0(getwd(),"//Data/BD_Matrícula_Estatal/"))
# data <- tibble()
# for (file in files) {
#   row <- read_xlsx(paste0(getwd(),"//Data/BD_Matrícula_Estatal/", file), sheet="data")
#   
#   data <- rbind(data, row)
# }
# data <- data |> 
#   mutate(id= row_number())
# fwrite(data, paste0(getwd(),"//Data/BD_Matrícula_Estatal/matricula_estatal.csv" ))

data <- fread(paste0(getwd(),"//Data/BD_Matrícula_Estatal/matricula_estatal.csv" ))

data <- data |> mutate(TIPO_MATRICULA = if_else(TIPO_MATRICULA=="Primer ingreso", "Primer Ingreso", TIPO_MATRICULA))


p1 <- data |> 
  group_by(AÑO, TIPO_MATRICULA) |> 
  summarise(n=n(),
            n=round(n/1000,1)) 
  
p1 |> 
  ggplot(aes(x=factor(AÑO), y=n, color=TIPO_MATRICULA, label=n, group = 1))+
    geom_line()+
    geom_label( ,size = 7)+
    scale_color_viridis(end = 0.45, discrete = TRUE)+
  facet_wrap(vars(TIPO_MATRICULA), scales ="free_y" ,nrow = 2)+
  labs(x="Años",
       y="Miles de personas",
       color="")+
  theme_minimal()+
  theme(legend.position = "none")


names(data)

p2 <- data |>
  filter(TIPO_MATRICULA=="Primer Ingreso") |> 
  group_by(AÑO, UNIVERSIDAD) |> 
  summarise(n=n(),
            n=round(n/1000,1)) 

p2 |> 
  ggplot(aes(x=factor(AÑO), y=n, color=UNIVERSIDAD, label=n, group = UNIVERSIDAD))+
  geom_line()+
  geom_label( ,size = 7)+
  scale_color_viridis( discrete = TRUE)+
  # facet_wrap(vars(UNIVERSIDAD), scales ="free_y", nrow = 5)+
  labs(x="Años",
       y="Miles de personas",
       color="")+
  theme_minimal()+
  theme(legend.position = "top",
        #legend.key.size = unit(7), 
        legend.text = element_text(size = 15),
        panel.grid= element_blank(),
        axis.text.y = element_blank())


p3 <- data |>
  # filter(TIPO_MATRICULA=="Primer Ingreso") |> 
  group_by(AÑO, AREA_CONOCIMIENTO) |> 
  summarise(n=n(),
            n=round(n/1000,1)) |> 
  ungroup() |> 
  group_by(AREA_CONOCIMIENTO) |> 
  mutate(lag_1=lag(n),
         var_t1=round(100*(n/lag_1-1),1),
         mean_var = round(mean(var_t1, na.rm = TRUE),1)) 

p3 |> 
  ggplot(aes(x=factor(AÑO), group = AREA_CONOCIMIENTO))+
  geom_line(aes(y=mean_var), colour="#741306", linetype = 2)+
  # geom_line(aes(y=mean_var), alpha=0.4)+
  geom_col(aes(y=var_t1), alpha=0.4, width = 0.6)+
  geom_label(aes(y=var_t1, label=var_t1),colour="#741306" ,size = 5)+
  # scale_color_viridis( discrete = TRUE)+
  facet_wrap(vars(AREA_CONOCIMIENTO), scales ="free_y" ,  nrow = 2)+
  labs(x="Años",
       y="Miles de personas",
       color="")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid= element_blank())
