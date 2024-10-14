# 8 - Evaluación FUNNEL CR
# Creación :     18-sep-2024
# Modificacion : 18-sep-2024

rm(list=ls())
#### Librerías ####

library(tidyverse)
library(data.table)
library("DBI")
library("openxlsx2")
library("ggridges")
library("viridis")
library(naniar)
#### Data ####

## Conección
con <- DBI::dbConnect(odbc::odbc(), 
                      .connection_string = "Driver={ODBC Driver 17 for SQL Server};",
                      Server   = "SGCN05.udla-ec.int",  #"sntssql01"
                      database=  "Reportes",
                      Trusted_Connection="yes")


query_crm <- "SELECT * FROM [BDD_Proyectos].[dbo].[CadenaComercialZOHO]"

df_crm <- dbGetQuery(con, query_crm) 

data <- df_crm |> 
  select(c("LE.Id", "LE.Hora de creación", "LE.Marca", "LE.Sede" ,"LE.Ciclo",
           "LE.Carrera de Interés", "LE.Base Residual?",
           "LE.Propietario de Posible cliente Name",
           "OPORTUNIDAD.Id", "OPORTUNIDAD.Fecha creación",
           "OPORTUNIDAD.Fase", "OPORTUNIDAD.SIS - Fecha de Matrícula",
           "OPORTUNIDAD.Propietario de Oportunidad Name" )) |> 
  filter(`LE.Hora de creación` > as.Date("2024-05-01"),
         `LE.Hora de creación` <= as.Date("2024-09-18")) |>
  mutate(leads = if_else(!is.na(LE.Id),1,0),
         ops   = ifelse(!is.na(OPORTUNIDAD.Id), 1, 0),
         docu  = if_else(ops==1 & OPORTUNIDAD.Fase=="7. Matriculado" ,1,0),
         conv_lead_op =ops/leads,
         conv_op_docu =docu/ops,
         lead_week = lubridate::week(`LE.Hora de creación`),
         inicio_clases_week = lubridate::week("2024-09-09")
         ) |> 
  mutate(week_dist= (lead_week- inicio_clases_week)) |> 
  filter(week_dist>=-12) 

# table(data$week_dist)

names(df_crm)

# Resumen Funnel
x <- data |> 
  group_by(week_dist, LE.Marca) |> 
  summarise(leads = sum(leads, na.rm=T),
            ops = sum(ops, na.rm=T),
            docu = sum(docu, na.rm=T),
            conv_lead_op= round(100*mean(conv_lead_op, na.rm=T),2),
            conv_op_docu= round(100*mean(conv_op_docu, na.rm=T),2)) |> 
  ungroup() |> 
  group_by(LE.Marca) |> 
  mutate(mean_conv_lead_op= round(mean(conv_lead_op),1),
         mean_conv_op_docu= round(mean(conv_op_docu),1))


# Funnel Comercial
p1 <-   x |> 
  ggplot(aes(x=week_dist, ))+
  geom_line(aes(y=leads, color = "1. Leads"), linewidth = 1 )+
  geom_label(aes(y=leads, label=leads, color = "1. Leads"),size = 5 )+
  geom_line(aes(y=ops, color = "2. Oportunidades"), linewidth = 1 )+
  geom_label(aes(y=ops, label=ops, color = "2. Oportunidades") ,size = 5)+
  geom_line(aes(y=docu, color = "3. Documentados"), linewidth = 1 )+
  geom_label(aes(y=docu, label=docu, color = "3. Documentados") ,size = 5)+
  scale_color_viridis(discrete = TRUE, end = 0.6, option = "magma")+
  labs(x="Semanas antes del inicio de clases",
       y="",
       colour="")+
  facet_wrap(~LE.Marca)+
  theme_minimal()+
  theme(legend.position = "top",
        panel.grid= element_blank(),
        axis.text.y = element_blank())
p1

factor <- 5

p1 <- x |> 
  ggplot(aes(x=week_dist))+
  geom_line(aes(y=leads, color = "1. Leads"), linewidth = 1 )+
  geom_label(aes(y=leads, label=leads, color = "1. Leads"), size = 5)+
  geom_line(aes(y=ops, color = "2. Oportunidades"), linewidth = 1 )+
  geom_label(aes(y=ops, label=ops, color = "2. Oportunidades"), size = 5)+
  geom_line(aes(y=docu*factor, color = "3. Documentados"), linewidth = 1 )+  # Ajuste del factor
  geom_label(aes(y=docu*factor, label=docu, color = "3. Documentados"), size = 5)+
  scale_color_viridis(discrete = TRUE, end = 0.6, option = "magma")+
  scale_y_continuous(
    #name = "Leads y Oportunidades", 
    sec.axis = sec_axis(~./factor, name = "")  # Segundo eje Y
  )+
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 0.6) +
  labs(x="Semanas antes del inicio de clases",
       y="",
       colour="")+
  facet_wrap(~LE.Marca)+
  theme_minimal()+
  theme(legend.position = "top",
        panel.grid= element_blank(),
        axis.text.y = element_blank())

p1
# matrículas totales
p2 <-   x |> 
    ggplot(aes(x=week_dist, color=LE.Marca, group = 1))+
    geom_line(aes(y=conv_lead_op,    color = "1. Lead-Op" ), size = 1)+
    geom_line(aes(y=mean_conv_lead_op,    color = "1. Lead-Op" ), linetype=2)+
    geom_text(aes(x=max(week_dist), label= mean_conv_lead_op,
                  y=mean_conv_lead_op,     color = "1. Lead-Op" ))+
    geom_line(aes(y=conv_op_docu,    color = "2. Op-Docu"), size = 1)+
    geom_line(aes(y=mean_conv_op_docu, color = "2. Op-Docu"), linetype=2)+
    geom_text(aes(x=max(week_dist), label= mean_conv_op_docu,
                  y=mean_conv_op_docu, color = "2. Op-Docu") )+
    scale_color_viridis(discrete = TRUE, end = 0.6, option = "magma")+
    labs(color = "",
         x="Semanas antes del inicio de clases",
         y="%")+
    facet_wrap(~LE.Marca)+
    theme_minimal()+
    theme(legend.position = "top")
          # panel.grid= element_blank(),
          # axis.text.y = element_blank())
  
p2  

# División por marca y sede
x1 <- data |> 
  group_by(LE.Marca, LE.Sede) |> 
  summarise(leads = sum(leads, na.rm=T),
            ops = sum(ops, na.rm=T),
            docu = sum(docu, na.rm=T),
            conv_lead_op= round(100*mean(conv_lead_op, na.rm=T),2),
            conv_op_docu= round(100*mean(conv_op_docu, na.rm=T),2)) |> 
  ungroup() |> 
  group_by(LE.Marca) |> 
  mutate(mean_conv_lead_op= round(mean(conv_lead_op, na.rm = T),1),
         mean_conv_op_docu= round(mean(conv_op_docu, na.rm = T),1))

# Tasas de conversión
# Tiempo entre eventos
