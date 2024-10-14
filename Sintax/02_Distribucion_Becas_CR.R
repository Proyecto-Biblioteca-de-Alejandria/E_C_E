# Elasticiddad precio de la demanda
# Inteligencia de la información
# Creación :     25-sep-2023
# Modificacion : 25-sep-2023

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

#### DATA ####

#### Enrollment ####
query_enrollment <- read_lines(paste0(getwd(), "/Sintax/Hugo_query_historico_enrollment.sql")) |> paste(collapse = "\n")

query_enrollment <- dbGetQuery(con, query_enrollment) |> 
  mutate(PerfilAlumno=str_trim(PerfilAlumno ,"both"),
         CARNET_     =str_trim(CARNET_      ,"both")) |>
  filter(PerfilAlumno %in% c("N", "NC"))

### Facturas ###
query_facturas <- read_lines(paste0(getwd(), "/Sintax/Hugo_query_facturacion.sql")) |> paste(collapse = "\n")

query_facturas <- dbGetQuery(con, query_facturas) |> 
  mutate(CARNET_ESTUDIANTE = str_trim(CARNET_ESTUDIANTE ,"both"))


#### Match de enrollment con facturación ####
data <- left_join(query_enrollment, query_facturas, 
                  by=join_by("MARCA",
                             "CARRERA",
                             "CARNET_"=="CARNET_ESTUDIANTE")) |> 
  distinct(.keep_all = TRUE) |> 
  select(MARCA, SEDE, FACULTAD, GRADO, CARRERA,  CARNET_, 
         CEDULA, ANO_PERIODO,monto_matricula, des_matricula, monto_curso, des_curso, 
         TOTAL_CURSOS,monto_neto_factura, MONTO_CONTADO,MONTO_TARJETA,MONTO_FINANCIADO)

rm(query_enrollment, query_facturas)


tipo_pago <- c("MONTO_CONTADO","MONTO_TARJETA","MONTO_FINANCIADO")


#### Elasticidad ####
# Agrupamos datos a nivel de estudiante
elast_data <- data |> 
  group_by(MARCA,SEDE, FACULTAD, GRADO, CARRERA, CEDULA, ANO_PERIODO) |> 
  summarise(Estudiante=1, 
            monto_matricula = sum(monto_matricula    , na.rm=T),
            des_matricula   = sum(des_matricula      , na.rm=T),
            monto_curso     = sum(monto_curso        , na.rm=T),
            des_curso       = sum(des_curso          , na.rm=T),
            neto_factura    = sum(monto_neto_factura , na.rm=T),
            MONTO_CONTADO   = sum(MONTO_CONTADO      , na.rm=T),
            MONTO_TARJETA   = sum(MONTO_TARJETA      , na.rm=T),
            MONTO_FINANCIADO= sum(MONTO_FINANCIADO   , na.rm=T)) |>
  rowwise() |>
  mutate(beca= round((abs(des_matricula+des_curso)/(abs(des_matricula+des_curso)+ monto_matricula+monto_curso)),2),
         flag_3          = if_else((MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO)==0,1,0), # Flag por pago=0
         flag_2          = if_else(neto_factura>4000000,1,0), # Flag por facturas infladas
         flag            = if_else((MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO)!=neto_factura,1,0), # Flag por inconsistencias entre pagos.
         pr_diff         = if_else(flag==1, (MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO / neto_factura),NA_real_),
         diff            = neto_factura-(MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO),
         pr_contado      = MONTO_CONTADO   / (MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO),
         pr_tarjeta      = MONTO_TARJETA   / (MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO),
         pr_finan        = MONTO_FINANCIADO/ (MONTO_CONTADO+MONTO_TARJETA+MONTO_FINANCIADO),
         MONTO_CONTADO   = if_else(flag==1,pr_contado * diff + MONTO_CONTADO,MONTO_CONTADO ),
         MONTO_TARJETA   = if_else(flag==1,pr_tarjeta * diff + MONTO_TARJETA,MONTO_TARJETA ), 
         MONTO_FINANCIADO= if_else(flag==1,pr_finan   * diff + MONTO_FINANCIADO,MONTO_FINANCIADO),
         scores          = list(c_across(all_of(tipo_pago))),
         max_index       = order(-scores)[1],
         Tipo_pago       = tipo_pago[max_index],
         Tipo_pago       = str_remove(Tipo_pago, "MONTO_"),
         periodo         = str_sub(ANO_PERIODO, 6,7),
         year            = str_sub(ANO_PERIODO, 1,4)) |> 
  filter(flag_2==0, flag_3==0)  |> 
  mutate(across(c(flag, flag_2, flag_3, pr_diff, diff, pr_contado, pr_tarjeta, pr_finan, scores, max_index), .fns = ~NULL))



# PLOTS
## Gráfico : Distribución de becas por universidad
elast_data |> 
  ggplot(aes(x=beca, y=year, fill=year))+
  # geom_violin(alpha=0.8)+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "magma", direction = -1, discrete = TRUE, alpha=0.8) +
  scale_x_continuous(limits = c(0, 0.5), breaks = seq(0,0.5,0.1))+
  labs(title = "Beca por Marca y periodo",
       x="Beca %",
       y="Año")+
  facet_wrap(vars(MARCA, periodo))+
  theme_minimal()+
  theme(legend.position = "none")