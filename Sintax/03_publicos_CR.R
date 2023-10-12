################################################################################
## Identificar públicos en UAM y Latina    ##
## Creado: 05-oct-2023
## Modif : 06-oct-2023
# Objetivo: 
# Identificar cómo son los estudiantes nuevos en universidades de CR
# las características de las personas DEBEN ser observables en ECE 
################################################################################
rm(list=ls())

#### Librerías ####
set.seed(999)

library(tidyverse)
library(data.table)
library("DBI")
# library("openxlsx2")

#### Data ####

# 0. Coneccion Reportes
con <- DBI::dbConnect(odbc::odbc(), 
                      .connection_string = "Driver={ODBC Driver 17 for SQL Server};",
                      Server   = "SGCN05.udla-ec.int", 
                      database= "Reportes", 
                      UID = "USRREPORTES",
                      PWD="USRREPORTES")



# 1.  Estudiantes Nuevos (Enrollment + Bulk)

query_bulk <- read_lines(paste0(getwd(),"/Sintax/bulk_query.sql"))|> paste(collapse = "\n")

df_bulk    <- dbGetQuery(con, query_bulk) |> as_tibble() 

df_bulk <- df_bulk|> 
  distinct(IdInstitucion, NOMBRE_CARRERA, PERIODO, CedulaPasaporte ,.keep_all = TRUE)



enrollment_consulta <- read_lines(paste0(getwd(),"/Sintax/query_Enrollment.sql")) |>
                       paste(collapse = "\n")


df_enrollment_CR <- dbGetQuery(con, enrollment_consulta) 


df_enrollment_CR <- df_enrollment_CR |> 
  as_tibble()|> 
  distinct(IdInstitucion, IdCarrera, Semestre ,CIAlumno, .keep_all = TRUE)



data <- df_enrollment_CR |> 
  left_join(df_bulk, 
            by= join_by("IdInstitucion", 
                        "IdCarrera",
                        "Semestre"=="PERIODO",
                        "CIAlumno"=="CedulaPasaporte"),
            relationship="one-to-one")




res_marca <- data |> 
  mutate(edad = round(as.numeric(difftime(FechaInicioSemestre, FechaNacimiento, units="days")/365.25),0),
         estadoCivil    =if_else(is.na(estadoCivil),"Desconocido",estadoCivil),
         es_soltero     =if_else(estadoCivil %in% c("Soltero(a)", "Divorciado(a)", "Viudez"),1,0),
         es_soltero     =if_else(estadoCivil=="Desconocido", NA_real_,es_soltero),
         jefehogar      =as.integer(jefehogar),
         hijos          =as.integer(Hijos),
         hijos          =if_else(hijos>0,1,0),
         trabaja        =as.integer(status_lab),
         IngresoPersonal=as.integer(IngresoPersonal),
         ingresoPadre   =as.integer(ingresoPadre),
         ingresoMadre   =as.integer(ingresoMadre) ) |> 
  rowwise() |> 
  mutate(ingreso_hogar=(IngresoPersonal+ingresoPadre + ingresoMadre)) |> 
  filter(!is.na(ingreso_hogar))

# class(res_marca$ingreso_hogar)

sum_marca <- res_marca|> 
  ungroup() |> 
  group_by(IdInstitucion) |> 
  summarise(n=n(),
            mean_edad    =round(mean(edad,      na.rm=T),0),
            q1_edad      = quantile(edad, 1/4),
            q3_edad      = quantile(edad, 3/4),
            es_soltero   =round(100*mean(es_soltero, na.rm=T),1),
            es_jefe_hogar=round(100*mean(jefehogar, na.rm=T),1),
            trabaja      =round(100*mean(trabaja, na.rm=T),1),
            hijos        =round(100*mean(hijos, na.rm=T),1),
            med_ingreso_hogar= median(ingreso_hogar, na.rm=T),
            q1_ingreso  = quantile(ingreso_hogar, 1/4),
            q3_ingreso  = quantile(ingreso_hogar, 3/4))

# Exportar publicos de referencia
fwrite(sum_marca, paste0(getwd(),"/Output/publico_referencia.csv"))
