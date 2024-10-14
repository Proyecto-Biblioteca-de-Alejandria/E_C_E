##############################################
##     Encuesta Continua de Empleo [CR]     ##
## Creado: 28-sep-2023
## Modif : 28-sep-2023
# Objetivo: Carga y tratamiento de datos.

rm(list=ls())
# Librerías
library(tidyverse)
library(haven)
library(survey)
library(viridis)
library(data.table)

# Funciones de ayuida
source(paste0(getwd(),"/Sintax/00_funciones_ayuda.R"))

## Variables de interés
vars_interes <- c("Condicion_actividad", "Subempleado", 
                  "Rama_actividad", "Educacion_asiste", 
                  "Motivo_nobusco","Joven_nini")

variables_trabajo <- c("Desea_trabajar", "Educacion_nivel_grado")

vars_demograf <- c("Edad", "Sexo", "Estado_conyugal")

vars_ident <- c("ID_AMO","ID_TRIMESTRE",
                "ID_VIVIENDA","Factor_ponderacion")

# Carga
  ## Archivos:
files_ECE <- list.files(paste0(getwd(),"/Data"), ".sav")

ECE_data <- tibble()

# Loop

for (file in files_ECE){

  # file <-   files_ECE[2]
  print(file)


df_ECE <- read_sav(paste0(getwd(),"/Data/", file)) |> as_tibble()

 # names(df_ECE)

# Tratamiento
  ## Codificamos la tabla
df_ECE <- ece_codif(df_ECE) |> 
  ## Extracción variables de interés
    select(all_of(c(vars_ident, vars_demograf, vars_interes, variables_trabajo))) |> 
    mutate(Trimestre =str_c(ID_AMO, "-0",ID_TRIMESTRE),
  ## Variables calculadas
           numerador_no_edu=if_else((Edad>=18 & 
                                     Edad <=30 & 
                                     Educacion_asiste     != "3.Parauniversitaria o universitaria" &
                                     Educacion_nivel_grado!="Parauniversitario|Universidad"        &
                                    (Desea_trabajar      %in% c("8.Si podría, sin ninguna restricción", 
                                                         "9.Si podría, con ciertas condiciones" ) |
                                     Condicion_actividad=="1.Ocupado")),1, NA_real_),
          denominador_no_edu=if_else((Edad>=18 &  
                                      Edad <=30 &
                                      Educacion_asiste     != "3.Parauniversitaria o universitaria" &
                                      Educacion_nivel_grado!="Parauniversitario|Universidad"),
                                      1,NA_real_)
                              ) |> 
    select(-all_of(variables_trabajo))

# sum(df_ECE$numerador, na.rm=T)  / sum(df_ECE$denominador, na.rm=T)  
  # >18 que no tienen y no asisten a educación superior y que quieren trabajar o esta trabajando
  # /
  # >18 que no tienen y no asisten a educación superior
  
# Declaramos Diseño Muestral
diseño_ECE <- svydesign(ids=~ID_VIVIENDA, 
                        weights=~Factor_ponderacion, 
                        data = df_ECE)

# nuevas variables de interés: antiguas mas calculadas.
all_vars <- c(vars_interes,"numerador_no_edu", "denominador_no_edu")

# Indicadores
for (variable in all_vars){

  # print(variable)
  # variable <- "no_edu_sup_trabajo"
  
  trim <- svytable(~get(variable), design = diseño_ECE)
  
  trim <- trim |> 
    as_tibble() |> 
    mutate(var = as.character(variable),
           prop = round(100* n/sum(n),1),
           Trimestre = df_ECE$Trimestre[1])
  
  ECE_data <- rbind(ECE_data, trim)
}
rm(variable, trim, diseño_ECE, df_ECE, file)
}

# Datos de Encuesta resumida 

ECE_data <- ECE_data |> 
  rename(categoria=`get(variable)`) |> 
  select(Trimestre,var, categoria, n, prop)


fwrite(ECE_data, paste0(getwd(),"/Output/ECE_data_resumen.csv"))
