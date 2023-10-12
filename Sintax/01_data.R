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

vars_demograf <- c("Edad", "Sexo", "Estado_conyugal")

vars_ident <- c("ID_AMO","ID_TRIMESTRE",
                "ID_VIVIENDA","Factor_ponderacion")

# Carga
  ## Archivos:
files_ECE <- list.files(paste0(getwd(),"/Data"), ".sav")

ECE_data <- tibble()

# Loop

for (file in files_ECE){

  file <-   files_ECE[18]
  print(file)


df_ECE <- read_sav(paste0(getwd(),"/Data/", file)) |> as_tibble()

# names(df_ECE)

# Tratamiento
  ## Codificamos la tabla
df_ECE <- ece_codif(df_ECE) |> 
  ## Extracción variables de interés
    select(all_of(c(vars_ident, vars_demograf, vars_interes))) |> 
    mutate(Trimestre =str_c(ID_AMO, "-0",ID_TRIMESTRE))


# Declaramos Diseño Muestral
diseño_ECE <- svydesign(ids=~ID_VIVIENDA, 
                        weights=~Factor_ponderacion, 
                        data = df_ECE)

# Indicadores
for (variable in vars_interes){

  print(variable)
  
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
