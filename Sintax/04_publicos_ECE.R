################################################################################
## Identificar mercado potencial ECE    ##
## Creado: 06-oct-2023
## Modif : 06-oct-2023
# Objetivo: 
# Identificar cómo evoluciona la cantidad de personas con las características de 
# los estudiantes de latina y UAM
################################################################################


rm(list=ls())
# Librerías
library(tidyverse)
library(haven)
library(survey)
library(viridis)
library(data.table)
library(sf)

# Funciones de ayuida
source(paste0(getwd(),"/Sintax/00_funciones_ayuda.R"))


# Publicos de referencia
publico_referencia <- fread(paste0(getwd(),"/Output/publico_referencia.csv"))

# objeto UAM
u_uam <- publico_referencia |> filter(IdInstitucion==3)
# objeto Latina
u_latina <- publico_referencia |> filter(IdInstitucion==2)

# # Shape Files
# cr_shapes <- st_read(paste0(getwd(),"/Data/shapes_CR/CRI_adm2.shp")) |>
#           mutate(prov_code=ID_1,
#                  canton_code=ID_2,
#                  prov_des=NAME_1,
#                  canton_des=NAME_2) |> 
#           select(prov_code, prov_des, canton_code, canton_des,geometry )


## Variables de interés

vars_demograf <- c("Edad", "Estado_conyugal",
                   "Relacion_parentesco", "Condicion_actividad",
                   "Ingreso_total", "Educacion_nivel_grado")

vars_ident <- c("ID_AMO","ID_TRIMESTRE",
                "ID_VIVIENDA","ID_HOGAR","ID_LINEA", "Consecutivo",
                "Factor_ponderacion")


# Carga
## Archivos:
files_ECE <- list.files(paste0(getwd(),"/Data"), ".sav")

publico_potencial <- tibble()
# Loop
for (file in files_ECE){
  
  # file <-   files_ECE[18]
  print(file)
  
  df_ECE <- read_sav(paste0(getwd(),"/Data/", file)) |> as_tibble()
# names(df_ECE)

# Tratamiento
  ## Codificamos y depuramos la tabla
  df_ECE <- ece_codif(df_ECE) |> 
    select(all_of(c(vars_ident, vars_demograf))) |> 
    mutate(ID_VIVIENDA= str_pad(ID_VIVIENDA, side = "left", width=2, pad = "0"),
           ID_HOGAR   = str_pad(ID_HOGAR   , side = "left", width=1, pad = "0"),
           Consecutivo= str_pad(Consecutivo, side = "left", width=4, pad = "0"),
           id_grupo   = str_c(ID_VIVIENDA, ID_HOGAR, Consecutivo),
           Trimestre  = str_c(ID_AMO, "-0",ID_TRIMESTRE)) |>  
    # Construcción variables seguimiento
    group_by(id_grupo) |> 
    mutate(ingresos_hogar= sum(Ingreso_total,na.rm=T),
           Edad          = as.numeric(Edad),
           es_infante    = if_else(Edad<=5,1,0),
           hay_niños     = sum(es_infante,na.rm=T),
           hay_niños     = if_else(hay_niños>0,1,0))
 

  # Declaración Elegibles UAM y Latina
  df_ECE <- df_ECE |> 
    mutate(  
    elegible_Latina=if_else( ( ## Edad
      Edad>=u_latina$q1_edad &
        Edad<=u_latina$q3_edad &
        ## Estado civil
        Estado_conyugal=="6.Soltero(a)" &
        ## Jefe de Hogar
        Relacion_parentesco!="Jefe o jefa|Nuevo jefe" &
        ## Trabaja
        Condicion_actividad=="1.Ocupado" &
        ## Ingresos del hogar
        ingresos_hogar>=u_latina$q1_ingreso &
        ingresos_hogar<=u_latina$q3_ingreso &
        ## Educación
        Educacion_nivel_grado %in% c("Parauniversitario","Secundaria")),
      1,0),
    elegible_UAM=if_else( 
      ( ## Edad
        Edad>=u_uam$q1_edad &
        Edad<=u_uam$q3_edad &
        ## Estado civil
        Estado_conyugal=="6.Soltero(a)" &
        ## Jefe del hogar con hijos o no jefe
        ( Relacion_parentesco %in% c("Jefe o jefa","Nuevo jefe","Conyuge", "Nuevo cónyuge") & hay_niños==1 |
         !Relacion_parentesco %in% c("Jefe o jefa","Nuevo jefe","Conyuge", "Nuevo cónyuge"))&
        ## Trabaja
        Condicion_actividad=="1.Ocupado" &
        ## Ingresos del hogar
        ingresos_hogar>=u_uam$q1_ingreso &
        ingresos_hogar<=u_uam$q3_ingreso & 
        ## Educación
        Educacion_nivel_grado %in% c("Parauniversitario","Secundaria")),
        1,0)
    )
  
  
  # Declaramos Diseño Muestral
  diseño_ECE <- svydesign(ids=~ID_VIVIENDA, 
                          weights=~Factor_ponderacion, 
                          data = df_ECE)
# Tablas de elegibles
  x_uam    <- svytotal(~elegible_UAM    ,design = diseño_ECE) |> as_tibble() |> select(total)
  x_latina <- svytotal(~elegible_Latina ,design = diseño_ECE) |> as_tibble() |> select(total)
  
  row <- tibble(Trimestre        = df_ECE$Trimestre[1],  
                potencial_UAM    = x_uam$total,
                potencial_latina = x_latina$total) 
  
  publico_potencial <- rbind(publico_potencial, row)
    
}


# Tratamiento Público Potencial
publico_potencial <- publico_potencial |> 
  pivot_longer(cols=c("potencial_latina", "potencial_UAM")) |> 
  rename(Universidad=name,
         Potencial=value) |> 
  mutate(Universidad=str_remove(Universidad,"potencial_"),
         Universidad=str_to_upper(Universidad))

# write_xlsx(publico_potencial, paste0(getwd(), "/Data/Mercado_potencial_CR.xlsx"))

table(publico_potencial$Universidad)

## PLot
publico_potencial |> 
  ggplot(aes(x=Trimestre,y=Potencial,group=Universidad, colour=Universidad))+
  geom_line( linewidth=1)+  
  scale_color_viridis(discrete = TRUE)+
  labs(title = "Mercado Potencial Costa Rica",
       x="",
       y="",
       colour="",
       caption = "Fuente: INEC [CR] - Encuesta Continua de Empleo")+
  theme_minimal()+
  theme(legend.position = "top")
  
publico_potencial



# 
