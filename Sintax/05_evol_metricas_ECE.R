################################################################################
## Identificar mercado potencial ECE    ##
## Creado: 06-oct-2023
## Modif : 15-ene-2024
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

evol_metricas_ece <- tibble()
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
  
  
  # Elegibles Latina Y UAM 
  df_ECE <- df_ECE |> 
    mutate(  
      elegible_Latina=if_else( ( 
        ## Edad
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
      Edad_in_Lat               =if_else(( Edad>=u_latina$q1_edad & Edad<=u_latina$q3_edad),1,0),
      Estado_conyugal_in_Lat    =if_else(( Estado_conyugal=="6.Soltero(a)"),1,0),
      Condicion_actividad_in_Lat=if_else(( Condicion_actividad=="1.Ocupado"),1,0),
      ingresos_in_Lat           =if_else(( ingresos_hogar>=u_latina$q1_ingreso & ingresos_hogar<=u_latina$q3_ingreso),1,0),
      jefe_hog_hijo_in_Lat      =if_else(( Relacion_parentesco %in% c("Jefe o jefa","Nuevo jefe","Conyuge", "Nuevo cónyuge") & hay_niños==1),1,0),
      no_jefe_hog_in_Lat        =if_else((!Relacion_parentesco %in% c("Jefe o jefa","Nuevo jefe","Conyuge", "Nuevo cónyuge")),1,0),
      
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
        1,0),
      Edad_in_UAM               =if_else(( Edad>=u_uam$q1_edad & Edad<=u_uam$q3_edad),1,0),
      Estado_conyugal_in_UAM    =if_else(( Estado_conyugal=="6.Soltero(a)"),1,0),
      Condicion_actividad_in_UAM=if_else(( Condicion_actividad=="1.Ocupado"),1,0),
      ingresos_in_UAM           =if_else(( ingresos_hogar>=u_uam$q1_ingreso & ingresos_hogar<=u_uam$q3_ingreso),1,0),
      jefe_hog_hijo_in_UAM      =if_else(( Relacion_parentesco %in% c("Jefe o jefa","Nuevo jefe","Conyuge", "Nuevo cónyuge") & hay_niños==1),1,0),
      no_jefe_hog_in_UAM        =if_else((!Relacion_parentesco %in% c("Jefe o jefa","Nuevo jefe","Conyuge", "Nuevo cónyuge")),1,0)
    )
  
  
  # Declaramos Diseño Muestral
  diseño_ECE <- svydesign(ids=~ID_VIVIENDA, 
                          weights=~Factor_ponderacion, 
                          data = df_ECE)
  # Variables de seguimiento
  vars <- names(df_ECE)[str_detect(names(df_ECE), "_in_") ]
  
  for (var in vars) {
    key  <- svytotal(~get(var)  , design = diseño_ECE) |> as_tibble() |> select(total)
    
    row <- tibble(Trimestre = df_ECE$Trimestre[1],
                  variable  = var,
                  Total     = key$total)
    
    evol_metricas_ece <- rbind(evol_metricas_ece, row)
    } 
  
}

x <- evol_metricas_ece |>
  mutate(universidad= if_else(str_detect(variable, "in_Lat"),"Latina", "UAM"),
         variable = str_remove_all(variable, "_in_Lat|_in_UAM"),
         Total=Total/1000)


x |> 
  ggplot(aes(x=Trimestre, y=Total, group=variable, colour=variable))+
  geom_line(size=1.3)+
  scale_color_viridis(option="magma",discrete = TRUE, end = .8,)+
  ylim(500, 3000)+
  labs(title = "Evolución de variables de interés",
       x="",
       y="Miles de personas",
       colour="",
       caption = "Fuente: INEC [CR] - Encuesta Continua de Empleo")+
  facet_wrap(~universidad)+
  theme_minimal()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1))
  


