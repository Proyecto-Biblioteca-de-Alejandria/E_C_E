SELECT 
  e.[CIAlumno]
, e.[Genero] 
, e.[Edad]
, e.[RangoEdad] 
, e.[Semestre]
, e.[FechaNacimiento] 
, e.[FechaInicioSemestre] 
, e.[IdInstitucion]
, e.[FacultadAlumno]
, e.[CarreraHomologada]
, e.[IdCarrera]
FROM [Reportes].[dbo].[Vw_HistoricoEnrollment] AS e
WHERE e.[PerfilAlumno]='NUEVO' 
AND e.[IdInstitucion]!=1 
AND e.[Regimen]='PREGRADO'
AND e.[Semestre]>='2020-01'