SELECT 
[CedulaPasaporte] 
, [IdInstitucion]
, [NOMBRE_CARRERA]
, [IdCarrera]
, [PERIODO]
, [estadoCivil]
, CAST([status_lab]   as bigint) as status_lab
, AVG([Hijos]) as Hijos
, AVG([jefehogar]) AS jefehogar
, AVG([IngresoPersonal])   as IngresoPersonal
, AVG([ingresoPadre])   as ingresoPadre
, AVG([ingresoMadre])   as ingresoMadre
FROM [Reportes].[dbo].[CRCONECTE_EjecucionBulk]
WHERE [NOMBRE_GRADO] IN ('BACHILLERATO', 'LICENCIATURA')
AND [PERIODO]>='2020-01'
GROUP BY 
[PERIODO], [CedulaPasaporte],
[IdInstitucion], [NOMBRE_CARRERA], [IdCarrera], 
[estadoCivil],  [status_lab], 
[FechaDescarga]
