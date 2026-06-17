# Required TP3 Scripts

Esta carpeta guarda los scripts de clase pedidos explicitamente por el guideline
del TP3 de elasticidades del comercio exterior.

## Scripts requeridos

Se conservan los nombres originales de los archivos:

- `Test.ADF_Ver.3.R`: test de raiz unitaria `Test.ADF.Ver.3`.
- `vcorr_res.R`: test de autocorrelacion para modelos VAR.

## Solicitados pero no encontrados como archivo separado

No se encontraron en la carpeta de clases de OneDrive como scripts `.R`
independientes:

- `VAR_white_no_cross.R`
- `VAR_lag_exclusion_wald.R`

Para cubrir esos puntos en el TP se puede documentar un equivalente econometrico
en el script principal, por ejemplo `arch.test()` para heterocedasticidad VAR y
tests Wald/restricciones sobre rezagos cuando corresponda.
