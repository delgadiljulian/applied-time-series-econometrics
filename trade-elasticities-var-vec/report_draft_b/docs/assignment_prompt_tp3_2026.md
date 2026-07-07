# Maestría en Economía Aplicada

**Materia:** Series de Tiempo

**Trabajo Práctico Número 3:** Elasticidades del comercio exterior

**Fecha de entrega:** 27 de Junio de 2026

## Nota Preliminar

Este trabajo práctico completa el análisis de las elasticidades del comercio exterior ya realizado en el TP 2 al incluír el análisis multivariado VAR - VEC.

El informe deberá ser completo, incluyendo los resultados del análisis de Engle-Granger (TP2) como el análisis y los resultados de la metodología VEC.

Recordar la importancia de la comparación de resultados con los papers de referencia y los comentarios pertinentes.

## Elasticidades del comercio exterior

Las ecuaciones clásicas del comercio exterior relacionan las importaciones (M) de un país con su producto (PIB) y con el Tipo de Cambio Real (TCR), ya sea en su versión Bilateral o Multilateral. Para el caso de las exportaciones (X) la relación es con el PIB Global (PIBG) y también con el TCR.

```math
M_t = a_0 + a_1 \cdot PIB_t + a_2 \cdot TCR_t + u_t
```

```math
X_t = b_0 + b_1 \cdot PIBG_t + b_2 \cdot TCR_t + \varepsilon_t
```

Cuando las variables están medidas en sus logaritmos naturales, la interpretación de los coeficientes ai y bi es la de elasticidades de largo plazo.

El problema que se presenta es que, en general, estas variables son no estacionarias, normalmente I(1) y por eso la regresión entre las mismas puede estar contaminada por correlaciones espurias. Por lo tanto, debe demostrarse la cointegración entre ellas, ya sea mediante un test de la metodología de Engle y Granger o mediante la metodología de Johansen basada en un modelo de Vectores Autorregresivos.

Se demuestre o no dicha cointegración, siempre queda la posibilidad de calcular relaciones de corto plazo utilizando las variables diferenciadas, y con ellas determinar las elasticidades que también serán de corto plazo.

En el caso de haber cointegración, si se utiliza la metodología Engle-Granger, las elasticidades de largo plazo quedan en principio determinadas por la ecuación de cointegración (1era etapa). Las elasticidades de corto plazo deben ser luego estimadas mediante modelos con término de corrección del error (ECM) que incluyan los residuos rezagados de la 1era etapa (la relación de cointegración).

Al ser el ECM una regresión de la econometría tradicional, con variables estacionarias, debe corregirse la autocorrelación agregando rezagos de las diferencias de las variables. Además los coeficientes no significativos deben eliminarse.

Para evitar el sesgo de muestra chica debido a la realización de una metodología de 2 etapas, se puede realizar una estimación conjunta de los coeficientes, siguiendo a Wickens y Breusch, y luego calcular los coeficientes de largo plazo, que son los de principal interés.

Si en cambio se utiliza la metodología VAR y se verifica cointegración, se deberá estimar un modelo vectorial con término de corrección del error (VECM) en el cual se estiman simultáneamente el vector de cointegración con las elasticidades de largo plazo y los coeficientes de las elasticidades de corto plazo.

En el caso de no verificarse cointegración, el término de corrección del error será no significativo y por tanto deberá omitirse en el ECM, quedando una ecuación en diferencias. Identicamente, en el caso multivariado, si no se verifica cointegración, se deberá calcular un VAR en diferencias. En ese caso también se dispondrá sólo de las elasticidades de corto plazo.

Para el manejo de la metodología y las interpretaciones económicas algunas referencias útiles a tener en cuenta son (además de Enders y las notas de clase):

Berrettoni, D. y Castresana, S. (2008), "Elasticidades de comercio de la Argentina para el período 1993-2008", en Revista del CEI: Comercio Exterior e Integración, Nro. 16, pp. 85-97.

Bus, A. G. y Nicolini-Llosa, J. L. (2007), "Importaciones de Argentina, una estimación econométrica", en XLII Reunión Anual de la Asociación Argentina de Economía Política, Bahía Blanca, 14 - 16 noviembre.

Zack, G. y Sotelsek, D. (2016). Las posibilidades de crecimiento de la Argentina a partir de una estimación de sus elasticidades de comercio exterior. Revista de Economía Política de Buenos Aires. Universidad de Buenos Aires. Facultad de Ciencias Económicas

Song, H., Witt, S. F., & Li, G. (2008). The advanced econometrics of tourism demand. Routledge.

## Desarrollo del Trabajo Práctico

El trabajo a realizar consiste en calcular las elasticidades del comercio exterior argentino para el largo y el corto plazo. Se utilizarán en principio los modelos clásicos ya mencionados, pero se alentará la iniciativa de los alumnos en el sentido de incluir alguna otra variable pertinente (ver la bibliografía mencionada)

Los datos a utilizar serán los ya utilizados en el TP2:

Se recomienda la siguiente "hoja de ruta"

1. Grafique las series y realice un análisis estadístico para determinar la existencia de raíces unitarias, el orden de integración, y la presencia de estacionalidad. Use las series en logaritmos naturales.

2. De acuerdo con los resultados del ítem anterior realice pruebas de cointegración mediante las dos metodologías antes mencionadas. Tenga en cuenta que la metodología Engle - Granger es menos precisa, pero no requiere la normalidad de los residuos, mientras que la metodología de Johansen (más precisa) si la requiere. De todas maneras, si no verifica normalidad realice igual el test dejando constancia de su validez sólo indicativa.

3. En el caso de demostrar cointegración por alguna de las metodologías ensayadas (sea flexible, utilice un valor de significatividad de 0.10 en vez del usual de 0.05), estime las elasticidades de largo plazo. Recuerde que en este caso la estimación debe realizarse mediante un modelo con término de corrección del error, ya sea en su versión univariada (ECM) o multivariada (VEC) o con ambas (en el caso de la metodología Engle Granger, verfique el vector según Wickens y Breusch). Conjuntamente se estimarán las elasticidades de corto plazo, correspondientes a los coeficientes de las diferencias de las variables.

4. En el caso de que no fuera posible demostrar la cointegracíon de las series, se podrán calcular de todas maneras las elasticidades de corto plazo mediante modelos en diferencias de las series.

5. Aún cuando no obtenga evidencia de cointegración realice todos los pasos que se indicaron para el caso en que esta se verifica y reporte los resultados con fines indicativos

6. Reporte las conclusiones: Se pide una interpretación de los resultados obtenidos, su carácter válido o sólo indicativo y la comparación con los resultados de las distintas metodologías.

7. También deberá realizar comparaciones de sus resultados con los de los papers indicados más arriba. Construya cuadros adecuados para realizar las comparaciones en forma expeditiva. Se espera que en esta parte aporte conceptos económicos además de los metodológicos.

8. Se espera que los alumnos utilicen las herramientas proporcionadas en el curso. Más específicamente:

Test de raíz unitaria con Test.ADF.Ver.3.R

Test de autocorrelación con vcorr_res.R

Test de heterocedasticidad con VAR_white_no_cross.R

Test de exclusión de lags con VAR_lag_exclusion_wald.R

Paquetes svar, urca y lpirfs de R

## Presentación del Trabajo Práctico

Se pretende una presentación con formato de artículo de nivel científico (guíese por los papers de referencia). No olvide incluir breves menciones al marco teórico económico, al marco metodológico (econométrico), evaluación de los resultados obtenidos y las conclusiones a las que arriba.

Se recomienda ubicar las salidas de las regresiones en un anexo al final del trabajo para no interrumpir la lectura del paper con cuadros recargados. En el cuerpo del paper se consignarán los resultados en forma sucinta.

El trabajo es grupal y el informe de cada grupo deberá ser realizado con el procesador de texto Word (se deberá entregar el informe en Word y PDF). Las regresiones se realizarán con el software econométrico RStudio y se acompañará la entrega del informe con el script de dicho programa que debe correr sin interrupción una vez cargadas las series y paquetes necesarios y reportar las tablas y resultados que se consignan en el TP. Se pide organizar el rotulados de los objetos en el script de manera de facilitar el seguimiento y la corrección

Los archivos de Word, PDF y R se deberán subir compactados en un archivo RAR o ZIP al Campus de la Materia (ACTIVIDADES -> ENTREGAS) con el siguiente nombre "Apellido - TP 3" en el cual el Apellido corresponderá a uno de los integrantes del grupo (Ejemplo: "Pérez García - TP 3"). No olvide referir los integrantes del grupo completo en la carátula del TP.
