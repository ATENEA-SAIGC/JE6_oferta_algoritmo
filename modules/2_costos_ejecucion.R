#==============================================================================================
# MODULO 2 (JE6_Main.R): COSTOS POR SEMESTRE Y PROYECCION DE PAGOS POR VIGENCIA
#==============================================================================================
# Novedad de JE6 frente a JE5: el costo ya no se trata como un valor unico de cohorte,
# sino como un flujo de pagos que cae en anos fiscales concretos (2027-2033).
#
# Que hace:
#   - Pasa los costos por semestre (2027_1 ... ) de formato ancho a largo y conserva
#     solo los semestres dentro de la duracion del programa (Periodos_Referencia).
#   - Parte cada semestre en dos giros: Pago1 = 80% y Pago2 = 20%.
#   - Regla de vigencia: el Pago2 de un semestre impar (xxxx1) se queda en el mismo ano;
#     el Pago2 de un semestre par (xxxx2) pasa al ano siguiente.
#   - ID_ORDEN numera los giros de cada programa en orden cronologico; el Modulo 4 lo
#     usa para decidir que giros pueden ir a las bolsas de "apalancamiento" (ID_ORDEN 1-3).
#
# Resultado: OFERTA_COSTO_EJECUCION, un registro por (programa, semestre, pago).
#----------------------------------------------------------------------------------------------

#==============================================================================================
# MODULO 2: CÁLCULO DE COSTOS Y PROYECCIÓN DE EJECUCIÓN
#==============================================================================================
# Este módulo toma LISTADO_OFERTA_PROPUESTA los costos por semestre y desagrega por pagos.
# Aplica la lógica de pagos (Pago 1 y Pago 2) para definir el flujo de caja
# por vigencia (Año de ejecución).
#===============================================================================

# 1. SELECCIÓN DE VARIABLES DE COSTO
#-------------------------------------------------------------------------------
OFERTA_COSTO <- LISTADO_OFERTA_PROPUESTA[LISTADO_OFERTA_PROPUESTA$PROGRAMA_HABILITADO=="Habilitado",c(1,4,22,24:39)]

# Transponer de formato ancho (columnas por semestre) a formato largo
OFERTA_COSTO <- pivot_longer(OFERTA_COSTO, cols = 4:17, names_to = "SEMESTRE", values_to = "COSTO_SEMESTRE_ATENEA")

# 2. LIMPIEZA Y ORDENAMIENTO
#-------------------------------------------------------------------------------
# Transformar la variable SEMESTRE a solo numérico (eliminar texto "Semestre 1" -> 1)
OFERTA_COSTO$SEMESTRE <- gsub("\\D", "", OFERTA_COSTO$SEMESTRE)
OFERTA_COSTO$SEMESTRE <- as.double(OFERTA_COSTO$SEMESTRE)

# Creación de secuencia de ordenamiento por programa
OFERTA_COSTO <- OFERTA_COSTO %>% group_by(ID,CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR) %>%  mutate(SECUENCIA_SEMESTRES = order(SEMESTRE))

# Depurar semestres que exceden la duración del programa (Periodos Referencia)
OFERTA_COSTO <- OFERTA_COSTO[OFERTA_COSTO$SECUENCIA_SEMESTRES <= OFERTA_COSTO$Periodos_Referencia, ]
OFERTA_COSTO$ANIO <- substr(OFERTA_COSTO$SEMESTRE, 1,4)


# 3. LÓGICA DE EJECUCIÓN DE PAGOS
#-------------------------------------------------------------------------------
# Se duplica la estructura para calcular la dispersión de pagos
OFERTA_COSTO_EJECUCION <- OFERTA_COSTO
OFERTA_COSTO_EJECUCION$ANIO_SEMESTRE_EJECUCION <- OFERTA_COSTO_EJECUCION$SEMESTRE

# Inicialización de pagos
OFERTA_COSTO_EJECUCION$Pago1 <- .8
OFERTA_COSTO_EJECUCION$Pago2 <- .2

# Transponer los pagos a filas individuales
# Nota: Usamos nombres de columnas explícitos para mayor seguridad que los índices numéricos
OFERTA_COSTO_EJECUCION <- pivot_longer(OFERTA_COSTO_EJECUCION,  cols = c("Pago1", "Pago2"), names_to = "NUMERO_PAGO", values_to = "PORCENTAJE_PAGO")

# Se depuran los porcentajes del 0% (pagos inexistentes)
OFERTA_COSTO_EJECUCION <- OFERTA_COSTO_EJECUCION[OFERTA_COSTO_EJECUCION$PORCENTAJE_PAGO > 0,]

# 4. CÁLCULO FINAL DE COSTOS POR GIRO
#-------------------------------------------------------------------------------
# Calcular el valor monetario real del giro
OFERTA_COSTO_EJECUCION$COSTO_SEMESTRE_ATENEA_PAGO <- OFERTA_COSTO_EJECUCION$COSTO_SEMESTRE_ATENEA * OFERTA_COSTO_EJECUCION$PORCENTAJE_PAGO

# Funcion para determinar semestres pares
es_par <- function(x) {
  return(x %% 2 == 0)
}

# - Semestres PARES: El Pago 1 es el 80%
# - Semestres IMPARES: El Pago 2 es el 0% (Se paga todo en el Pago 1)

# REGLA DE VIGENCIA:
# El Pago 2 del semestre impar mantiene el periodo fiscal. Se suma constante = 1 meses/periodo contable aprox.
OFERTA_COSTO_EJECUCION[es_par(OFERTA_COSTO_EJECUCION$SEMESTRE)== "FALSE" & OFERTA_COSTO_EJECUCION$NUMERO_PAGO=="Pago2", "ANIO_SEMESTRE_EJECUCION" ] <- OFERTA_COSTO_EJECUCION[es_par(OFERTA_COSTO_EJECUCION$SEMESTRE)== "FALSE" & OFERTA_COSTO_EJECUCION$NUMERO_PAGO=="Pago2", "ANIO_SEMESTRE_EJECUCION" ] + 1
# REGLA DE VIGENCIA:
# El Pago 2 del semestre par pasa al siguiente periodo fiscal. Se suma constante = 9 meses/periodo contable aprox.
OFERTA_COSTO_EJECUCION[es_par(OFERTA_COSTO_EJECUCION$SEMESTRE)== "TRUE" & OFERTA_COSTO_EJECUCION$NUMERO_PAGO=="Pago2", "ANIO_SEMESTRE_EJECUCION" ] <- OFERTA_COSTO_EJECUCION[es_par(OFERTA_COSTO_EJECUCION$SEMESTRE)== "TRUE" & OFERTA_COSTO_EJECUCION$NUMERO_PAGO=="Pago2", "ANIO_SEMESTRE_EJECUCION" ] + 9

# Extraer el Año de Ejecución
OFERTA_COSTO_EJECUCION$ANIO_EJECUCION <- substr(OFERTA_COSTO_EJECUCION$ANIO_SEMESTRE_EJECUCION, 1,4)

# Creación variable de ordenamiento final para el algoritmo de asignación
OFERTA_COSTO_EJECUCION <- OFERTA_COSTO_EJECUCION %>% group_by(ID,CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR) %>%  mutate(ID_ORDEN = order(SEMESTRE,ANIO_SEMESTRE_EJECUCION))


