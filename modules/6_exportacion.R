#==============================================================================================
# MODULO 6 (JE6_Main.R): CONSOLIDACION Y EXPORTACION
#==============================================================================================
# Que hace:
#   - Une ISOES + demanda social por ORDENAMIENTO_PROG; los programas no evaluados en
#     demanda social quedan con EVALUADO_DEMANDA_SOCIAL = "N" y cupos en 0.
#   - Imprime pruebas de cierre presupuestal en consola.
#   - Exporta output/JE6_OFERTA_VF_YYYYMMDD.xlsx con 7 hojas: JE6_PROGRAMAS,
#     OFERTA_DETALLE_COSTO (solo giros financiados), PPTO_GLOBAL, PPTO_ATENEA_VIGENCIA,
#     PPTO_IES, IES_NIVEL y EXCEDENTE_NIVEL.
#----------------------------------------------------------------------------------------------

#==============================================================================================
# MODULO 6: CONSOLIDACION Y EXPORTACION DE RESULTADOS
#==============================================================================================
# Une la asignacion del mecanismo ISOES (Modulo 4) con la del componente de
# demanda social (Modulo 5) y exporta un libro Excel con las hojas:
#   - JE6_PROGRAMAS:    resultado detallado por programa
#   - OFERTA_DETALLE_COSTO: Detalle por cupo asignado el costo por pago 
#   - EXCEDENTES_NIVEL: saldo final de la bolsa comun por Nivel
#   - IES_NIVEL:        auditoria de la bolsa IES x Nivel (Paso 1)
#   - CINE:             auditoria de la bolsa CINE x Nivel (componente demanda social)
#----------------------------------------------------------------------------------------------

RESULTADOS <- merge(
  x = ORDENAMIENTO_PROG,
  y = OFERTA_DEMANDA_SOCIAL[, c("ORDENAMIENTO_PROG", "EVALUADO_DEMANDA_SOCIAL",
                                "CUPO_ASIGNADO_CINE", "COSTO_CUPOS_ASIGNADO_CINE",
                                "CUPO_ASIGNADO_NIVEL_DH", "COSTO_CUPOS_ASIGNADO_NIVEL_DH",
                                "CUPOS_ASIGNADOS_DEMANDA_SOCIAL",
                                "COSTO_CUPOS_DEMANDA_SOCIAL")],
  by = "ORDENAMIENTO_PROG",
  all.x = TRUE
)

# Imputacion de programas no evaluados en demanda social
RESULTADOS[is.na(RESULTADOS$EVALUADO_DEMANDA_SOCIAL), "EVALUADO_DEMANDA_SOCIAL"] <- "N"
RESULTADOS[RESULTADOS$EVALUADO_DEMANDA_SOCIAL == "N", "CUPO_ASIGNADO_CINE"]          <- 0
RESULTADOS[RESULTADOS$EVALUADO_DEMANDA_SOCIAL == "N", "CUPO_ASIGNADO_NIVEL_DH"]      <- 0
RESULTADOS[RESULTADOS$EVALUADO_DEMANDA_SOCIAL == "N", "COSTO_CUPOS_ASIGNADO_CINE"]   <- 0
RESULTADOS[RESULTADOS$EVALUADO_DEMANDA_SOCIAL == "N", "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] <- 0
RESULTADOS[is.na(RESULTADOS$CUPOS_ASIGNADOS_DEMANDA_SOCIAL), "CUPOS_ASIGNADOS_DEMANDA_SOCIAL"] <- 0
RESULTADOS[is.na(RESULTADOS$COSTO_CUPOS_DEMANDA_SOCIAL),    "COSTO_CUPOS_DEMANDA_SOCIAL"]    <- 0

# Totales finales (ISOES + demanda social)
RESULTADOS$TOTAL_CUPOS_ASIGNADOS <- RESULTADOS$CUPOS_ASIGNADOS_MECANISMO_ISOES + RESULTADOS$CUPOS_ASIGNADOS_DEMANDA_SOCIAL
RESULTADOS$TOTAL_COSTO_CUPOS_ASIGNADOS <- RESULTADOS$COSTO_CUPOS_ASIGNADOS_MECANISMO_ISOES + RESULTADOS$COSTO_CUPOS_DEMANDA_SOCIAL

#-------------------------------------------------------------------------------
# Pruebas de cierre presupuestal
#-------------------------------------------------------------------------------
print(paste("Cupos asignados (ISOES):",        sum(RESULTADOS$CUPOS_ASIGNADOS_MECANISMO_ISOES)))
print(paste("Cupos asignados (Demanda Social):", sum(RESULTADOS$CUPOS_ASIGNADOS_DEMANDA_SOCIAL)))
print(paste("Cupos asignados TOTAL:",          sum(RESULTADOS$TOTAL_CUPOS_ASIGNADOS)))
print(paste("Costo TOTAL:",
            format(sum(RESULTADOS$TOTAL_COSTO_CUPOS_ASIGNADOS),
                   big.mark = ".", decimal.mark = ",", nsmall = 2)))
print(paste("Excedente final:",
            format(sum(EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL),
                   big.mark = ".", decimal.mark = ",", nsmall = 2)))
print(paste("Diferencia vs PPTO_GLOBAL:",
            sum(PPTO_GLOBAL$Recurso) - (sum(RESULTADOS$TOTAL_COSTO_CUPOS_ASIGNADOS) +
                              sum(EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL))))

#-------------------------------------------------------------------------------
# Exportacion a Excel
#-------------------------------------------------------------------------------
OUTPUT_FILE <- file.path("output",
                         paste0("JE6_OFERTA_VF_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

write.xlsx(RESULTADOS, OUTPUT_FILE, sheetName="JE6_PROGRAMAS", append=TRUE)
wb <- loadWorkbook(OUTPUT_FILE)
addWorksheet(wb,"OFERTA_DETALLE_COSTO")
writeData(wb,"OFERTA_DETALLE_COSTO",OFERTA_DETALLE_COSTO[!is.na(OFERTA_DETALLE_COSTO$BOLSA_ASIGNADA),])
addWorksheet(wb,"PPTO_GLOBAL")
writeData(wb,"PPTO_GLOBAL",PPTO_GLOBAL)
addWorksheet(wb,"PPTO_ATENEA_VIGENCIA")
writeData(wb,"PPTO_ATENEA_VIGENCIA",PPTO_ATENEA_VIGENCIA)
addWorksheet(wb,"PPTO_IES")
writeData(wb,"PPTO_IES",PPTO_IES)
addWorksheet(wb,"IES_NIVEL")
writeData(wb,"IES_NIVEL",IES)
addWorksheet(wb,"EXCEDENTE_NIVEL")
writeData(wb,"EXCEDENTE_NIVEL",EXCEDENTE_NIVEL)
saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)

print(paste("Archivo exportado:", OUTPUT_FILE))
