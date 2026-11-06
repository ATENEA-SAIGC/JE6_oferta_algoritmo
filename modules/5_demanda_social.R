#==============================================================================================
# MODULO 5 (JE6_Main.R): COMPONENTE DE ALTA DEMANDA SOCIAL
#==============================================================================================
# Que hace:
#   - Bolsa: 10% del presupuesto + todo el excedente del mecanismo ISOES.
#   - Universo (filtros en cascada): CINE F detallado con < 6 cupos ISOES en total,
#     programas con cupos pendientes, indicador de demanda social disponible y por
#     encima del percentil 50 de su nivel.
#   - Reparto 85% Universitario / 15% TyT y, dentro de cada nivel, por CINE x Nivel
#     proporcional a los inscritos historicos (DH_INSCRITOS).
#   - Recorrido identico al Modulo 4: primero dentro de cada bolsa CINE x Nivel, luego
#     el excedente en bolsa comun por Nivel; misma regla de financiacion completa por
#     bolsas FDL -> Atenea y mismo control del techo por IES.
#
# Nota: el filtro "< 6 cupos" se calcula agregando por CINE F detallado (no por
# programa), tal como lo hace el codigo.
#----------------------------------------------------------------------------------------------

#==============================================================================================
# MODULO 5: COMPONENTE DE ALTA DEMANDA SOCIAL
#==============================================================================================
# Implementa la Actividad 3 del lineamiento (numeral 5.4.1.7):
#   - Universo: programas con <6 cupos asignados via ISOES y por encima del percentil 50
#     en el indicador de demanda social (inscritos / cupos por CINE F detallado)
#   - Bolsa: 10% del PPTO_GLOBAL + excedente ISOES, distribuido 85% UNI / 15% TyT
#   - Distribucion por bolsas CINE x Nivel proporcional a inscritos historicos (DH_INSCRITOS)
#   - Recorrido analogo al Modulo 3 (Paso 2 dentro de bolsa CINE + Paso 3 excedente por Nivel)
#----------------------------------------------------------------------------------------------

# Recalculo del presupuesto de demanda social: 10% original + excedente del mecanismo ISOES
EXCEDENTE_MECANISMO_ACTUAL <- sum(EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL)
PPTO_DemandaSocial         <- PPTO_DemandaSocial + EXCEDENTE_MECANISMO_ACTUAL
PPTO_GLOBAL$RECURSO_SOBRANTE_MecanismoDemandaSocial <- PPTO_GLOBAL$RECURSO_SOBRANTE_MecanismoDemandaSocial + PPTO_GLOBAL$RECURSO_SOBRANTE_MecanismoISOES

#===============================================================================
# Filtrado de programas elegibles para demanda social
#===============================================================================
# 1) Programas con menos de 6 cupos asignados via mecanismo ISOES
CONTEO_DEMANDA_SOCIAL <- ORDENAMIENTO_PROG %>%   
  group_by(CINE_F_2013_AC_CAMPO_DETALLADO) %>%
  summarise_at(vars(CUPOS_ASIGNADOS_MECANISMO_ISOES),
               list(TOTAL_CUPOS_MECANISMO_ISOES = sum))

CONTEO_DEMANDA_SOCIAL <- CONTEO_DEMANDA_SOCIAL[
  CONTEO_DEMANDA_SOCIAL$TOTAL_CUPOS_MECANISMO_ISOES < 6, ]

# 2) Cruce con programas que tienen cupos pendientes
OFERTA_DEMANDA_SOCIAL <- merge(
  x = CONTEO_DEMANDA_SOCIAL,
  y = ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$CUPO_PENDIENTES > 0, ],
  by  = "CINE_F_2013_AC_CAMPO_DETALLADO",
  all = FALSE)

OFERTA_DEMANDA_SOCIAL <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$CUPO_PENDIENTES > 0, ]

# 3) Registros con indicador de demanda social disponible
OFERTA_DEMANDA_SOCIAL <- OFERTA_DEMANDA_SOCIAL[
  !is.na(OFERTA_DEMANDA_SOCIAL$DH_INDICADOR_DEMANDA_SOCIAL), ]

# 4) Programas por encima del percentil 50 en demanda social (segun nivel)
OFERTA_DEMANDA_SOCIAL <- OFERTA_DEMANDA_SOCIAL[
  OFERTA_DEMANDA_SOCIAL$`DH_PERCENTIL_50_(POR_ENCIMA_/_POR_DEBAJO)` == "POR ENCIMA PERCENTIL 50", ]

#===============================================================================
# Paso 1.1 Demanda Social: Presupuesto CINE x Nivel (% segun inscritos JE2)
#===============================================================================
CINE_NIVEL <- unique(OFERTA_DEMANDA_SOCIAL[, c("CINE_F_2013_AC_CAMPO_DETALLADO",
                                               "NIVEL_PROGRAMA_SNIES_AJUSTE",
                                               "DH_INSCRITOS")])

CINE <- CINE_NIVEL %>%
  group_by(NIVEL_PROGRAMA_SNIES_AJUSTE) %>%
  summarise_at(vars(DH_INSCRITOS), list(TOTAL_BENEF_NIVEL = sum))

CINE_NIVEL <- merge(x = CINE_NIVEL, y = CINE,
                    by = "NIVEL_PROGRAMA_SNIES_AJUSTE", all = FALSE)
CINE_NIVEL$DISTRIBUCION_CINE_NIVEL <- CINE_NIVEL$DH_INSCRITOS / CINE_NIVEL$TOTAL_BENEF_NIVEL

# Distribucion 85% Universitario / 15% TyT sobre el presupuesto de demanda social
PPTO_DemandaSocial_TYT <- PPTO_DemandaSocial * 0.15
PPTO_DemandaSocial_UNI <- PPTO_DemandaSocial * 0.85

CINE_NIVEL$PPTO_DemandaSocial <- 0
CINE_NIVEL[CINE_NIVEL$NIVEL_PROGRAMA_SNIES_AJUSTE == "TYT",          "PPTO_DemandaSocial"] <- PPTO_DemandaSocial_TYT
CINE_NIVEL[CINE_NIVEL$NIVEL_PROGRAMA_SNIES_AJUSTE == "UNIVERSITARIO","PPTO_DemandaSocial"] <- PPTO_DemandaSocial_UNI

CINE_NIVEL$VALOR_DISTRIBUCION_CINE_NIVEL <-
  CINE_NIVEL$DISTRIBUCION_CINE_NIVEL * CINE_NIVEL$PPTO_DemandaSocial

# Identificador CINE x Nivel para el recorrido
CINE_NIVEL <- CINE_NIVEL %>%
  arrange(CINE_F_2013_AC_CAMPO_DETALLADO, NIVEL_PROGRAMA_SNIES_AJUSTE) %>%
  mutate(ID_CINE_NIVEL = row_number())

CINE_NIVEL$VALOR_SOBRANTE_CINE_NIVEL    <- CINE_NIVEL$VALOR_DISTRIBUCION_CINE_NIVEL
OFERTA_DEMANDA_SOCIAL$CUPO_ASIGNADO_CINE         <- 0
OFERTA_DEMANDA_SOCIAL$COSTO_CUPOS_ASIGNADO_CINE  <- 0


ASIGNACION_OFERTA_DETALLE_COSTO<-  OFERTA_DETALLE_COSTO[is.na(OFERTA_DETALLE_COSTO$BOLSA_ASIGNADA),]

#===============================================================================
# Asignacion por bolsa CINE x Nivel
#===============================================================================
for (ID_CINE_NIVEL in unique(CINE_NIVEL$ID_CINE_NIVEL)) {
  print(paste("ORDEN_CINE_", ID_CINE_NIVEL))
  
  EVALUAR_CINE <- CINE_NIVEL[CINE_NIVEL$ID_CINE_NIVEL == ID_CINE_NIVEL, ]
  print(paste(EVALUAR_CINE$CINE_F_2013_AC_CAMPO_DETALLADO,
              EVALUAR_CINE$NIVEL_PROGRAMA_SNIES_AJUSTE,
              EVALUAR_CINE$VALOR_SOBRANTE_CINE_NIVEL, sep = "|"))
  
  SEMILLA_PROGRAMAS <- OFERTA_DEMANDA_SOCIAL[
    OFERTA_DEMANDA_SOCIAL$CINE_F_2013_AC_CAMPO_DETALLADO == EVALUAR_CINE$CINE_F_2013_AC_CAMPO_DETALLADO &
      OFERTA_DEMANDA_SOCIAL$NIVEL_PROGRAMA_SNIES_AJUSTE == EVALUAR_CINE$NIVEL_PROGRAMA_SNIES_AJUSTE,
  ]
  SEMILLA_PROGRAMAS <- SEMILLA_PROGRAMAS[order(SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG), ]
  
  # SE RECORRE LA SEMILLA CON LOS PROGRAMAS DE LA IES POR CINE
  for (ID_PROGRAMA in SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG) {
    print(paste(" ORDEN_PROG_", ID_PROGRAMA))
    EVALUAR_PROG <- SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG == ID_PROGRAMA, ]
    
    #DETALLE DE COSTOS DEL PROGRAMA
    EVALUAR_PROG_COSTOS <- ASIGNACION_OFERTA_DETALLE_COSTO[ASIGNACION_OFERTA_DETALLE_COSTO$ID==EVALUAR_PROG$ID & ASIGNACION_OFERTA_DETALLE_COSTO$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR==EVALUAR_PROG$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR,]
    
    # SE LLENA VARIABLE CON EL PRESUPUESTO DE LA CINE - NIVEL (ojo el sobrante es el que se recalcula y tiene en cuenta)
    VALOR_CINE <- CINE_NIVEL[CINE_NIVEL$ID_CINE_NIVEL==ID_CINE_NIVEL,"VALOR_SOBRANTE_CINE_NIVEL"]
    
    
    # RECORRER TODO EL DETALLE DE LA OFERTA PARA EL MAXIMO DE CUPOS
    for(cupo in unique(EVALUAR_PROG_COSTOS$DETALLE_CUPO) ){
      
      print(paste("ID=", EVALUAR_PROG$ID,"CUPO=",cupo) )
      
      #PAra conocer TECHOS
      PRESUPUESTO_IES    <- PPTO_IES[PPTO_IES$CODIGO_SNIES_IES==EVALUAR_PROG$COD_IES,]
      
      #IES CUBRE CUPO
      if(PRESUPUESTO_IES$PPTO_TECHO / EVALUAR_PROG$TOTAL_VALOR_COHORTE_ATENEA   >= 1) {
        
        CUPO_OFERTA_DTLL <- EVALUAR_PROG_COSTOS[EVALUAR_PROG_COSTOS$DETALLE_CUPO==cupo,]
        CUPO_OFERTA_DTLL<- CUPO_OFERTA_DTLL[order(CUPO_OFERTA_DTLL$ID_ORDEN),]
        CUPO_OFERTA_DTLL$BOLSA_ASIGNADA <- NA
        
        #----------------------------------------------------------------------------------------
        # VALIDAR SI CINE TIENE PRESUPUESTO PARA EL PROGRAMA
        #----------------------------------------------------------------------------------------
        if(VALOR_CINE - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO) >0 ){
          
          #----------------------------------------------------------------------------------------
          # RECORRIDO BOLSAS FDL (Bolsas 1 y 2)
          #----------------------------------------------------------------------------------------
          
          TEMPORAL_BOLSAS <- PPTO_GLOBAL
          TEMPORAL_BOLSAS<- TEMPORAL_BOLSAS[order(TEMPORAL_BOLSAS$ID_ORDEN_BOLSA),]
          
          # PARA UN CUPO SE RECORRE TODOS LOS SEMESTRES
          for(semestre in CUPO_OFERTA_DTLL$ID_ORDEN){
            
            print(semestre)
            
            REGISTRO_SEMESTRE <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,]
            #RECORRO POR BOLSAS TEMPORALES FDL
            for (id_bolsa in 1:2) {
              
              BOLSA <- TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa,]
              #-------------------
              #  1 FDL  Apalancamiento
              #-------------------
              if(id_bolsa ==1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                #PREGUNTO SI ES SEMESTRE 1 o 2
                if(semestre %in% c(1,2,3)){
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 1 FDL  Apalancamiento")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                  }
                }
              }#  CIERRE 1 FDL  Apalancamiento 
              #-------------------
              # 2 FDL
              #-------------------
              else if(id_bolsa ==2 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                
                if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                  print("BOLSA - 2 FDL")
                  #MARCAR REGISTROS DE FINANCIACION
                  CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                  REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                  #DESCONTAR RECURSOS EN DETALLES
                  BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                  TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                }
              } # CIERRE 2 FDL
              
            } # CIERRE BOLSAS FDL
          } # CIERRE RECORRIDO SEMESTRES CON BOLSAS FDL
          
          
          #--------------------------------------------------
          # PREGUNTAR SI EL CUPO SE FINANCIA COMPLETO POR FDL
          #--------------------------------------------------
          if(nrow(CUPO_OFERTA_DTLL[!is.na(CUPO_OFERTA_DTLL$BOLSA_ASIGNADA),])/2 == unique(CUPO_OFERTA_DTLL$Periodos_Referencia)){
            
            #ASIGNACION, MARACION Y CONTEO
            for (llave in CUPO_OFERTA_DTLL$LLAVE_PRIMARIA) {
              OFERTA_DETALLE_COSTO[OFERTA_DETALLE_COSTO$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"] <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"] 
            }
            
            #RESTAR VALOR EN PRESUPUESTO CINE-NIVEL
            VALOR_CINE <- VALOR_CINE - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            CINE_NIVEL[CINE_NIVEL$ID_CINE_NIVEL==ID_CINE_NIVEL, "VALOR_SOBRANTE_CINE_NIVEL"] <- VALOR_CINE
            
            # AFECTAR BOLSAS FDL
            for (llave in 1:2) {
              PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"] 
            }
            
            #CONTAR CUPOS
            CUPO_CALCULADO <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_CINE"] + 1
            OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_CINE"] <- CUPO_CALCULADO
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_CINE"] <- CUPO_CALCULADO
            
            # CONTAR CUPOS FDL
            CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] + 1
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] <- CUPO_CALCULADO
            
            #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
            COSTO_CUPOS <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_CINE"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_CINE"] <- COSTO_CUPOS
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_CINE"] <- COSTO_CUPOS
            
            #AFECTAMOS COSTO DE IES 
            PRESUPUESTO_IES[,"PPTO_TECHO"] <-  PRESUPUESTO_IES[,"PPTO_TECHO"] - EVALUAR_PROG$TOTAL_VALOR_COHORTE_ATENEA 
            PPTO_IES[PPTO_IES$CODIGO_SNIES_IES==EVALUAR_PROG$COD_IES, "PPTO_TECHO" ] <- PRESUPUESTO_IES$PPTO_TECHO
            
          }
          #----------------------------------------------------------------------------------------
          # else{
          #   stop("PARA EVALUAR CON BOLSAS ATENEA")
          # }
          else{
            #----------------------------------------------------------------------------------------
            # RECORRIDO BOLSAS FDL (Bolsas 3 y 4)
            #----------------------------------------------------------------------------------------
            
            #PRESUPUESTO PARA EVALUAR DETALLES
            TEMPORAL_BOLSAS <- PPTO_GLOBAL
            TEMPORAL_BOLSAS<- TEMPORAL_BOLSAS[order(TEMPORAL_BOLSAS$ID_ORDEN_BOLSA),]
            CUPO_OFERTA_DTLL$BOLSA_ASIGNADA <- NA
            
            #OJO
            CUPO_OFERTA_DTLL$ID_BOLSA_ASIGNADA <- NA
            
            # PARA UN CUPO SE RECORRE TODOS LOS SEMESTRES
            for(semestre in CUPO_OFERTA_DTLL$ID_ORDEN){
              
              print(semestre)
              
              REGISTRO_SEMESTRE <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,]
              #RECORRO POR BOLSAS TEMPORALES ATENEA
              for (id_bolsa in 3:4) {
                
                BOLSA <- TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa,]
                #-------------------
                #  3 Atenea Apalancamiento
                #-------------------
                if(id_bolsa ==3 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                  #PREGUNTO SI ES SEMESTRE 1 o 2
                  if(semestre %in% c(1,2,3)){
                    if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                      print("BOLSA - 3 Atenea Apalancamiento")
                      #MARCAR REGISTROS DE FINANCIACION
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                      REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                      #DESCONTAR RECURSOS EN DETALLES
                      BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                      TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                    }
                  }
                }#  CIERRE 3 Atenea Apalancamiento
                #-------------------
                # 4 Atenea Vigencia
                #-------------------
                else if(id_bolsa ==4 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 4 Atenea Vigencia")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                  }
                } # CIERRE 4 Atenea Vigencia
                
              } # CIERRE BOLSAS FDL
            } # CIERRE RECORRIDO SEMESTRES CON BOLSAS ATENEA
            #--------------------------------------------------
            # PREGUNTAR SI EL CUPO SE FINANCIA COMPLETO POR ATENEA
            #--------------------------------------------------
            if(nrow(CUPO_OFERTA_DTLL[!is.na(CUPO_OFERTA_DTLL$BOLSA_ASIGNADA),])/2 == unique(CUPO_OFERTA_DTLL$Periodos_Referencia)){
              
              #------------------------------------------------------------------
              # calcular Anualidad de pagos financiados por Vigencia Futura
              TEMPORAL_ATENEA_VIGENCIA <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_BOLSA_ASIGNADA==4,] %>% group_by(ANIO_EJECUCION) %>%  summarise_at(vars(COSTO_SEMESTRE_ATENEA_PAGO), list(COSTO_ANIO = sum))
              TEMPORAL_ATENEA_VIGENCIA <- merge(x=PPTO_ATENEA_VIGENCIA, y=TEMPORAL_ATENEA_VIGENCIA, by.x = "ANIO_VIGENCIA", by.y = "ANIO_EJECUCION", all = FALSE)
              # ANALISIS_ANIO_VIGENCIA
              TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA_FINANCIA <- "S"
              TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$RECURSO_SOBRANTE / TEMPORAL_ATENEA_VIGENCIA$COSTO_ANIO  < 1, "ANIO_VIGENCIA_FINANCIA"  ] <- "N"
              #------------------------------------------------------------------
              
              # PREGUNTAR SI TODAS LAS VIGENCIAS SE FINANCIAN, EN CASO QUE EXISTA UNA EN VALOR 'N', NO SE PUEDE ASIGNAR
              if(nrow(TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA_FINANCIA=="N",])==0){
                
                print("Financia Vigencia Atenea")
                
                #ASIGNACION, MARACION Y CONTEO
                for (llave in CUPO_OFERTA_DTLL$LLAVE_PRIMARIA) {
                  OFERTA_DETALLE_COSTO[OFERTA_DETALLE_COSTO$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"] <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"]
                }
                
                #RESTAR VALOR EN PRESUPUESTO CINE-NIVEL
                VALOR_CINE <- VALOR_CINE - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                CINE_NIVEL[CINE_NIVEL$ID_CINE_NIVEL==ID_CINE_NIVEL, "VALOR_SOBRANTE_CINE_NIVEL"] <- VALOR_CINE
                
                # AFECTAR BOLSAS ATENEA
                for (llave in 3:4) {
                  PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"]
                }
                
                #CONTAR CUPOS
                CUPO_CALCULADO <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_CINE"] + 1
                OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_CINE"] <- CUPO_CALCULADO
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_CINE"] <- CUPO_CALCULADO
                
                # CONTAR CUPOS ATENEA
                CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] + 1
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] <- CUPO_CALCULADO
                
                #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
                COSTO_CUPOS <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_CINE"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_CINE"] <- COSTO_CUPOS
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_CINE"] <- COSTO_CUPOS
                
                #AFECTAMOS COSTO DE IES 
                PRESUPUESTO_IES[,"PPTO_TECHO"] <-  PRESUPUESTO_IES[,"PPTO_TECHO"] - EVALUAR_PROG$TOTAL_VALOR_COHORTE_ATENEA 
                PPTO_IES[PPTO_IES$CODIGO_SNIES_IES==EVALUAR_PROG$COD_IES, "PPTO_TECHO" ] <- PRESUPUESTO_IES$PPTO_TECHO
                
                # AFECTAMOS ANUALIDAD ATENEA VIGENCIAS
                TEMPORAL_ATENEA_VIGENCIA$New_RECURSO_SOBRANTE <- TEMPORAL_ATENEA_VIGENCIA$RECURSO_SOBRANTE - TEMPORAL_ATENEA_VIGENCIA$COSTO_ANIO 
                for (llave in TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA) {
                  PPTO_ATENEA_VIGENCIA[PPTO_ATENEA_VIGENCIA$ANIO_VIGENCIA==llave, "RECURSO_SOBRANTE"] <-TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA==llave, "New_RECURSO_SOBRANTE"]
                }
                
              }else{
                print(paste("NO Financia Vigencia Atenea",TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA_FINANCIA=="N", "ANIO_VIGENCIA"],sep = "-"))
              }
              
            }
            else{
              print("?????? SIN_PRESPUESTO FDL y ATENEA")
            }
            #stop("PARA EVALUAR CON BOLSAS ATENEA")
          }
        } # CIERRE VALIDACION PRESUPUESTO IES NIVEL CON PROGRAMA
        else{
          print("!!!!!!!! IES-NIVEL SIN PRESPUESTO")
        }
      }# CIERRE TODO EL DETALLE DE LA OFERTA PARA EL MAXIMO DE CUPOS
    } # CIERRE IES CUBRE CUPO
    
  } # fin recorrido programas
} # fin recorrido CINE x Nivel

#===============================================================================
# Paso 3 Demanda Social: Excedente CINE -> Bolsa comun por Nivel
#===============================================================================
EXCEDENTE_NIVEL <- CINE_NIVEL %>%
  group_by(NIVEL_PROGRAMA_SNIES_AJUSTE) %>%
  summarise_at(vars(VALOR_SOBRANTE_CINE_NIVEL),
               list(VALOR_SOBRANTE_NIVEL = sum))

EXCEDENTE_NIVEL <- EXCEDENTE_NIVEL %>%
  arrange(NIVEL_PROGRAMA_SNIES_AJUSTE) %>%
  mutate(ID_NIVEL = row_number())

EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL_ORI <- EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL

# Cupos pendientes despues de la asignacion por CINE
OFERTA_DEMANDA_SOCIAL$CUPO_PENDIENTES <- OFERTA_DEMANDA_SOCIAL$CUPOS_SEGÚN_CAPACIDAD -
  (OFERTA_DEMANDA_SOCIAL$CUPOS_ASIGNADOS_MECANISMO_ISOES +
     OFERTA_DEMANDA_SOCIAL$CUPO_ASIGNADO_CINE)

OFERTA_DEMANDA_SOCIAL$CUPO_ASIGNADO_NIVEL_DH        <- 0
OFERTA_DEMANDA_SOCIAL$COSTO_CUPOS_ASIGNADO_NIVEL_DH <- 0

ASIGNACION_NIVEL_PROG <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$CUPO_PENDIENTES > 0, ]
ASIGNACION_NIVEL_PROG_DETALLE_COSTO<-  OFERTA_DETALLE_COSTO[is.na(OFERTA_DETALLE_COSTO$BOLSA_ASIGNADA),]

#===============================================================================
# Recorrido por NIVEL
#===============================================================================
for (ID_NIVEL in unique(EXCEDENTE_NIVEL$ID_NIVEL)) {
  print(paste("ORDEN_NIVEL ",ID_NIVEL))
  
  
  EVALUAR_NIVEL <- EXCEDENTE_NIVEL[EXCEDENTE_NIVEL$ID_NIVEL==ID_NIVEL,] 
  
  ## SE LLENA LA SEMILLA UNICAMENTE LOS PROGRAMAS CORRESPONDIENTES A LA IES Y EL NIVEL
  SEMILLA_PROGRAMAS <- ASIGNACION_NIVEL_PROG[ASIGNACION_NIVEL_PROG$NIVEL_PROGRAMA_SNIES_AJUSTE==EVALUAR_NIVEL$NIVEL_PROGRAMA_SNIES_AJUSTE, ]
  
  #ORDENAR PROGRAMAS DE LA SEMILLA
  SEMILLA_PROGRAMAS <-SEMILLA_PROGRAMAS[order(SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG),]
  
  # SE RECORRE LA SEMILLA CON LOS PROGRAMAS DE LA IES POR NIVEL
  for (ID_PROGRAMA in SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG){
    print(paste(" ORDEN_PROG_",ID_PROGRAMA))
    EVALUAR_PROG <- SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG==ID_PROGRAMA,]
    
    # SE LLENA VARIABLE CON EL PRESUPUESTO NIVEL (ojo el sobrante es el que se recalcula y tiene en cuenta)
    VALOR_NIVEL <- as.double(EXCEDENTE_NIVEL[EXCEDENTE_NIVEL$ID_NIVEL==ID_NIVEL,"VALOR_SOBRANTE_NIVEL"]) 
    ##################################
    
    #DETALLE DE COSTOS DEL PROGRAMA
    EVALUAR_PROG_COSTOS <- ASIGNACION_NIVEL_PROG_DETALLE_COSTO[ASIGNACION_NIVEL_PROG_DETALLE_COSTO$ID==EVALUAR_PROG$ID & ASIGNACION_NIVEL_PROG_DETALLE_COSTO$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR==EVALUAR_PROG$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR,]
    
    # RECORRER TODO EL DETALLE DE LA OFERTA PARA EL MAXIMO DE CUPOS
    for(cupo in unique(EVALUAR_PROG_COSTOS$DETALLE_CUPO) ){
      
      print(paste("ID=", EVALUAR_PROG$ID,"CUPO=",cupo) )
      
      PRESUPUESTO_IES    <- PPTO_IES[PPTO_IES$CODIGO_SNIES_IES==EVALUAR_PROG$COD_IES,]
      
      #IES CUBRE CUPO
      if(PRESUPUESTO_IES$PPTO_TECHO / EVALUAR_PROG$TOTAL_VALOR_COHORTE_ATENEA   >= 1) {
        CUPO_OFERTA_DTLL <- EVALUAR_PROG_COSTOS[EVALUAR_PROG_COSTOS$DETALLE_CUPO==cupo,]
        CUPO_OFERTA_DTLL<- CUPO_OFERTA_DTLL[order(CUPO_OFERTA_DTLL$ID_ORDEN),]
        CUPO_OFERTA_DTLL$BOLSA_ASIGNADA <- NA
        
        #----------------------------------------------------------------------------------------
        # VALIDAR SI LA IES TIENE PRESUPUESTO PARA EL PROGRAMA
        #----------------------------------------------------------------------------------------
        if(VALOR_NIVEL - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO) >0 ){
          
          #----------------------------------------------------------------------------------------
          # RECORRIDO BOLSAS FDL (Bolsas 1 y 2)
          #----------------------------------------------------------------------------------------
          
          TEMPORAL_BOLSAS <- PPTO_GLOBAL
          TEMPORAL_BOLSAS<- TEMPORAL_BOLSAS[order(TEMPORAL_BOLSAS$ID_ORDEN_BOLSA),]
          
          # PARA UN CUPO SE RECORRE TODOS LOS SEMESTRES
          for(semestre in CUPO_OFERTA_DTLL$ID_ORDEN){
            
            print(semestre)
            
            REGISTRO_SEMESTRE <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,]
            #RECORRO POR BOLSAS TEMPORALES FDL
            for (id_bolsa in 1:2) {
              
              BOLSA <- TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa,]
              #-------------------
              #  1 FDL  Apalancamiento
              #-------------------
              if(id_bolsa ==1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                #PREGUNTO SI ES SEMESTRE 1 o 2
                if(semestre %in% c(1,2,3)){
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 1 FDL  Apalancamiento")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                  }
                }
              }#  CIERRE 1 FDL  Apalancamiento 
              #-------------------
              # 2 FDL
              #-------------------
              else if(id_bolsa ==2 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                
                if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                  print("BOLSA - 2 FDL")
                  #MARCAR REGISTROS DE FINANCIACION
                  CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                  REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                  #DESCONTAR RECURSOS EN DETALLES
                  BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                  TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                }
              } # CIERRE 2 FDL
              
            } # CIERRE BOLSAS FDL
          } # CIERRE RECORRIDO SEMESTRES CON BOLSAS FDL
          
          
          #--------------------------------------------------
          # PREGUNTAR SI EL CUPO SE FINANCIA COMPLETO POR FDL
          #--------------------------------------------------
          if(nrow(CUPO_OFERTA_DTLL[!is.na(CUPO_OFERTA_DTLL$BOLSA_ASIGNADA),])/2 == unique(CUPO_OFERTA_DTLL$Periodos_Referencia)){
            
            #ASIGNACION, MARACION Y CONTEO
            for (llave in CUPO_OFERTA_DTLL$LLAVE_PRIMARIA) {
              OFERTA_DETALLE_COSTO[OFERTA_DETALLE_COSTO$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"] <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"] 
            }
            
            #RESTAR VALOR EN PRESUPUESTO IES-NIVEL
            VALOR_NIVEL <- VALOR_NIVEL - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            EXCEDENTE_NIVEL[EXCEDENTE_NIVEL$ID_NIVEL==ID_NIVEL,"VALOR_SOBRANTE_NIVEL"] <- VALOR_NIVEL
            
            # AFECTAR BOLSAS FDL
            for (llave in 1:2) {
              PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"] 
            }
            
            #CONTAR CUPOS
            CUPO_CALCULADO <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL_DH"] + 1
            OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL_DH"] <- CUPO_CALCULADO
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL_DH"] <- CUPO_CALCULADO
            
            # CONTAR CUPOS FDL
            CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] + 1
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] <- CUPO_CALCULADO
            
            #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
            COSTO_CUPOS <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] <- COSTO_CUPOS
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] <- COSTO_CUPOS
            
            #AFECTAMOS COSTO DE IES 
            PRESUPUESTO_IES[,"PPTO_TECHO"] <-  PRESUPUESTO_IES[,"PPTO_TECHO"] - EVALUAR_PROG$TOTAL_VALOR_COHORTE_ATENEA 
            PPTO_IES[PPTO_IES$CODIGO_SNIES_IES==EVALUAR_PROG$COD_IES, "PPTO_TECHO" ] <- PRESUPUESTO_IES$PPTO_TECHO
            
          }
          #----------------------------------------------------------------------------------------
          # else{
          #   stop("PARA EVALUAR CON BOLSAS ATENEA")
          # }
          else{
            #----------------------------------------------------------------------------------------
            # RECORRIDO BOLSAS FDL (Bolsas 3 y 4)
            #----------------------------------------------------------------------------------------
            
            #PRESUPUESTO PARA EVALUAR DETALLES
            TEMPORAL_BOLSAS <- PPTO_GLOBAL
            TEMPORAL_BOLSAS<- TEMPORAL_BOLSAS[order(TEMPORAL_BOLSAS$ID_ORDEN_BOLSA),]
            CUPO_OFERTA_DTLL$BOLSA_ASIGNADA <- NA
            
            #OJO
            CUPO_OFERTA_DTLL$ID_BOLSA_ASIGNADA <- NA
            
            # PARA UN CUPO SE RECORRE TODOS LOS SEMESTRES
            for(semestre in CUPO_OFERTA_DTLL$ID_ORDEN){
              
              print(semestre)
              
              REGISTRO_SEMESTRE <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,]
              #RECORRO POR BOLSAS TEMPORALES ATENEA
              for (id_bolsa in 3:4) {
                
                BOLSA <- TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa,]
                #-------------------
                #  3 Atenea Apalancamiento
                #-------------------
                if(id_bolsa ==3 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                  #PREGUNTO SI ES SEMESTRE 1 o 2
                  if(semestre %in% c(1,2,3)){
                    if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                      print("BOLSA - 3 Atenea Apalancamiento")
                      #MARCAR REGISTROS DE FINANCIACION
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                      REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                      #DESCONTAR RECURSOS EN DETALLES
                      BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                      TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                    }
                  }
                }#  CIERRE 3 Atenea Apalancamiento
                #-------------------
                # 4 Atenea Vigencia
                #-------------------
                else if(id_bolsa ==4 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 4 Atenea Vigencia")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoDemandaSocial" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoDemandaSocial
                  }
                } # CIERRE 4 Atenea Vigencia
                
              } # CIERRE BOLSAS FDL
            } # CIERRE RECORRIDO SEMESTRES CON BOLSAS ATENEA
            #--------------------------------------------------
            # PREGUNTAR SI EL CUPO SE FINANCIA COMPLETO POR ATENEA
            #--------------------------------------------------
            if(nrow(CUPO_OFERTA_DTLL[!is.na(CUPO_OFERTA_DTLL$BOLSA_ASIGNADA),])/2 == unique(CUPO_OFERTA_DTLL$Periodos_Referencia)){
              
              #------------------------------------------------------------------
              # calcular Anualidad de pagos financiados por Vigencia Futura
              TEMPORAL_ATENEA_VIGENCIA <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_BOLSA_ASIGNADA==4,] %>% group_by(ANIO_EJECUCION) %>%  summarise_at(vars(COSTO_SEMESTRE_ATENEA_PAGO), list(COSTO_ANIO = sum))
              TEMPORAL_ATENEA_VIGENCIA <- merge(x=PPTO_ATENEA_VIGENCIA, y=TEMPORAL_ATENEA_VIGENCIA, by.x = "ANIO_VIGENCIA", by.y = "ANIO_EJECUCION", all = FALSE)
              # ANALISIS_ANIO_VIGENCIA
              TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA_FINANCIA <- "S"
              TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$RECURSO_SOBRANTE / TEMPORAL_ATENEA_VIGENCIA$COSTO_ANIO  < 1, "ANIO_VIGENCIA_FINANCIA"  ] <- "N"
              #------------------------------------------------------------------
              
              # PREGUNTAR SI TODAS LAS VIGENCIAS SE FINANCIAN, EN CASO QUE EXISTA UNA EN VALOR 'N', NO SE PUEDE ASIGNAR
              if(nrow(TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA_FINANCIA=="N",])==0){
                
                print("Financia Vigencia Atenea")
                
                #ASIGNACION, MARACION Y CONTEO
                for (llave in CUPO_OFERTA_DTLL$LLAVE_PRIMARIA) {
                  OFERTA_DETALLE_COSTO[OFERTA_DETALLE_COSTO$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"] <- CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$LLAVE_PRIMARIA==llave, "BOLSA_ASIGNADA"]
                }
                
                #RESTAR VALOR EN PRESUPUESTO IES-NIVEL
                VALOR_NIVEL <- VALOR_NIVEL - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                EXCEDENTE_NIVEL[EXCEDENTE_NIVEL$ID_NIVEL==ID_NIVEL,"VALOR_SOBRANTE_NIVEL"] <- VALOR_NIVEL
                
                # AFECTAR BOLSAS ATENEA
                for (llave in 3:4) {
                  PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoDemandaSocial"]
                }
                
                #CONTAR CUPOS
                CUPO_CALCULADO <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL_DH"] + 1
                OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL_DH"] <- CUPO_CALCULADO
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL_DH"] <- CUPO_CALCULADO
                
                # CONTAR CUPOS ATENEA
                CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] + 1
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] <- CUPO_CALCULADO
                
                #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
                COSTO_CUPOS <- OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                OFERTA_DEMANDA_SOCIAL[OFERTA_DEMANDA_SOCIAL$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] <- COSTO_CUPOS
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL_DH"] <- COSTO_CUPOS
                
                #AFECTAMOS COSTO DE IES 
                PRESUPUESTO_IES[,"PPTO_TECHO"] <-  PRESUPUESTO_IES[,"PPTO_TECHO"] - EVALUAR_PROG$TOTAL_VALOR_COHORTE_ATENEA 
                PPTO_IES[PPTO_IES$CODIGO_SNIES_IES==EVALUAR_PROG$COD_IES, "PPTO_TECHO" ] <- PRESUPUESTO_IES$PPTO_TECHO
                
                # AFECTAMOS ANUALIDAD ATENEA VIGENCIAS
                TEMPORAL_ATENEA_VIGENCIA$New_RECURSO_SOBRANTE <- TEMPORAL_ATENEA_VIGENCIA$RECURSO_SOBRANTE - TEMPORAL_ATENEA_VIGENCIA$COSTO_ANIO 
                for (llave in TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA) {
                  PPTO_ATENEA_VIGENCIA[PPTO_ATENEA_VIGENCIA$ANIO_VIGENCIA==llave, "RECURSO_SOBRANTE"] <-TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA==llave, "New_RECURSO_SOBRANTE"]
                }
                
              }else{
                print(paste("NO Financia Vigencia Atenea",TEMPORAL_ATENEA_VIGENCIA[TEMPORAL_ATENEA_VIGENCIA$ANIO_VIGENCIA_FINANCIA=="N", "ANIO_VIGENCIA"],sep = "-"))
              }
            }
            else{
              print("?????? SIN_PRESPUESTO FDL y ATENEA")
            }
            #stop("PARA EVALUAR CON BOLSAS ATENEA")
          }
        } # CIERRE VALIDACION PRESUPUESTO IES NIVEL CON PROGRAMA
        else{
          print("!!!!!!!! IES-NIVEL SIN PRESPUESTO")
        }
      } # CIERRE IES CUBRE CUPO
      
    }# CIERRE TODO EL DETALLE DE LA OFERTA PARA EL MAXIMO DE CUPOS
    
  } # CIERRE RECORRIDOS PROGRAMA
  
} # CIERRE RECORRIDO IES - NIVEL

#-------------------------------------------------------------------------------
# Consolidacion del componente de demanda social
#-------------------------------------------------------------------------------
OFERTA_DEMANDA_SOCIAL$CUPOS_ASIGNADOS_DEMANDA_SOCIAL <- OFERTA_DEMANDA_SOCIAL$CUPO_ASIGNADO_CINE + OFERTA_DEMANDA_SOCIAL$CUPO_ASIGNADO_NIVEL_DH 
OFERTA_DEMANDA_SOCIAL$COSTO_CUPOS_DEMANDA_SOCIAL <- OFERTA_DEMANDA_SOCIAL$COSTO_CUPOS_ASIGNADO_CINE + OFERTA_DEMANDA_SOCIAL$COSTO_CUPOS_ASIGNADO_NIVEL_DH

OFERTA_DEMANDA_SOCIAL$EVALUADO_DEMANDA_SOCIAL <- "S"


