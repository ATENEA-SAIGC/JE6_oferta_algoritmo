#==============================================================================================
# MODULO 4 (JE6_Main.R): ASIGNACION POR MECANISMO ISOES (PASOS 2 Y 3)
#==============================================================================================
# Que hace:
#   - Ordena los programas: ISOES desc > menor costo de cohorte > mayor capacidad >
#     semilla (set.seed(20261008)). na.last = TRUE.
#   - Paso 2: por cada IES x Nivel recorre sus programas en ese orden y, cupo a cupo,
#     intenta financiar TODOS los giros del cupo:
#       a) primero con bolsas FDL (1 Apalancamiento solo para giros ID_ORDEN 1-3; 2 FDL);
#       b) si FDL no cubre el cupo completo, se descarta lo parcial y se intenta con
#          bolsas Atenea (3 Apalancamiento solo giros 1-3; 4 Vigencia Futura), exigiendo
#          ademas que CADA ano fiscal de la vigencia futura tenga saldo.
#     Un cupo se asigna solo si queda financiado completo; nunca se financia a medias.
#     Cada cupo descuenta la bolsa IES x Nivel, el techo de la IES y las bolsas usadas.
#   - Paso 3: los sobrantes IES x Nivel se agregan en una bolsa comun por Nivel y se
#     recorre de nuevo la lista (mismo orden ISOES) para los programas con cupos
#     pendientes, con la misma logica de bolsas y techo IES.
#
# Resultado: ORDENAMIENTO_PROG con CUPO_ASIGNADO_IES, CUPO_ASIGNADO_NIVEL, CUPO_FDL,
# CUPO_ATENEA y CUPOS_ASIGNADOS_MECANISMO_ISOES. Es el modulo mas lento (varios minutos).
#----------------------------------------------------------------------------------------------

#==============================================================================================
# MODULO 4: ASIGNACION POR MECANISMO ISOES
#==============================================================================================
# Implementa los pasos 2 y 3 del lineamiento (numeral 5.4.1.6):
#   - Ordenamiento intra-IES por ISOES desc, luego menor cohorte, mayor capacidad, semilla
#   - Asignacion iterativa de cupos contra la bolsa IES-Nivel
#   - Reasignacion del excedente IES en bolsa comun por Nivel (Paso 3)
#
# Reproducibilidad: set.seed(20261008) - día siguiente al cierre de la etapa de validación de la oferta con las IES
#----------------------------------------------------------------------------------------------


# Valor de la cohorte para Atenea
OFERTA_GENERAL$TOTAL_VALOR_COHORTE_ATENEA <- OFERTA_GENERAL$TOTAL_VALOR_ATENEA

# Capacidad maxima de cupos a asignar
OFERTA_GENERAL$NÚMERO_DE_CUPOS_A_OFERTAR <- OFERTA_GENERAL$CUPOS_SEGÚN_CAPACIDAD 


#===============================================================================
# Paso 2 - i): Ordenamiento de programas dentro de la IES
#===============================================================================
# Criterios (Nota del lineamiento, numeral 5.4.1.6 Paso 2):
#   1. ISOES descendente
#   2. Menor TOTAL_VALOR_COHORTE_ATENEA (menor costo)
#   3. Mayor CUPOS_SEGUN_CAPACIDAD
#   4. Semilla aleatoria auditable

set.seed(20261008)
OFERTA_GENERAL$SEMILLA <- OFERTA_GENERAL$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR[
  sample(length(OFERTA_GENERAL$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR))
]

# VARIABLES PARA ORDENAMIENTO
ORDENAMIENTO_PROG <- OFERTA_GENERAL[,c("ID","COD_IES","NOMBRE_IES","CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR","NOMBRE_PROGRAMA_SNIES","JORNADA_DEL_PROGRAMA", "NIVEL_DE_FORMACIÓN","NIVEL_PROGRAMA_SNIES_AJUSTE","CINE_F_2013_AC_CAMPO_DETALLADO", "ISOES_PROGRAMA","TIR_PROGRAMA","TOTAL_VALOR_COHORTE_ATENEA","CUPOS_SEGÚN_CAPACIDAD","SEMILLA")] 

ORDENAMIENTO_PROG <-ORDENAMIENTO_PROG %>%  arrange(desc(ISOES_PROGRAMA),
                                     TOTAL_VALOR_COHORTE_ATENEA,
                                     desc(CUPOS_SEGÚN_CAPACIDAD),
                                     SEMILLA, na.last=TRUE)

ORDENAMIENTO_PROG$ORDENAMIENTO_PROG <- as.double(row.names(ORDENAMIENTO_PROG)) 

# Pegue demanda historica para uso posterior en el componente de demanda social
ORDENAMIENTO_PROG <- merge(
  x = ORDENAMIENTO_PROG,
  y = DemandaHistorica,
  by.x = c("CINE_F_2013_AC_CAMPO_DETALLADO", "NIVEL_PROGRAMA_SNIES_AJUSTE"),
  by.y = c("DH_CINE_DETALLADO", "DH_NIVEL_PROGRAMA_SNIES_AJUSTE"),
  all.x = TRUE
)

OFERTA_GENERAL <- merge(
  x = ORDENAMIENTO_PROG[, c("ORDENAMIENTO_PROG", "ID", "DH_INSCRITOS", "DH_CUPOS",
                            "DH_INDICADOR_DEMANDA_SOCIAL",
                            "DH_PERCENTIL_50_(POR_ENCIMA_/_POR_DEBAJO)")],
  y = OFERTA_GENERAL,
  by = "ID",
  all = FALSE
)

# Identificador IES x Nivel para el recorrido
IES <- PRESUPUESTO_IES_NIVEL %>%
  arrange(COD_IES, NIVEL_PROGRAMA_SNIES_AJUSTE) %>%
  mutate(ID_IES_NIVEL = row_number())

# Construccion de las bolsas
IES$VALOR_SOBRANTE_IES_NIVEL          <- IES$VALOR_DISTRIBUCION_IES_NIVEL
ORDENAMIENTO_PROG$CUPO_ASIGNADO_IES   <- 0
ORDENAMIENTO_PROG$COSTO_CUPOS_ASIGNADO_IES <- 0
ORDENAMIENTO_PROG$CUPO_FDL <- 0
ORDENAMIENTO_PROG$CUPO_ATENEA <- 0

# Construccion de presupuestos por bolsa
PPTO_GLOBAL$RECURSO_SOBRANTE_MecanismoISOES <- PPTO_GLOBAL$Recurso * 0.9
PPTO_GLOBAL$RECURSO_SOBRANTE_MecanismoDemandaSocial <- PPTO_GLOBAL$Recurso * 0.1

PPTO_ATENEA_VIGENCIA$RECURSO_SOBRANTE <- PPTO_ATENEA_VIGENCIA$RECURSO 
OFERTA_DETALLE_COSTO$BOLSA_ASIGNADA <- NA

#===============================================================================
# Paso 2 - ii a iv): Asignacion iterativa intra-IES
#===============================================================================
# Por cada (IES, Nivel) se recorren sus programas en el orden definido y se
# asignan cupos mientras alcance la bolsa de la IES.

for (ID_IES_NIVEL in unique(IES$ID_IES_NIVEL)) {
  #print(paste("ORDEN_IES_",ID_IES_NIVEL))
  
  # IES QUE SE VA A RECORRER SUS PROGRAMAS
  EVALUAR_IES <- IES[IES$ID_IES_NIVEL==ID_IES_NIVEL,]
  print(paste(EVALUAR_IES$COD_IES, EVALUAR_IES$NIVEL_PROGRAMA_SNIES_AJUSTE, EVALUAR_IES$VALOR_SOBRANTE_IES_NIVEL,sep = " "))
  
  ## SE LLENA LA SEMILLA UNICAMENTE LOS PROGRAMAS CORRESPONDIENTES A LA IES Y EL NIVEL
  SEMILLA_PROGRAMAS <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$COD_IES==EVALUAR_IES$COD_IES & ORDENAMIENTO_PROG$NIVEL_PROGRAMA_SNIES_AJUSTE==EVALUAR_IES$NIVEL_PROGRAMA_SNIES_AJUSTE, ]
  
  #ORDENAR PROGRAMAS DE LA SEMILLA
  SEMILLA_PROGRAMAS <-SEMILLA_PROGRAMAS[order(SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG),]
  
  # SE RECORRE LA SEMILLA CON LOS PROGRAMAS DE LA IES POR NIVEL
  for (ID_PROGRAMA in SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG){
    print(paste(" ORDEN_PROG_",ID_PROGRAMA))
    EVALUAR_PROG <- SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG==ID_PROGRAMA,]
    
    #DETALLE DE COSTOS DEL PROGRAMA
    EVALUAR_PROG_COSTOS <- OFERTA_DETALLE_COSTO[OFERTA_DETALLE_COSTO$ID==EVALUAR_PROG$ID & OFERTA_DETALLE_COSTO$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR==EVALUAR_PROG$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR,]
    
    # SE LLENA VARIABLE CON EL PRESUPUESTO DE LA IES - NIVEL (ojo el sobrante es el que se recalcula y tiene en cuenta)
    VALOR_IES <- IES[IES$ID_IES_NIVEL==ID_IES_NIVEL,"VALOR_SOBRANTE_IES_NIVEL"]
    
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
        if(VALOR_IES - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO) >0 ){
          
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
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 1 FDL  Apalancamiento")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
                  }
                }
              }#  CIERRE 1 FDL  Apalancamiento 
              #-------------------
              # 2 FDL
              #-------------------
              else if(id_bolsa ==2 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                
                if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                  print("BOLSA - 2 FDL")
                  #MARCAR REGISTROS DE FINANCIACION
                  CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                  REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                  #DESCONTAR RECURSOS EN DETALLES
                  BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                  TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
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
            VALOR_IES <- VALOR_IES - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            IES[IES$ID_IES_NIVEL==ID_IES_NIVEL,"VALOR_SOBRANTE_IES_NIVEL"] <- VALOR_IES
            
            # AFECTAR BOLSAS FDL
            for (llave in 1:2) {
              PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"] 
            }
            
            #CONTAR CUPOS
            CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_IES"] + 1
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_IES"] <- CUPO_CALCULADO
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_IES"] <- CUPO_CALCULADO
            
            # CONTAR CUPOS FDL
            CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] + 1
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] <- CUPO_CALCULADO
            
            #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
            COSTO_CUPOS <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_IES"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_IES"] <- COSTO_CUPOS
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_IES"] <- COSTO_CUPOS
            
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
                    if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                      print("BOLSA - 3 Atenea Apalancamiento")
                      #MARCAR REGISTROS DE FINANCIACION
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                      REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                      #DESCONTAR RECURSOS EN DETALLES
                      BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                      TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
                    }
                  }
                }#  CIERRE 3 Atenea Apalancamiento
                #-------------------
                # 4 Atenea Vigencia
                #-------------------
                else if(id_bolsa ==4 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 4 Atenea Vigencia")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
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
                VALOR_IES <- VALOR_IES - sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                IES[IES$ID_IES_NIVEL==ID_IES_NIVEL,"VALOR_SOBRANTE_IES_NIVEL"] <- VALOR_IES
                
                # AFECTAR BOLSAS ATENEA
                for (llave in 3:4) {
                  PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"]
                }
                
                #CONTAR CUPOS
                CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_IES"] + 1
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_IES"] <- CUPO_CALCULADO
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_IES"] <- CUPO_CALCULADO
                
                # CONTAR CUPOS ATENEA
                CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] + 1
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] <- CUPO_CALCULADO
                
                #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
                COSTO_CUPOS <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_IES"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_IES"] <- COSTO_CUPOS
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_IES"] <- COSTO_CUPOS
                
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
  } # CIERRE RECORRIDOS PROGRAMA
} # CIERRE RECORRIDO IES - NIVEL

#===============================================================================
# Paso 3: Asignacion del excedente en bolsa comun por Nivel
#===============================================================================
# Los sobrantes por IES se agrupan por Nivel y se reasignan a programas que
# aun tienen cupos pendientes, respetando el orden ISOES.

EXCEDENTE_NIVEL <- IES %>% group_by(NIVEL_PROGRAMA_SNIES_AJUSTE) %>%  summarise_at(vars(VALOR_SOBRANTE_IES_NIVEL), list(VALOR_SOBRANTE_NIVEL = sum))
EXCEDENTE_NIVEL <- EXCEDENTE_NIVEL %>%  arrange(NIVEL_PROGRAMA_SNIES_AJUSTE) %>%  mutate(ID_NIVEL = row_number())
EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL_ORI <- EXCEDENTE_NIVEL$VALOR_SOBRANTE_NIVEL

# Cupos pendientes por programa luego del Paso 2
ORDENAMIENTO_PROG$CUPO_PENDIENTES <- ORDENAMIENTO_PROG$CUPOS_SEGÚN_CAPACIDAD - ORDENAMIENTO_PROG$CUPO_ASIGNADO_IES 
ORDENAMIENTO_PROG$CUPO_ASIGNADO_NIVEL <- 0
ORDENAMIENTO_PROG$COSTO_CUPOS_ASIGNADO_NIVEL <- 0

ASIGNACION_NIVEL_PROG <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$CUPO_PENDIENTES >0, ]
ASIGNACION_NIVEL_PROG <- merge(x=ASIGNACION_NIVEL_PROG, y=IES[,c("COD_IES","NIVEL_PROGRAMA_SNIES_AJUSTE","ID_IES_NIVEL")], by=c("COD_IES","NIVEL_PROGRAMA_SNIES_AJUSTE"), all = FALSE )
ASIGNACION_NIVEL_PROG_DETALLE_COSTO<-  OFERTA_DETALLE_COSTO[is.na(OFERTA_DETALLE_COSTO$BOLSA_ASIGNADA),]

#-------------------------------------------------------------------------------
# CALCULO DE CUPOS BOLSA COMUN
# SE RECORRE NIVEL PARA PROGRAMAS CON CUPOS
#-------------------------------------------------------------------------------
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
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 1 FDL  Apalancamiento")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
                  }
                }
              }#  CIERRE 1 FDL  Apalancamiento 
              #-------------------
              # 2 FDL
              #-------------------
              else if(id_bolsa ==2 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                
                if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                  print("BOLSA - 2 FDL")
                  #MARCAR REGISTROS DE FINANCIACION
                  CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                  REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                  #DESCONTAR RECURSOS EN DETALLES
                  BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                  TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
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
              PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"] 
            }
            
            #CONTAR CUPOS
            CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL"] + 1
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL"] <- CUPO_CALCULADO
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL"] <- CUPO_CALCULADO
            
            # CONTAR CUPOS FDL
            CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] + 1
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_FDL"] <- CUPO_CALCULADO
            
            #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
            COSTO_CUPOS <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
            ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL"] <- COSTO_CUPOS
            SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL"] <- COSTO_CUPOS
            
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
                    if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                      print("BOLSA - 3 Atenea Apalancamiento")
                      #MARCAR REGISTROS DE FINANCIACION
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                      CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                      REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                      #DESCONTAR RECURSOS EN DETALLES
                      BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                      TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
                    }
                  }
                }#  CIERRE 3 Atenea Apalancamiento
                #-------------------
                # 4 Atenea Vigencia
                #-------------------
                else if(id_bolsa ==4 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA) ){
                  
                  if(BOLSA$RECURSO_SOBRANTE_MecanismoISOES/REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO >= 1 & is.na(REGISTRO_SEMESTRE$BOLSA_ASIGNADA)){
                    print("BOLSA - 4 Atenea Vigencia")
                    #MARCAR REGISTROS DE FINANCIACION
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"BOLSA_ASIGNADA"] <- BOLSA$Fuente
                    CUPO_OFERTA_DTLL[CUPO_OFERTA_DTLL$ID_ORDEN==semestre,"ID_BOLSA_ASIGNADA"] <- BOLSA$ID_ORDEN_BOLSA
                    REGISTRO_SEMESTRE$BOLSA_ASIGNADA  <- BOLSA$Fuente
                    #DESCONTAR RECURSOS EN DETALLES
                    BOLSA$RECURSO_SOBRANTE_MecanismoISOES <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES - REGISTRO_SEMESTRE$COSTO_SEMESTRE_ATENEA_PAGO
                    TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==id_bolsa, "RECURSO_SOBRANTE_MecanismoISOES" ] <- BOLSA$RECURSO_SOBRANTE_MecanismoISOES
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
                  PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"] <-TEMPORAL_BOLSAS[TEMPORAL_BOLSAS$ID_ORDEN_BOLSA==llave, "RECURSO_SOBRANTE_MecanismoISOES"]
                }
                
                #CONTAR CUPOS
                CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL"] + 1
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL"] <- CUPO_CALCULADO
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ASIGNADO_NIVEL"] <- CUPO_CALCULADO
                
                # CONTAR CUPOS ATENEA
                CUPO_CALCULADO <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] + 1
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "CUPO_ATENEA"] <- CUPO_CALCULADO
                
                #COSTO A RESTAR EN EL PRESUPUESTO DE LA IES
                COSTO_CUPOS <- ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL"] + sum(CUPO_OFERTA_DTLL$COSTO_SEMESTRE_ATENEA_PAGO)
                ORDENAMIENTO_PROG[ORDENAMIENTO_PROG$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL"] <- COSTO_CUPOS
                SEMILLA_PROGRAMAS[SEMILLA_PROGRAMAS$ORDENAMIENTO_PROG== ID_PROGRAMA, "COSTO_CUPOS_ASIGNADO_NIVEL"] <- COSTO_CUPOS
                
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
# Consolidacion mecanismo ISOES (Paso 2 + Paso 3)
#-------------------------------------------------------------------------------
ORDENAMIENTO_PROG$CUPOS_ASIGNADOS_MECANISMO_ISOES <- ORDENAMIENTO_PROG$CUPO_ASIGNADO_IES + ORDENAMIENTO_PROG$CUPO_ASIGNADO_NIVEL
ORDENAMIENTO_PROG$COSTO_CUPOS_ASIGNADOS_MECANISMO_ISOES <- ORDENAMIENTO_PROG$COSTO_CUPOS_ASIGNADO_IES + ORDENAMIENTO_PROG$COSTO_CUPOS_ASIGNADO_NIVEL


# Actualizacion de cupos pendientes para la fase de demanda social
ORDENAMIENTO_PROG$CUPO_PENDIENTES <- ORDENAMIENTO_PROG$CUPOS_SEGÚN_CAPACIDAD - ORDENAMIENTO_PROG$CUPOS_ASIGNADOS_MECANISMO_ISOES


