#==============================================================================================
# MODULO 3 (JE6_Main.R): BOLSAS DE FINANCIACION, TECHOS POR IES Y PRESUPUESTO IES x NIVEL
#==============================================================================================
# Que hace:
#   - Techo por IES: PPTO_TECHO = Techo de asignacion JE6 - costo ya comprometido en la
#     linea Talentos. Ninguna IES puede recibir cupos por encima de ese techo.
#   - Universo: programas "Habilitado", con CUPOS_SEGUN_CAPACIDAD > 0 y cuya IES aparece
#     en PPTO_IES (convenio vigente). El merge con PPTO_IES es interno: las IES sin
#     convenio desaparecen del listado.
#   - OFERTA_DETALLE_COSTO: expande cada programa a un registro por cupo ofertado y por
#     giro (cupo x semestre x pago). Es la unidad que se financia en los Modulos 4 y 5.
#     El bucle imprime un ID por programa; es lento pero no altera el resultado.
#   - Presupuesto global: 4 bolsas (FDL Apalancamiento, FDL, Atenea Apalancamiento,
#     Atenea Vigencia Futura) MAS el sobrante de la linea Talentos. La bolsa Atenea
#     Vigencia Futura se desagrega ademas por ano (PPTO_ATENEA_VIGENCIA, 2027-2033).
#   - Particion: 90% mecanismo ISOES / 10% alta demanda social.
#   - Bolsa por IES proporcional a (Valor inicial ATENEA + Total adiciones ATENEA).
#   - Bolsa IES x Nivel: 100% si la IES oferta un solo nivel; 85% Universitario /
#     15% TyT si oferta ambos.
#
# Nota: el encabezado original del bloque dice "95% / 5%" y "valor del convenio JE4";
# el codigo efectivamente aplica 85% / 15% y valor inicial + adiciones. Manda el codigo.
#----------------------------------------------------------------------------------------------

#==============================================================================================
# MODULO 3: DISTRIBUCION DE COSTOS POR PAGO Y DEL PRESUPUESTO POR IES
#==============================================================================================
# Implementa los pasos 0 y 1 del lineamiento (numerales 5.1 y 5.4.1.4):
#   - 90% del presupuesto se asigna por el mecanismo ISOES
#   - 10% se reserva para el componente de alta demanda social
#   - La bolsa por IES se calcula como proporcion del valor del convenio JE4:
#         Recursos_IES_i = (Valor_convenio_i / Sum(Valor_convenio)) * 90% * PPTO_GENERAL
#   - Distribucion intra-IES por nivel: 95% Universitario / 5% TyT cuando hay ambos
#----------------------------------------------------------------------------------------------

# Calcular Techos por IES conforme costos de asignación en la linea Talentos 
PPTO_IES$PPTO_TECHO <- PPTO_IES$`Techo de asignación para JE6 ATENEA` - PPTO_IES$COSTO_JE6_TALENTOS 
sum(PPTO_IES$PPTO_TECHO)


# Incluir convenio vigente de la cuarta convocatoria (ATENEA-IA-JE-003-2025)
#----------------------------------------------------------
OFERTAS_CONVENIO_JE4 <- merge(
  x = LISTADO_OFERTA_PROPUESTA,
  y = PPTO_IES[, c("CODIGO_SNIES_IES", "CONVENIO")],
  by.x = "COD_IES",
  by.y = "CODIGO_SNIES_IES",
  all  = FALSE
)
LISTADO_OFERTA_PROPUESTA <- merge(
  x = LISTADO_OFERTA_PROPUESTA,
  y = OFERTAS_CONVENIO_JE4[, c("ID", "CONVENIO")],
  by = "ID",
  all = FALSE
)
rm(OFERTAS_CONVENIO_JE4)

cro(LISTADO_OFERTA_PROPUESTA$PROGRAMA_HABILITADO)

#===============================================================================
# FILTRO DE OFERTA HABILITADA
# (Programa habilitado + Cupos según capacidad > 0 + IES con convenio JE4)
#===============================================================================
OFERTA_GENERAL <- unique(LISTADO_OFERTA_PROPUESTA[LISTADO_OFERTA_PROPUESTA$PROGRAMA_HABILITADO=="Habilitado" &  LISTADO_OFERTA_PROPUESTA$CUPOS_SEGÚN_CAPACIDAD > 0,])


#-------------------------------------------------------------------------------
# CONSTRUCCION DETALLE DE COSTOS X SEMESTRE Y CUPOS CONFORME A LA CAPACIDAD
#-------------------------------------------------------------------------------
OFERTA_DETALLE_COSTO <- NA
TMP <- OFERTA_GENERAL[,c("ID", "CUPOS_SEGÚN_CAPACIDAD")]
for (i in TMP$ID) {
  print(i)
  r <- TMP[TMP$ID==i,]
  C <- as.data.frame(1:r$CUPOS_SEGÚN_CAPACIDAD)
  colnames(C) <- c("DETALLE_CUPO")
  r<- merge(C,r)
  OFERTA_DETALLE_COSTO <- union_all(OFERTA_DETALLE_COSTO,r)
}

OFERTA_DETALLE_COSTO <- merge(x=OFERTA_DETALLE_COSTO, y=OFERTA_COSTO_EJECUCION, by="ID", all = FALSE)
rm(C,r,i,TMP)

# Ordenar
OFERTA_DETALLE_COSTO <-OFERTA_DETALLE_COSTO %>%  arrange(ID,
                                                         DETALLE_CUPO,
                                                         ID_ORDEN, na.last=TRUE)

# Crear identificador unico
OFERTA_DETALLE_COSTO$LLAVE_PRIMARIA <- as.double(row.names(OFERTA_DETALLE_COSTO)) 


#===============================================================================
# Paso 0: DISTRIBUCION GENERAL DEL PRESUPUESTO
#===============================================================================

# Presupuesto total disponible para la sexta convocatoria
# Pegar sobrante por anualidad de Talentos
PPTO_GLOBAL <- merge(x=PPTO_GLOBAL, y=PPTO_GLOBAL_TE[,c("ID_ORDEN_BOLSA","RECURSO_SOBRANTE_TALENTOS")], by="ID_ORDEN_BOLSA", all.x = TRUE)
PPTO_GLOBAL$Recurso_General <- PPTO_GLOBAL$Recurso 
# AL presupuesto General, se suma el sobrante de presupuesto de talentos posterior a la asignacion 
PPTO_GLOBAL$Recurso <- PPTO_GLOBAL$Recurso_General + PPTO_GLOBAL$RECURSO_SOBRANTE_TALENTOS

# Calcular para la Bolsa Atenea Vigencia Futura, el detalle por anualidad  
PPTO_ATENEA_VIGENCIA <- PPTO_GLOBAL[PPTO_GLOBAL$ID_ORDEN_BOLSA==4,c(4:10)]
PPTO_ATENEA_VIGENCIA <- pivot_longer(PPTO_ATENEA_VIGENCIA, cols = 1:7, names_to = "ANIO_VIGENCIA", values_to = "RECURSO") 
# Pegar recurso sobrante por anualidad de Talentos
PPTO_ATENEA_VIGENCIA <- merge(x=PPTO_ATENEA_VIGENCIA, y=PPTO_ATENEA_TE[,c("ANIO_VIGENCIA","RECURSO_SOBRANTE_TALENTOS")], by="ANIO_VIGENCIA", all.x = TRUE) 
PPTO_ATENEA_VIGENCIA$RECURSO_GENERAL <- PPTO_ATENEA_VIGENCIA$RECURSO
# AL presupuesto General, se suma el sobrante de presupuesto de talentos posterior a la asignacion 
PPTO_ATENEA_VIGENCIA$RECURSO <- PPTO_ATENEA_VIGENCIA$RECURSO_GENERAL + PPTO_ATENEA_VIGENCIA$RECURSO_SOBRANTE_TALENTOS

PPTO_MecanismoActual <- sum(PPTO_GLOBAL$Recurso) * 0.9 # 90% mecanismo ISOES
PPTO_DemandaSocial   <- sum(PPTO_GLOBAL$Recurso) * 0.1 # 10% componente de alta demanda social


#===============================================================================
# (numeral 5.4.1.6)
# Paso 1: Bolsa de recursos por IES segun % de Valor Convenio JE4 y adicion JE5
#===============================================================================

# Distribucion proporcional al valor del convenio suscrito en JE4 y adicion JE5
PPTO_IES$DISTRIBUCION <-
  (PPTO_IES$`Valor inicial ATENEA` + PPTO_IES$`Total Adiciones ATENEA`) / (sum(PPTO_IES$`Valor inicial ATENEA`) + sum(PPTO_IES$`Total Adiciones ATENEA`))


# Valor monetario asignado a cada IES (90% del PPTO_GLOBAL)
PPTO_IES$VALOR_DISTRIBUCION_IES <- PPTO_MecanismoActual * PPTO_IES$DISTRIBUCION


#===============================================================================
# Paso 1.1: Distribucion intra-IES por Nivel (85% Universitario / 15% TyT)
#===============================================================================

# Total de ofertas habilitadas por IES y nivel
PRESUPUESTO_IES_NIVEL <- LISTADO_OFERTA_PROPUESTA[
    LISTADO_OFERTA_PROPUESTA$PROGRAMA_HABILITADO=="Habilitado" & 
    LISTADO_OFERTA_PROPUESTA$CUPOS_SEGÚN_CAPACIDAD > 0 &
    !is.na(LISTADO_OFERTA_PROPUESTA$CONVENIO),
] %>%
  group_by(COD_IES, NIVEL_PROGRAMA_SNIES_AJUSTE) %>%
  summarise(OFERTA_IES_NIVEL = n())

# Cruzar con presupuesto IES
PRESUPUESTO_IES_NIVEL <- merge(
  x = PRESUPUESTO_IES_NIVEL,
  y = PPTO_IES[, c("CODIGO_SNIES_IES", "VALOR_DISTRIBUCION_IES")],
  by.x = "COD_IES",
  by.y = "CODIGO_SNIES_IES",
  all  = FALSE
)

# Identificar si la IES tiene uno o dos niveles ofertados
IES <- PRESUPUESTO_IES_NIVEL %>%
  group_by(COD_IES) %>%
  summarise(TOTAL_REGISTROS_IES_NIVEL = n())

PRESUPUESTO_IES_NIVEL <- merge(
  x = PRESUPUESTO_IES_NIVEL,
  y = IES,
  by  = "COD_IES",
  all = FALSE
)

# Distribucion por nivel:
#   IES con UNICO nivel  -> 100% al nivel ofertado
#   IES con AMBOS niveles -> 85% UNIVERSITARIO / 15% TYT
PRESUPUESTO_IES_NIVEL$DISTRIBUCION_IES_NIVEL <- 1
PRESUPUESTO_IES_NIVEL[PRESUPUESTO_IES_NIVEL$TOTAL_REGISTROS_IES_NIVEL == 2 &
                        PRESUPUESTO_IES_NIVEL$NIVEL_PROGRAMA_SNIES_AJUSTE == "UNIVERSITARIO",
                      "DISTRIBUCION_IES_NIVEL"] <- 0.85
PRESUPUESTO_IES_NIVEL[PRESUPUESTO_IES_NIVEL$TOTAL_REGISTROS_IES_NIVEL == 2 &
                        PRESUPUESTO_IES_NIVEL$NIVEL_PROGRAMA_SNIES_AJUSTE == "TYT",
                      "DISTRIBUCION_IES_NIVEL"] <- 0.15

# Valor final por IES x Nivel
PRESUPUESTO_IES_NIVEL$VALOR_DISTRIBUCION_IES_NIVEL <-
  PRESUPUESTO_IES_NIVEL$VALOR_DISTRIBUCION_IES *
  PRESUPUESTO_IES_NIVEL$DISTRIBUCION_IES_NIVEL


