#==============================================================================================
# MODULO 1 (JE6_Main.R): HABILITACION DE PROGRAMAS Y AJUSTE DE NIVEL
#==============================================================================================
# Referencia: lineamiento operativo JE6 (numeral por validar con el area responsable).
#
# Que hace:
#   - Cruza la oferta (LISTADO_OFERTA_PROPUESTA) con el insumo de costos JE6_InsumoCostos
#     por codigo SNIES, tomando solo el primer registro (ID_ORDEN == 1) y EXCLUYENDO la
#     linea "Talentos": esa linea se asigna en un proceso aparte y aqui solo llega su
#     costo (PPTO_IES$COSTO_JE6_TALENTOS) y su sobrante (PPTO_*_TE).
#   - El merge es interno (all = FALSE): un programa sin costos de linea General queda
#     fuera del universo, no "no habilitado".
#   - Marca como "No habilitado" los programas de Medicina (Medicina_Talento_Excepcional
#     = "Si") y los SNIES 110412 y 105719 (comunicado de la Corporacion Universitaria
#     Iberoamericana).
#   - Homologa el nivel: todo lo que no es "Universitario" pasa a "TYT".
#
# Nota: los comentarios originales del bloque siguiente mencionan "convenio JE4" y
# "2026-2"; se conservan tal cual porque el codigo no se modifica.
#----------------------------------------------------------------------------------------------

#==============================================================================================
# MODULO 1: HABILITACION DE IES Y PROGRAMAS
#==============================================================================================
# Aplica los criterios habilitantes del lineamiento operativo (numeral 5.4.1.3):
#   a) IES con convenio vigente JE4 (Invitacion Publica ATENEA-IA-JE-003-2025)
#   b) Relación de los programas académicos considerados para la asignación de cupos en la presente convocatoria, correspondientes a la oferta presentada y habilitada en la cuarta convocatoria (JE4)
#   c) Exclusion explicita de programas de Medicina
#   d) Exclusion explicita de programas para Corporación Universitaria Iberoamericana
#----------------------------------------------------------------------------------------------

#----------------------------------------------------------
# ACTUALIZACION DE PERIODOS Y COSTOS desde 2027-1
#----------------------------------------------------------
# Pegue de costos actualizados sobre el listado de oferta 

cro(JE6_InsumoCostos$Línea)
# Programas diferentes a Talentos
TMP <- merge(x=LISTADO_OFERTA_PROPUESTA[,c("ID","CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR")], y=JE6_InsumoCostos[JE6_InsumoCostos$ID_ORDEN==1 & JE6_InsumoCostos$Línea !="Talentos" ,c(4,19,36:51)], by = "CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR", all = FALSE  )
names(TMP)

LISTADO_OFERTA_PROPUESTA <- merge(x=LISTADO_OFERTA_PROPUESTA[,c(1:21)], y=TMP[,c(-1)], by="ID", all = FALSE  )
names(LISTADO_OFERTA_PROPUESTA)
rm(TMP)

# Linea General no habilita Medicina
LISTADO_OFERTA_PROPUESTA[!is.na(LISTADO_OFERTA_PROPUESTA$Medicina_Talento_Excepcional) & LISTADO_OFERTA_PROPUESTA$Medicina_Talento_Excepcional=="Si", "PROGRAMA_HABILITADO"] <- "No habilitado"

# Linea General no habilita programa SNIES 110412 y 105719 por comunicado de Corporación Universitaria Iberoamericana
LISTADO_OFERTA_PROPUESTA[LISTADO_OFERTA_PROPUESTA$CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR %in% c(110412,105719), "PROGRAMA_HABILITADO"] <- "No habilitado"
cro(LISTADO_OFERTA_PROPUESTA$PROGRAMA_HABILITADO)

#===============================================================================
# AJUSTE DE NIVEL DE FORMACION (Universitario / TyT)
#===============================================================================
LISTADO_OFERTA_PROPUESTA$NIVEL_PROGRAMA_SNIES_AJUSTE <- "UNIVERSITARIO"
LISTADO_OFERTA_PROPUESTA[LISTADO_OFERTA_PROPUESTA$NIVEL_DE_FORMACIÓN!="Universitario", "NIVEL_PROGRAMA_SNIES_AJUSTE"] <-"TYT"
cro(LISTADO_OFERTA_PROPUESTA$NIVEL_DE_FORMACIÓN, LISTADO_OFERTA_PROPUESTA$NIVEL_PROGRAMA_SNIES_AJUSTE)

