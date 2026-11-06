#==============================================================================================
# JE6 - Sexta convocatoria Jovenes a la E
# Mecanismo de seleccion y aprobacion de las ofertas tecnicas (linea General)
# 2026-11-05
#==============================================================================================
# ORQUESTADOR PRINCIPAL (Entry Point)
# Ejecutar desde la raiz del repositorio: source("JE6_Main.R")
#==============================================================================================

#-----------------------------------------
# ACTIVAR RENV (Gestion de dependencias)
#-----------------------------------------
source("renv/activate.R")

#-----------------------------------------
# CONFIGURACION DEL ENTORNO
#-----------------------------------------
library(sqldf)
library(expss)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(eeptools)
library(openxlsx)
library(lubridate)
options(scipen=999)

#-----------------------------------------
# CARGAR INSUMOS PARA EL CALCULO DE OFERTA
#-----------------------------------------
load("data/JE6_Insumo_Oferta_General.RData")

#-----------------------------------------
# EJECUTAR MODULOS
#-----------------------------------------

# Modulo 1: Habilitacion de programas (costos linea General, exclusiones) y ajuste de nivel
source("modules/1_habilitacion.R")

# Modulo 2: Costos por semestre y proyeccion de pagos (80/20) por vigencia fiscal
source("modules/2_costos_ejecucion.R")

# Modulo 3: Techos por IES, detalle de costo por cupo, bolsas FDL/Atenea y presupuesto IES x Nivel
source("modules/3_presupuesto_bolsas.R")

# Modulo 4: Asignacion ISOES - Paso 2 (intra-IES) y Paso 3 (excedente por Nivel)
source("modules/4_asignacion_isoes.R")

# Modulo 5: Componente de alta demanda social (10% + excedente ISOES)
source("modules/5_demanda_social.R")

# Modulo 6: Consolidacion y exportacion de resultados a Excel
source("modules/6_exportacion.R")
