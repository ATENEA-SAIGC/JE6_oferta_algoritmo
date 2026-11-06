# Mecanismo de Asignacion de Cupos: Sexta Convocatoria Jovenes a la E (JE6) - Oferta Linea General

<div align="center">

![R Version](https://img.shields.io/badge/R-4.5.1%2B-blue?style=for-the-badge&logo=r)
![License](https://img.shields.io/badge/Licencia-CC%20BY-green?style=for-the-badge)
![Status](https://img.shields.io/badge/Estado-Producci%C3%B3n%20Auditada-success?style=for-the-badge)
![Reproducibilidad](https://img.shields.io/badge/Reproducibilidad-renv%20%7C%20Seed-orange?style=for-the-badge)

**Agencia Distrital para la Educacion Superior, la Ciencia y la Tecnologia (ATENEA)**
*Subgerencia de Analisis de Informacion y Gestion del Conocimiento*

</div>

---

## Tabla de Contenidos

1. [Ficha de Transparencia Algoritmica](#1-ficha-de-transparencia-algoritmica)
2. [Arquitectura del Proyecto](#2-arquitectura-del-proyecto)
3. [Requisitos Tecnicos](#3-requisitos-tecnicos)
4. [Logica Detallada del Algoritmo](#4-logica-detallada-del-algoritmo)
    * [Fase 1: Habilitacion de Programas y Ajuste de Nivel](#fase-1-habilitacion-de-programas-y-ajuste-de-nivel-modulo-1)
    * [Fase 2: Costos por Semestre y Proyeccion de Pagos](#fase-2-costos-por-semestre-y-proyeccion-de-pagos-modulo-2)
    * [Fase 3: Bolsas de Financiacion, Techos y Presupuesto por IES](#fase-3-bolsas-de-financiacion-techos-y-presupuesto-por-ies-modulo-3)
    * [Fase 4: Asignacion por Mecanismo ISOES](#fase-4-asignacion-por-mecanismo-isoes-modulo-4)
    * [Fase 5: Componente de Alta Demanda Social](#fase-5-componente-de-alta-demanda-social-modulo-5)
    * [Fase 6: Consolidacion y Exportacion](#fase-6-consolidacion-y-exportacion-modulo-6)
5. [Diferencias frente al algoritmo JE5](#5-diferencias-frente-al-algoritmo-je5)
6. [Diccionario de Datos](#6-diccionario-de-datos)
7. [Instrucciones de Ejecucion](#7-instrucciones-de-ejecucion)
8. [Garantia de Reproducibilidad](#8-garantia-de-reproducibilidad)
9. [Preguntas Frecuentes y Glosario](#9-preguntas-frecuentes-y-glosario)
10. [Uso del Codigo](#10-uso-del-codigo)

---

## 1. Ficha de Transparencia Algoritmica

| Campo | Contenido |
|---|---|
| **Tipo de algoritmo** | Procesamiento de datos (regla de negocio deterministica). No usa aprendizaje automatico ni inferencia estadistica. |
| **Nombre** | Seleccion de oferta de programas academicos - sexta convocatoria "Jovenes a la E" (JE6), linea General. |
| **Objetivo** | Aplicar los criterios del lineamiento operativo para definir la cantidad de cupos a financiar por cada programa presentado por las IES con convenio vigente, verificando que cada cupo quede financiado en todos sus giros y vigencias fiscales. |
| **Funcion institucional** | Asignar cupos a programas de niveles Tecnico, Tecnologico y Universitario del programa de beneficios de ATENEA "Jovenes a la E", que promueve acceso y permanencia en educacion superior de los bachilleres de Bogota. |
| **Estado** | En implementacion (vigente para la sexta convocatoria - noviembre 2026). |
| **Datos personales** | No procesa datos personales. Opera sobre atributos de IES y programas (codigos SNIES, costos por semestre, capacidad, ISOES, TIR, presupuestos). |
| **Tipo de datos** | Identificadores SNIES (IES y programa), costos por semestre 2027-2033, cupos por capacidad, ISOES, TIR, techos y convenios por IES, bolsas de financiacion por fuente y vigencia, demanda historica por CINE F. Ver [Diccionario de Datos](#6-diccionario-de-datos). |
| **Desarrollador** | Agencia Distrital para la Educacion Superior, la Ciencia y la Tecnologia (ATENEA) - Subgerencia de Analisis de Informacion y Gestion del Conocimiento (SAIGC). |
| **Insumo normativo** | Lineamiento operativo de la sexta convocatoria JE6 ([docs/](docs/)). |
| **Universo procesado** | 770 registros de oferta de 41 IES (611 Universitario, 101 Tecnologico, 58 Tecnico profesional); 17.698 cupos ofertados segun capacidad. |
| **Presupuesto** | $106.344.063.426 COP: $106.064.278.266 de cuatro bolsas (FDL Apalancamiento, FDL - Fondo Cuenta, Atenea Apalancamiento, Atenea Vigencia Futura 2027-2033) mas $279.785.160 sobrantes de la linea Talentos. |

**Principios de diseno:**

* **Transparencia:** reglas de negocio codificadas explicitamente, sin "cajas negras".
* **Auditabilidad:** trazas en consola y libro Excel con el detalle de cada giro financiado y el estado final de cada bolsa.
* **Reproducibilidad:** semilla fija `set.seed(20261008)` y dependencias congeladas con `renv`.

**Como funciona (resumen):** el algoritmo (1) **habilita** la oferta de la linea General, (2) **proyecta los pagos** de cada cupo semestre a semestre y ano fiscal a ano fiscal, (3) **reparte el presupuesto** en bolsas (90% mecanismo ISOES + 10% demanda social; dentro de ISOES, una bolsa por IES proporcional a su convenio y adiciones, con un techo por IES), (4) **ordena los programas** por ISOES > costo de cohorte > capacidad > semilla y **asigna cupo a cupo** solo si el cupo queda financiado completo por las bolsas FDL o, en su defecto, por las bolsas ATENEA, (5) **reasigna excedentes** entre IES del mismo nivel y luego al componente de demanda social. El detalle esta en la [Seccion 4](#4-logica-detallada-del-algoritmo).

---

## 2. Arquitectura del Proyecto

El codigo esta organizado en una arquitectura modular secuencial. Cada modulo aborda una fase del lineamiento operativo y puede auditarse por separado. Los modulos son cortes literales del script original `JE6_Calculo_Ofertas_VF.R`: el codigo de negocio no se modifico.

```text
JE6_oferta_algoritmo/
|
|-- JE6_Main.R                      # ORQUESTADOR PRINCIPAL (Entry Point)
|-- README.md                       # Documentacion (este archivo)
|-- LICENSE                         # CC BY - Obra institucional ATENEA
|-- renv.lock                       # Manifiesto de dependencias exactas
|
|-- data/                           # Insumos
|   +-- JE6_Insumo_Oferta_General.RData   # Oferta, costos, techos IES, bolsas, demanda historica
|
|-- output/                         # Resultados generados
|   +-- JE6_OFERTA_VF_YYYYMMDD.xlsx
|
|-- docs/                           # Lineamiento operativo y soporte
|
+-- modules/                        # Logica de negocio
    |-- 1_habilitacion.R            # Costos linea General, exclusiones (Medicina, SNIES), nivel
    |-- 2_costos_ejecucion.R        # Pagos 80/20 por semestre y ano fiscal de ejecucion
    |-- 3_presupuesto_bolsas.R      # Techos IES, detalle cupo x giro, bolsas, IES x Nivel
    |-- 4_asignacion_isoes.R        # Paso 2 (intra-IES) y Paso 3 (excedente por Nivel)
    |-- 5_demanda_social.R          # 10% + excedente ISOES, bolsas CINE x Nivel
    +-- 6_exportacion.R             # Consolidacion y exportacion XLSX
```

---

## 3. Requisitos Tecnicos

* **R** 4.5.1 o superior y **renv** 1.1.5+.
* Dependencias declaradas en `renv.lock` y restauradas con `renv::restore()`.

Librerias principales: `dplyr`, `tidyr`, `sqldf`, `readr`, `readxl`, `openxlsx`, `lubridate`, `expss`, `eeptools`.

---

## 4. Logica Detallada del Algoritmo

### Fase 1: Habilitacion de Programas y Ajuste de Nivel (Modulo 1)

**Archivo:** `modules/1_habilitacion.R`

| Paso | Regla |
|---|---|
| Costos de la linea General | Se cruza la oferta con `JE6_InsumoCostos` por codigo SNIES, tomando el primer registro (`ID_ORDEN == 1`) y **excluyendo la linea "Talentos"**. El cruce es interno: un programa sin costos de linea General sale del universo. |
| Exclusion de Medicina | Programas con `Medicina_Talento_Excepcional == "Si"` pasan a `"No habilitado"`. |
| Exclusion por comunicado de IES | SNIES 110412 y 105719 (Corporacion Universitaria Iberoamericana) pasan a `"No habilitado"`. |
| Nivel de formacion | `NIVEL_PROGRAMA_SNIES_AJUSTE`: `UNIVERSITARIO` si el nivel es Universitario; `TYT` en cualquier otro caso. |

> **La linea Talentos no se asigna aqui.** Este algoritmo corresponde a la linea General. De Talentos solo llegan su costo ya comprometido por IES (`COSTO_JE6_TALENTOS`), que se descuenta del techo, y su sobrante presupuestal, que se suma a las bolsas.

---

### Fase 2: Costos por Semestre y Proyeccion de Pagos (Modulo 2)

**Archivo:** `modules/2_costos_ejecucion.R`

Cada cupo se descompone en giros concretos que caen en anos fiscales:

1. Los costos por semestre (`2027_1`, `2027_2`, ...) pasan a formato largo y se conservan solo los semestres dentro de la duracion del programa (`Periodos_Referencia`).
2. Cada semestre se divide en **Pago 1 = 80%** y **Pago 2 = 20%**.
3. **Regla de vigencia:**

| Semestre | Pago 1 (80%) | Pago 2 (20%) |
|---|---|---|
| Impar (`xxxx1`) | Ano del semestre | Mismo ano |
| Par (`xxxx2`) | Ano del semestre | **Ano siguiente** |

4. `ID_ORDEN` numera los giros de cada programa en orden cronologico.

> Un programa de 10 semestres genera 20 giros por cupo. Un cupo solo se considera financiado si **todos** sus giros quedan cubiertos.

---

### Fase 3: Bolsas de Financiacion, Techos y Presupuesto por IES (Modulo 3)

**Archivo:** `modules/3_presupuesto_bolsas.R`

**Universo habilitado:** `PROGRAMA_HABILITADO == "Habilitado"`, `CUPOS_SEGUN_CAPACIDAD > 0` e IES presente en `PPTO_IES` (convenio vigente).

**Techo por IES:**

$$PPTO\_TECHO_{IES} = TechoJE6_{IES} - CostoTalentos_{IES}$$

**Bolsas de financiacion** (`PPTO_GLOBAL`, recurso general + sobrante Talentos):

| ID | Bolsa | Recurso | Restriccion |
|---:|---|---:|---|
| 1 | FDL Apalancamiento | $6.963.225.936 | Solo giros `ID_ORDEN` 1 a 3 |
| 2 | FDL - Fondo Cuenta | $31.404.159.218 | Cualquier giro |
| 3 | Atenea Apalancamiento | $2.777.340.338 | Solo giros `ID_ORDEN` 1 a 3 |
| 4 | Atenea Vigencia Futura | $65.199.337.934 | Ademas, cada ano 2027-2033 debe tener saldo (`PPTO_ATENEA_VIGENCIA`) |

**Particion del presupuesto:** 90% mecanismo ISOES / 10% alta demanda social, aplicada a cada bolsa.

**Bolsa por IES** (Paso 1), proporcional al convenio mas adiciones:

$$Recursos_{IES_i} = \frac{ValorInicial_i + Adiciones_i}{\sum_i (ValorInicial_i + Adiciones_i)} \times 90\% \times PPTO\_GLOBAL$$

**Distribucion intra-IES por nivel** (Paso 1.1): 100% al nivel ofertado si la IES tiene un solo nivel; **85% Universitario / 15% TyT** si tiene ambos.

> El encabezado original de este bloque menciona "95% / 5%" y "valor del convenio JE4". El codigo aplica 85% / 15% y valor inicial + adiciones; esta documentacion describe lo que hace el codigo.

---

### Fase 4: Asignacion por Mecanismo ISOES (Modulo 4)

**Archivo:** `modules/4_asignacion_isoes.R`

**Ordenamiento de programas (estricto y deterministico, `na.last = TRUE`):**

```text
1. ISOES_PROGRAMA              descendente  (mayor ISOES primero)
2. TOTAL_VALOR_COHORTE_ATENEA  ascendente   (menor costo primero)
3. CUPOS_SEGUN_CAPACIDAD       descendente  (mayor capacidad primero)
4. SEMILLA                     set.seed(20261008)
```

**Paso 2 - Asignacion intra-IES, cupo a cupo:**

```text
PARA CADA (IES, Nivel) EN ORDEN:
  PARA CADA Programa EN ranking:
    PARA CADA cupo = 1 .. CUPOS_SEGUN_CAPACIDAD:

      SI techo_IES / costo_cohorte < 1:            -> no se asigna
      SI bolsa_IES_Nivel - costo_del_cupo <= 0:    -> no se asigna

      # Intento 1: bolsas FDL
      PARA CADA giro del cupo (en orden cronologico):
          financiar con Bolsa 1 (solo giros 1-3) o, si no, con Bolsa 2
      SI todos los giros quedaron financiados:
          asignar cupo (CUPO_FDL + 1)
      SINO:
          # Intento 2: se descarta lo parcial y se prueba con bolsas ATENEA
          PARA CADA giro: financiar con Bolsa 3 (solo giros 1-3) o con Bolsa 4
          SI todos los giros quedaron financiados
             Y cada ano fiscal de la Bolsa 4 tiene saldo suficiente:
              asignar cupo (CUPO_ATENEA + 1)
          SINO:
              no se asigna

      Al asignar: se descuenta bolsa IES-Nivel, techo IES, bolsas usadas
                  y (si aplica) cada ano de la vigencia futura.
```

**Paso 3 - Reasignacion del excedente por Nivel:** los saldos de cada IES x Nivel se agrupan en una bolsa comun por Nivel (Universitario / TyT). Se recorre de nuevo la lista en el mismo orden ISOES para programas con `CUPO_PENDIENTES > 0`, con la misma regla de financiacion completa FDL -> ATENEA y el mismo control del techo por IES.

> **Un cupo nunca se financia a medias.** Si una bolsa alcanza para los primeros semestres pero no para los ultimos, o si algun ano de la vigencia futura no tiene saldo, el cupo no se asigna y el algoritmo pasa al siguiente.

---

### Fase 5: Componente de Alta Demanda Social (Modulo 5)

**Archivo:** `modules/5_demanda_social.R`

**Bolsa inicial:** 10% del presupuesto **mas** todo el excedente del mecanismo ISOES.

**Indicador de demanda social** (por nivel, sobre datos historicos):

$$Demanda\_Social_{CINE_i} = \frac{Inscritos\ en\ CINE\ F\ detallado\ i}{Cupos\ en\ CINE\ F\ detallado\ i}$$

**Universo elegible** (filtros en cascada):

1. CINE F detallado con **menos de 6 cupos** asignados en ISOES (sumando todos sus programas).
2. Programas con `CUPO_PENDIENTES > 0`.
3. Indicador de demanda social disponible.
4. Por encima del percentil 50 del indicador en su nivel.

**Distribucion:** 85% Universitario / 15% TyT. Dentro de cada nivel, bolsas **CINE x Nivel** proporcionales a los inscritos historicos (`DH_INSCRITOS`).

**Recorrido:** identico al Modulo 4 (primero dentro de cada bolsa CINE x Nivel, luego excedente por Nivel), con la misma regla de financiacion completa por bolsas y techo por IES.

---

### Fase 6: Consolidacion y Exportacion (Modulo 6)

**Archivo:** `modules/6_exportacion.R`

1. Merge de resultados ISOES + Demanda Social por `ORDENAMIENTO_PROG`; programas no evaluados en demanda social quedan con `EVALUADO_DEMANDA_SOCIAL = "N"`.
2. `TOTAL_CUPOS_ASIGNADOS = ISOES + Demanda Social`; igual para costos.
3. Pruebas de cierre presupuestal en consola.
4. Exportacion a `output/JE6_OFERTA_VF_YYYYMMDD.xlsx`:

| Hoja | Contenido |
|---|---|
| `JE6_PROGRAMAS` | Resultado por programa: ranking, cupos por paso (IES, Nivel, CINE, Nivel DH), cupos FDL/ATENEA y costos. |
| `OFERTA_DETALLE_COSTO` | Cada giro financiado: cupo, semestre, pago, ano de ejecucion y bolsa asignada. |
| `PPTO_GLOBAL` | Bolsas de financiacion con su saldo final por componente. |
| `PPTO_ATENEA_VIGENCIA` | Saldo final por ano fiscal de la bolsa Atenea Vigencia Futura. |
| `PPTO_IES` | Techo y distribucion por IES, con techo remanente. |
| `IES_NIVEL` | Auditoria de la bolsa IES x Nivel (Paso 1) con remanente final. |
| `EXCEDENTE_NIVEL` | Saldo final de la bolsa comun por Nivel (demanda social). |

**Resultado de la corrida de referencia (noviembre 2026):** 2.453 cupos asignados (2.283 ISOES + 170 demanda social; 1.880 Universitario y 573 TyT), de los cuales 838 financiados por FDL y 1.615 por ATENEA, en 216 programas de 39 IES, por $100.819.581.420.

---

## 5. Diferencias frente al algoritmo JE5

| Aspecto | JE5 | JE6 |
|---|---|---|
| Presupuesto | $17.753.034.702 (adicion FDL) | $106.344.063.426 en 4 bolsas FDL / ATENEA + sobrante Talentos |
| Unidad de costo | Valor de cohorte | Giros por semestre (80/20) asignados a anos fiscales 2027-2033 |
| Financiacion del cupo | Bolsa IES-Nivel | Bolsa IES-Nivel + techo IES + financiacion completa por bolsa FDL o ATENEA + saldo por vigencia |
| Asignacion | Bloque de cupos (`floor(bolsa/costo)`) | Cupo a cupo |
| Bolsa IES (Paso 1) | Proporcional al valor del convenio JE4 | Proporcional a valor inicial + adiciones ATENEA |
| Techo por IES | No | Si (techo JE6 menos costo Talentos) |
| Habilitacion | Ejecutoria SNIES + exclusion Medicina | Costos linea General + exclusion Medicina + exclusion SNIES 110412 y 105719 |
| Distribucion por nivel | 95% Universitario / 5% TyT | 85% Universitario / 15% TyT |
| Particion ISOES / Demanda | 90% / 10% | Igual |
| Semilla | `set.seed(20260427)` | `set.seed(20261008)` |
| Salida | XLSX con 4 hojas | XLSX con 7 hojas |

> El repositorio JE5 publicable se encuentra en [ATENEA-Analitica/JE5_oferta_algoritmo](https://github.com/ATENEA-Analitica/JE5_oferta_algoritmo).

---

## 6. Diccionario de Datos

### Input: `data/JE6_Insumo_Oferta_General.RData`

| Objeto | Dimensiones | Descripcion |
|---|---|---|
| `LISTADO_OFERTA_PROPUESTA` | 770 x 38 | Oferta tecnica de las IES (programas, capacidad, ISOES, TIR, habilitacion). |
| `JE6_InsumoCostos` | 884 x 68 | Costos por semestre 2027-2033 por programa y linea (General, Talentos, General + Talentos). |
| `PPTO_IES` | 39 x 9 | IES con convenio: valor inicial, adiciones, techo JE6 y costo comprometido en Talentos. |
| `PPTO_GLOBAL` | 4 x 10 | Bolsas de financiacion (fuente, recurso total y desagregado 2027-2033). |
| `PPTO_GLOBAL_TE` | 4 x 11 | Mismas bolsas tras la linea Talentos, con su sobrante. |
| `PPTO_ATENEA_TE` | 7 x 3 | Sobrante de Talentos por ano en la bolsa Atenea Vigencia Futura. |
| `DemandaHistorica` | 126 x 6 | Demanda social historica por CINE F detallado y nivel (inscritos, cupos, indicador, percentil 50). |

**Campos clave:**

| Campo | Objeto | Descripcion |
|---|---|---|
| `ID` | Oferta | Identificador unico del registro de oferta |
| `COD_IES` / `CODIGO_SNIES_IES` | Oferta / PPTO_IES | Codigo SNIES de la IES |
| `CÓDIGO_SNIES_DEL_PROGRAMA_A_OFERTAR` | Oferta / Costos | Codigo SNIES del programa |
| `NIVEL_DE_FORMACIÓN` | Oferta | Nivel original del SNIES |
| `CINE_F_2013_AC_CAMPO_DETALLADO` | Oferta | CINE F campo detallado (base demanda social) |
| `ISOES_PROGRAMA` | Oferta | Indice ISOES del programa |
| `CUPOS_SEGÚN_CAPACIDAD` | Oferta | Cupos ofertados por la IES |
| `PROGRAMA_HABILITADO` | Oferta | Habilitacion administrativa |
| `Periodos_Referencia` | Costos | Numero de semestres financiados |
| `2027_1` ... `2033_2` | Costos | Aporte ATENEA por semestre |
| `TOTAL_VALOR_ATENEA` | Costos | Costo total ATENEA de la cohorte |
| `Medicina_Talento_Excepcional` | Costos | Marca de programas de Medicina |
| `Techo de asignación para JE6 ATENEA` | PPTO_IES | Techo maximo por IES |
| `COSTO_JE6_TALENTOS` | PPTO_IES | Costo ya comprometido en la linea Talentos |

### Variables calculadas

| Variable | Modulo | Descripcion |
|---|---|---|
| `NIVEL_PROGRAMA_SNIES_AJUSTE` | 1 | `UNIVERSITARIO` / `TYT` |
| `NUMERO_PAGO`, `PORCENTAJE_PAGO` | 2 | Pago1 (80%) / Pago2 (20%) |
| `ANIO_EJECUCION` | 2 | Ano fiscal en que cae el giro |
| `ID_ORDEN` | 2 | Orden cronologico del giro dentro del programa |
| `PPTO_TECHO` | 3 | Techo IES neto de Talentos (se descuenta al asignar) |
| `DETALLE_CUPO`, `LLAVE_PRIMARIA` | 3 | Numero de cupo y llave unica del giro |
| `VALOR_DISTRIBUCION_IES_NIVEL` | 3 | Bolsa inicial IES x Nivel |
| `SEMILLA`, `ORDENAMIENTO_PROG` | 4 | Desempate aleatorio y posicion en el ranking |
| `BOLSA_ASIGNADA` | 4-5 | Bolsa que financio el giro (NA si no se financio) |
| `CUPO_ASIGNADO_IES`, `CUPO_ASIGNADO_NIVEL` | 4 | Cupos ISOES Paso 2 y Paso 3 |
| `CUPO_FDL`, `CUPO_ATENEA` | 4-5 | Cupos segun fuente de financiacion |
| `CUPOS_ASIGNADOS_MECANISMO_ISOES` | 4 | Total cupos ISOES |
| `CUPO_ASIGNADO_CINE`, `CUPO_ASIGNADO_NIVEL_DH` | 5 | Cupos demanda social por CINE y por excedente de nivel |
| `CUPOS_ASIGNADOS_DEMANDA_SOCIAL` | 5 | Total cupos demanda social |
| `TOTAL_CUPOS_ASIGNADOS`, `TOTAL_COSTO_CUPOS_ASIGNADOS` | 6 | Totales finales |

---

## 7. Instrucciones de Ejecucion

1. Instale R (4.5+) y RStudio. Clone este repositorio.
2. Instale dependencias:
   ```r
   if (!require("renv")) install.packages("renv")
   renv::restore()
   ```
3. El insumo `data/JE6_Insumo_Oferta_General.RData` ya viene incluido en el repositorio.
4. Abra el proyecto en la raiz del repositorio y ejecute `source("JE6_Main.R")`. El script activa `renv`, carga librerias y corre los 6 modulos en secuencia. Los modulos **no se ejecutan de forma independiente**.
5. Resultado: `output/JE6_OFERTA_VF_YYYYMMDD.xlsx`.

> A diferencia del script original, **no hay que editar ninguna ruta**.

> La asignacion recorre cupo a cupo y giro a giro, e imprime cada paso en consola: la corrida completa tarda **varios minutos**.

---

## 8. Garantia de Reproducibilidad

El algoritmo es deterministico bajo las mismas entradas:

* **Semilla fija:** `set.seed(20261008)` (dia siguiente al cierre de la etapa de validacion de la oferta con las IES) para desempates.
* **Presupuesto explicito:** bolsas, vigencias y techos incluidos en el insumo `.RData`.
* **Entorno controlado:** `renv.lock` congela versiones de dependencias.
* **Sin rutas absolutas:** todas las rutas son relativas a la raiz del repositorio.
* **Versionamiento:** codigo bajo git.
* **Validacion:** la version modular se ejecuto y se comparo celda por celda, en todas las hojas, contra el script original y contra el archivo de resultados entregado por el area.

---

## 9. Preguntas Frecuentes y Glosario

**Por que un programa puede recibir menos cupos de los que oferto?**
Porque los cupos se asignan uno a uno mientras alcancen la bolsa de su IES y nivel, el techo de la IES y las bolsas de financiacion. Solo se aprueba lo que se puede financiar completo sin exceder el presupuesto.

**Por que un programa con buen ISOES puede no recibir cupos y otro de menor ISOES si?**
El ranking ISOES se aplica dentro de cada IES y nivel. Ademas, un cupo caro puede no caber en el saldo disponible (o en algun ano de la vigencia futura) mientras que uno mas barato posterior en el ranking si cabe.

**Que significa que un cupo quede financiado por FDL o por ATENEA?**
El algoritmo intenta primero financiar todos los giros del cupo con recursos de los Fondos de Desarrollo Local; si no alcanza para el cupo completo, lo intenta con recursos de ATENEA. La hoja `OFERTA_DETALLE_COSTO` muestra la bolsa de cada giro.

**Por que Medicina no aparece aprobada?**
Por decision explicita: en la linea General los programas marcados como Medicina quedan "No habilitado".

**Que pasa con la linea Talentos?**
Se asigna en un proceso aparte. Aqui solo se descuenta su costo del techo de cada IES y se suma su sobrante a las bolsas.

**Que pasa con los excedentes del mecanismo ISOES?**
Se acumulan a la bolsa de demanda social (10% + excedente), que se reparte entre programas de campos CINE con baja asignacion ISOES, cupos pendientes y demanda historica por encima de la mediana de su nivel.

**Es posible replicar los resultados fuera de ATENEA?**
Si. Con este repositorio, el insumo incluido y `renv::restore()` cualquier persona reproduce los resultados.

### Glosario

| Termino | Definicion |
|---|---|
| **IES** | Institucion de Educacion Superior. |
| **Cupo** | Matricula financiada para una persona en un programa especifico durante toda su duracion. |
| **Giro** | Pago a la IES por un semestre (Pago 1 = 80%, Pago 2 = 20%). |
| **Vigencia** | Ano fiscal de ejecucion del gasto. |
| **Vigencia futura** | Recurso comprometido hoy para ejecutarse en anos fiscales posteriores (2027-2033). |
| **FDL** | Fondos de Desarrollo Local de las alcaldias locales de Bogota. |
| **Apalancamiento** | Bolsas (FDL y ATENEA) que solo financian los primeros giros del cupo. |
| **Techo IES** | Monto maximo de recursos JE6 que puede recibir una IES. |
| **ISOES** | Indice de Seleccion de Oferta para la Educacion Superior: pertinencia, calidad y permanencia. |
| **TIR** | Tasa Interna de Retorno del programa (componente de pertinencia del ISOES). |
| **CINE F** | Clasificacion Internacional Normalizada de la Educacion - campo detallado (UNESCO). |
| **SNIES** | Sistema Nacional de Informacion de la Educacion Superior (MEN). |
| **Mecanismo ISOES** | Componente que distribuye el 90% del presupuesto por ranking ISOES. |
| **Demanda Social** | Componente que distribuye el 10% (mas excedentes) por demanda historica. |

---

## 10. Uso del Codigo

Obra institucional de ATENEA (articulo 91 de la Ley 23 de 1982) publicada bajo **Creative Commons Attribution 4.0 (CC BY 4.0)**, en cumplimiento de los principios de transparencia y acceso a la informacion publica.

Cualquier persona puede reproducir, distribuir, comunicar publicamente y transformar esta obra, citando a ATENEA como autora institucional. Las modificaciones o versiones derivadas son responsabilidad exclusiva de quien las realice. Esta autorizacion no implica cesion de derechos patrimoniales ni afecta los derechos morales sobre la obra original.

(c) 2026 ATENEA - Agencia Distrital para la Educacion Superior, la Ciencia y la Tecnologia.

**Desarrollado por:** SAIGC - ATENEA · **Publicacion:** Noviembre 2026
