

## aqui se colocan toda la descripcion de la muestra 

# Prepare workspace -------------------------------------------------------

# Load library
library(data.table)
library(skimr)
library(dplyr)
library(gtsummary)

# Cargar datos
datos <- fread("frailhrv_d.csv", stringsAsFactors = FALSE)

# -------------------------------------------------------------------------
# Crear variable con 3 grupos
datos[, grupo := fcase(
  frail_cat == "NO", "No frágil",
  frail_cat == "PRE_FRAGIL", "Prefrágil",
  frail_cat == "FRAGIL", "Frágil",
  default = NA_character_
)]

# Verificar conteo (3 grupos)
datos[!is.na(grupo), .N, by = grupo]

# -------------------------------------------------------------------------
# Seleccionar variables de interés
datos_para_tabla <- datos[!is.na(grupo), .(
  grupo,
  age,
  sex,
  imc,
  distance_tm6,
  bp_systolic,
  bp_diastolic,
  bp_pam,
  bp_pp,
  fc,
  fc_max,
  sdnn_Pre, 
  rmssd_Pre, 
  mean_rr_Pre,
  hf_Pre,
  lf_Pre,
  vlf_Pre,
  sns_Pre,
  pns_Pre,
  stress_Pre
)]

# Convertir sex a factor
datos_para_tabla[, sex := as.factor(sex)]

# Crear tabla descriptiva 

tabla_final <- datos_para_tabla %>%
  tbl_summary(
    by = grupo,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no",
    label = list(
      age = "Edad (años)",
      sex = "Sexo",
      imc = "IMC (kg/m²)",
      distance_tm6 = "Distancia TM6 (m)",
      bp_systolic = "bp sistolica",
      bp_diastolic = "bp diastolica",
      bp_pam = "bp PAM",
      bp_pp = "bp PP",
      fc = "FC",
      fc_max = "FC max",
      sdnn_Pre = "SDNN Pre (ms)",
      rmssd_Pre = "RMSSD Pre (ms)",
      mean_rr_Pre = "Mean RR Pre (ms)",
      hf_Pre = "HF Pre (ms)",
      lf_Pre = "LF Pre (ms)",
      vlf_Pre = "VLF Pre (ms)",
      sns_Pre = "SNS Pre (ms)",
      pns_Pre = "PNS Pre (ms)",
      stress_Pre = "Stress Pre (ms)"
    )
  ) %>%
  add_overall() %>%   
  add_p() %>%
  modify_header(label = "**Variable**")

# Mostrar tabla
tabla_final

skim(datos)




