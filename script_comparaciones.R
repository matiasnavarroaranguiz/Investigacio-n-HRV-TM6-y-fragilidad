
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(statsExpressions)
library(parameters)
library(lme4)
library(ggplot2)
library(dplyr)


## Load data
datos <- fread("frailhrv_d.csv")

# -------------------------------------------------------------------------

datos_largo <- melt.data.table(
  data = datos, 
  id.vars = "id", 
  measure.vars = patterns("_Post$|_Pre$|_Peri$")
)

datos_largo[, time := fcase(
  grepl("Pre", variable), "Pre",
  grepl("Peri", variable), "Peri",
  grepl("Post", variable), "Post"
)][]

datos_largo[, time := factor(time, levels = c("Pre", "Peri", "Post"))]

datos_largo[, variable := gsub("_Pre|_Peri|_Post", "", variable)][]

resultados <- datos_largo[, statsExpressions::oneway_anova(.SD, x = time, y = value, subject.id = id, paired = TRUE), variable]


# -------------------------------------------------------------------------

ggplot(datos_largo, aes(x = time, y = value)) +
  facet_wrap(~variable, scales = "free") +
  stat_summary(geom = "line", aes(group = 1), fun = mean) +
  stat_summary(aes(group = 1)) +
  stat_summary(aes(group = 1), geom = "ribbon", alpha = 0.1) +
  theme_classic()


# -------------------------------------------------------------------------

datos_semianchos <- dcast(
  data = datos_largo, formula = id + time ~ variable, value.var = "value"
)
 

# -------------------------------------------------------------------------


datos_modelo <- datos_semianchos[datos[, .SD, .SDcols = c("id", names(datos[, sex:frail_above_0]))], on = "id"]

# -------------------------------------------------------------------------

# Modelo lineal de efectos aleatorios

lme4::lmer(hf ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(lf ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(vlf ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(sdnn ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(rmssd ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(mean_rr ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(pns ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(sns ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(stress ~ time * frail_above_0 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 


# Modelo lineal de efectos aleatorios

lme4::lmer(hf ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(lf ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(vlf ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(sdnn ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(rmssd ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(mean_rr ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(pns ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(sns ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(stress ~ time * frail_above_0 + sex + age + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 



# # Modelo lineal de efectos aleatorios -----------------------------------

lme4::lmer(hf ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(lf ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(vlf ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(sdnn ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(rmssd ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(mean_rr ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(pns ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(sns ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 
lme4::lmer(stress ~ time * frail_above_0 + sex + age + velocity_tm6 + (1|id), data = datos_modelo, REML = TRUE) |> parameters::parameters() 








