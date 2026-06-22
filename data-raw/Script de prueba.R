# Cargar paquetes necesarios DE DATOS  ---------------------------------------------------

library(tidyr)
library(ggplot2)
library(afex)
library(dplyr)


# SE CARGA LOS ARCHIVOS ---------------------------------------------------------------------

 ## funcion "read.csv" lee el archivo original y cargar la base de datos. 
 ## buscar la funcion stringsAsFactors

datos_prueba <- read.csv("frailhrv_d.csv", stringsAsFactors = FALSE)

head(datos_prueba)


# Transformar a formato largo ---------------------------------------------
## pivot_longer es la funcion clave para pasar de formato ancho a largo

datos_largos_sdnn <- 
  datos_anchos_sdnn %>% 
  pivot_longer(cols = c(sdnn_Pre,sdnn_Peri,sdnn_Post),
               names_to = "mometno",
               values_to = "sdnn_valor") 


# visualizar datos --------------------------------------------------------
   
## este 1er codigo es para 
prueba2 <- datos_prueba %>%
  pivot_longer(
    cols = contains(match = c("mean_rr", "rmssd","sdnn","hf","lf","vlf","stress","pns","sns")), 
    names_to = "momento",
    values_to = "hrv_value"
  ) %>%
  mutate(time = case_when(
    grepl("Pre", momento) ~ "Pre",
    grepl("Peri", momento) ~ "Peri",
    grepl("Post", momento) ~ "Post"
  )) %>%
  mutate(time = factor(time, levels = c("Pre", "Peri", "Post")))


prueba3 <- prueba2 %>%
  mutate(momento = gsub("_Pre|_Peri|_Post", "", momento))


prueba4 <- prueba3 %>%
  pivot_wider(names_from = momento,
              values_from = hrv_value)


# -------------------------------------------------------------------------


data_viz <- prueba3 %>%
  group_by(momento) %>%
  mutate(hrv_value = scale(hrv_value)) %>%
  mutate(hrv_value = hrv_value - mean(hrv_value[time == "Pre"], na.rm = TRUE))

ggplot(data_viz, aes(x = time, y = hrv_value, col = momento)) +
  facet_wrap(~momento) +
  stat_summary(geom = "line", fun = mean, aes(group = momento), show.legend = FALSE) +
  stat_summary(aes(group = momento), show.legend = FALSE) +
  geom_line(aes(group = id), alpha = 0.1, show.legend = FALSE) +
  theme_light()


# -------------------------------------------------------------------------

## Test de friedman: HRV ~ tiempo | Identificador
friedman.test(mean_rr ~ time | id, data = prueba4 %>% filter(!is.na(mean_rr)))
friedman.test(sdnn ~ time | id, data = prueba4 %>% group_by(id) %>% filter(all(!is.na(sdnn))))
friedman.test(rmssd ~ time | id, data = prueba4 %>% group_by(id) %>% filter(all(!is.na(rmssd))))
friedman.test(hf ~ time | id, data = prueba4 %>% group_by(id) %>% filter(all(!is.na(hf))))

## ANOVA
afex::aov_ez(id = "id", dv = "mean_rr", within = "time", data = prueba4)
afex::aov_ez(id = "id", dv = "sdnn", within = "time", data = prueba4)
afex::aov_ez(id = "id", dv = "rmssd", within = "time", data = prueba4)

# -------------------------------------------------------------------------

## prueba el ggstatsplot y su documentacion

statsExpressions::oneway_anova(prueba4, x = time, y = mean_rr, subject.id = id, 
                               type = "n")

statsExpressions::oneway_anova(prueba4, x = time, y = mean_rr, subject.id = id, 
                               type = "p")




###### funcion skim para previsualizar los datos

# Skim general de los datos
skim(datos)

# Skim por grupo (solo No frágil y Prefrágil)
datos[!is.na(grupo)] %>% 
  group_by(grupo) %>% 
  skim()

# Skim específico para variables de HRV
datos[!is.na(grupo), .(sdnn_Pre, sdnn_Peri, sdnn_Post, 
                       rmssd_Pre, rmssd_Peri, rmssd_Post)] %>% 
  skim()


