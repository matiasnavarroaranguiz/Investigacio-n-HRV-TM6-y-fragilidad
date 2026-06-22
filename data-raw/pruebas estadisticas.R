

# Cargar librerias --------------------------------------------------------
library(data.table)
library(tidyr)
library(afex)
library(dplyr)
library(rstatix)

# Cargar la base de datos -------------------------------------------------

datos_prueba <- fread("frailhrv_d.csv", stringsAsFactors = FALSE)

head(datos_prueba)


# MEAN RR -----------------------------------------------------------------
mean_rr_largo <-
  datos_prueba %>%
  select(id, mean_rr_Pre, mean_rr_Peri, mean_rr_Post) %>%
  group_by(id) %>%
    pivot_longer(
      cols = c(mean_rr_Pre, mean_rr_Peri, mean_rr_Post),
      names_to = "momento",
      values_to = "mean rr"
    ) %>%
    mutate(
      momento = case_when(
        momento == "mean_rr_Pre" ~ "Pre",
        momento == "mean_rr_Peri" ~ "Peri",
        momento == "mean_rr_Post" ~ "Post"
      ),
      momento = factor(momento, levels = c("Pre", "Peri", "Post")),
      id = as.factor(id)
    ) %>% 
    na.omit()


statsExpressions::oneway_anova(data = mean_rr_largo, 
                               x = momento, y = mean r, subject.id = id, 
                               type = "parametric", paired = TRUE)

statsExpressions::pairwise_comparisons(data = mean_rr_largo[!mean_rr_largo$id %in% c(48,81,97,108),], 
                                       x = momento, y = mean_rr, subject.id = id, 
                                       type = "parametric", paired = TRUE, 
                                       p.adjust.method = "none")

ggstatsplot::ggwithinstats(data = ungroup(mean_rr_largo[!mean_rr_largo$id %in% c(48,81,97,108),]), 
                           x = momento, y = mean_rr, type = "parametric", pairwise.display = "s", 
                           p.adjust.method = "none") este me tiro error

# SDNN ---------------------------------------------

sdnn_largo <- 
  datos_prueba %>%
  select(id, sdnn_Pre, sdnn_Peri, sdnn_Post) %>%
  group_by(id) %>%
  pivot_longer(
    cols = c(sdnn_Pre, sdnn_Peri, sdnn_Post),
    names_to = "momento",
    values_to = "sdnn",
  ) %>%
  mutate(
    momento = case_when(
      momento == "sdnn_Pre" ~ "Pre",
      momento == "sdnn_Peri" ~ "Peri",
      momento == "sdnn_Post" ~ "Post"
    ),
    momento = factor(momento, levels = c("Pre", "Peri", "Post")),
    id = as.factor(id)
  ) %>% 
na.omit()

table(sdnn_largo$id, sdnn_largo$momento)

statsExpressions::oneway_anova(data = sdnn_largo,
                               x = momento, y = sdnn, subject.id = id, 
                               type = "parametric", paired = TRUE)

statsExpressions::pairwise_comparisons(data = sdnn_largo[!sdnn_largo$id %in% c(48,81,97,108),], 
                                       x = momento, y = sdnn, subject.id = id, 
                                       type = "parametric", paired = TRUE, 
                                       p.adjust.method = "none")

ggstatsplot::ggwithinstats(data = ungroup(sdnn_largo[!sdnn_largo$id %in% c(48,81,97,108),]), 
                           x = momento, y = sdnn, type = "parametric", pairwise.display = "s", 
                           p.adjust.method = "none")

#pruebas friedman 


sdnn_ancho <- datos_prueba %>%
  select(id, sdnn_Pre, sdnn_Peri, sdnn_Post) %>%
  as.data.frame()

friedman_result <- friedman.test(
  as.matrix(sdnn_ancho[, c("sdnn_Pre", "sdnn_Peri", "sdnn_Post")])
)

print(friedman_result)

friedman_result <- friedman.test(
  as.matrix(sdnn_ancho[, c("sdnn_Pre", "sdnn_Peri", "sdnn_Post")])
)
print(friedman_result)


# RMSDD -------------------------------------------------------------------
rmssd_largo <-
  datos_prueba %>%
  select(id, rmssd_Pre, rmssd_Peri, rmssd_Post) %>%
  group_by(id) %>%
  pivot_longer(
    cols = c(rmssd_Pre, rmssd_Peri, rmssd_Post),
    names_to = "momento",
    values_to = "rmssd"
  ) %>%
mutate(
  momento = case_when(
    momento == "rmssd_Pre" ~ "Pre",
    momento == "rmssd_Peri" ~ "Peri",
    momento == "rmssd_Post" ~ "Post"
  ),
  momento = factor(momento, levels = c("Pre", "Peri", "Post")),
  id = as.factor(id)
) %>% 
  na.omit()

statsExpressions::oneway_anova(data = rmssd_largo, 
                               x = momento, y = rmssd, subject.id = id, 
                               type = "parametric", paired = TRUE)

statsExpressions::pairwise_comparisons(data = rmssd_largo[!rmssd_largo$id %in% c(48,81,97,108),], 
                                       x = momento, y = rmssd, subject.id = id, 
                                       type = "parametric", paired = TRUE, 
                                       p.adjust.method = "none")
ggstatsplot::ggwithinstats(data = ungroup(rmssd_largo[!rmssd_largo$id %in% c(48,81,97,108),]), 
                           x = momento, y = rmssd, type = "parametric", pairwise.display = "s", 
                           p.adjust.method = "none")

# HF ----------------------------------------------------------------------

lf_largo <-
  datos_prueba %>%
  select(id, lf_Pre, lf_Peri, lf_Post) %>%
  group_by(id) %>%
    pivot_longer(
      c()
    )
  
  
# LF ----------------------------------------------------------------------

lf_largo <- 
  datos_prueba %>%
  select(id, lf_Pre, lf_Peri, lf_Post) %>%
  group_by(id) %>%
  pivot_longer(
    cols = c(lf_Pre, lf_Peri, lf_Post),
    names_to = "momento",
    values_to = "lf",
  ) %>%
  mutate(
    momento = case_when(
      momento == "lf_Pre" ~ "Pre",
      momento == "lf_Peri" ~ "Peri",
      momento == "lf_Post" ~ "Post"
    ),
    momento = factor(momento, levels = c("Pre", "Peri", "Post")),
    id = as.factor(id)
  ) %>% 
  na.omit()


statsExpressions::oneway_anova(data = lf_largo, 
                               x = momento, y = lf, subject.id = id, 
                               type = "parametric", paired = TRUE)

statsExpressions::pairwise_comparisons(data = lf_largo[!lf_largo$id %in% c(48,81,97,108),], 
                                       x = momento, y = lf, subject.id = id, 
                                       type = "parametric", paired = TRUE, 
                                       p.adjust.method = "none")
ggstatsplot::ggwithinstats(data = ungroup(lf_largo[!lf_largo$id %in% c(48,81,97,108),]), 
                           x = momento, y = lf, type = "parametric", pairwise.display = "s", 
                           p.adjust.method = "none")






