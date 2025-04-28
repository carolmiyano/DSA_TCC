install.packages("pROC")

library(pROC)
library(clipr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)



#function
`%notin%` <- Negate(`%in%`)

bd_final <- bd_final %>%
  mutate(cmr_pl = CMR / PL,
         default = ifelse(cmr_pl > 1.2, 1, 0))

# Remoção de linhas com problemas (PL = 0, NA, ou resseguradora com < 24 meses)
bd_final <- bd_final %>%
  filter(
    coenti != "37753",                      # Exclui resseguradora com < 24 meses
    !is.na(cmr_pl),                         # Remove NAs
    is.finite(cmr_pl),                      # Remove Inf/NaN
    !is.na(default)                         # Remove NAs na variável resposta
  )


default_por_entidade <- bd_final %>%
  group_by(coenti, noenti) %>%
  summarise(default_final = as.integer(any(default == 1))) %>%
  ungroup()


pd_susep_final <- default_por_entidade %>% mutate(
  pd_susep = 0.0193
) %>% select (-default_final)


base_completa <- default_por_entidade %>%
  left_join(pd_logit_final, by = c("coenti", "noenti")) %>%
  left_join(pd_tree_final, by = c("coenti", "noenti")) %>%
  left_join(pd_susep_final, by = c("coenti", "noenti"))  


roc_reg <- roc(base_completa$default_final, base_completa$pd_final_logit)
roc_tree <- roc(base_completa$default_final, base_completa$pd_final_tree)
roc_susep <- roc(base_completa$default_final, base_completa$pd_susep)


auc(roc_reg)
auc(roc_tree)
auc(roc_susep)


# Função para extrair as métricas de um objeto ROC
extrair_metricas <- function(roc_obj, nome_modelo) {
  coords_best <- coords(roc_obj, "best", ret = c("sensitivity", "specificity"), transpose = FALSE)
  
  tibble(
    Modelo = nome_modelo,
    AUC = as.numeric(auc(roc_obj)),  # conversão aqui
    Sensibilidade = coords_best$sensitivity,
    Especificidade = coords_best$specificity
  )
}

extrair_metricas <- function(roc_obj, nome_modelo) {
  coords_best <- coords(roc_obj, "best", ret = c("sensitivity", "specificity"), transpose = FALSE)[1, ]
  
  tibble(
    Modelo = nome_modelo,
    AUC = as.numeric(auc(roc_obj)),
    Sensibilidade = coords_best$sensitivity,
    Especificidade = coords_best$specificity
  )
}



# Gerar as métricas
metrics_reg <- extrair_metricas(roc_reg, "Regressão")
metrics_tree <- extrair_metricas(roc_tree, "Árvore de Decisão")
metrics_susep <- extrair_metricas(roc_susep, "SUSEP")

# Quadro resumo final
quadro_resumo <- bind_rows(metrics_reg, metrics_tree, metrics_susep)
print(quadro_resumo)


plot(roc_reg, col = "#1f77b4", lwd = 2, main = "Comparação das Curvas ROC")
plot(roc_tree, col = "#2ca02c", lwd = 2, add = TRUE)
plot(roc_susep, col = "#d62728", lwd = 2, add = TRUE)

legend("bottomright", legend = c("Regressão (Logit)", "Árvore de Decisão", "SUSEP"),
       col = c("#1f77b4", "#2ca02c", "#d62728"), lwd = 2)

# Função auxiliar para transformar um objeto ROC em data.frame
roc_df <- function(roc_obj, modelo) {
  data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Modelo = modelo
  )
}

# Aplicar para os três modelos
df_reg <- roc_df(roc_reg, "Regressão")
df_tree <- roc_df(roc_tree, "Árvore de Decisão")
df_susep <- roc_df(roc_susep, "SUSEP")

# Juntar todos em um único data frame
roc_data <- bind_rows(df_reg, df_tree, df_susep)

ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Regressão" = "#1b9e77", 
                                "Árvore de Decisão" = "#d95f02", 
                                "SUSEP" = "#7570b3")) +
  labs(
    title = "Curvas ROC por Modelo",
    x = "Taxa de Falsos Positivos (1 - Especificidade)",
    y = "Taxa de Verdadeiros Positivos (Sensibilidade)",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")






