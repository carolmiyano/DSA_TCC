# Instale se ainda não tiver
install.packages("corrplot")
install.packages("glmnet")
install.packages("Matrix")



# 1. Carregar os pacotes
library(glmnet)   # Para regressão logística com LASSO
library(caret)    # Para pré-processamento e split de dados
library(Matrix)   # Para matriz esparsa (usada pelo glmnet)
library(corrplot)
library(dplyr)
library(tidyr)
library(clipr)
library(ggplot2)

bd_final <- bd_final %>%
  mutate(cmr_pl = CMR / PL,
         default = ifelse(cmr_pl > 1.2, 1, 0)) %>%
  filter(!is.na(default), is.finite(cmr_pl)) %>%
  mutate(default = as.factor(default))

# Remoção de linhas com problemas (PL = 0, NA, ou resseguradora com < 24 meses)
bd_final <- bd_final %>%
  filter(
    coenti != "37753",                      # Exclui resseguradora com < 24 meses
    !is.na(cmr_pl),                         # Remove NAs
    is.finite(cmr_pl),                      # Remove Inf/NaN
    !is.na(default)                         # Remove NAs na variável resposta
  )

# Seleciona apenas variáveis numéricas (exceto identificadores e variável resposta)
variaveis_numericas <- bd_final %>%
  select(-c(default)) %>%
  select(where(is.numeric)) 
  

# Calcula a matriz de correlação
cor_mat <- cor(variaveis_numericas, use = "complete.obs")

# Visualiza a matriz com corrplot
corrplot(cor_mat,
         method = "color",      # Cor para indicar magnitude e direção da correlação
         type = "upper",        # Apenas metade superior
         tl.cex = 0.7,          # Tamanho dos nomes das variáveis
         number.cex = 0.6,      # Tamanho dos números (se usar type = "number")
         order = "hclust")      # Organiza por similaridade (clustering)


# Transforma a matriz em long format para facilitar análise
cor_data <- as.data.frame(as.table(cor_mat)) %>%
  filter(Var1 != Var2) %>%  # remove a diagonal
  mutate(abs_cor = abs(Freq)) %>%
  filter(abs_cor > 0.8) %>%  # define aqui seu limiar
  arrange(desc(abs_cor))


variaveis_para_remover <- c(
  "Creditos_receber_ATNCIRC",
  "Outras_Apps_ATCIRC",
  "App_Exterior_ATNCIRC",
  "Titulo_RF_ATNCIRC",
  "Ativo_Circulante_Total",
  "Passivo_Circulante_Total",
  "Ativos_Ress_Retro_ATNCIRC"
)

bd_modelo <- bd_final %>% 
  select(-all_of(variaveis_para_remover))


# 2. Separar variáveis preditoras e variável resposta
# Supondo que sua base tratada final seja 'bd_modelo'
# E que 'default' é a variável resposta binária

# Remove colunas de identificação e a resposta da base X
X <- bd_modelo %>%
  select(-default, -coenti) %>%
  select(where(is.numeric))  # Remover colunas de ID e resposta

y <- bd_modelo$default  # Variável resposta

# 3. Padronizar as variáveis preditoras
preproc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preproc, X)

# 4. Converter X para matriz esparsa (necessária para glmnet)
X_matrix <- as.matrix(X_scaled)

# 5. Ajustar modelo com LASSO
# alpha = 1 indica LASSO (penalização L1)
# family = "binomial" porque é regressão logística
modelo_lasso <- cv.glmnet(X_matrix, y, alpha = 1, family = "binomial", type.measure = "class")

#warnings()

# 6. Ver lambda ótimo (menor erro de classificação)
lambda_otimo <- modelo_lasso$lambda.min

# 7. Usar o modelo final para prever as probabilidades de default
pd_logit_lasso <- predict(modelo_lasso, newx = X_matrix, s = lambda_otimo, type = "response")

# 8. Anexar ao seu data frame
bd_modelo$pd_logit_lasso <- as.numeric(pd_logit_lasso)

# 9. Verificar os resultados
summary(bd_modelo$pd_logit_lasso)
hist(bd_modelo$pd_logit_lasso, breaks = 20, main = "Probabilidades - Logit com LASSO", xlab = "PD")


bd_modelo <- bd_modelo %>%
  mutate(peso = cessao_Resseguro + recuperacao_Resseguro + 
           Provisoes_Tecnicas_PASSCIRC + Provisoes_Tecnicas_PASSNCIRC)

pd_logit_final <- bd_modelo %>%
  group_by(coenti, noenti) %>%
  summarise(pd_final_logit = weighted.mean(pd_logit_lasso, peso, na.rm = TRUE)) %>%
  ungroup()

bd_reg <- bd_modelo

saveRDS(bd_reg, file = "bd_reg.rds")
saveRDS(pd_logit_final, file = "pd_logit_final.rds")
