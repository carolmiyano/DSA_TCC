# Instalação de pacotes (caso ainda não tenha)
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("caret")

# Carregando pacotes
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)

# ---------------------------
# 1. Pré-processamento da base
# ---------------------------

# Criação da variável de default com base no critério cmr/pl
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

# Transforma a variável default em fator
bd_final$default <- as.factor(bd_final$default)

# -------------------------------
# 2. Separando treino e teste
# -------------------------------

set.seed(123)  # Para reprodutibilidade

# Partição estratificada
index <- createDataPartition(bd_final$default, p = 0.7, list = FALSE)
base_treino <- bd_final[index, ]
base_teste <- bd_final[-index, ]

# ----------------------------------------
# 3. Treinando o modelo (usando variáveis corretas)
# ----------------------------------------

# Lista de variáveis preditoras (removendo colunas de identificação e criadas artificialmente)
variaveis_modelo <- setdiff(names(bd_final), c("data_base", "coenti", "noenti", "PL", "CMR", "cmr_pl", "default"))

# Criação da fórmula
formula_modelo <- as.formula(paste("default ~", paste(variaveis_modelo, collapse = " + ")))

# Modelo de árvore de decisão
modelo_arvore <- rpart(formula_modelo, data = base_treino, method = "class")

# -------------------------------
# 4. Previsão de probabilidades
# -------------------------------

# Previsão com probabilidades
probs <- predict(modelo_arvore, base_teste, type = "prob")

# Visualiza as 6 primeiras linhas
head(probs)

# Adiciona a coluna de probabilidade de default (classe 1) à base de teste
base_teste$prob_default <- probs[, "1"]

# Visualiza a distribuição
summary(base_teste$prob_default)
hist(base_teste$prob_default, breaks = 20, main = "Distribuição das probabilidades de default", col = "lightblue")


base_tree <- base_teste %>%
  mutate(peso = cessao_Resseguro + recuperacao_Resseguro + 
           Provisoes_Tecnicas_PASSCIRC + Provisoes_Tecnicas_PASSNCIRC)

pd_tree_final <- base_tree %>%
  group_by(coenti, noenti) %>%
  summarise(pd_final_tree = weighted.mean(prob_default, peso, na.rm = TRUE)) %>%
  ungroup()


saveRDS(pd_tree_final, file = "pd_tree_final.rds")
saveRDS(base_tree, file = "base_tree.rds")





