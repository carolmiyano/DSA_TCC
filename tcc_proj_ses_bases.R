install.packages("dplyr")
install.packages("lubridate")
install.packages("clipr") 
install.packages("tidyr")
install.packages("stringr")

library(clipr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

#function
`%notin%` <- Negate(`%in%`)

#ajustando ses_cia
Ses_cias$Noenti <- iconv(Ses_cias$Noenti, from = "latin1", to = "UTF-8")
Ses_cias <- Ses_cias %>% rename(coenti = Coenti)

#ajustando variaveis da tabela ses_cessoes_recebidas
Ses_Cessoes_Recebidas <- Ses_Cessoes_Recebidas %>% mutate(
  coenti = as.character(coenti),
  data_base = as.Date(ymd(paste0(damesano, "01"))),
  grupo = as.character(gracodigo),
  tipo_cessao = as.character(tipo_cessao),
  cessao = ifelse(is.na(cessao), 0,as.double(cessao)),
  recuperacao = ifelse(is.na(recuperacao), 0,as.double(recuperacao)),
  comissao = ifelse(is.na(comissao), 0,as.double(comissao)),
  corretagem = ifelse(is.na(corretagem), 0,as.double(corretagem))
)%>% filter(data_base >= "2017-01-01") %>% select(data_base,
  coenti,
  grupo,
  tipo_cessao,
  cessao,
  recuperacao,
  comissao,
  corretagem)

#adicionando nome da cia em tabela de cessão
Ses_Cessoes_Recebidas <- left_join(Ses_Cessoes_Recebidas, Ses_cias, by = "coenti")
bd_cess_rec_grupo <- Ses_Cessoes_Recebidas %>% rename(noenti = Noenti) %>% select(data_base,
    coenti,
    noenti,
    grupo,
    tipo_cessao,
    cessao,
    recuperacao)

#tirando o grupo
bd_cess_rec <- bd_cess_rec_grupo %>%
  group_by(data_base, coenti, noenti, tipo_cessao) %>%
  summarise(
    across(
      .cols = c(cessao, recuperacao),
      .fns = ~sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>% filter(
  coenti %notin% c(
                "99999", #veio sem nome, parece um erro 
                "09998",  #veio sem nome, parece um erro 
                "06254",  #veio sem nome, parece um erro 
                "09999",  #veio sem nome, parece um erro
                "00000",  #veio sem nome, parece um erro
                "34891", #veio sem nome, parece um erro 
                "04383")  #esta companhia esta com dados duplicados na tabela ses_valoresmovgrupos 
)
#pivotando a base
cess_rec <-  bd_cess_rec %>%
  pivot_wider(
    id_cols = c(data_base, coenti, noenti),
    names_from = tipo_cessao,
    values_from = c(cessao, recuperacao),
    values_fill = 0  )

#adicionando o nome das cias na pl_margem, ajustando os campos e filtrando periodo
pl_margem <- Ses_pl_margem
pl_margem <- left_join(pl_margem, Ses_cias, by = "coenti")
pl_margem <- pl_margem %>% rename(noenti = Noenti) %>% 
  mutate (data_base = as.Date(ymd(paste0(damesano, "01"))), 
          PL = ifelse(is.na(pl), 0,as.double(pl)),
          PLA = ifelse(is.na(plajustado), 0,as.double(plajustado)),
          Novo_PLA = ifelse(is.na(NovoPla), 0,as.double(NovoPla)),
          CMR = ifelse(is.na(CMR), 0,as.double(CMR))) %>% select(data_base, coenti, noenti, CMR, PL) %>% filter(data_base >= "2017-01-01")

#criando bd para juntar pl_margem e dados de cessão
bd_join1 <- full_join(cess_rec, pl_margem, by =c("coenti", "noenti", "data_base"))

#Ajustando variaveis de SES_BALANCO e filtrando periodo
bd_balanco <- SES_Balanco %>%
  mutate(coenti = as.character(coenti),
    data_base = as.Date(ymd(paste0(damesano, "01"))),
    cmpid = as.character(cmpid),
    valor = as.double(valor),
    quadro = as.character(quadro))%>% filter(damesano >= "2017-01-01")

#selecionando colunas e filtra cmpids e exclui cias com problema
bd_balanco <- bd_balanco %>% select(data_base,
  coenti,
  quadro,
  cmpid,
  valor) %>% filter( 
  coenti %notin% c("09998","06254","09999","00000","34891","04383"),
  cmpid %in% c("1479","1040","6569","6100","5917","6449","6125","3162","3165","11142","6155","1491","362","3170","351","11160","6152","1496","5497","11137","13521","358","11138","323","1503","5920","1502","3183","3175","11169","3179","1036","3192","3185","5503","11187","6165","1038","5500","11164","345","13522","11165")) 

bd_balanco <- left_join(bd_balanco, Ses_cias, by = "coenti")

#traz nome das companhias e traz codigos de tabelas que possuem apenas seguradoras
seguradoras <- distinct(SES_ValoresMovRamos, coenti)%>% mutate(tp_cia = "Seguradora")  
Ses_seguros <- distinct(Ses_seguros, coenti)%>% mutate(tp_cia = "Seguradora")
seguradoras <-union(seguradoras, Ses_seguros)
#traz apenas codigos de tabelas que possuem dados de resseguradoras
resseguradoras <- distinct(ses_valoresresmovgrupos, COENTI) %>% rename(coenti = COENTI) %>% mutate(coenti = as.character(coenti), tp_cia = "Resseguradora")
#traz apenas codigos de tabelas que possuem dados de capitalização
capital_prev <- distinct(Ses_Dados_Cap, coenti) %>% mutate(
  coenti = as.character(coenti),
  tp_cia = "capital e previdencia")
Ses_Contrib_Benef <- distinct(Ses_Contrib_Benef, coenti) %>% mutate(coenti = as.character(coenti), tp_cia = "capital e previdencia")
capital_prev <- union(capital_prev, Ses_Contrib_Benef)
#junta todos os tipos de companhia
cias_tipo <- bind_rows(seguradoras, resseguradoras, capital_prev) %>% distinct() %>%filter(!(coenti == 38873) | tp_cia == "Resseguradora")
#corrigindo cias que tem mais de 1 tp_cia ex: sebemi seguradora (1007) que é seg e capital e prev. 
cias_tipo <- cias_tipo %>%
  mutate(tp_cia = factor(tp_cia, levels = c("Seguradora", "Resseguradora", "capital e previdencia"))) %>%
  group_by(coenti) %>%
  slice_min(tp_cia) %>%  # Mantém a menor categoria de acordo com a ordem definida
  ungroup()

#traz coluna de tipo de companhia para filtrar as resseguradoras
bd_balanco <- left_join(bd_balanco, cias_tipo, by = "coenti")
bd_balanco <- bd_balanco %>% select(
  data_base,
  coenti,
  noenti = Noenti,
  tp_cia,
  quadro,
  cmpid,
  valor)
balanco_ress <- bd_balanco %>% filter(tp_cia == "Resseguradora")

#traz nomes ajustados
balanco_ress <- left_join(balanco_ress, cmpids_pivot_nomes, by = "cmpid")

#select de colunas antes do pivot
balanco_ress <- balanco_ress %>% select(data_base, coenti, noenti, nome_pivot, valor)

#pivotando a base
balanco <-  balanco_ress %>%
  pivot_wider(
    id_cols = c(data_base, coenti, noenti),
    names_from = nome_pivot,
    values_from = c(valor))

#inclui a coluna de tp_cia e exclui o que não é ressegulador local
bd_join1_t <- left_join(bd_join1,cias_tipo, by = "coenti")
bd_join1_t <- bd_join1_t %>%
  rowwise() %>%
  filter(sum(c_across(-c(data_base, coenti, noenti, tp_cia, cessao_Retrocessao, cessao_Resseguro, recuperacao_Retrocessao, recuperacao_Resseguro)), na.rm = TRUE) != 0) %>%
  ungroup()
bd_join1_t <- bd_join1_t %>% filter(tp_cia == "Resseguradora") %>% select (-tp_cia)

#criando base final 
bd_final <- full_join(bd_join1_t, balanco, by =c("coenti", "noenti", "data_base"))
saveRDS(bd_final, file = "bd_final.rds")

