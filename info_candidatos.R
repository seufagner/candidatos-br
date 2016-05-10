library(dplyr)
library(plyr)
source("tse_file_loader.R")

bens_cand_2012_path <- "raw_data/bem_candidato_2012/"
cand_2012_path <- "raw_data/consulta_cand_2012/"
leg_2012_path <- "raw_data/consulta_legendas_2012/"
bens_cand_2014_path <- "raw_data/bem_candidato_2014/"
cand_2014_path <- "raw_data/consulta_cand_2014/"
leg_2014_path <- "raw_data/consulta_legendas_2014/"

bens_2012_df <- load_files_on(bens_cand_2012_path, with_col_names = cols_bem_candidato)
candidatos_2012_df <- load_files_on(cand_2012_path, with_col_names = cols_consulta_candidato_2012)
legendas_2012_df <- load_files_on(leg_2012_path, with_col_names = cols_legenda)
bens_2014_df <- load_files_on(bens_cand_2014_path, with_col_names = cols_bem_candidato)
candidatos_2014_df <- load_files_on(cand_2014_path, with_col_names = cols_consulta_candidato_2014)
legendas_2014_df <- load_files_on(leg_2014_path, with_col_names = cols_legenda)

candidatos_por_raca <- aggregate(candidatos_df$CODIGO_COR_RACA, by=list(candidatos_df$DESCRICAO_COR_RACA), FUN=sum)
pie(candidatos_por_raca$x, labels = candidatos_por_raca$Group.1, main="Candidatos por raça 2014")

x <- candidatos_df %>%
  group_by(SIGLA_PARTIDO) %>%
  summarise(total = sum(DESPESA_MAX_CAMPANHA),freq = n())
pie(x$total, labels = x$SIGLA_PARTIDO, main="Gasto campanha 2014")

candidato_partido <- candidatos_df %>%
  select(SEQUENCIAL_CANDIDATO, NOME_CANDIDATO, NUMERO_PARTIDO, SIGLA_PARTIDO, ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_CARGO, DESCRICAO_CARGO)
  
candidato_bens_total <- bens_df %>% 
  group_by(SQ_CANDIDATO) %>%
  summarise(total = sum(VALOR_BEM),freq = n())

candidato_bens <- merge(candidato_partido, candidato_bens_total, by.x = "SEQUENCIAL_CANDIDATO", by.y = "SQ_CANDIDATO")

partido_bens <- candidato_bens %>%
  group_by(SIGLA_PARTIDO) %>%
  summarise(TOTAL_PARTIDO = sum(total),freq = n())
pie(partido_bens$TOTAL_PARTIDO, partido_bens$SIGLA_PARTIDO, mmain = "Total bens por partido 2014")

###
x <- candidato_bens %>%
  group_by(SIGLA_UF) %>%
  filter(SIGLA_PARTIDO == "PSOL") %>%
  summarise(TOTAL_UF = sum(total), freq = n())
pie(x$TOTAL_UF, x$SIGLA_UF, main = "Total bens PSOL por Estado 2014")

candidato_bens %>% 
  select(SIGLA_UF, NOME_CANDIDATO, DESCRICAO_CARGO, SIGLA_PARTIDO, total) %>%
  filter(total == 2135000000)

###

pmdb <- candidato_bens %>%
  group_by(SIGLA_UF) %>%
  filter(SIGLA_PARTIDO == "PMDB") %>%
  summarise(TOTAL_UF = sum(total), freq = n())%>%
  arrange(TOTAL_UF)
pie(pmdb$TOTAL_UF, pmdb$SIGLA_UF, main = "Total bens PMDB por estado 2014")

###

psdb <- candidato_bens %>%
  group_by(SIGLA_UF) %>%
  filter(SIGLA_PARTIDO == "PSDB") %>%
  summarise(TOTAL_UF = sum(total), freq = n())%>%
  arrange(TOTAL_UF)
pie(pmdb$TOTAL_UF, pmdb$SIGLA_UF, main = "Total bens PSDB por estado 2014")
