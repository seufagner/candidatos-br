library(dplyr)
library(plyr)
source("tse_file_loader.R")

bens_cand_path <- "raw_data/bem_candidato_2014/"
cand_path <- "raw_data/consulta_cand_2014/"
leg_path <- "raw_data/consulta_legendas_2014/"

bens_df <- load_files_on(bens_cand_path, with_col_names = cols_bem_candidato)
candidatos_df <- load_files_on(cand_path, with_col_names = cols_consulta_candidato_2012_2014)
legendas_df <- load_files_on(leg_path, with_col_names = cols_legenda)

candidatos_por_raca <- aggregate(candidatos_df$CODIGO_COR_RACA, by=list(candidatos_df$DESCRICAO_COR_RACA), FUN=sum)
pie(candidatos_por_raca$x, labels = candidatos_por_raca$Group.1, main="Candidatos por raça 2014")

simple_candidatos_df <- subset(candidatos_df, select = c("SEQUENCIAL_CANDIDATO", 
                                                         "NOME_CANDIDATO",
                                                         "NOME_PARTIDO", 
                                                         "SIGLA_PARTIDO",
                                                         "NUMERO_PARTIDO", 
                                                         "CPF_CANDIDATO",
                                                         "CODIGO_SEXO",
                                                         "DESCRICAO_GRAU_INSTRUCAO",
                                                         "COD_GRAU_INSTRUCAO",
                                                         "CODIGO_CARGO",
                                                         "DESCRICAO_CARGO",
                                                         "DESPESA_MAX_CAMPANHA"))


x <- simple_candidatos_df %>%
  group_by(SIGLA_PARTIDO) %>%
  summarise(total = sum(DESPESA_MAX_CAMPANHA),freq = n())
pie(x$total, labels = x$SIGLA_PARTIDO, main="Gasto campanha 2014")

