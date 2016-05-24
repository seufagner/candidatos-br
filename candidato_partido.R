library(ggplot2)
library(dplyr)
library(reshape2)
source("tse_file_loader.R")

# agrupamento faceted dos 6 maiores partidos por grupo economico x eleicao ? (bar dodged)


eleitos_2010 <- candidatos_2010_df %>%
  group_by(SIGLA_PARTIDO) %>%
  filter(DESC_SIT_TOT_TURNO %in% c("MÉDIA", "ELETO"))
View(eleitos_2010)

fe_2010 <- eleitos_2010 %>%
  filter(grepl("FÉ", NOME_LEGENDA) || grepl("AMOR", NOME_LEGENDA) || grepl("POVO", NOME_LEGENDA))
View(fe_2010)
