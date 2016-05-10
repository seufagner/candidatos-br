library(ggplot2)
library(dplyr)
library(reshape2)
source("tse_file_loader.R")

#### load datasets
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

#### candidatos por raça 2014
candidatos_por_raca_2014 <- aggregate(candidatos_2014_df$CODIGO_COR_RACA, by=list(candidatos_2014_df$DESCRICAO_COR_RACA), FUN=sum)
pie(candidatos_por_raca_2014$x, labels = candidatos_por_raca_2014$Group.1, main="Candidatos por raça 2014")

rm(candidatos_por_raca_2014)

#### candidato max despesa campanha 2012x2014
calc_despesa = function(candidatos_df) {
  candidatos_df %>%
    group_by(SIGLA_PARTIDO) %>%
    summarise(total = sum(DESPESA_MAX_CAMPANHA)/n(), freq = n())
}

despesa_2014 <- calc_despesa(candidatos_2014_df)
despesa_2012 <- calc_despesa(candidatos_2012_df)

siglas <- intersect(despesa_2014$SIGLA_PARTIDO, despesa_2012$SIGLA_PARTIDO)

overlap_bars_df <- data.frame(x=siglas, ano_2014=despesa_2014[SIGLA_PARTIDO %in% siglas]$total, ano_2012=despesa_2012[SIGLA_PARTIDO %in% siglas]$total)
melted <- melt(overlap_bars_df, id="x")
ggplot(melted, aes(x=x, y=value, fill=variable)) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete("Sigla") +
  scale_y_continuous("Gasto max. R$ (pelo número candidatos") + 
  geom_bar(stat = "identity", position = "identity") +
  ggtitle("Gasto max campanha por sigla")

rm(despesa_2012)
rm(despesa_2014)
rm(siglas)
rm(overlap_bars_df)
rm(melted)

#### prevalencia candidatos eleicao municipal x federal
candidatos_partido = function(candidatos_df) {
  candidatos_df %>%
    group_by(SIGLA_PARTIDO) %>%
    summarise(total = n())
}
cand_partido_2012 <- candidatos_partido(candidatos_2012_df)
cand_partido_2014 <- candidatos_partido(candidatos_2014_df)

siglas <- intersect(cand_partido_2014$SIGLA_PARTIDO, cand_partido_2012$SIGLA_PARTIDO)
overlap_bars_df <- data.frame(x=siglas, municipais_2012=cand_partido_2012[SIGLA_PARTIDO %in% siglas]$total, federais_2014=cand_partido_2014[SIGLA_PARTIDO %in% siglas]$total)
melted <- melt(overlap_bars_df)

ggplot(melted,aes(x=x, y=value, fill=variable)) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous("Candidatos") + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Candidatos por eleição")

rm(cand_partido_2012)
rm(cand_partido_2014)
rm(siglas)
rm(overlap_bars_df)
rm(melted)

#### partido x bens declarados
partido_bens = function(candidatos_df, bens_df) {
  candidato_partido <- candidatos_df %>%
    select(SEQUENCIAL_CANDIDATO, NOME_CANDIDATO, NUMERO_PARTIDO, SIGLA_PARTIDO, ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_CARGO, DESCRICAO_CARGO)
  candidato_bens_total <- bens_df %>% 
    group_by(SQ_CANDIDATO) %>%
    summarise(total = sum(VALOR_BEM),freq = n())
  candidato_bens <- merge(candidato_partido, candidato_bens_total, by.x = "SEQUENCIAL_CANDIDATO", by.y = "SQ_CANDIDATO")
  
  candidato_bens %>%
    group_by(SIGLA_PARTIDO) %>%
    summarise(TOTAL_PARTIDO = sum(total),freq = n())  
}

partido_bens_2012 <- partido_bens(candidatos_2012_df, bens_2012_df)
partido_bens_2014 <- partido_bens(candidatos_2014_df, bens_2014_df)

pie(partido_bens_2012$TOTAL_PARTIDO, partido_bens_2012$SIGLA_PARTIDO, mmain = "Total bens por partido 2012")
pie(partido_bens_2014$TOTAL_PARTIDO, partido_bens_2014$SIGLA_PARTIDO, mmain = "Total bens por partido 2014")

ggplot(partido_bens_2012, aes(x=SIGLA_PARTIDO, y=TOTAL_PARTIDO)) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat="identity", fill="#FF9999") +
  ggtitle("Bens por partido 2012")

rm(partido_bens_2012)
rm(partido_bens_2014)
###