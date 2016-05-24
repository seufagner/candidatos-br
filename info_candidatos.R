library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)
source("tse_file_loader.R")

#### candidatos por raça 2014 (unico ano com raça)
candidatos_por_raca_2014 <- aggregate(candidatos_2014_df$CODIGO_COR_RACA, by=list(candidatos_2014_df$DESCRICAO_COR_RACA), FUN=sum)
percents <- paste0(round(100*candidatos_por_raca_2014$x/sum(candidatos_por_raca_2014$x), 1), "%", " (",candidatos_por_raca_2014$Group.1,")" )

png(file = "candidatos_raca_2014.png")
l <- length(candidatos_por_raca_2014$x)

ggplot(candidatos_por_raca_2014, aes(x=Group.1, y=x, fill=Group.1)) + 
  xlab("") + 
  ylab("") + 
  geom_bar(stat = "identity") +
  ggtitle("Candidatos por raça 2014")

dev.off()
rm(candidatos_por_raca_2014)

#### candidato max despesa campanha 
calc_despesa = function(candidatos_df) {
  candidatos_df %>%
    group_by(SIGLA_PARTIDO) %>%
    summarise(total = sum(as.numeric(DESPESA_MAX_CAMPANHA))/n(), freq = n())
}

#nao disponivel
#despesa_2008 <- calc_despesa(candidatos_2008_df)
#nao disponivel
#despesa_2010 <- calc_despesa(candidatos_2010_df)
despesa_2012 <- calc_despesa(candidatos_2012_df)
despesa_2014 <- calc_despesa(candidatos_2014_df)


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
cand_partido_2008 <- candidatos_partido(candidatos_2008_df)
cand_partido_2010 <- candidatos_partido(candidatos_2010_df)
cand_partido_2012 <- candidatos_partido(candidatos_2012_df)
cand_partido_2014 <- candidatos_partido(candidatos_2014_df)

siglas <- intersect(cand_partido_2014$SIGLA_PARTIDO, cand_partido_2012$SIGLA_PARTIDO)
siglas <- intersect(siglas, candidatos_2008_df$SIGLA_PARTIDO)
siglas <- intersect(siglas, candidatos_2010_df$SIGLA_PARTIDO)
overlap_bars_df <- data.frame(x=siglas, municipais_2008=cand_partido_2008[SIGLA_PARTIDO %in% siglas]$total, municipais_2012=cand_partido_2012[SIGLA_PARTIDO %in% siglas]$total, federais_2014=cand_partido_2014[SIGLA_PARTIDO %in% siglas]$total, federais_2010=cand_partido_2010[SIGLA_PARTIDO %in% siglas]$total)
melted <- melt(overlap_bars_df)

ggplot(melted,aes(x=x, y=value, fill=variable)) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous("Candidatos") + 
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Candidatos por eleição")

rm(cand_partido_2008)
rm(cand_partido_2010)
rm(cand_partido_2012)
rm(cand_partido_2014)
rm(siglas)
rm(overlap_bars_df)
rm(melted)

#### partido x bens declarados

candidato_bens = function(candidatos_df, bens_df) {
  candidato_partido <- candidatos_df %>%
    select(SEQUENCIAL_CANDIDATO, NOME_CANDIDATO, ANO_ELEICAO, NUMERO_PARTIDO, SIGLA_PARTIDO, ANO_ELEICAO, SIGLA_UF, SIGLA_UE, CODIGO_CARGO, DESCRICAO_CARGO)
  
  candidato_bens_total <- bens_df %>% 
    group_by(SQ_CANDIDATO) %>%
    summarise(total = sum(VALOR_BEM), freq = n())
  
  merge(x = candidato_partido, y = candidato_bens_total, by.x = "SEQUENCIAL_CANDIDATO", by.y = "SQ_CANDIDATO")
}

partido_bens = function(candidatos_df, bens_df) {
  candidato_bens <- candidato_bens(candidatos_df, bens_df)
  
  candidato_bens %>%
    group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>%
    summarise(TOTAL_PARTIDO = sum(total), TOTAL_MEDIAN_PARTIDO = median(total), freq = n())  
}

partido_bens_2008 <- partido_bens(candidatos_2008_df, bens_2008_df)
partido_bens_2010 <- partido_bens(candidatos_2010_df, bens_2010_df)
partido_bens_2012 <- partido_bens(candidatos_2012_df, bens_2012_df)
partido_bens_2014 <- partido_bens(candidatos_2014_df, bens_2014_df)

pie(partido_bens_2008$TOTAL_PARTIDO, partido_bens_2008$SIGLA_PARTIDO, main = "Total bens por partido 2008")
pie(partido_bens_2010$TOTAL_PARTIDO, partido_bens_2010$SIGLA_PARTIDO, main = "Total bens por partido 2010")
pie(partido_bens_2012$TOTAL_PARTIDO, partido_bens_2012$SIGLA_PARTIDO, main = "Total bens por partido 2012")
pie(partido_bens_2014$TOTAL_PARTIDO, partido_bens_2014$SIGLA_PARTIDO, main = "Total bens por partido 2014")

pb_2008 <- partido_bens_2008 %>%
  group_by(SIGLA_PARTIDO) %>%
  select(ANO_ELEICAO, SIGLA_PARTIDO, TOTAL_PARTIDO, TOTAL_MEDIAN_PARTIDO) %>%
  filter(SIGLA_PARTIDO == "PT" || SIGLA_PARTIDO == "PMDB" || SIGLA_PARTIDO == "DEM" || SIGLA_PARTIDO == "PP" || SIGLA_PARTIDO == "PSDB") %>%
  arrange(SIGLA_PARTIDO)

pb_2010 <- partido_bens_2010 %>%
  group_by(SIGLA_PARTIDO) %>%
  select(ANO_ELEICAO, SIGLA_PARTIDO, TOTAL_PARTIDO, TOTAL_MEDIAN_PARTIDO) %>%
  filter(SIGLA_PARTIDO == "PT" || SIGLA_PARTIDO == "PMDB" || SIGLA_PARTIDO == "DEM" || SIGLA_PARTIDO == "PP" || SIGLA_PARTIDO == "PSDB") %>%
  arrange(SIGLA_PARTIDO)


pb_2012 <- partido_bens_2012 %>%
  group_by(SIGLA_PARTIDO) %>%
  select(ANO_ELEICAO, SIGLA_PARTIDO, TOTAL_PARTIDO, TOTAL_MEDIAN_PARTIDO) %>%
  filter(SIGLA_PARTIDO == "PT" || SIGLA_PARTIDO == "PMDB" || SIGLA_PARTIDO == "DEM" || SIGLA_PARTIDO == "PP" || SIGLA_PARTIDO == "PSDB") %>% 
  arrange(SIGLA_PARTIDO)
  
pb_2014 <- partido_bens_2014 %>%
  group_by(SIGLA_PARTIDO) %>%
  select(ANO_ELEICAO, SIGLA_PARTIDO, TOTAL_PARTIDO, TOTAL_MEDIAN_PARTIDO) %>%
  filter(SIGLA_PARTIDO == "PT" || SIGLA_PARTIDO == "PMDB" || SIGLA_PARTIDO == "DEM" || SIGLA_PARTIDO == "PP" || SIGLA_PARTIDO == "PSDB") %>% 
  arrange(SIGLA_PARTIDO)

rm(partido_bens_2008)
rm(partido_bens_2010)
rm(partido_bens_2012)
rm(partido_bens_2014)

### quem mudou de partido e se elegeu?
eleicoes_municipais <- intersect(candidatos_2008_df$CPF_CANDIDATO, candidatos_2012_df$CPF_CANDIDATO)
eleicoes_federais <- intersect(candidatos_2010_df$CPF_CANDIDATO, candidatos_2014_df$CPF_CANDIDATO)

candidato_partido = function(candidato_df, eleicao_candidato) {
  candidato_df %>%
    select(as.numeric(CPF_CANDIDATO), NOME_CANDIDATO, SIGLA_PARTIDO, DESC_SIT_TOT_TURNO) %>%
    filter(CPF_CANDIDATO %in% eleicao_candidato & DESC_SIT_TOT_TURNO != "#NULO#" & DESC_SIT_TOT_TURNO != "#NE#")
}

# estiveram em ambas as eleicoes e se elegeram ou nao
candidato_partido_2008 <- candidato_partido(candidatos_2008_df, eleicoes_municipais)
candidato_partido_2008$CPF_CANDIDATO <- as.numeric(candidato_partido_2008$CPF_CANDIDATO)
candidato_partido_2012 <- candidato_partido(candidatos_2012_df, eleicoes_municipais)
candidato_partido_2010 <- candidato_partido(candidatos_2010_df, eleicoes_federais)
candidato_partido_2014 <- candidato_partido(candidatos_2014_df, eleicoes_federais)

candidatos_municipais <- merge(candidato_partido_2008, candidato_partido_2012, by.x = "CPF_CANDIDATO", by.y = "CPF_CANDIDATO")
candidatos_federais <- merge(candidato_partido_2010, candidato_partido_2014, by.x = "CPF_CANDIDATO", by.y = "CPF_CANDIDATO")

mudanca_partido_federais <- candidatos_federais %>%
  select(NOME_CANDIDATO.x, SIGLA_PARTIDO.x, SIGLA_PARTIDO.y, DESC_SIT_TOT_TURNO.x, DESC_SIT_TOT_TURNO.y) %>%  
  filter(SIGLA_PARTIDO.x != SIGLA_PARTIDO.y)
mudanca_partido_federais$TRANSICAO_PARTIDOS <- with(mudanca_partido_federais, paste0(DESC_SIT_TOT_TURNO.x, "-", DESC_SIT_TOT_TURNO.y))
mudanca_partido_federais$TRANSICAO_RESULTADOS <- with(mudanca_partido_federais, paste0(DESC_SIT_TOT_TURNO.x, "-", DESC_SIT_TOT_TURNO.y))

x <- mudanca_partido_federais %>%
  group_by(SIGLA_PARTIDO.x, SIGLA_PARTIDO.y,TRANSICAO_PARTIDOS, TRANSICAO_RESULTADOS,DESC_SIT_TOT_TURNO.x, DESC_SIT_TOT_TURNO.y) %>%
  summarise(COUNT = sum(ifelse(DESC_SIT_TOT_TURNO.x != DESC_SIT_TOT_TURNO.y, 1, 0)),freq = n()) %>% 
  select(SIGLA_PARTIDO.x, SIGLA_PARTIDO.y, TRANSICAO_PARTIDOS, TRANSICAO_RESULTADOS, COUNT, DESC_SIT_TOT_TURNO.x, DESC_SIT_TOT_TURNO.y) %>%
  filter(COUNT != 0)

rm(eleicoes_federais)
rm(eleicoes_municipais)
rm(mudanca_partido_federais)
rm(x)
rm(ingresso_pt)
rm(ingresso_pmdb)
rm(candidato_partido_2008)
rm(candidato_partido_2012)
rm(candidato_partido_2010)
rm(candidato_partido_2014)


x <- partido_bens_2014$TOTAL_PARTIDO
h <- hist(x, col = "red")

xfit<-seq(min(x),max(x),length=100) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

partido_bens_2014[which(partido_bens_2014$TOTAL_PARTIDO > 4000000000),]

library(sm)

plot(density(partido_bens_2008$TOTAL_PARTIDO))
plot(density(partido_bens_2010$TOTAL_PARTIDO))
plot(density(partido_bens_2014$TOTAL_PARTIDO))
plot(density(partido_bens_2012$TOTAL_PARTIDO))

range(partido_bens_2012$TOTAL_PARTIDO)
summary(partido_bens_2012$TOTAL_PARTIDO)
var(partido_bens_2012$TOTAL_PARTIDO)
hist(partido_bens_2012$TOTAL_PARTIDO, breaks = 10)

t.test(partido_bens_2012$TOTAL_PARTIDO, partido_bens_2008$TOTAL_PARTIDO, 
       alternative = "two.sided")
