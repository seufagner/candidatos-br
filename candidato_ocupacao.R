library(ggplot2)
library(dplyr)
library(tidyr)
source("tse_file_loader.R")

plot_candidato_ocupacao <- function(df, year) {
  candidatos_por_ocupacao <- df %>% 
    group_by(DESCRICAO_OCUPACAO) %>% 
    filter(!DESC_SIT_TOT_TURNO == "#NULO#" && 
             !DESC_SIT_TOT_TURNO == "#NE#" &&
             !DESCRICAO_OCUPACAO == "OUTROS") %>%
    summarise(total = sum(n())) %>%
    arrange(total) %>%
    filter(!is.na(total)) 
  
  candidatos_por_ocupacao_maxs <- tail(candidatos_por_ocupacao, n = 10)
  
  png(file = paste0("candidatos_ocupacao_",year,".png"),
      width = 800, height = 800, units = "px")
  title <- paste0("Top 10 Ocupação x Candidatos ", year, " (total de ", sum(candidatos_por_ocupacao$total), " candidatos)")
  
  g <- ggplot(candidatos_por_ocupacao_maxs, aes(x=DESCRICAO_OCUPACAO, y=total, fill=total)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position="none") +
    geom_bar(stat = "identity", position = "identity") +
    ggtitle(title)
  
  print(g)
  dev.off()  
  
  candidatos_por_ocupacao_maxs
}
candidatos_por_ocupacao_2008_top <- plot_candidato_ocupacao(candidatos_2008_df, "2008")
candidatos_por_ocupacao_2010_top <- plot_candidato_ocupacao(candidatos_2010_df, "2010")
candidatos_por_ocupacao_2012_top <- plot_candidato_ocupacao(candidatos_2012_df, "2012")
candidatos_por_ocupacao_2014_top <- plot_candidato_ocupacao(candidatos_2014_df, "2014")

# ocupacao x resultado
ocupacao_resultado = function(df) {
  df %>% 
    group_by(DESCRICAO_OCUPACAO, DESC_SIT_TOT_TURNO) %>% 
    filter(!DESC_SIT_TOT_TURNO == "#NULO#" && 
             !DESC_SIT_TOT_TURNO == "#NE#" &&
             !DESC_SIT_TOT_TURNO == "SUPLENTE" &&
             !DESCRICAO_OCUPACAO == "OUTROS" && !is.na(COD_SIT_TOT_TURNO)) %>%
    summarise(total = sum(n())) %>%
    arrange(DESCRICAO_OCUPACAO) %>%
    filter(!is.na(total))   
}

ocupacao_resultado_2012_top <- ocupacao_resultado(candidatos_2012_df  %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2012_top$DESCRICAO_OCUPACAO))
ocupacao_resultado_2010_top <- ocupacao_resultado(candidatos_2010_df  %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2010_top$DESCRICAO_OCUPACAO))
ocupacao_resultado_2014_top <- ocupacao_resultado(candidatos_2014_df %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2014_top$DESCRICAO_OCUPACAO))
ocupacao_resultado_2008_top <- ocupacao_resultado(candidatos_2008_df %>% 
                                                    filter(nchar(DESC_SIT_TOT_TURNO) < 100) %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2008_top$DESCRICAO_OCUPACAO))

ocupacao_resultado_2008_top[which(nchar(DESC_SIT_TOT_TURNO) > 100),]$DESC_SIT_TOT_TURNO

merge_municipais <- merge(ocupacao_resultado_2008_top, ocupacao_resultado_2012_top, by = c("DESCRICAO_OCUPACAO", "DESC_SIT_TOT_TURNO"))
  
merge_federais <- merge(ocupacao_resultado_2010_top, ocupacao_resultado_2014_top, by = c("DESCRICAO_OCUPACAO", "DESC_SIT_TOT_TURNO"))

df <- merge_federais %>% gather(classe_ano, total, total.x:total.y)

ggplot(ocupacao_resultado_2014_top, aes(x=DESCRICAO_OCUPACAO, y=total, fill=DESC_SIT_TOT_TURNO)) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete("Área atuação") +
  scale_y_continuous("No. Candidatos") + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Relação candidatos por área de atuação x resultado eleições 2014")

# cruzar numero deputados x eleitos por qp ou por media

