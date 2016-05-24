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
plot_resultado_ocupacao = function(df, year) {
  df <- df %>% 
    group_by(DESCRICAO_OCUPACAO, DESC_SIT_TOT_TURNO) %>% 
    filter(!DESC_SIT_TOT_TURNO == "#NULO#" && 
             !DESC_SIT_TOT_TURNO == "#NE#" &&
             !DESC_SIT_TOT_TURNO == "SUPLENTE" &&
             !DESCRICAO_OCUPACAO == "OUTROS" && !is.na(COD_SIT_TOT_TURNO)) %>%
    summarise(total = sum(n())) %>%
    arrange(DESCRICAO_OCUPACAO) %>%
    filter(!is.na(total))   
  
  png(file = paste0("resultado_ocupacao_",year,".png"),
      width = 800, height = 800, units = "px")
  
  g <- ggplot(df, aes(x=DESCRICAO_OCUPACAO, y=total, fill=DESC_SIT_TOT_TURNO)) + 
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete("Área atuação") +
    scale_y_continuous("No. Candidatos") + 
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(paste0("Relação candidatos por área de atuação x resultado eleições ", year))
  
  print(g)
  dev.off()  
  
  df
}

ocupacao_resultado_2012_top <- plot_resultado_ocupacao(candidatos_2012_df  %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2012_top$DESCRICAO_OCUPACAO), "2012")
ocupacao_resultado_2010_top <- plot_resultado_ocupacao(candidatos_2010_df  %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2010_top$DESCRICAO_OCUPACAO), "2010")
ocupacao_resultado_2014_top <- plot_resultado_ocupacao(candidatos_2014_df %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2014_top$DESCRICAO_OCUPACAO), "2014")

# o arquivo do ano de 2008 está corrompido, por isso as clausulas filter
ocupacao_resultado_2008_top <- plot_resultado_ocupacao(candidatos_2008_df %>% 
                                                    filter(nchar(DESC_SIT_TOT_TURNO) < 100) %>% 
                                                    filter(DESCRICAO_OCUPACAO %in% candidatos_por_ocupacao_2008_top$DESCRICAO_OCUPACAO), "2008")

# cruzar numero deputados x eleitos por qp ou por media
# a partir de 2012 os levels para DESC_SIT_TOT_TURNO foram mais resumidos e nao vao 
consolidar_resultado_formato_anterior_2012 = function(df) {
  
  
  
  
}

levels(as.factor(ocupacao_resultado_2012_top$DESC_SIT_TOT_TURNO))
