library(ggplot2)
library(dplyr)
library(reshape2)
library(d3heatmap)
source("tse_file_loader.R")

plot_candidato_ocupacao <- function(df, year) {
  candidatos_por_ocupacao <- df %>% 
    group_by(DESCRICAO_OCUPACAO) %>% 
    filter(!DESC_SIT_TOT_TURNO == "#NULO#" && !DESCRICAO_OCUPACAO == "OUTROS") %>%
    summarise(total = sum(n())) %>%
    arrange(total) %>%
    filter(!is.na(total)) 
  
  candidatos_por_ocupacao_maxs <- tail(candidatos_por_ocupacao, n = 10)
  
  png(file = paste0("plots/candidatos_ocupacao_",year,".png"),
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
}
plot_candidato_ocupacao(candidatos_2008_df, "2008")
plot_candidato_ocupacao(candidatos_2010_df, "2010")
plot_candidato_ocupacao(candidatos_2012_df, "2012")
plot_candidato_ocupacao(candidatos_2014_df, "2014")
