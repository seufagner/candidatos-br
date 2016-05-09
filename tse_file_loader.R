require(data.table)

#ignore scientific notation
options(scipen=999)

####################
# load all files on specific directory
####################
load_files_on <- function(path, with_col_names) {
  files <- dir(path, pattern = '*.txt')
  tables <- lapply(paste(path, files, sep = "/"), 
                   read.csv, sep=";", fileEncoding="latin1", header = F)
  df <- rbindlist(tables)
  colnames(df) <- with_col_names
  return(df)
}

####################
# file layouts
####################
cols_bem_candidato <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", 
                        "DESCRICAO_ELEICAO", "SIGLA_UF","SQ_CANDIDATO",
                        "CD_TIPO_BEM_CANDIDATO","DS_TIPO_BEM_CANDIDATO",
                        "DETALHE_BEM","VALOR_BEM","DATA_ULTIMA_ATUALIZACAO",
                        "HORA_ULTIMA_ATUALIZACAO")

cols_consulta_candidato_2014 <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO",
                                  "NUM_TURNO", "DESCRICAO_ELEICAO","SIGLA_UF",
                                  "SIGLA_UE","DESCRICAO_UE","CODIGO_CARGO",
                                  "DESCRICAO_CARGO", "NOME_CANDIDATO", 
                                  "SEQUENCIAL_CANDIDATO","NUMERO_CANDIDATO", 
                                  "CPF_CANDIDATO","NOME_URNA_CANDIDATO", 
                                  "COD_SITUACAO_CANDIDATURA", "DES_SITUACAO_CANDIDATURA",
                                  "NUMERO_PARTIDO", "SIGLA_PARTIDO","NOME_PARTIDO",
                                  "CODIGO_LEGENDA","SIGLA_LEGENDA","COMPOSICAO_LEGENDA",
                                  "NOME_LEGENDA","CODIGO_OCUPACAO","DESCRICAO_OCUPACAO",
                                  "DATA_NASCIMENTO","NUM_TITULO_ELEITORAL_CANDIDATO",
                                  "IDADE_DATA_ELEICAO","CODIGO_SEXO","DESCRICAO_SEXO",
                                  "COD_GRAU_INSTRUCAO","DESCRICAO_GRAU_INSTRUCAO",
                                  "CODIGO_ESTADO_CIVIL","DESCRICAO_ESTADO_CIVIL",
                                  "CODIGO_COR_RACA","DESCRICAO_COR_RACA",
                                  "CODIGO_NACIONALIDADE","DESCRICAO_NACIONALIDADE",
                                  "SIGLA_UF_NASCIMENTO","CODIGO_MUNICIPIO_NASCIMENTO",
                                  "NOME_MUNICIPIO_NASCIMENTO","DESPESA_MAX_CAMPANHA",
                                  "COD_SIT_TOT_TURNO","DESC_SIT_TOT_TURNO", "NM_EMAIL")

cols_legenda <- c("DATA_GERACAO","HORA_GERACAO","ANO_ELEICAO","NUM_TURNO","DESCRICAO_ELEICAO",
                  "SIGLA_UF","SIGLA_UE","NOME_UE","CODIGO_CARGO","DESCRICAO_CARGO",
                  "TIPO_LEGENDA","NUM_PARTIDO","SIGLA_PARTIDO","NOME_PARTIDO","SIGLA_COLIGACAO",
                  "NOME_COLIGACAO","COMPOSICAO_COLIGACAO","SEQUENCIAL_COLIGACAO")