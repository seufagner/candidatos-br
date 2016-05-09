require(data.table)

load_files <- function(path) {
  files <- dir(path, pattern = '*.txt')
  tables <- lapply(paste(path, files, sep = "/"), 
                   read.csv, sep=";", fileEncoding="latin1", header = F)
  return(rbindlist(tables))
}

bens_cand_path <- "raw_data/bem_candidato_2014/"
bens_df <- load_files(bens_cand_path)

cand_path <- "raw_data/consulta_cand_2014/"
candidatos_df <- load_files(cand_path)

leg_path <- "raw_data/consulta_legendas_2014/"
legendas_df <- load_files(cand_path)

