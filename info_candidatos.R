source("tse_file_loader.R")

bens_cand_path <- "raw_data/bem_candidato_2014/"
cand_path <- "raw_data/consulta_cand_2014/"
leg_path <- "raw_data/consulta_legendas_2014/"

bens_df <- load_files_on(bens_cand_path, with_col_names = cols_bem_candidato)
candidatos_df <- load_files_on(cand_path, with_col_names = cols_consulta_candidato_2012_2014)
legendas_df <- load_files_on(leg_path, with_col_names = cols_legenda)

names(bens_df)
