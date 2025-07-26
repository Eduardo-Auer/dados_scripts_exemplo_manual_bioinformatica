#Instalar pacotes####
install.packages("tidyverse", dependencies = TRUE)
install.packages("genio", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)

#Carregar pacotes necessários####
library(tidyverse)
library(genio)           # Usado para manipular arquivos de dados genéticos, como os gerados pelo PLINK.
library(data.table)   

#selecionar diretório de trabalho

#carregar arquivo do plink (arquivos .bim .fam e .bed devem estar no mesmo diretório)
plink_data <- read_plink("plink_example_chr_21")

#criar matriz de genótipos e trocar a codificação dos alélicos do formato numérico para o formato alfabético
genotypes <- geno_to_char(plink_data$X, plink_data$bim)    # Converte a matriz de genótipos do formato numérico (PLINK) para caracteres (A/C/G/T).
gc()    # Libera a memória (garbage collector) para otimizar o uso de recursos.

#converter matriz para data.frame
#Além disso Fazer com que as variantes e seus genótipos sejam colunas e indivíduos por linhas

genotypes_df <- as.data.frame(t(genotypes)) # Transpõe a matriz (linhas viram colunas e vice-versa) e converte para data.frame.
gc() 

#remover objetos e data.frames não necessários
rm(plink_data, genotypes)  # Remove os objetos que não são mais necessários para liberar memória.
gc()  

#Transformar os nomes das linhas (ID dos indivpoduos) para a coluna "IID"
genotypes_df <- genotypes_df %>% rownames_to_column(var = "IID")   # Move os IDs dos indivíduos que estão como "nomes das linhas" para uma coluna chamada "IID".
gc()

genotypes_df[1:10, 1:10] # Exibe uma prévia das primeiras 10 linhas e 10 colunas do data.frame para verificar a estrutura dos dados.

#Salvar Genótipos no formato tabular
write.table(genotypes_df, "genotypes_tabular_format.txt", row.names = FALSE, col.names = TRUE, quote = FALSE)  # Salva os dados no formato tabular (txt), sem aspas, com nomes de colunas e sem incluir os nomes das linhas.
