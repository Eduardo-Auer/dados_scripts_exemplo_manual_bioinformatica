#Instalar pacotes 
install.packages("BiocManager", dependencies = TRUE)
BiocManager::install(version = "3.19")
BiocManager::install("snpStats", version = "3.19")
install.packages("ggplot2", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("openxlsx", dependencies = TRUE)

#Carregar pacotes
library(snpStats)        # Manipulação e análises estatíticas de dados genotípicos no formato PLINK
library(tidyverse)       # Conjunto de pacotes para manipulação de dados e visualizações
library(ggplot2)         # Pacote para criar gráficos
library(reshape2)        # Pacote para transformação de dados em formato longo
library(RColorBrewer)    # Pacote para paletas de cores
library(openxlsx)        # Pacote para ler e salvar arquivos em formato do Excel

# Selecione o diretório de trabalho onde está os arquivos de entrada
# Session > Set Working Directory > Choose directory

# Carregar IDs dos SNPs a serem selecionados####
selecao_snps <- read.table("selecao_snps.txt", quote="\"", comment.char="")  # Lê o arquivo com a lista de SNPs para selecionar
selecao_snps <- unique(selecao_snps$V1)  # Extrai a primeira coluna contendo os IDs dos SNPs

# 1. Carregar dados genotípicos do PLINK
# Substitua pelos seus arquivos .bed, .bim e .fam
plink_data <- read.plink("plink_example_chr_21.bed",  # Carrega arquivo de genótipos no formato .bed
                         "plink_example_chr_21.bim",  # Carrega arquivo de mapa de SNPs no formato .bim
                         "plink_example_chr_21.fam",  # Carrega arquivo de dados de família no formato .fam
                         select.snps = selecao_snps)  # Seleciona apenas os SNPs especificados na lista


# 2. Separar as informações genotípicas e os dados dos SNPs
genotypes <- plink_data$genotypes  # Extrai a matriz de genótipos em formato SnpMatrix
snp_info <- plink_data$map         # Extrai informações dos SNPs, como nomes, posições e IDs

#mudar os IDs das variantes para incluir dbSNPID no objeto de genótipos####

#identificar ordem dos IDs originais
colnames(genotypes@.Data)  # Exibe os IDs originais das variantes na matriz de genótipos

#Substituir os IDs do dbSNP correspondentes na mesma ordem que os IDs originais
colnames(genotypes@.Data) <-  # Substitui os IDs pelos dbSNP IDs fornecidos manualmente
  c("rs55907926",
    "rs60455889",
    "rs1468704",
    "rs9977552",
    "rs2822556",
    "rs2823624",
    "rs2825544",
    "rs4817471",
    "rs2898187",
    "rs8127706",
    "rs2211677",
    "rs2226292",
    "rs4817472",
    "rs4817476")

# Exibir uma prévia dos genótipos e dos SNPs
print(genotypes[1:5, 1:5])  # Mostra os genótipos dos 5 primeiros indivíduos e 5 primeiros SNPs
print(head(snp_info))       # Mostra as primeiras linhas de informações dos SNPs, como IDs e posições

# 3. Calcular a matriz de LD (R²)####
# 'depth' define o número de SNPs a ser calculada as estatísticas de desequilíbrio de ligação (LD)
ld_matrix <- ld(genotypes, depth = 50, stats = "R.squared")  # Calcula o desequilíbrio de ligação usando a estatística R² para pares de SNPs até uma profundidade de 50 SNPs


# 4. Criar o gráfico heatmap de LD (R²)####

#criar matrix de de LD R² trangular
ld_tri <- as.matrix(ld_matrix)   # Converte a matriz de LD para uma matriz numérica
ld_tri[lower.tri(ld_tri)] <- NA  # Remove valores da parte inferior da matriz para focar na triangular superior

#Transformar a matrix triangular em formato de tabela longa
ld_tri_df <- melt(ld_tri,                        # Modifica a matriz para o formato "longo" 
                  varnames = c("SNP1", "SNP2"),  # Define os nomes das colunas como SNP1 e SNP2
                  value.name = "LD")             # Define a coluna com valores de LD

#Remover comparações com NA
ld_tri_df <- na.omit(ld_tri_df)           # Remove todas as linhas com valores NA
ld_tri_df$LD <- as.numeric(ld_tri_df$LD)  # Converte a coluna de valores LD para numérica
ld_tri_df$LD <- round(ld_tri_df$LD, 2)    # Arredonda os valores de LD para 2 casas decimais

#fazer com que comparações entre mesmas variantes sejam "NA" e omitidas no gráfico
ld_tri_df$LD <- ifelse(ld_tri_df$SNP1 == ld_tri_df$SNP2, NA, ld_tri_df$LD)  # Define o LD como NA quando comparando o mesmo SNP (SNP1 == SNP2)

ld_tri_df <- na.omit(ld_tri_df)  # Remove novamente linhas com NA

#salvar gráfico heatmap#
tiff(filename = "heatmap_r2.tiff", width = 20, height = 15, units = "cm", compression = "lzw", res = 300)  # Define o nome e parâmetros do arquivo para salvar o gráfico como um TIFF
ggplot(ld_tri_df, aes(x = SNP1, y = SNP2, fill = LD)) +   # Cria o gráfico de calor (heatmap) usando ggplot2
  geom_tile() +                                           # Gera os quadrados do heatmap
  scale_fill_gradientn(                                   # Define o gradiente de cores para o LD
    colors = c("blue", "orange", "red"),                  # Escolhe as cores para o gradiente
    na.value = 'grey',                                    # Define a cor para valores NA como cinza
    name = expression(R ^ 2),                             # Define o rótulo da legenda como R²
    breaks = c(0, 0.25, 0.5, 0.8, 1),                     # Define os intervalos na legenda
    limits = c(0, 1)) +                                   # Define os limites para o valor da estatística de LD
  theme_classic() +                                       # Aplica um tema clássico ao gráfico
  geom_text(aes(x = SNP1, y = SNP2, label = LD),          # Adiciona os valores da estatística de LD como rótulos no gráfico
            colour = "white",                             # Define a cor dos rótulos como branco
            check_overlap = TRUE) +                       # Evita sobreposição de rótulos
  theme(                                                  # Configura a aparência dos textos e legendas
    axis.text.x = element_text(
      size = 10,
      face = "plain",
      colour = "black",
      angle = 90,  # Gira o texto do eixo X em 90 graus
      vjust = 0.5,
      hjust=1),
    axis.text.y = element_text(size = 10, face = "plain", colour = "black"),
    text = element_text(size = 12, face = "plain", colour = "black")) +
  labs(x = NULL, y = NULL)  # Remove os rótulos dos eixos
dev.off()                   # Finaliza o dispositivo gráfico e salva o arquivo TIFF
