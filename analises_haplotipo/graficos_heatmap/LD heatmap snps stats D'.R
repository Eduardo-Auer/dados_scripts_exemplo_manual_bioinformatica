#Instalar pacotes 
install.packages("BiocManager", dependencies = TRUE)
BiocManager::install("snpStats")
install.packages("ggplot2", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)

#Carregar pacotes
library(snpStats)        # Manipulação e análises estatíticas de dados genotípicos no formato PLINK
library(tidyverse)       # Conjunto de pacotes para manipulação de dados e visualizações
library(ggplot2)         # Pacote para criar gráficos
library(reshape2)        # Pacote para transformação de dados em formato longo
library(RColorBrewer)    # Pacote para paletas de cores
library(openxlsx)        # Pacote para ler e salvar arquivos em formato do Excel

# Selecione o diretório de trabalho onde está os arquivos de entrada
#Session > Set Working Directory > Choose directory

# Carregar IDs dos SNPs a serem selecionados
selecao_snps <- read.table("selecao_snps.txt", quote="\"", comment.char="")  # Lê o arquivo com IDs dos SNPs
selecao_snps <- selecao_snps$V1  # Extrai a primeira coluna contendo os IDs dos SNPs


# 1. Carregar dados genotípicos do PLINK
plink_data <- read.plink("plink_example_chr_21.bed", 
                         "plink_example_chr_21.bim", 
                         "plink_example_chr_21.fam",
                         select.snps = selecao_snps)  # Carrega os dados de genótipos do PLINK e seleciona os SNPs de interesse


# 2. Separar as informações genotípicas e os dados dos SNPs
genotypes <- plink_data$genotypes    # Genótipos em formato SnpMatrix
snp_info <- plink_data$map           # Informações sobre os SNPs (nomes, posições, etc.)
 

#mudar os IDs das variantes para incluir dbSNPID no objeto de genótipos

#identificar ordem dos IDs originais
colnames(genotypes@.Data)  # Mostra os nomes das colunas (IDs dos SNPs) no objeto genotypes

#Substituir os IDs do dbSNP correspondentes na mesma ordem que os IDs originais
colnames(genotypes@.Data) <-  # Substitui os IDs originais pelos IDs do dbSNP
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
print(genotypes[1:5, 1:5])   # Mostra os 5 primeiros indivíduos e 5 primeiros SNPs
print(head(snp_info))        # Mostra as primeiras linhas de informações dos SNPs


# 3. Calcular a matriz de LD D')####
# 'depth' define o número de SNPs a ser calculada as estatísticas de desequilíbrio de ligação (LD)
ld_matrix <- ld(genotypes, depth = 50, stats = "D.prime")  # Calcula LD para até 50 pares de SNPs usando a estatística D'


# 4. Criar o gráfico heatmap de LD D'####

#criar matrix de de LD D' triangular
ld_tri <- as.matrix(ld_matrix)  # Converte a matriz de LD em formato matricial
ld_tri[lower.tri(ld_tri)] <- NA  # Remove a parte inferior da matriz para que ela seja triangular

#Transformar a matrix triangular em formato de tabela longa
ld_tri_df <- melt(ld_tri,                        # modifica a matriz de LD para um formato de tabela longa
                  varnames = c("SNP1", "SNP2"),
                  value.name = "LD")

#Remover comparações com NA
ld_tri_df <- na.omit(ld_tri_df)              # Remove linhas com valores NA
ld_tri_df$value <- as.numeric(ld_tri_df$LD)  # Converte os valores de LD para numérico
ld_tri_df$LD <- round(ld_tri_df$LD, 2)       # Arredonda os valores de LD para duas casas decimais

#fazer com que comparações entre mesmas variantes sejam "NA" e omitidas no gráfico
ld_tri_df$LD <- ifelse(ld_tri_df$SNP1 == ld_tri_df$SNP2, NA, ld_tri_df$LD)  # Define LD entre os mesmos SNPs como NA
ld_tri_df <- na.omit(ld_tri_df)                                             # Remove as comparações onde LD é NA

#salvar gráfico heatmap#
tiff(filename = "heatmap_D'.tiff", width = 20, height = 15, units = "cm", compression = "lzw", res = 300)  # Configurações para salvar o gráfico em formato TIFF
ggplot(ld_tri_df, aes(x = SNP1, y = SNP2, fill = LD)) +  # Cria um gráfico heatmap
  geom_tile() +                                          # Adiciona os blocos de cores
  scale_fill_gradientn(                                  # Define o gradiente de cores
    colors = c("blue", "orange", "red"),                 # Especifica as cores do gradiente
    na.value = 'grey',                                   # Define a cor para valores NA
    name = expression(D ^ "'"),                          # Nome do eixo de preenchimento (LD D')
    breaks = c(0, 0.25, 0.5, 0.8, 1),                    # Quebra do eixo de preenchimento
    limits = c(0, 1)) +                                  # Define os limites para o valor da estatística de LD
  theme_classic() +                                      # Aplica um tema clássico ao gráfico
  geom_text(aes(x = SNP1, y = SNP2, label = LD),         # Adiciona rótulos para os valores de LD
            colour = "white",
            check_overlap = TRUE) +                      # Evita sobreposição de texto
  theme(                                                 # Configurações estéticas dos eixos e textos
    axis.text.x = element_text(
      size = 10,
      face = "plain",
      colour = "black",
      angle = 90,
      vjust = 0.5,
      hjust=1
    ), axis.text.y = element_text(size = 10,
                                  face = "plain",
                                  colour = "black"),
    
    text = element_text(size = 12,
                        face = "plain",
                        colour = "black")) +
  labs(x = NULL, y = NULL)                           # Remove os rótulos dos eixos X e Y
dev.off()                                            # Salva o arquivo
