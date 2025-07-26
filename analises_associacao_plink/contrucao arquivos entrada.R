#instalar pacotes####
#Lembrando que se você já instalou esses pacotes em outro momento basta carregar.
install.packages("tidyverse", dependencies = TRUE)
install.packages("openxlsx", dependencies = TRUE)

#carregar pacotes####
library(openxlsx)
library(tidyverse)

#Selecionar diretorio de trabalho

#Junção arquivo modelo plink com dados do fenótipo e covariáveis####

#ler arquivo fam
arquivo_fam_plink <- read.delim("plink_example_chr_21.fam" , sep = " ", header = FALSE)
arquivo_fam_plink <- arquivo_fam_plink %>% select(V1, V2) %>% rename(FID = V1, IID = V2)

#ler arquivo com dados dos indivíduos
planilha_dados <- read.xlsx("sample_data.xlsx", sheet = 1, colNames = T)
planilha_dados <- planilha_dados %>% rename(IID = individual.ID)

#juntar dados dos indivíduos com os IDs do PLINK
dados_phe_cov <- left_join(arquivo_fam_plink, planilha_dados, by = "IID")
dados_phe_cov <- dados_phe_cov %>% drop_na(IID)


#alterar dados para formato plink

dados_phe_cov$phenotype[dados_phe_cov$phenotype == 1] <- 2 
dados_phe_cov$phenotype[dados_phe_cov$phenotype == 0] <- 1

dados_phe_cov$gender[dados_phe_cov$gender == "male"] <- 0 
dados_phe_cov$gender[dados_phe_cov$gender == "female"] <- 1


#converter dados não-numéricos para dados numéricos (garantir que os dados realmente são numéricos)
dados_phe_cov <- dados_phe_cov %>% mutate_at(vars(phenotype:gender), as.numeric)


#Criar arquivo do fenótipo separado
dados_phe <- dados_phe_cov %>% select(FID, IID, phenotype)


#Criar arquivo de covariáveis separado
dados_cov <- dados_phe_cov %>% select(FID, IID, gender, age)
rm(arquivo_fam_plink, planilha_dados, dados_phe_cov)

#salvar arquivo pheno plink pronto####
write.table(dados_phe, file = "phenotype.txt", quote = FALSE, col.names = TRUE, row.names = FALSE, sep = " ", na = "NA")

#salvar arquivo covar plink pronto####
write.table(dados_cov, file = "covariates.txt", quote = FALSE, col.names = TRUE, row.names = FALSE, sep = " ", na = "NA")
