#intalar os pacotes necessarios 
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")

#carregar os pacotes necessarios
library(openxlsx)
library(tidyverse)
library(dplyr)      
library(tidyr)     
library(readr)      
library(openxlsx)   

# CORRECOES - BONFERRONI, FDR E adicao da Permutação de MONTE CARLO #### 

# Importa o arquivo de resultados do modelo aditivo gerado pelo PLINK.
Plink_add_model <- read.table("analises_modelo_aditivo.assoc.logistic", quote = "", header = TRUE)

# Importa o arquivo de permutação gerado pelo PLINK, que contém os p-valores ajustados por permutação (Monte Carlo).
Plink_add_model_perm <- read.table("analises_modelo_aditivo.assoc.logistic.mperm", quote = "", header = TRUE)

# Remove linhas com valores ausentes na coluna de p-valores ("P") do dataframe principal.
Plink_add_model <- Plink_add_model %>% drop_na(P)

# Remove linhas com valores ausentes na coluna "EMP1" (p-valor por permutação) do dataframe de permutação e seleciona apenas as colunas relevantes ("SNP" e "EMP1"). Renomeia a coluna "EMP1" para "p_monte_carlo".
Plink_add_model_perm <- Plink_add_model_perm %>% drop_na(EMP1) %>% select(SNP, EMP1) %>% rename(p_monte_carlo = EMP1)

# Realiza um "left join" (união à esquerda) para combinar os dois dataframes com base na coluna "SNP", adicionando a informação dos p-valores de Monte Carlo ao dataframe principal.
Plink_add_model <- left_join(Plink_add_model, Plink_add_model_perm, by = "SNP")

# Aplica a correção de Bonferroni aos p-valores originais, adicionando uma nova coluna "p_bonferroni" com os p-valores ajustados.
Plink_add_model$p_bonferroni <- p.adjust(Plink_add_model$P, method = "bonferroni")

# Aplica a correção de FDR (False Discovery Rate) aos p-valores originais, adicionando uma nova coluna "p_fdr" com os p-valores ajustados.
Plink_add_model$p_fdr <- p.adjust(Plink_add_model$P, method = "fdr")

# Filtra os resultados para manter apenas aqueles que têm p-valores originais e de Monte Carlo abaixo de 0.05, ou seja, os SNPs mais significativos.
Plink_add_model <- Plink_add_model %>% filter(P <= 0.049 & p_monte_carlo <= 0.049)

# Remove o dataframe intermediário de permutação da memória para liberar espaço.
rm(Plink_add_model_perm)

# Chama o garbage collector para otimizar o uso de memória no R.
gc()

#importar variantes do modelo recessivo e aplicar correção para múltipla testagem (bonferroni e FDR e permutação de montecarlo)####
Plink_rec_model <- read.table("analises_modelo_recessivo.assoc.logistic", quote = "", header = TRUE)
Plink_rec_model_perm <- read.table("analises_modelo_recessivo.assoc.logistic.mperm", quote = "", header = TRUE)
Plink_rec_model <- Plink_rec_model %>% drop_na(P)
Plink_rec_model_perm <- Plink_rec_model_perm %>% drop_na(EMP1) %>% select(SNP, EMP1) %>% rename(p_monte_carlo = EMP1)
Plink_rec_model <- left_join(Plink_rec_model, Plink_rec_model_perm, by= "SNP")
Plink_rec_model$p_bonferroni <- p.adjust(Plink_rec_model$P, method="bonferroni")
Plink_rec_model$p_fdr <- p.adjust(Plink_rec_model$P, method="fdr")
Plink_rec_model <- Plink_rec_model %>% filter(P <= 0.049 & p_monte_carlo <= 0.049)
rm(Plink_rec_model_perm)
gc()

#importar variantes do modelo dominante e aplicar correção para múltipla testagem (bonferroni e FDR e permutação de montecarlo)####
Plink_dom_model <- read.table("analises_modelo_dominante.assoc.logistic", quote = "", header = TRUE)
Plink_dom_model_perm <- read.table("analises_modelo_dominante.assoc.logistic.mperm", quote = "", header = TRUE)
Plink_dom_model <- Plink_dom_model %>% drop_na(P)
Plink_dom_model_perm <- Plink_dom_model_perm %>% drop_na(EMP1) %>% select(SNP, EMP1) %>% rename(p_monte_carlo = EMP1)
Plink_dom_model <- left_join(Plink_dom_model, Plink_dom_model_perm, by= "SNP")
Plink_dom_model <- Plink_dom_model %>% drop_na(p_monte_carlo)
Plink_dom_model$p_bonferroni <- p.adjust(Plink_dom_model$P, method="bonferroni")
Plink_dom_model$p_fdr <- p.adjust(Plink_dom_model$P, method="fdr")
Plink_dom_model <- Plink_dom_model %>% filter(P <= 0.049 & p_monte_carlo <= 0.049)
rm(Plink_dom_model_perm)
gc()



# TABELA DE RESULTADOS ####
# combinar os resultados obtidos (equilíbrio de Hardy-Weinberg, frequências e associações) em uma única tabela.

# Criar data.frame (tabela) com os resultados de todos os modelos juntos (aditivo, dominante e recessivo) ####
Plink_all_models_joined <- rbind(Plink_add_model, Plink_rec_model, Plink_dom_model)  # Combina os dataframes dos três modelos (aditivo, recessivo e dominante) em um único dataframe

Plink_all_models_joined$TEST[Plink_all_models_joined$TEST == "ADD"] <- "Modelo aditivo"  # Substitui "ADD" por "Modelo aditivo" na coluna "TEST"
Plink_all_models_joined$TEST[Plink_all_models_joined$TEST == "REC"] <- "Modelo recessivo"  # Substitui "REC" por "Modelo recessivo" na coluna "TEST"
Plink_all_models_joined$TEST[Plink_all_models_joined$TEST == "DOM"] <- "Modelo dominante"  # Substitui "DOM" por "Modelo dominante" na coluna "TEST"

Plink_all_models_joined <- Plink_all_models_joined %>% mutate_at(vars(OR, U95, L95), ~round(., 2))  # Arredonda os valores das colunas OR, U95 e L95 para duas casas decimais
Plink_all_models_joined <- Plink_all_models_joined %>% mutate_at(vars(P, p_bonferroni, p_fdr, p_monte_carlo), ~signif(., 2))  # Arredonda os valores das colunas P, p_bonferroni, p_fdr e p_monte_carlo para duas casas significativas

Plink_all_models_joined <- Plink_all_models_joined %>% select(CHR, BP, SNP, A1, TEST, OR, L95, U95, P, p_fdr, p_bonferroni, p_monte_carlo) %>% rename(modelo = TEST)  # Seleciona e renomeia as colunas relevantes, alterando "TEST" para "modelo"

Plink_all_models_joined$IC_95 <- paste(Plink_all_models_joined$L95, Plink_all_models_joined$U95, sep = " - ")  # Cria uma nova coluna "IC_95" com o intervalo de confiança combinado em um único campo
Plink_all_models_joined <- Plink_all_models_joined %>% select(-c(L95, U95)) %>% select(CHR, BP, SNP, A1, modelo, OR, IC_95, P, p_fdr, p_bonferroni, p_monte_carlo)  # Remove as colunas L95 e U95 após combiná-las e reordena as colunas

rm(Plink_add_model, Plink_dom_model, Plink_rec_model)  # Remove dataframes intermediários para liberar memória
gc()  # Executa a coleta de lixo para liberar memória


# Adicionar frequências alélicas das variantes dos exomas de somente menonitas ####
contagem_alelica_todos <- read.table("contagem_alelica_todas_variantes_todas_amostras.frq.counts", header = TRUE, quote = "", comment.char = "")  # Importa a contagem alélica das variantes
contagem_alelica_todos <- contagem_alelica_todos %>% select(-c(CHR, G0)) %>% rename(Menonitas_contagem_alelos_A1 = C1, Menonitas_contagem_alelos_A2 = C2)  # Remove colunas irrelevantes e renomeia as colunas de contagem alélica

frequencia_alelica_todos <- read.table("frequencia_alelica_todas_variantes_todas_amostras.frq", header = TRUE, quote = "", comment.char = "")  # Importa a frequência alélica das variantes
frequencia_alelica_todos <- frequencia_alelica_todos %>% rename(Menonitas_frequencia_alelica_A1 = MAF) %>% select(SNP, Menonitas_frequencia_alelica_A1)  # Renomeia e seleciona as colunas relevantes

tabela_frequencia_contagem_alelica_todos <- left_join(contagem_alelica_todos, frequencia_alelica_todos, by = "SNP")  # Junta as tabelas de contagem e frequência alélica pelo SNP
tabela_frequencia_contagem_alelica_todos <- tabela_frequencia_contagem_alelica_todos %>% mutate(Menonitas_frequencia_alelica_A2 = Menonitas_contagem_alelos_A2 / (Menonitas_contagem_alelos_A1 + Menonitas_contagem_alelos_A2))  # Calcula a frequência alélica de A2

tabela_frequencia_contagem_alelica_todos$Menonitas_contagem_alelos_A1 <- as.numeric(tabela_frequencia_contagem_alelica_todos$Menonitas_contagem_alelos_A1)  # Converte a contagem de A1 para numérico
tabela_frequencia_contagem_alelica_todos$Menonitas_contagem_alelos_A2 <- as.numeric(tabela_frequencia_contagem_alelica_todos$Menonitas_contagem_alelos_A2)  # Converte a contagem de A2 para numérico
tabela_frequencia_contagem_alelica_todos$Menonitas_frequencia_alelica_A1 <- as.numeric(tabela_frequencia_contagem_alelica_todos$Menonitas_frequencia_alelica_A1)  # Converte a frequência de A1 para numérico
tabela_frequencia_contagem_alelica_todos$Menonitas_frequencia_alelica_A2 <- as.numeric(tabela_frequencia_contagem_alelica_todos$Menonitas_frequencia_alelica_A2)  # Converte a frequência de A2 para numérico

Plink_freq_junto <- left_join(Plink_all_models_joined, tabela_frequencia_contagem_alelica_todos, by = "SNP") %>% select(SNP, CHR, BP, A1.x, A2, modelo, OR, IC_95, P, p_fdr, p_bonferroni, p_monte_carlo, Menonitas_contagem_alelos_A1, Menonitas_frequencia_alelica_A1, Menonitas_contagem_alelos_A2, Menonitas_frequencia_alelica_A2) %>% rename(A1 = "A1.x")  # Combina as frequências alélicas ao dataframe principal e ajusta nomes de colunas

rm(contagem_alelica_todos, frequencia_alelica_todos, tabela_frequencia_contagem_alelica_todos, Plink_all_models_joined)  # Remove dataframes intermediários para liberar memória
gc()  # Executa a coleta de lixo para liberar memória


# Tabela frequências alelos nos casos e controles ####

Plink_freq_alelo_caso_controle <- read.table("frequencia_alelica_caso_controle.frq.cc", quote = "", header = TRUE)  # Importa dados de frequência alélica para casos e controles

Plink_freq_alelo_caso_controle <- Plink_freq_alelo_caso_controle %>% select(-c(CHR, A1, A2, NCHROBS_A, NCHROBS_U)) %>% rename(Caso = MAF_A, Controle = MAF_U)  # Remove colunas irrelevantes e renomeia as colunas de frequência alélica

Plink_freq_alelo_caso_controle <- Plink_freq_alelo_caso_controle %>% pivot_longer(cols = c("Caso", "Controle"), names_to = "grupo", values_to = "frequencia_alelo_associado_A1")  # Transforma a tabela de formato largo para longo, separando caso e controle

Plink_freq_todos_caso_controle_junto <- left_join(Plink_freq_junto, Plink_freq_alelo_caso_controle, by = "SNP")  # Junta os dados de frequência alélica de casos e controles ao dataframe principal

rm(Plink_freq_junto, Plink_freq_alelo_caso_controle)  # Remove dataframes intermediários para liberar memória

# Executa a coleta de lixo para liberar memória
gc()  
gc()  


# Ler arquivo com teste para equilíbrio de Hardy-Weinberg caso-controle e genótipos nos casos e controles ####
Plink_hardy_genotipos_caso_controle <- read.table("hardy_weinberg_caso_controle.hwe", quote = "", header = TRUE)  

Plink_hardy_genotipos_caso_controle <- Plink_hardy_genotipos_caso_controle %>%
  separate(GENO, c("contagem_homozigotos_alelo_associado_A1", "heterozigotos_A1_A2", "contagem_homozigotos_alelo_não_associado_A2"), sep = "/") %>%  # Separa a coluna GENO em três colunas: contagem dos homozigotos para A1, heterozigotos e homozigotos para A2.
  rename(valor_de_p_equilibrio_hardy_weinberg = P, grupo = TEST)  # Renomeia as colunas para facilitar a interpretação.

Plink_hardy_genotipos_caso_controle$grupo[Plink_hardy_genotipos_caso_controle$grupo == "ALL"] <- "Todos indivíduos com fenótipos"  # Altera "ALL" para "Todos indivíduos com fenótipos".
Plink_hardy_genotipos_caso_controle$grupo[Plink_hardy_genotipos_caso_controle$grupo == "AFF"] <- "Caso"        # Altera "AFF" para "Caso".
Plink_hardy_genotipos_caso_controle$grupo[Plink_hardy_genotipos_caso_controle$grupo == "UNAFF"] <- "Controle"  # Altera "UNAFF" para "Controle".

Plink_hardy_genotipos_caso_controle <- Plink_hardy_genotipos_caso_controle %>%
  select(-c("CHR", "O.HET.", "E.HET.")) %>%  # Remove colunas desnecessárias relacionadas a cromossomos e heterozigotos.
  select(-c(A1, A2))  # Remove as colunas de alelos para simplificar a tabela.

Plink_freq_todos_caso_controle_HW_junto <- left_join(Plink_freq_todos_caso_controle_junto, Plink_hardy_genotipos_caso_controle, by = c("SNP", "grupo"))  # Realiza um join das duas tabelas usando "SNP" e "grupo" como chaves.

Plink_freq_todos_caso_controle_HW_junto <- Plink_freq_todos_caso_controle_HW_junto %>% arrange(P)  # Organiza os resultados pela coluna "P" para priorizar os SNPs mais significativos.

# Limpeza de memória e remoção de objetos desnecessários
rm(Plink_hardy_genotipos_caso_controle, Plink_freq_todos_caso_controle_junto)  # Remove objetos temporários para liberar espaço na memória.
gc()  
gc()  


# Arredondar valores das frequências e do valor de p para o teste de equilíbrio de Hardy-Weinberg
Plink_freq_todos_caso_controle_HW_junto$Menonitas_frequencia_alelica_A1 <- round(Plink_freq_todos_caso_controle_HW_junto$Menonitas_frequencia_alelica_A1, 2)  # Arredonda a frequência alélica A1 dos menonitas para 2 casas decimais
Plink_freq_todos_caso_controle_HW_junto$Menonitas_frequencia_alelica_A2 <- round(Plink_freq_todos_caso_controle_HW_junto$Menonitas_frequencia_alelica_A2, 2)  # Arredonda a frequência alélica A2 dos menonitas para 2 casas decimais
Plink_freq_todos_caso_controle_HW_junto$frequencia_alelo_associado_A1 <- round(Plink_freq_todos_caso_controle_HW_junto$frequencia_alelo_associado_A1, 2)  # Arredonda a frequência do alelo A1 associado (para casos e controles) para 2 casas decimais
Plink_freq_todos_caso_controle_HW_junto$valor_de_p_equilibrio_hardy_weinberg <- signif(Plink_freq_todos_caso_controle_HW_junto$valor_de_p_equilibrio_hardy_weinberg, 2)  # Arredonda o valor de p do teste de equilíbrio de Hardy-Weinberg para 2 algarismos significativos

# Excluir variantes com p < 0,05 nos controles (fora do equilíbrio de Hardy-Weinberg)
variantes_fora_equlibrio_HW <- Plink_freq_todos_caso_controle_HW_junto %>% 
  select(SNP, grupo, valor_de_p_equilibrio_hardy_weinberg) %>%                # Seleciona as colunas de interesse: SNP, grupo e valor de p
  filter(grupo == "Controle" & valor_de_p_equilibrio_hardy_weinberg <= 0.05)  # Filtra as variantes com p <= 0,05 nos controles
variantes_fora_equlibrio_HW <- unique(variantes_fora_equlibrio_HW$SNP)        # Mantém apenas os SNPs únicos que estão fora do equilíbrio de Hardy-Weinberg

Plink_freq_todos_caso_controle_HW_junto <- Plink_freq_todos_caso_controle_HW_junto %>% 
  filter(!(SNP %in% variantes_fora_equlibrio_HW))  # Remove as variantes fora do equilíbrio de Hardy-Weinberg nos controles

# Salvar arquivo com todos os resultados juntos
write.xlsx(Plink_freq_todos_caso_controle_HW_junto, file = "Plink_analises_resultados_juntos.xlsx", colNames = TRUE, asTable = TRUE)  # Exporta o dataframe final para um arquivo Excel, com nomes de colunas e formato de tabela


