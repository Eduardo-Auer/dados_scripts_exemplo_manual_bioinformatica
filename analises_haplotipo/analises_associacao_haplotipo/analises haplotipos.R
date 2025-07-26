#Instalar pacotes####
install.packages("openxlsx", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("haplo.stats", dependencies = TRUE)
install.packages("SNPassoc", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)

#Carregar pacotes necessários####
library(openxlsx)    # Pacote para manipulação de arquivos Excel
library(tidyverse)   # Conjunto de pacotes para manipulação e visualização de dados
library(haplo.stats) # Pacote para análise de haplótipos
library(SNPassoc)    # Pacote para análise de associação genética 
library(data.table)  # Pacote para manipulação eficiente de grandes conjuntos de dados


#Selecionar diretório de trabalho

#Criar arquivo de genótipos das variantes####

genotypes <- fread("genotypes_tabular_format.txt",        # Lê o arquivo de genótipos tabulares
                   select = c("IID",                      # Seleciona as variantes e IDs desejados
                              "21:32175180:T:C",
                              "21:32175740:A:G",
                              "21:32177007:A:G",
                              "21:32177945:C:T",
                              "21:32178063:A:T",
                              "21:32178135:A:G",
                              "21:32182000:G:A"))

#Alterar para ID para dbSNP ID
#colocar IID sempre em primeiro e os IDs do dbSNP na mesma ordem que os IDs das suas variantes correpondentes
colnames(genotypes) <-           # Renomeia as colunas de acordo com os IDs do dbSNP
  c("IID",
    "rs4817471",
    "rs2898187",
    "rs8127706",
    "rs2211677",
    "rs2226292",
    "rs4817472",
    "rs4817476")

#Criar arquivos de fenótipos e genótipos juntos####
phenotype <- read.csv("phenotype.txt", sep="")            # Carrega o arquivo de fenótipo (previamente utilizado nas análises com o Plink)
phenotype <- phenotype %>% select(IID, phenotype)         # Seleciona a coluna IID e o fenótipo
phenotype$phenotype[phenotype$phenotype == 1] <- 0        # Converte os valores do fenótipo para o formato binário de "0" para controles
phenotype$phenotype[phenotype$phenotype == 2] <- 1        # Converte os valores do fenótipo para o formato binário de "1" para casos

covariates <- read.csv("covariates.txt", sep="")          # Carrega o arquivo de covariáveis (previamente utilizado nas análises com o Plink)
covariates <- covariates %>% select(IID, gender, age)     # Seleciona as covariáveis

phenotype_covariates <- left_join(phenotype, covariates, by = "IID")    # Junta os dados de fenótipo e covariáveis

phenotype_covariates <- phenotype_covariates %>% mutate_at(vars(phenotype:age), as.numeric)  # Converte os valores para numéricos

rm(phenotype, covariates) # Remove objetos desnecessários da memória
gc()    # Libera a memória (garbage collector) para otimizar o uso de recursos.


#Criar arquivo de genótipos para análises e estimativas####

#criar vetor com os nomes das colunas para separar os alelos de cada variante em uma coluna separada
dados_genotipos_haplotipo <- genotypes %>% select(-c(IID))  # Remove a coluna IID dos genótipos
label_myGeno <- colnames(dados_genotipos_haplotipo)         # Armazena os rótulos dos SNPs


#separar os alelos em cada coluna spearada para cada variante

dados_genotipos_haplotipo <- dados_genotipos_haplotipo %>% separate_wider_delim(all_of(label_myGeno), delim = "/", names_sep = "_allele_")
# 'all_of(label_myGeno)' seleciona todas as colunas com as variantes genéticas
# 'delim = "/"' define que a barra é o delimitador que separa os alelos
# 'names_sep = "_allele_"' adiciona sufixos '_allele_1', '_allele_2' para os alelos de cada variante

dados_genotipos_haplotipo <- as.data.frame(lapply(dados_genotipos_haplotipo, unlist))
# Converte o data frame para garantir que os dados estejam em formato adequado
# 'lapply()' aplica a função 'unlist()' a cada coluna, convertendo listas em vetores
# 'as.data.frame()' transforma o resultado novamente em um data frame


#Estimar as frequências e contagens dos haplótipos na população####

em_haplo <- haplo.em(dados_genotipos_haplotipo, locus.label = label_myGeno, miss.val = c(0, NA, "."))  
# Estima os haplótipos e suas frequências na população
# 'dados_genotipos_haplotipo': Data frame com os genótipos já separados por alelos
# 'locus.label = label_myGeno': Define os rótulos dos loci (nomes das variantes genéticas) para a análise
# 'miss.val = c(0, NA, ".")': Especifica os valores que serão tratados como dados ausentes (0, NA e .)

haplotype_frequency_and_counts <- print(em_haplo)  # Exibe resultados

haplotype_frequency_and_counts$Haplotype_counts <- round((haplotype_frequency_and_counts$hap.freq)*3202*2, 0)
# Calcula as contagens de haplótipos na população
# Multiplica a frequência do haplótipo (hap.freq) pelo número total de cromossomos na amostra (3202 indivíduos * 2 cromossomos por indivíduo)
# Depois, arredonda o resultado para o número inteiro mais próximo

haplotype_frequency_and_counts <- haplotype_frequency_and_counts %>% 
  arrange(desc(hap.freq))
# Ordena o data frame com as frequências de haplótipos de forma decrescente

haplotype_frequency_and_counts$hap.freq <- haplotype_frequency_and_counts$hap.freq*100  # Converte a frequência de haplótipos para porcentagem (multiplicando por 100)
haplotype_frequency_and_counts$hap.freq <- round(haplotype_frequency_and_counts$hap.freq, 2)  # Arredonda a frequência do haplótipo (em porcentagem) para duas casas decimais
haplotype_frequency_and_counts # Exibe o data frame atualizado com contagens e frequências de haplótipos

#salvar resultados em planilha do excel
write.xlsx(haplotype_frequency_and_counts,
           file = "Frequências e contagens dos haplótipos na população.xlsx",
           overwrite = TRUE)


#Salvar dados de haplótipos por indivíduos####

# Aumentar o número de linhas (individuos) reportadas para estimativas de haplótipos
# Isso é útil porque o próximo comando irá imprimir os haplótipos estimados para muitos indivíduos, e você deseja garantir que todas as linhas sejam mostradas.
options(max.print=100000)

# Estimar haplótipos por indivíduos com base nas combinações mais prováveis de alelos nas variantes analisadas.
haplotype_by_individuals <- print(summary(em_haplo, show.haplo=T,  digits= 0))
# O comando 'summary(em_haplo)' resume os resultados da análise de haplótipos realizada pelo objeto 'em_haplo'.
# 'show.haplo = TRUE' indica que os haplótipos estimados para cada indivíduo serão reportados.
# 'digits = 0' remove as casas decimais na exibição dos valores de probabilidade posterior de estimativa de cada haplótipo por indivíduo.

# Filtragem dos haplótipos por indivíduos, mantendo apenas os haplótipos com probabilidade posterior = 1, ou seja, aqueles com a maior probabilidade de serem os haplótipos corretos para cada indivíduo.
haplotype_by_individuals <- haplotype_by_individuals %>% filter(posterior == 1)
# O campo 'posterior' refere-se à probabilidade posterior de cada combinação de haplótipos, e o valor 1 indica a maior probabilidade de que aquele haplótipo existe no indivíduo.


# Incluir coluna do ID do indivíduo (IID) no dataframe 'haplotype_by_individuals'.

IID <- phenotype_covariates %>% select(IID)
# O 'phenotype_covariates' é um dataframe que contém o ID dos indivíduos (IID) junto com seus fenótipos e covariáveis.
# Usamos 'select(IID)' para selecionar apenas a coluna IID dos indivíduos.

haplotype_by_individuals <- cbind(IID, haplotype_by_individuals)
# Combina a coluna de IDs (IID) com o dataframe 'haplotype_by_individuals'.
# Isso garante que cada linha de haplótipos esteja associada ao indivíduo correto por meio de seu ID.

haplotype_by_individuals <- haplotype_by_individuals %>% select(-c(subj.id, posterior))
# Remove as colunas 'subj.id' e 'posterior' do dataframe 'haplotype_by_individuals'.
# 'subj.id' geralmente é uma coluna redundante (já temos o IID), e 'posterior' foi usado anteriormente para selecionar os haplótipos mais prováveis e não é mais necessário.


#criar uma coluna com a junção dos dois haplótipos em somente uma coluna

# Combinar as colunas que começam com "hap1" (que representam os alelos do primeiro haplótipo) em uma única coluna chamada "haplotype_1".
# O mesmo para as colunas que começam com "hap2" (que representam os alelos do segundo haplótipo) e criamos a coluna "haplotype_2".
haplotype_by_individuals <- haplotype_by_individuals %>% unite("haplotype_1", starts_with("hap1"), sep = "_", na.rm = TRUE)
haplotype_by_individuals <- haplotype_by_individuals %>% unite("haplotype_2", starts_with("hap2"), sep = "_", na.rm = TRUE)
# 'unite' junta várias colunas em uma só, e usamos 'sep = "_"' para separar os valores com um underline
# O parâmetro 'na.rm = TRUE' remove valores NA (faltantes) ao combinar as colunas.

# Combina os valores de "haplotype_1" e "haplotype_2" em uma nova coluna chamada "merged_haplotypes".
# Essa nova coluna contém os dois haplótipos separados por uma barra ("/"), indicando os dois haplótipos do indivíduo.
haplotype_by_individuals$merged_haplotypes <- paste(haplotype_by_individuals$haplotype_1, haplotype_by_individuals$haplotype_2, sep = "/")

# Exibe o dataframe 'haplotype_by_individuals'
view(haplotype_by_individuals)


#salvar resultados em planilha do excel
write.xlsx(haplotype_by_individuals,
           file = "haplótipos por indivíduos.xlsx",
           overwrite = TRUE)


#Análises de associação do haplótipo####

#criar vetores de fenótipos e covariáveis
caso_controle_dados <- phenotype_covariates$phenotype    # O vetor 'caso_controle_dados' armazenará a informação sobre os fenótipos dos indivíduos, que pode indicar um caso ou um controle.
caso_controle_dados <- as.numeric(caso_controle_dados)   # Converter os dados de 'caso_controle_dados' para numérico, garantindo que os dados sejam do tipo apropriado para análises estatísticas.
sexo <- phenotype_covariates$gender   # Criar um vetor que contém a informação do sexo dos indivíduos a partir do conjunto de dados 'phenotype_covariates'.
sexo <- as.numeric(sexo)   # Converter os dados de 'sexo' para numérico, transformando as categorias de gênero (por exemplo, masculino e feminino) em valores numéricos para facilitar a análise.
idade <- phenotype_covariates$age   # Criar um vetor que contém a informação de idade dos indivíduos a partir do conjunto de dados 'phenotype_covariates'.
idade <- as.numeric(idade)   # Converter os dados de 'idade' para numérico, assegurando que os valores de idade sejam reconhecidos como números para análises subsequentes.


#criar matrix de covariáveis unindo os vetores 'sexo' e 'idade'
cov_data <- cbind(sexo,idade)


#Exemplo de análise de associação (modelo aditivo)####
#obs: para os demais modelos o ajuste deve ser feito em haplo.effect ("dominant" ou "recessive")

mod_haplo_cc <- haplo.cc(caso_controle_dados,                  # Define o vetor de fenótipos (caso/controle) para análise
                         dados_genotipos_haplotipo,            # Fornece os dados de genótipos dos indivíduos, que contêm informações sobre os haplótipos
                         x.adj = cov_data,                     # Adiciona a matriz de covariáveis (sexo e idade) para ajustar a análise
                         ci.prob = 0.95,                       # Define o nível de confiança para os intervalos de confiança (95% neste caso) para o Odds ratio
                         miss.val = c(0, NA, "."),             # Especifica quais valores devem ser tratados como faltantes (0, NA e .)
                         locus.label = label_myGeno,           # Indica os rótulos dos loci (variantes genéticas) que estão sendo analisados
                         control = haplo.glm.control(          # Controla as configurações da análise de haplótipos
                           keep.rare.haplo = TRUE,             # Mantém haplótipos raros na análise
                           haplo.effect = "additive",          # Define o tipo de efeito dos haplótipos como modelo aditivo
                           haplo.freq.min = 0.01),             # Define a frequência mínima para incluir um haplótipo na análise (1% neste caso)
                         simulate = TRUE,                      # Ativa a correção e inclusão da permutação de Monte Carlo para obter o valor de p corrigido
                         sim.control = score.sim.control(      # Controla os parâmetros da permutação de Monte Carlo
                           min.sim = 10000,                    # Define o número mínimo de permutações de Monte Carlo a serem realizadas
                           max.sim = 10000))                   # Define o número máximo de permutações de Monte Carlo a serem realizadas



mod_haplo_cc_results <- mod_haplo_cc[["cc.df"]]                           # Extrai o dataframe de resultados de caso-controle da análise de haplótipos e armazena na variável 'mod_haplo_cc_results'

mod_haplo_cc_results$Total_haplotype_count <- (mod_haplo_cc[["cc.df"]][["pool.hf"]]) * # Calcula a contagem total de haplótipos
  ((mod_haplo_cc[["group.count"]][["case"]] +                      # Adiciona o número de indivíduos do grupo caso
      mod_haplo_cc[["group.count"]][["control"]]) * 2)             # e do grupo controle, multiplicando por 2 (para considerar diploidia)

mod_haplo_cc_results$Total_haplotype_count <- round(mod_haplo_cc_results$Total_haplotype_count, 0) # Arredonda o total de haplótipos 

mod_haplo_cc_results$Control_haplotype_count <- (mod_haplo_cc[["cc.df"]][["control.hf"]]) * # Calcula a contagem de haplótipos no grupo controle
  (mod_haplo_cc[["group.count"]][["control"]] * 2)                 # Multiplica pela contagem de indivíduos no grupo controle, considerando a diploidia

mod_haplo_cc_results$Control_haplotype_count <- round(mod_haplo_cc_results$Control_haplotype_count, 0) # Arredonda a contagem de haplótipos do controle

mod_haplo_cc_results$Case_haplotype_count <- (mod_haplo_cc[["cc.df"]][["case.hf"]]) * # Calcula a contagem de haplótipos no grupo caso
  (mod_haplo_cc[["group.count"]][["case"]] * 2)                     # Multiplica pela contagem de indivíduos no grupo caso, considerando a diploidia

mod_haplo_cc_results$Case_haplotype_count <- round(mod_haplo_cc_results$Case_haplotype_count, 0) # Arredonda a contagem de haplótipos do caso


mod_haplo_cc_results$pool.hf <- (mod_haplo_cc_results$pool.hf)*100             # Multiplica a frequência dos haplótipos agrupados (pool.hf) por 100 para converter de proporção para porcentagem.
mod_haplo_cc_results$pool.hf <- round(mod_haplo_cc_results$pool.hf, 2)         # Arredonda as porcentagens de haplótipos agrupados para duas casas decimais.
mod_haplo_cc_results$control.hf <- (mod_haplo_cc_results$control.hf)*100       # Multiplica a frequência dos haplótipos de controle (control.hf) por 100 para converter de proporção para porcentagem.
mod_haplo_cc_results$control.hf <- round(mod_haplo_cc_results$control.hf, 2)   # Arredonda as porcentagens de haplótipos de controle para duas casas decimais.
mod_haplo_cc_results$case.hf <- (mod_haplo_cc_results$case.hf)*100             # Multiplica a frequência dos haplótipos de caso (case.hf) por 100 para converter de proporção para porcentagem.
mod_haplo_cc_results$case.hf <- round(mod_haplo_cc_results$case.hf, 2)         # Arredonda as porcentagens de haplótipos de caso para duas casas decimais.
mod_haplo_cc_results$`p-val` <- signif(mod_haplo_cc_results$`p-val`, 2)        # Reduz o valor p de não corrigido (p-val) a duas casas significativas, mantendo apenas a precisão necessária para interpretação estatística.
mod_haplo_cc_results$`Hap-Score` <- round(mod_haplo_cc_results$`Hap-Score`, 2) # Arredonda o escore do haplótipo (Hap-Score) para duas casas decimais para melhor legibilidade.
mod_haplo_cc_results$OR <- round(mod_haplo_cc_results$OR, 2)                   # Arredonda a razão de chances (OR - Odds Ratio) para duas casas decimais, facilitando a interpretação.
mod_haplo_cc_results$OR.lower <- round(mod_haplo_cc_results$OR.lower, 2)       # Arredonda o limite inferior da razão de chances (OR.lower) para duas casas decimais.
mod_haplo_cc_results$OR.upper <- round(mod_haplo_cc_results$OR.upper, 2)       # Arredonda o limite superior da razão de chances (OR.upper) para duas casas decimais.


mod_haplo_cc_results <- mod_haplo_cc_results %>%      # Inicia uma cadeia de manipulação do dataframe mod_haplo_cc_results.
  rename("Total_haplotype_frequency" = pool.hf,       # Renomeia a coluna pool.hf para Total_haplotype_frequency.
         "Control_haplotype_frequency" = control.hf,  # Renomeia a coluna control.hf para Control_haplotype_frequency.
         "Case_haplotype_frequency" = case.hf) %>%    # Renomeia a coluna case.hf para Case_haplotype_frequency.
  drop_na(`p-val`) %>%                                # Remove linhas com valores ausentes na coluna p-val, garantindo que apenas associações com valores de p completos e que sejam considerados para análises posteriores.
  filter(!glm.eff == "Base")                          # Filtra as linhas onde a coluna glm.eff não é igual a "Base", removendo essas entradas do dataframe.
view(mod_haplo_cc_results)                            # Exibe o dataframe mod_haplo_cc_results atualizado, permitindo uma visualização dos dados manipulados.


#salvar resultados em planilha do excel
write.xlsx(mod_haplo_cc_results,
           file = "Análise de haplótipos (modelo aditivo).xlsx",
           overwrite = TRUE)


# Exemplo de comparação das frequências do haplótipo associado entre populações (exemplo = T_A_A_C_A_A_G) ####

# Criação de uma tabela de contingência que representa as frequências dos haplótipos
# Cria uma matriz 2x2 com os dados das contagens dos haplótipos.

Tabela_hap_comparacao <- as.table(rbind(c(2400, 4004), # nº de haplótipos analisado na população de estudo, nº de todos os haplótipos exceto o analisado na população de estudo
                                        c(423, 384)))  # nº de haplótipos analisado nos europeus não-finlandeses, nº de todos os haplótipos exceto o analisado nos europeus não-finlandeses

#Nomear as colunas e linhas da tabela
dimnames(Tabela_hap_comparacao) <- list(populacao = c("Populacao_estudo", "Europeus_nao_finlandeses"), # Define os nomes das colunas.
                                        haplotipos = c("haplotipo_analisado", "outros_haplotipos")) # Define os nomes das linhas.
print(Tabela_hap_comparacao)  # Exibe a tabela no console.


# Teste exato de Fisher para avaliar estatísticamente se há diferença das contagens do haplótipo estudado entre as populações
fisher.test(Tabela_hap_comparacao)  

  

