#Teste exato de fisher

# Construindo Tabela de contingencia 

#Instalar os pacotes necessarios
install.packages("tidyverse", dependencies = TRUE)
install.packages("rstatix", dependencies = TRUE)

#Carregar os pacotes
library(tidyverse)
library(rstatix)

# mensagem de alerta "The following object is masked from ‘package:stats’"
# está apenas alertando que os dois pacotes têm funções com o mesmo nome 
# a função do pacote carregado por último "mascara" a função do pacote que foi carregado primeiro.
# Se você precisar usar a função do pacote que foi carregado inicialmente, basta especificar no comando por exemplo: tidyverse: tidyverse::filter()


# Exemplo 1 - Tabela de contingencia #### 
# Criando a tabela de contingência com todas as populações comparadas 
# ex.: Menonitas, Europeus não-finlandeses, Amish e Brasileiros)
# Obs: dados de contagem fictícios.

Tabela_menonitas_vs_europeus_amish_brasileiros <- as.table(rbind(c(250, 400),     # nº de alelos alternativos menonitas, nº de alelos referência menonitas
                                                                 c(7057, 60973),  # nº de alelos alternativos europeus não-finlandeses, nº de alelos referência europeus não-finlandeses
                                                                 c(107, 805),     # nº de alelos alternativos Amish, nº de alelos referência Amish
                                                                 c(248, 2094)))   # nº de alelos alternativos brasileiros , nº de alelos referência brasileiros

#Nomear as colunas e linhas da tabela
dimnames(Tabela_menonitas_vs_europeus_amish_brasileiros) <- list(populacao = c("Menonitas", "Europeus_nao_finlandeses", "Amish", "Brasileiros"), alelos = c("numero_alelo_alternativo", "numero_alelo_referência"))
print(Tabela_menonitas_vs_europeus_amish_brasileiros)  # Exibe a tabela no console.


# Realizar as comparações entre as populações (incluindo o valor de p corrigido para múltipla testagem por FDR)

#Teste exato de fisher par a par ("pairwise")
# alternative = "two.sided":indica que o teste é bicaudal 
teste_exato_fisher_resultado <- pairwise_fisher_test(Tabela_menonitas_vs_europeus_amish_brasileiros, p.adjust.method = "none", alternative = "two.sided")


#Filtrar apenas comparações envolvendo menonitas e corrigir para FDR####

teste_exato_fisher_resultado <- teste_exato_fisher_resultado %>% select(-c("p.adj.signif", "n")) %>%
  filter(group1 == "Menonitas") %>%
  rename("Grupo_1" = group1, "Grupo_2" = group2, "valor_de_p_corrigido" = p.adj)

teste_exato_fisher_resultado$valor_de_p_corrigido <- p.adjust(teste_exato_fisher_resultado$p, method = "fdr")      # Corrige o valores de p usando o método FDR para controle de taxa de falsos positivos e atribui os valores ajustados à coluna "valor_de_p_corrigido"
teste_exato_fisher_resultado$valor_de_p_corrigido <- signif(teste_exato_fisher_resultado$valor_de_p_corrigido, 2)  # Arredonda os valores ajustados de p na coluna "valor_de_p_corrigido" para 2 casas decimais 
teste_exato_fisher_resultado$p <- signif(teste_exato_fisher_resultado$p, 2)  # Arredonda os valores brutos p na coluna "p" para 2 casas decimais 

view(teste_exato_fisher_resultado)


# Exemplo 2 - Importando dados do Excel ####

#Instalar pacotes 
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("writexl")

#Carregar pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(openxlsx) 

#Selecionar diretório de trabalho
setwd("coloque_o_caminho")

#Importar os dados do Excel (substitua "caminho/para/o/arquivo.xlsx" pelo caminho real do seu arquivo Excel)####
dados <- read_excel("dados.xlsx")

#Visualizar os dados
print(dados)

#Teste exato de Fisher para cada variante, comparando menonitas com EnF (Europeus não-finlandeses), Ami (Amish) e Br (Brasileiros)

#Cria Tabela de Contingencia
teste_fisher <- function(alt_menonitas, ref_menonitas, alt_ami, ref_ami, alt_enf, ref_enf, alt_br, ref_br) {
  tabela_men_ami <- matrix(c(alt_menonitas, ref_menonitas, alt_ami, ref_ami), nrow = 2)  # Cria uma tabela de contingência 2x2 para Menonitas vs. Amish, com alelos alternativos e referências
  tabela_men_enf <- matrix(c(alt_menonitas, ref_menonitas, alt_enf, ref_enf), nrow = 2)  # Cria uma tabela de contingência 2x2 para Menonitas vs. Europeus nao finlandeses, com alelos alternativos e referências
  tabela_men_br <- matrix(c(alt_menonitas, ref_menonitas, alt_br, ref_br), nrow = 2)    # Cria uma tabela de contingência 2x2 para Menonitas vs. Brasileiros, com alelos alternativos e referências

  
  # Realiza o teste exato de fisher 
  p_value_men_ami <- fisher.test(tabela_men_ami)$p.value  # Calcula o valor p do Teste Exato de Fisher para a tabela Menonitas vs. Amish
  p_value_men_enf <- fisher.test(tabela_men_enf)$p.value  # Calcula o valor p do Teste Exato de Fisher para a tabela Menonitas vs. Europeus nao finlandeses
  p_value_men_br <- fisher.test(tabela_men_br)$p.value   # Calcula o valor p do Teste Exato de Fisher para a tabela Menonitas vs. Brasileiros
  
  c(p_value_men_ami, p_value_men_enf, p_value_men_br)  # Retorna um vetor com os valores p calculados para as três comparações
}  

# O próximo passo deve ser realizado para corrigir a análise por FDR, podendo seguir duas abordagens:
# 1 – Correção pelo número de variantes genéticas testadas em cada população.
# 2 – Correção pelo número de populações testadas para cada variante.

#1_Correção de multipla testagem (FDR) pelo número de variantes testadas em cada população

resultados <- dados %>%  # Inicia o pipeline de manipulação de dados usando o dataframe 'dados'
  rowwise() %>%  # Garante que as operações subsequentes sejam aplicadas linha a linha
  mutate(p_values = list(teste_fisher(men_alt, men_ref, ami_alt, ami_ref, enf_alt, enf_ref, br_alt, br_ref))) %>%  # Cria uma nova coluna 'p_values' contendo uma lista com os valores p calculados pela função 'teste_fisher'
  mutate(
    p_men_ami = p_values[1],  # Extrai o primeiro valor p da lista para a comparação Menonitas vs. Amish e armazena na coluna 'p_men_ami'
    p_men_enf = p_values[2],  # Extrai o segundo valor p da lista para a comparação Menonitas vs. Enf e armazena na coluna 'p_men_enf'
    p_men_br = p_values[3]  # Extrai o terceiro valor p da lista para a comparação Menonitas vs. Brasileiros e armazena na coluna 'p_men_br'
  ) %>% 
  ungroup() %>%  # Remove o agrupamento de linhas para operações subsequentes
  mutate(
    fdr_men_ami = p.adjust(p_men_ami, method = "fdr"),  # Ajusta o valor p para a comparação Menonitas vs. Amish usando o método FDR
    fdr_men_enf = p.adjust(p_men_enf, method = "fdr"),  # Ajusta o valor p para a comparação Menonitas vs. Enf usando o método FDR
    fdr_men_br = p.adjust(p_men_br, method = "fdr")  # Ajusta o valor p para a comparação Menonitas vs. Brasileiros usando o método FDR
  ) %>%
  mutate(
    men_n_N = paste(men_alt, "/", men_tot, sep = ""),  # Cria uma coluna 'men_n_N' com a proporção de alelos alternativos e totais para Menonitas
    ami_n_N = paste(ami_alt, "/", ami_tot, sep = ""),  # Cria uma coluna 'ami_n_N' com a proporção de alelos alternativos e totais para Amish
    enf_n_N = paste(enf_alt, "/", enf_tot, sep = ""),  # Cria uma coluna 'enf_n_N' com a proporção de alelos alternativos e totais para Enf
    br_n_N = paste(br_alt, "/", br_tot, sep = "")  # Cria uma coluna 'br_n_N' com a proporção de alelos alternativos e totais para Brasileiros
  ) %>%
  select(
    SNV, ALT, REF, men_n_N, ami_n_N, enf_n_N, br_n_N, p_men_ami, p_men_enf, p_men_br, fdr_men_ami, fdr_men_enf, fdr_men_br  # Seleciona e organiza as colunas desejadas para o dataframe final
  ) %>%
  mutate(
    across(
      c("p_men_ami", "p_men_enf", "p_men_br", "fdr_men_ami", "fdr_men_enf", "fdr_men_br"),  # Especifica as colunas de valores p e FDR para formatação
      ~ ifelse(as.numeric(.) < 0.0099, sprintf("%.2e", as.numeric(.)), round(as.numeric(.), 2))  # Aplica formatação condicional: se o valor for menor que 0.0099, formata em notação científica (%.2e); caso contrário, arredonda para duas casas decimais
    )
  ) %>% mutate_at(vars("p_men_ami", "fdr_men_ami", "p_men_enf", "fdr_men_enf", "p_men_br", "fdr_men_br"), as.numeric) #força os dados de valores de p a serem dados numéricos

# Visualizar os resultados
print(resultados)

#2_Correção de multipla testagem (FDR) pelo número de populações em cada variante####

#cria matrix de comparação Menonitas vs Outras populações
teste_fisher_fdr <- function(alt_menonitas, ref_menonitas, alt_ami, ref_ami, alt_enf, ref_enf, alt_br, ref_br) {
  tabela_men_ami <- matrix(c(alt_menonitas, ref_menonitas, alt_ami, ref_ami), nrow = 2)  # Cria uma matriz 2x2 para a comparação Menonitas vs. Amish
  tabela_men_enf <- matrix(c(alt_menonitas, ref_menonitas, alt_enf, ref_enf), nrow = 2)  # Cria uma matriz 2x2 para a comparação Menonitas vs. Enf
  tabela_men_br <- matrix(c(alt_menonitas, ref_menonitas, alt_br, ref_br), nrow = 2)  # Cria uma matriz 2x2 para a comparação Menonitas vs. Brasileiros
  
  #Teste exato de fisher e Correção FDR
  p_value_men_ami <- fisher.test(tabela_men_ami)$p.value  # Calcula o valor p para a comparação Menonitas vs. Amish usando o teste exato de Fisher
  p_value_men_enf <- fisher.test(tabela_men_enf)$p.value  # Calcula o valor p para a comparação Menonitas vs. Enf usando o teste exato de Fisher
  p_value_men_br <- fisher.test(tabela_men_br)$p.value  # Calcula o valor p para a comparação Menonitas vs. Brasileiros usando o teste exato de Fisher
  
  p.adjust(c(p_value_men_ami, p_value_men_enf, p_value_men_br), method = "fdr")  # Ajusta os valores p obtidos pelo método FDR e retorna os valores ajustados
}

#Organiza os resultados
resultados <- dados %>%
  rowwise() %>%  # Aplica operações linha a linha no dataframe 'dados'
  mutate(p_values = list(teste_fisher(men_alt, men_ref, ami_alt, ami_ref, enf_alt, enf_ref, br_alt, br_ref))) %>%  # Calcula os valores p para as comparações utilizando o teste de Fisher
  mutate(p_values_fdr = list(teste_fisher_fdr(men_alt, men_ref, ami_alt, ami_ref, enf_alt, enf_ref, br_alt, br_ref))) %>%  # Calcula os valores p ajustados pelo método FDR (para múltiplos testes)
  mutate(p_men_ami = p_values[1], p_men_enf = p_values[2], p_men_br = p_values[3]) %>%  # Extrai os valores p individuais para as comparações Menonitas vs. Amish, Menonitas vs. Enf, Menonitas vs. Brasileiros
  mutate(fdr_men_ami = p_values_fdr[1], fdr_men_enf = p_values_fdr[2], fdr_men_br = p_values_fdr[3]) %>%  # Extrai os valores p ajustados (FDR) para as comparações
  ungroup() %>%  # Remove o agrupamento, caso tenha sido feito anteriormente, para garantir que as operações subsequentes não sejam feitas por grupos
  mutate(  # Cria novas colunas combinando as contagens de alterados e totais, com um formato "alterados/totais"
    men_n_N = paste(men_alt, "/", men_tot, sep = ""),  # Cria a coluna 'men_n_N' combinando 'men_alt' e 'men_tot' com barra
    ami_n_N = paste(ami_alt, "/", ami_tot, sep = ""),  # Cria a coluna 'ami_n_N' combinando 'ami_alt' e 'ami_tot' com barra
    enf_n_N = paste(enf_alt, "/", enf_tot, sep = ""),  # Cria a coluna 'enf_n_N' combinando 'enf_alt' e 'enf_tot' com barra
    br_n_N = paste(br_alt, "/", br_tot, sep = "")  # Cria a coluna 'br_n_N' combinando 'br_alt' e 'br_tot' com barra
  ) %>% 
  select(SNV, ALT, REF, men_n_N, ami_n_N, enf_n_N, br_n_N, p_men_ami, p_men_enf, p_men_br, fdr_men_ami, fdr_men_enf, fdr_men_br) %>%  # Seleciona as colunas desejadas para o dataframe final
  mutate(  # Aplica formatação condicional aos valores p e FDR
    across(
      c("p_men_ami", "fdr_men_ami", "p_men_enf", "fdr_men_enf", "p_men_br", "fdr_men_br"),  # Aplica a formatação para as colunas de valores p e FDR
      ~ ifelse(as.numeric(.) < 0.0099, sprintf("%.2e", as.numeric(.)), round(as.numeric(.), 2))  # Se o valor for menor que 0.0099, usa notação científica; senão arredonda para 2 casas decimais
    )
  ) %>% mutate_at(vars("p_men_ami", "fdr_men_ami", "p_men_enf", "fdr_men_enf", "p_men_br", "fdr_men_br"), as.numeric) #força os dados de valores de p a serem dados numéricos


# Visualizar os resultados
print(resultados)

# Exportar a tabela de resultados em xlsx. e inserir aba com legenda ####

# Criar um novo arquivo Excel com a planilha de resultados e uma aba de notas
wb <- createWorkbook()

# Adicionar aba de resultados
addWorksheet(wb, "Resultados")
writeData(wb, sheet = 1, resultados)

# Adicionar aba de legenda (altere como achar necessário)
addWorksheet(wb, "Nota")
writeData(wb, sheet = 2, x = "n: contagem de alelos alternativos;  N: contagem total de alelos;  men: Menonitas;  ami: Amish;  enf: Europeus não finlandeses;  br: Brasileiros", colNames = FALSE)

#negritar resultados com valores de p corrigidos < 0,05 ####
style_bold <- createStyle(textDecoration = "bold")
conditionalFormatting(wb, "Resultados", cols = 11:13, rows =1:nrow(resultados)+1, rule = "<0.05", style = style_bold)

# Salvar o arquivo Excel com as notas
saveWorkbook(wb, "resultados_fisher_corrigido_fdr.xlsx", overwrite = TRUE)


# Salvar resultados em docx. ####

# Instalar pacotes necessários 
install.packages("officer")
install.packages("flextable")
install.packages("dplyr")

# Carregar pacotes
library(officer)
library(flextable)
library(dplyr)

#Criar e Converter as colunas relevantes para numérico e aplicar formatação condicional
resultados_word <- resultados %>%
  mutate(across(c("p_men_ami", "p_men_enf", "p_men_br", 
                  "fdr_men_ami", "fdr_men_enf", "fdr_men_br"),
                ~ ifelse(as.numeric(.) < 0.0099, sprintf("%.2e", as.numeric(.)), round(as.numeric(.), 2))))

# Criar a tabela com o pacote flextable
ft <- flextable(resultados_word)

# Aplicar negrito aos valores menores que 0,05 nas colunas especificadas
ft <- bold(ft, j = "fdr_men_ami", i = ~ as.numeric(fdr_men_ami) < 0.05)
ft <- bold(ft, j = "fdr_men_enf", i = ~ as.numeric(fdr_men_enf) < 0.05)
ft <- bold(ft, j = "fdr_men_br", i = ~ as.numeric(fdr_men_br) < 0.05)

# Definir a fonte e o tamanho da fonte para todas as partes da tabela
ft <- font(ft, fontname = "Arial", part = "all")
ft <- fontsize(ft, size = 11, part = "all")

# Adicionar legenda ao rodapé
ft <- add_footer_row(ft, values = "n: contagem de alelos alternativos; N: contagem total de alelos; men: Menonitas; ami: Amish; enf: Europeus não finlandeses; br: Brasileiros", colwidths = ncol(resultados))
ft <- font(ft, fontname = "Arial", part = "footer")
ft <- fontsize(ft, size = 10, part = "footer")

# Criar um documento Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
doc <- body_end_section_landscape(doc)
print(doc, target = "resultados_fisher_corrigido_fdr.docx")
