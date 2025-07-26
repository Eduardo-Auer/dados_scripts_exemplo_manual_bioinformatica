#instalar pacotes####
install.packages("openxlsx", dependencies = TRUE)
install.packages("janitor", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("flextable", dependencies = TRUE)
install.packages("gtsummary", dependencies = TRUE)
install.packages("labelled", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)
install.packages("rstatix", dependencies = TRUE)

#Carregar pacotes####
library(openxlsx)
library(janitor)
library(tidyverse)
library(gtsummary)
library(flextable)
library(labelled)
library(readxl)
library(stringr)
library(rstatix)

#Selecinar o diretório de trabalho

#ler arquivo de dados/planilha####

dados <- read.xlsx("Z-Alizadeh sani dataset.xlsx", sheet = 1, colNames = T)


#Manipular dados####

#adequar nomes de colunas
dados_manipulados <- dados %>% clean_names()  # Limpa os nomes das colunas para padronizar o formato (ex. converte para letras minúsculas e substitui espaços por _)

# Selecionar as colunas de interesse para a análise

dados_manipulados <- dados_manipulados %>%
  select(cath,           # Diagnóstico de doença arterial coronariana (DAC)
         sex,            # Sexo biológico
         place,          # Local de residência
         current_smoker, # Fumante ativo
         ex_smoker,      # Ex-fumante
         airway_disease, # Doença respiratória
         dm,             # Diabetes mellitus
         dlp,            # Dislipidemia
         thyroid_disease,# Doença tireoidiana
         obesity,        # Obesidade
         fh,             # Histórico familiar de DAC
         htn,            # Hipertensão arterial sistêmica
         age,            # Idade
         bmi,            # Índice de Massa Corporal (IMC)
         tg,             # Triglicerídeos
         hdl,            # HDL (colesterol bom)
         ldl,            # LDL (colesterol ruim)
         na,             # Nível de sódio sérico
         k,              # Nível de potássio sérico
         bp)             # Pressão arterial


#dicotomizar dados categóricos para dados binários

dados_manipulados$cath[dados_manipulados$cath == "Cad"] <- 1  # CAD (doença arterial coronariana) recebe valor 1
dados_manipulados$cath[dados_manipulados$cath == "Normal"] <- 0  # Normal recebe valor 0

dados_manipulados$sex[dados_manipulados$sex == "Fmale"] <- 0  # Sexo feminino recebe valor 0
dados_manipulados$sex[dados_manipulados$sex == "Male"] <- 1   # Sexo masculino recebe valor 1

dados_manipulados$airway_disease[dados_manipulados$airway_disease == "Y"] <- 1  # Doença respiratória: sim (1)
dados_manipulados$airway_disease[dados_manipulados$airway_disease == "N"] <- 0  # Doença respiratória: não (0)

dados_manipulados$obesity[dados_manipulados$obesity == "Y"] <- 1  # Obesidade: sim (1)
dados_manipulados$obesity[dados_manipulados$obesity == "N"] <- 0  # Obesidade: não (0)

dados_manipulados$dlp[dados_manipulados$dlp == "Y"] <- 1  # Dislipidemia: sim (1)
dados_manipulados$dlp[dados_manipulados$dlp == "N"] <- 0  # Dislipidemia: não (0)

dados_manipulados$thyroid_disease[dados_manipulados$thyroid_disease == "Y"] <- 1  # Doença tireoidiana: sim (1)
dados_manipulados$thyroid_disease[dados_manipulados$thyroid_disease == "N"] <- 0  # Doença tireoidiana: não (0)

dados_manipulados$place[dados_manipulados$place == "A"] <- 0  # Local de residência A recebe valor 0
dados_manipulados$place[dados_manipulados$place == "B"] <- 1  # Local de residência B recebe valor 1
dados_manipulados$place[dados_manipulados$place == "C"] <- 2  # Local de residência C recebe valor 2


#forcar todas as colunas com os dados númericos (binários e contínuos) a serem reconhecidas como numéricas####

dados_manipulados <- dados_manipulados %>% 
  mutate_at(vars(cath:bp), as.numeric)


# Adiciona etiquetas descritivas para as variáveis (para facilitar a visualização em tabelas)####

dados_manipulados <- dados_manipulados %>%
  set_variable_labels(cath = "Diagnóstico de doença arterial coronariana",
    sex = "Sexo biológico masculino",
    age = "Idade (anos)",
    bmi = "IMC",
    current_smoker = "Fumante ativo",
    ex_smoker = "Ex-fumante",
    dm = "Diabetes",
    dlp = "Dislipidemia",
    thyroid_disease = "Doença tireoidiana",
    obesity = "Obesidade",
    airway_disease = "Doença respiratória",
    fh = "Histórico familiar (doença arterial coronariana)",
    htn = "Hipertensão arterial sistêmica",
    tg = "Triglicerídeos (mg/dL)",
    hdl = "HDL (mg/dL)",
    ldl = "LDL (mg/dL)",
    na = "Sódio sérico (mEq/L)",
    k = "Potássio sérico (mEq/L)",
    bp = "Pressão arterial (mmHg)",
    place = "Local de residência")


# Define o padrão de exibição das tabelas em português
theme_gtsummary_compact()
theme_gtsummary_language(language = "pt", iqr.sep = " - ", ci.sep = " - ")


#Regressões logísticas binárias univariadas (Diagnóstico de DAC)####

# Gera tabela de regressões univariadas e armazena no objeto "regressoes_univariadas"
regressoes_univariadas <- dados_manipulados %>%
  select(cath,
         age,
         place,
         sex,
         bmi,
         current_smoker,
         ex_smoker,
         airway_disease,
         dm,
         dlp,
         thyroid_disease,
         obesity,
         fh,
         htn,
         tg,
         hdl,
         ldl,
         na,
         k,
         bp) %>%
  tbl_uvregression(
    method = glm,
    y = cath,
    method.args = list(family = binomial),  # Especifica o modelo logístico binomial
    exponentiate = TRUE,                    # Exponencia os coeficientes para obter odds ratios
    hide_n = TRUE,                          # Oculta o número de observações na tabela
    formula = "{y} ~ {x}",                  # Fórmula para a regressão univariada
    pvalue_fun = function(x) {              # Formatação dos valores p
      if_else(
        is.na(x), 
        NA_character_,
        if_else(x < 0.001, format(x, digits = 2, scientific = TRUE), format(round(x, 3), scientific = F))
      )
    }) %>%
  add_q(method = "fdr") %>%      # Ajusta valores p para múltiplas comparações usando o método FDR
bold_p(q = TRUE, t = 0.05) %>%   # Destaca valores p corrigidos menores que 0,05
sort_p(q = TRUE) %>%             # Ordena as variáveis pelo valor p corrigido
modify_header(label ~ "Variáveis", p.value ~ "p", estimate ~ "OR", conf.low ~ "IC 95%", q.value = "pcorr") %>%
  as_flex_table()


#vizualizar tabela regressão univariada
regressoes_univariadas

#salvar planilha de regressões univariadas em formato ".docx" (arquivo do microsoft word)
save_as_docx(regressoes_univariadas, path = "Regressões univariadas DAC.docx")



#Regressões multivariadas####

#modelo completo

glm(cath ~ age + dm + htn + tg + k + bp + place, family = binomial, data = dados_manipulados) %>% 
  tbl_regression(exponentiate = TRUE,
                 pvalue_fun = function(x) {
                   if_else(is.na(x), NA_character_,
                           if_else(x < 0.001, format(x, digits = 2, scientific = TRUE), format(round(x, 3), scientific = F))   # Se o valor p for menor que 0,001, ele é formatado com notação científica, exibindo duas casas decimais.
                   )                                                                                                           # Se o valor p for maior ou igual a 0,001, ele é arredondado para três casas decimais e exibido sem notação científica.
                 }) %>%
  add_q(method = "fdr") %>%     # Ajusta valores p para múltiplas comparações
bold_p(q = TRUE, t = 0.05) %>%  # Destaca valores p corrigidos menores que 0,05
sort_p(q = TRUE) %>%            # Ordena as variáveis pelo valor p corrigido
modify_header(label ~ "Variáveis", p.value ~ "p", estimate ~ "OR", conf.low ~ "IC 95%", q.value = "pcorr") %>% 
  as_flex_table()


#modelo reduzido

regressao_multivariada <- glm(cath ~ age + dm + tg + k + bp + place, family = binomial, data = dados_manipulados) %>% 
  tbl_regression(
    exponentiate = TRUE,           # Exponencia os coeficientes para obter odds ratios (OR)
    pvalue_fun = function(x) {     # Formatação dos valores p
      if_else(is.na(x), NA_character_,
              if_else(x < 0.001, format(x, digits = 2, scientific = TRUE), format(round(x, 3), scientific = F))    # Se o valor p for menor que 0,001, ele é formatado com notação científica, exibindo duas casas decimais.
      )                                                                                                            # Se o valor p for maior ou igual a 0,001, ele é arredondado para três casas decimais e exibido sem notação científica.
    }
  ) %>%
  add_q(method = "fdr") %>%    # Ajusta valores p para múltiplas comparações usando o método FDR
bold_p(q = TRUE, t = 0.05) %>%  # Destaca valores p corrigidos menores que 0,05
sort_p(q = TRUE) %>%           # Ordena as variáveis pelo valor p corrigido
modify_header(label ~ "Variáveis", p.value ~ "p", estimate ~ "OR", conf.low ~ "IC 95%", q.value = "pcorr") %>% 
  as_flex_table()

# Salva a tabela do modelo reduzido em formato ".docx"
save_as_docx(regressao_multivariada, path = "Regressão multivariada DAC.docx")

