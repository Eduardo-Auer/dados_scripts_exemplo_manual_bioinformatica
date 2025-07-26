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
install.packages("ggplot2", dependencies = TRUE)
install.packages("ggstatsplot", dependencies = TRUE)

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
library(ggstatsplot)

#Selecionar pasta de trabalho

#ler arquivo de dados/planilha####

dados <- read.xlsx("Z-Alizadeh sani dataset.xlsx", sheet = 1, colNames = T)

# sheet = 1 , indica que a aba  de interesse a ser utilizada é a 1 
# colNames = T, TRUE indica que a primeira linha do arquivo contém os nomes das colunas


#Manipular dados####

#adequar nomes de colunas
dados_manipulados <- dados %>% clean_names() 

# clean_names() transforma os nomes das colunas para garantir que sejam válidos em R. 
# Isso inclui converter os caracteres para minúsculas, substituir espaços e caracteres especiais por “_”. 

#selecionar colunas de interesse
dados_manipulados <- dados_manipulados %>%
  select(cath,                  # dac, doenca arterial coronariana        
         sex,                   # sexo 
         place,                 # local de origem (ex. colonias menonitas)
         current_smoker,        # fumante
         ex_smoker,             # ex fumante
         airway_disease,        # doencas de vias aereas
         dm,                    # diabetes mellitus
         dlp,                   # dislipidemia
         thyroid_disease,       # doenca da tireoide (hipertireoidismo...)
         obesity,               # obesidade
         fh,                    # historico familiar
         htn,                   # hipertensao
         age,                   # idade
         bmi,                   # indice de massa corporal
         tg,                    # triglicerideos
         hdl,                   # high-Density Lipoprotein, ou colesterol "bom".
         ldl,                   # low-Density Lipoprotein, ou colesterol "ruim".
         na,                    # sodio
         k,                     # potassio
         bp)                    # pressão arterial (Blood Pressure)



#Categorizar e substituir valores

dados_manipulados <- dados_manipulados %>%
  mutate_at(vars(cath),                                  #mutate_at() é usada para aplicar uma transformação em colunas específicas de um data frame. ".vars(cath)" indica que a transformação será aplicada à coluna cath.
            ~str_replace(., "Cad", "DAC")) %>%           #A função str_replace(., "Cad", "DAC") substitui todas as ocorrências de "Cad" por "DAC" 
  mutate_at(vars(cath),
            ~str_replace(., "Normal", "Controle")) %>%
  mutate_at(vars(sex),
            ~str_replace(., "Fmale", "Feminino")) %>%
  mutate_at(vars(sex),
            ~str_replace(., "Male", "Masculino")) %>% 
  mutate_at(vars(airway_disease,
                 obesity,
                 dlp,
                 thyroid_disease),
            ~str_replace(., "Y", "Sim")) %>%
  mutate_at(vars(airway_disease,
                 obesity,
                 dlp,
                 thyroid_disease),
            ~str_replace(., "N", "Não"))%>% 
  mutate_at(vars(current_smoker,
                 ex_smoker,
                 dm,
                 fh,
                 htn),
            ~replace(., . == 1, "Sim")) %>%
  mutate_at(vars(current_smoker,
                 ex_smoker,
                 dm,
                 fh,
                 htn),
            ~replace(., . == 0, "Não"))

  
#forcar todas as colunas com os dados númericos (binários e contínuos) a serem reconhecidas como numéricas####
#A função as.numeric() é usada para garantir que os valores sejam tratados como números. 
#Isso é importante para realizar as análises estatísticas.

dados_manipulados <- dados_manipulados %>% 
  mutate_at(vars(age:bp), as.numeric)


#eitquetas para as variáveis####
#é possível criar etiquetas para substituir o nome das colunas por nomes que você deseja que apareçam na tabela de resultados

dados_manipulados <- dados_manipulados %>%
  set_variable_labels(cath = "Diagnóstico de doença arterial coronariana",
                      sex = "Sexo biológico",
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
                      k = "Postássio sérico (mEq/L)",
                      bp = "Pressão arterial (mmHg)",
                      place = "Local de residência")


#padrão da tabela em português####

theme_gtsummary_compact()    #aplica um tema compacto, resultando em tabelas com uma aparência mais compacta.

theme_gtsummary_language(language = "pt", iqr.sep = " - ", ci.sep = " - ") #ajusta as configurações de idioma 

# language = "pt" define o idioma para português. 
# iqr.sep = " - " define o separador para o intervalo interquartil. 
# ci.sep = " - " define o separador para o intervalo de confiança.


#tabela de estatísticas sumárias dos indivíduos menonitas por colônias####

#teste de normalidade - shapiro-wilk para todas as variáveis e por agrupamento 
 
shapiro_test <- dados_manipulados %>%
  select(cath, age, bmi, tg, hdl, ldl, na, k, bp) %>%    # Seleciona as colunas de interesse do data frame (cath e as variáveis numéricas)
  drop_na(cath) %>%                                      # Remove linhas com valores ausentes (NA) na variável cath.
  group_by(cath) %>%                                     # Agrupa os dados por 'cath', permitindo a realização do teste por grupo (DAC, controle).
  shapiro_test(age, bmi, tg, hdl, ldl, na, k, bp) %>%    # Aplica o teste de Shapiro-Wilk para verificar a normalidade das variáveis numéricas selecionadas
  arrange(variable) %>%                                  # Organiza o resultado em ordem alfabética das variáveis
  filter(p >= 0.05) %>%                                  # Filtra apenas as variáveis que têm p-valor >= 0.05, indicando que seguem uma distribuição normal (variáveis paramétricas)
  mutate_at(vars(statistic, p), ~signif(.,2))            # Arredonda os valores das colunas P, p_bonferroni, p_fdr e p_monte_carlo para duas casas significativas
view(shapiro_test)                                       # mostra os resultados dos testes de shapiro-wilk

#variáveis não-paramétricas: imc, pressão arterial, hdl, ldl, na, k (p < 0.05 em um dos grupos)
#variáveis paramétricas: idade (p > 0.05 nos dois grupos)


#teste F para testar homogeneidade de variâncias entre os grupos####
#teste estatístico paramétrico (para 2 grupos)

var.test(age ~ cath, data = dados_manipulados)   # ~ indica que age é a variável analisada e cath é a variável de agrupamento.

# p > 0.05 - Aceita a hipotese nula (H0) ->  os grupos tem homogeneidade de variância para a idade


#tabelas de estatísticas sumárias e comparação estatística entre os grupos####

descricao_indiviuos_por_grupo <- dados_manipulados %>%
  select(  # Seleciona as variáveis do data frame 'dados_manipulados'
    cath,
    place,
    sex,
    current_smoker,
    ex_smoker,
    airway_disease,
    dm,
    dlp,
    thyroid_disease,
    obesity,
    fh,
    htn,
    age,
    bmi,
    tg,
    hdl,
    ldl,
    na,
    k,
    bp
  ) %>%                 # Neste ponto caso fosse de interesse filtrar  dados para, por exemplo, incluir apenas indivíduos com idade >= 18, poderia inserir no comando por exemplo  filter(age >=18)
  tbl_summary(          # Cria uma tabela resumo das estatísticas, mais infos em - https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html
    type = list(        # Define o tipo das variáveis para a sumarização
      c(sex,
        place) ~ "categorical",   # Variável categórica
      c(current_smoker,
        ex_smoker,
        airway_disease,
        dm,
        dlp,
        thyroid_disease,
        obesity,
        htn
      ) ~ "dichotomous",   # Variáveis dicotômicas (binárias)
      c(age,
        bmi,
        tg,
        hdl,
        ldl,
        na,
        k,
        bp) ~  "continuous2"),  #variáveis contínuas
    statistic = list(           # Define as estatísticas a serem calculadas para cada tipo de variável
      c(age) ~ c("{min} - {max}", "{mean} ({sd})"), # Estatísticas para idade: mínimo-máximo e média-desvio padrão
      c(bmi,
      tg,
      hdl,
      ldl,
      na,
      k,
      bp) ~  c("{min} - {max}", "{median} ({p25} - {p75})"), # Estatísticas para outras variáveis continua: #Exibe a mediana e o intervalo interquartil (25º e 75º percentis)
      all_categorical() ~ c("{n} / {N} ({p}%)")              # Exibe a contagem e % de observações para cada categoria
    ),
    digits = list(all_continuous2() ~ c(2, 2),      # indica duas casas decimais para cada uma dessas variáveis.
                  all_categorical() ~ c(0, 0, 1)),  # o primeiro 0 é para o número de casas decimais da contagem de observações (n),  o segundo para a contagem total de observações (N),  o 1 é para a porcentagem (p%).
    missing = "no",                                 # Define que valores ausentes não devem ser exibidos
    value = list(     
      c(current_smoker,
        ex_smoker,
        airway_disease,
        dm,
        dlp,
        thyroid_disease,
        obesity,
        htn) ~ "Sim"   # Apenas reporta na tabela os valores "Sim" nos dados dicotômicos
    ),
    by = cath   # Agrupa os resultados pela variável 'cath'
  ) %>%
  add_p(        # Adiciona p-valores de testes estatísticos ao comparar os grupos
    test = list(c(age) ~ "t.test",  # Teste t para a idade
                c(bmi,
                  tg,
                  hdl,
                  ldl,
                  na,
                  k,
                  bp) ~ "wilcox.test",                  # Teste de Wilcoxon (Mann-Whitney) para as outras variáveis contínuas
      all_categorical() ~ "fisher.test"),               # Teste de Fisher para variáveis categóricas
    test.args = list(c(age) ~ list(var.equal = TRUE)),  # Define argumentos para o teste t, assumindo homogeneidade de variâncias entre os grupos para a variável "age"
    pvalue_fun = function(x) {                          # Formata os valores de p
      if_else(is.na(x), NA_character_, if_else(
        x < 0.001,
        format(x, digits = 2, scientific = TRUE),
        format(round(x, 3), scientific = F)
      ))
    }
  ) %>%
  add_q(method = "fdr") %>%                    # cria uma coluna de valores de p corrigidos usando o método FDR (False Discovery Rate)
  sort_p(q = TRUE) %>%                         # Ordena as variáveis de acordo com o menor valor de p corrigido
  bold_p(q = TRUE) %>%                         # Aplica negrito aos valores de p corrigidos quando menores que 0.05
  modify_header(label ~ "Dados clínicos",      # Modifica o cabeçalho da tabela
                p.value = "p",                 # Renomeia a coluna p.value para "p"
                q.value = "pcorr") %>%         # Renomeia a coluna q.value para "pcorr"
  separate_p_footnotes() %>%                   # Separa as notas de rodapé
  as_flex_table()                              # Converte a tabela para o formato flexível de tabelas para documentos Word


descricao_indiviuos_por_grupo #reporta a tabela de regressão univariada salva na aba "viewer" do Rstudio


save_as_docx(descricao_indiviuos_por_grupo, path = "Estatísticas entre os grupos.docx") #salva a tabela tabela de regressão univariada em formato ".docx"


#Teste de fisher pairwise####
#Teste para avaliar qual das comparações entre os grupos é significativa com a sua variável de interesse (no caso o seu fenótipo)
#Teste usado quando um teste de fisher anterior feito com mais de dois grupos foi significativo

#criação da tabela de contigência
tabela_contingencia <- dados_manipulados %>% select(cath, place) %>% drop_na(cath, place) %>% group_by(place) %>% count(cath)
tabela_contingencia <- xtabs(n ~ place + cath, tabela_contingencia)

#teste de exato de fisher pairwise com correção para múltipla testagem (método FDR)

pairwise_fisher_test(tabela_contingencia, p.adjust.method = "fdr")


#exemplo de gráfico boxplot para comparar variáveis contínuas entre grupos####

#idade entre os grupos DAC e controles (dado paramétrico)

tiff(filename = "Box-plot idade entre os grupos.tiff", res = 300, width = 20, height = 16, units = "cm", compression = "lzw") # Define a configuração para salvar o gráfico em formato TIFF, com resolução de 300 dpi, largura de 20 cm e altura de 16 cm, usando compressão LZW
ggbetweenstats( # Cria um gráfico de boxplot para comparar a idade entre os grupos
  data = dados_manipulados, # Data frame com os dados a serem plotados
  x = cath, # Variável que define os grupos no eixo x
  y = age, # Variável contínua (idade) no eixo y
  ggsignif.args = list(textsize = 4.3, tip_length = 0.01), # Ajusta o tamanho do texto e o comprimento das linhas de destaque para comparações significativas
  type = "parametric", # Especifica que o teste estatístico deve ser paramétrico
  var.equal = TRUE, # Assume variâncias iguais para o teste paramétrico
  k = 2, # número de casas decimais após a vírgula
  outlier.tagging = FALSE, # Não marca os outliers no gráfico
  centrality.type = "parametric", # Define o tipo de centralidade como paramétrica (média)
  centrality.plotting = TRUE, # Plota a centralidade (média) no gráfico
  centrality.point.args = list(size = 3, color = "black"), # Ajusta o tamanho e a cor do ponto que representa a centralidade
  centrality.label.args = list(size = 4, nudge_x = 0.5, segment.linetype = 4), # Ajusta o tamanho do rótulo, deslocamento horizontal e o tipo de linha dos segmentos de rótulo
  xlab = "Grupos", # Rótulo do eixo x
  ylab = "Idade (anos)", # Rótulo do eixo y
  ggstatsplot.layer = FALSE, # Remove a camada de estatísticas do gráfico
  pairwise.comparisons = TRUE, # Realiza comparações pairwise entre os grupos
  pairwise.display = "significant", # Exibe apenas comparações significativas
  p.adjust.method = "fdr", # Ajusta os valores p usando o método FDR (False Discovery Rate)
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.15), size = 1.5, stroke = 0), # Ajusta a posição dos pontos, o tamanho e a espessura da borda dos pontos
  violin.args = list(width = 0, linewidth = 0) # Define a largura e a espessura da linha do gráfico de violino como zero
) +
  ggplot2::scale_color_manual(values = c("black", "black")) + # Define a cor manual dos elementos do gráfico (preto para todas as categorias)
  xlab(NULL) + # Remove o rótulo do eixo x
  labs(caption = NULL) + # Remove a legenda (caption) do gráfico
  theme( # Ajusta o tema do gráfico, incluindo tamanhos e cores do texto
    text = element_text(size=13), # Define o tamanho do texto no gráfico
    axis.title.y = element_text(size = 12, colour = "black", face = "plain"), # Ajusta o tamanho, cor e estilo do título do eixo y
    axis.text.x = element_text(size = 12, colour = "black"), # Ajusta o tamanho e a cor do texto do eixo x
    axis.text = element_text(size = 13, colour = "black"), # Ajusta o tamanho e a cor do texto dos eixos
    axis.text.y = element_text(size = 12, colour = "black"), # Ajusta o tamanho e a cor do texto do eixo y
    plot.caption = element_text(size = 13, colour = "black"), # Ajusta o tamanho e a cor do texto da legenda (caption)
    plot.subtitle = element_text(size = 13, colour = "black") # Ajusta o tamanho e a cor do texto do subtítulo do gráfico
  )
dev.off() # Fecha o dispositivo gráfico, salvando o arquivo TIFF


#pressão sanguínea entre os grupos DAC e controles (dado não-paramétrico)

tiff(filename = "Box-plot pressão sanguínea entre os grupos.tiff", res = 300, width = 20, height = 16, units = "cm", compression = "lzw") # Define a configuração para salvar o gráfico de boxplot em formato TIFF, com resolução de 300 dpi, largura de 20 cm e altura de 16 cm, usando compressão LZW
ggbetweenstats( # Cria um gráfico de boxplot para comparar a pressão sanguínea entre os grupos
  data = dados_manipulados, # Data frame com os dados a serem plotados
  x = cath, # Variável que define os grupos no eixo x
  y = bp, # Variável contínua (pressão sanguínea) no eixo y
  ggsignif.args = list(textsize = 4.3, tip_length = 0.01), # Ajusta o tamanho do texto e o comprimento das linhas de destaque para comparações significativas
  type = "nonparametric",  # Especifica que o teste estatístico deve ser não paramétrico (ex: teste de Wilcoxon)
  k = 2,  # Número de grupos a serem comparados (no caso, dois grupos)
  outlier.tagging = FALSE, # Não marca os outliers no gráfico
  centrality.type = "nonparametric", # Define o tipo de centralidade como não paramétrica (mediana)
  centrality.plotting = TRUE, # Plota a centralidade (mediana) no gráfico
  centrality.point.args = list(size = 3, color = "black"), # Ajusta o tamanho e a cor do ponto que representa a centralidade
  centrality.label.args = list(size = 4, nudge_x = 0.5, segment.linetype = 4), # Ajusta o tamanho do rótulo, deslocamento horizontal e o tipo de linha dos segmentos de rótulo
  xlab = "Grupos", # Rótulo do eixo x
  ylab = "Pressão sanguínea", # Rótulo do eixo y
  ggstatsplot.layer = FALSE, # Remove a camada de estatísticas do gráfico
  pairwise.comparisons = TRUE, # Realiza comparações pairwise entre os grupos
  pairwise.display = "significant", # Exibe apenas comparações significativas
  p.adjust.method = "fdr", # Ajusta os valores p usando o método FDR (False Discovery Rate)
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.15), size = 1.5, stroke = 0), # Ajusta a posição dos pontos, o tamanho e a espessura da borda dos pontos
  violin.args = list(width = 0, linewidth = 0) # Define a largura e a espessura da linha do gráfico de violino como zero
) +
  ggplot2::scale_color_manual(values = c("black", "black")) + # Define a cor manual dos elementos do gráfico (preto para todas as categorias)
  xlab(NULL) + # Remove o rótulo do eixo x
  labs(caption = NULL) +             # Remove a legenda (caption) do gráfico
  theme(               # Ajusta o tema do gráfico, incluindo tamanhos e cores do texto
    text = element_text(size=13), # Define o tamanho do texto no gráfico
    axis.title.y = element_text(size = 12, colour = "black", face = "plain"), # Ajusta o tamanho, cor e estilo do título do eixo y
    axis.text.x = element_text(size = 12, colour = "black"),  # Ajusta o tamanho e a cor do texto do eixo x
    axis.text = element_text(size = 13, colour = "black"),    # Ajusta o tamanho e a cor do texto dos eixos
    axis.text.y = element_text(size = 12, colour = "black"),  # Ajusta o tamanho e a cor do texto do eixo y
    plot.caption = element_text(size = 13, colour = "black"), # Ajusta o tamanho e a cor do texto da legenda (caption)
    plot.subtitle = element_text(size = 13, colour = "black") # Ajusta o tamanho e a cor do texto do subtítulo do gráfico
  )
dev.off() # Fecha o dispositivo gráfico, salvando o arquivo TIFF

#Gráfico de correlação (idade e pressão sanguínea)####

tiff(filename = "Correlação entre pressão sanguínea e idade.tiff", res = 300, width = 20, height = 16, units = "cm", compression = "lzw") # Define a configuração para salvar o gráfico de correlação em formato TIFF, com resolução de 300 dpi, largura de 20 cm e altura de 16 cm, usando compressão LZW
ggscatterstats( # Cria um gráfico de dispersão para mostrar a correlação entre idade e pressão sanguínea
  data = dados_manipulados, # Data frame com os dados a serem plotados
  x = age, # Variável contínua (idade) no eixo x
  y = bp, # Variável contínua (pressão sanguínea) no eixo y
  type = "nonparametric", # Escolhe se a correlação é não-paramétrica (Spearman)
  k = 2, # Ajusta o número de dígitos dos resultados
  marginal = FALSE, # Exclui histogramas marginais que normalmente são exibidos ao lado dos eixos
  results.subtitle = TRUE, # Mantém as estatísticas de correlação no topo do gráfico
  point.args = list(size = 2, color = "black"), # Configura o tamanho e a cor dos pontos de dados
  xlab = "Idade (Anos)", # Rótulo do eixo x
  ylab = "Pressão sanguínea" # Rótulo do eixo y
) +
  theme( # Ajusta o tema do gráfico, incluindo tamanhos e cores do texto
    text = element_text(size=13), # Define o tamanho do texto no gráfico
    axis.title.y = element_text(size = 12, colour = "black", face = "plain"), # Ajusta o tamanho, cor e estilo do título do eixo y
    axis.title.x = element_text(size = 12, colour = "black", face = "plain"), # Ajusta o tamanho, cor e estilo do título do eixo x
    axis.text.x = element_text(size = 12, colour = "black"),  # Ajusta o tamanho e a cor do texto do eixo x
    axis.text = element_text(size = 13, colour = "black"),    # Ajusta o tamanho e a cor do texto dos eixos
    axis.text.y = element_text(size = 12, colour = "black"),  # Ajusta o tamanho e a cor do texto do eixo y
    plot.caption = element_text(size = 14, colour = "black"), # Ajusta o tamanho e a cor do texto da legenda (caption)
    plot.subtitle = element_text(size = 14, colour = "black") # Ajusta o tamanho e a cor do texto do subtítulo do gráfico
  )
dev.off() # Fecha o dispositivo gráfico, salvando o arquivo TIFF