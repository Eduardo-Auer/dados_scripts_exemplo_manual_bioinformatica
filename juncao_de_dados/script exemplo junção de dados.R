#instalar pacotes####
install.packages("openxlsx", dependencies = TRUE)
install.packages("janitor", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)

#Carregar pacotes####
library(openxlsx)
library(janitor)
library(tidyverse)

#Selecionar diretório de trabalho

#Carregar e selecionar dados da aba do local A#####

# Lê os dados da primeira aba do arquivo Excel
dados_local_a <- read.xlsx("arquivo_exemplo_juncao.xlsx", sheet = 1, colNames = T)

# Limpa e padroniza os nomes das colunas (deixando em minúsculas e substituindo espaços por _)
dados_local_a_manipulados <- dados_local_a %>% clean_names()

# Seleciona apenas as colunas de interesse da planilha do local A

dados_local_a_manipulados <- dados_local_a_manipulados %>%
  select(cath,                # Doeça arterial coronariana
         sex,                 # Sexo
         place,               # Local
         current_smoker,      # Fumante atual
         ex_smoker,           # Ex-fumante
         airway_disease,      # Doença das vias aéreas
         dm,                  # Diabetes Mellitus
         dlp,                 # Dislipidemia
         thyroid_disease,     # Doença da tireoide
         obesity,             # Obesidade
         fh,                  # Histórico familiar
         htn,                 # Hipertensão
         age,                 # Idade
         bmi,                 # Índice de Massa Corporal
         tg,                  # Triglicerídeos
         hdl,                 # Colesterol HDL
         ldl,                 # Colesterol LDL
         na,                  # Sódio
         k,                   # Potássio
         bp)                  # Pressão arterial


#Carregar e selecionar dados da aba do local B#####

dados_local_b <- read.xlsx("arquivo_exemplo_juncao.xlsx", sheet = 2, colNames = T)

dados_local_b_manipulados<- dados_local_b %>% clean_names()

dados_local_b_manipulados<- dados_local_b_manipulados %>%
  select(cath,
         sex,
         place,
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
         bp)

#Carregar e selecionar dados da aba do local C#####

dados_local_c <- read.xlsx("arquivo_exemplo_juncao.xlsx", sheet = 3, colNames = T)

dados_local_c_manipulados <- dados_local_c %>% clean_names()

dados_local_c_manipulados <- dados_local_c_manipulados %>%
  select(cath,
         sex,
         place,
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
         bp)


# Junta as três planilhas em uma única, combinando as linhas (empilhamento) ####

dados_manipulados_locais_juntos <- rbind(dados_local_a_manipulados,
                                         dados_local_b_manipulados,
                                         dados_local_c_manipulados)


# Remover os objetos que não são mais necessários para liberar memória ####
rm(dados_local_a,
   dados_local_a_manipulados,
   dados_local_b,
   dados_local_b_manipulados,
   dados_local_c,
   dados_local_c_manipulados)

#liberar espaço na memória RAM (após a remoção dos dados)###
# Executa o garbage collector para liberar memória
gc()

# salvar dados após a junção em planilha do excel####

write.xlsx(dados_manipulados_locais_juntos, file = "dados_locais_juntos.xlsx")
