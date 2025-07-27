#Instalar pacotes####

install.packages("plotROC", dependencies = TRUE)
install.packages("ggtext", dependencies = TRUE)
install.packages("glue", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("openxlsx", dependencies = TRUE)
install.packages("pROC", dependencies = TRUE)
install.packages("ggstatsplot", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)

#Carregar pacotes####

library(plotROC)
library(ggtext)
library(glue)
library(ggplot2)
library(pROC)
library(openxlsx)
library(ggstatsplot)
library(tidyverse)

#carregar arquivo####

dados_curva_roc <- read.xlsx("Raw_Data.xlsx", sheet = 1, colNames = TRUE)

#alterar dados binários para categóricos####
dados_curva_roc$Appendicitis_categorico[dados_curva_roc$Appendicitis == 1] <- "Yes"
dados_curva_roc$Appendicitis_categorico[dados_curva_roc$Appendicitis == 0] <- "No"


#avaliar se os níveis dos controles são maiores ou menores que os casos####
#Isso serve para ver a direção:
#direção ">" (controles têm valores maiores que os casos)
#direção "<" (controles têm valores menores que os casos)
#Em nosso exemplo a direção é "<" porque os níveis são maiores em casos do que controles

ggbetweenstats( # Cria um gráfico de boxplot para comparar a idade entre os grupos
  data = dados_curva_roc, # Data frame com os dados a serem plotados
  x = Appendicitis_categorico, # Variável que define os grupos no eixo x
  y = CRP, # Variável contínua (idade) no eixo y
  ggsignif.args = list(textsize = 4.3, tip_length = 0.01), # Ajusta o tamanho do texto e o comprimento das linhas de destaque para comparações significativas
  type = "nonparametric", # Especifica que o teste estatístico deve ser paramétrico
  var.equal = FALSE, # Assume variâncias iguais para o teste paramétrico
  k = 2, # Número de grupos a serem comparados (no caso, dois grupos)
  outlier.tagging = FALSE, # Não marca os outliers no gráfico
  centrality.type = "nonparametric", # Define o tipo de centralidade como paramétrica (média)
  centrality.plotting = TRUE, # Plota a centralidade (média) no gráfico
  centrality.point.args = list(size = 3, color = "black"), # Ajusta o tamanho e a cor do ponto que representa a centralidade
  centrality.label.args = list(size = 4, nudge_x = 0.5, segment.linetype = 4), # Ajusta o tamanho do rótulo, deslocamento horizontal e o tipo de linha dos segmentos de rótulo
  xlab = "Appendicitis", # Rótulo do eixo x
  ylab = "CRP (mg/dL)", # Rótulo do eixo y
  ggstatsplot.layer = FALSE, # Remove a camada de estatísticas do gráfico
  pairwise.comparisons = FALSE, # Realiza comparações pairwise entre os grupos
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

#valor de p para avaliar a significância estatística da curva ROC (aceitar ou rejeitar H0 - a hipótese que o AUC = 0.5)####

#regressão logística - Anova do Modelo com o marcador vs modelo nulo = valor de p para aceitar ou rejeitar H0
#o resultado sera incluido no grafico
modelo_nulo <- glm(Appendicitis ~ 1, family = binomial, data = dados_curva_roc)

modelo_preditor <- glm(Appendicitis ~ CRP, family = binomial, data = dados_curva_roc)


resultado_anova_glm <- anova(modelo_nulo, modelo_preditor, test="Chisq")

resultado_anova_glm_valor_p <- resultado_anova_glm$`Pr(>Chi)`[2]

resultado_anova_glm_valor_p <- ifelse(resultado_anova_glm_valor_p < 0.0099, sprintf("%.2e", resultado_anova_glm_valor_p), signif(resultado_anova_glm_valor_p, 2))

resultado_anova_glm_valor_p_formated <- gsub("e", " x 10<sup>", resultado_anova_glm_valor_p)

#calcular poder estatístico da curva ROC com os dados reais####
#Criar curva ROC
curva_roc_power_grafico <- roc(data = dados_curva_roc, response = Appendicitis,
                       predictor = CRP, ci = T, direction = "<")



#fórmula para arredondamento (duas casas decimais e utilizando a vírgula para casas decimais)####

arred_fun <- function(x){
  x <- scales::number(x, decimal.mark = ",",
                      big.mark = ".", accuracy = 0.01)
  return(x)
}

#calcular o poder estatístico####
#o resultado sera incluido no grafico
power<- power.roc.test(curva_roc_power_grafico, sig.level = 0.05, alternative = "two.sided")
power_value<- arred_fun(power$power*100)



#criar e salvar gráfico curva ROC (usando dados analisados no R)####
#gerar resultados de AUC com IC 95%
#os resultados serao incluidos no grafico

auc<- pROC::ci.auc(curva_roc_power_grafico,
                   method = "delong")
auc_ci_low_value <- arred_fun(auc[1])
auc_value <- arred_fun(auc[2])
auc_ci_high_value <- arred_fun(auc[3])

#gerar resultados  sensibilidade e especificidade com IC 95%
#os resultados serao incluidos no grafico
youden_cutoff<- pROC::coords(curva_roc_power_grafico, x = "best",
             ret = c("threshold", "specificity", "sensitivity"),
             best.method = "youden")


youden_cutoff_sen_spe_values <- pROC::ci.coords(curva_roc_power_grafico,
                ret = c("threshold" ,"specificity",
                        "sensitivity", "npv", "ppv"),
                x = youden_cutoff$threshold, input = "threshold", best.method = "youden")


#valores de especificidade
#o resultado sera incluido no grafico
specificity_ci_low_value<- arred_fun(youden_cutoff_sen_spe_values[["specificity"]][1]*100)
specificity_value<- arred_fun(youden_cutoff_sen_spe_values[["specificity"]][2]*100)
specificity_ci_high_value<- arred_fun(youden_cutoff_sen_spe_values[["specificity"]][3]*100)

#valores de sensiblidade
#o resultado sera incluido no grafico
sensitivity_ci_low_value<- arred_fun(youden_cutoff_sen_spe_values[["sensitivity"]][1]*100)
sensitivity_value<- arred_fun(youden_cutoff_sen_spe_values[["sensitivity"]][2]*100)
sensitivity_ci_high_value<- arred_fun(youden_cutoff_sen_spe_values[["sensitivity"]][3]*100)


#valores preditivos positivos
#o resultado sera incluido no grafico
vpp_ci_low_value<- arred_fun(youden_cutoff_sen_spe_values[["ppv"]][1]*100)
vpp_value<- arred_fun(youden_cutoff_sen_spe_values[["ppv"]][2]*100)
vpp_ci_high_value<- arred_fun(youden_cutoff_sen_spe_values[["ppv"]][3]*100)


#valores preditivos negativos
#o resultado sera incluido no grafico
vpn_ci_low_value<- arred_fun(youden_cutoff_sen_spe_values[["npv"]][1]*100)
vpn_value<- arred_fun(youden_cutoff_sen_spe_values[["npv"]][2]*100)
vpn_ci_high_value<- arred_fun(youden_cutoff_sen_spe_values[["npv"]][3]*100)


#criar o gráfico em si
tiff(filename = "Curva ROC.tiff", res = 300, width = 25, height = 17, units = "cm", compression = "lzw") # Define a configuração para salvar o gráfico de boxplot em formato TIFF, com resolução de 300 dpi, largura de 20 cm e altura de 16 cm, usando compressão LZW
ggroc(curva_roc_power_grafico)+
  geom_abline(slope = 1, intercept = 1, color = "black", linetype = "dashed")+
  geom_richtext(x = -0.25, y = .20, size = 4,
  label = glue("AUC = {auc_value} ({auc_ci_low_value} - {auc_ci_high_value})<br>
                <i>p</i> = {resultado_anova_glm_valor_p_formated}</sup><br>
                Especificidade = {specificity_value}% ({specificity_ci_low_value}% - {specificity_ci_high_value}%)<br>
                Valor preditivo negativo = {vpn_value}% ({vpn_ci_low_value}% - {vpn_ci_high_value}%)<br>
                Sensibilidade = {sensitivity_value}% ({sensitivity_ci_low_value}% - {sensitivity_ci_high_value}%)<br>
                Valor preditivo positivo = {vpp_value}% ({vpp_ci_low_value}% - {vpp_ci_high_value}%)<br>
                Poder estatístico = {power_value}%"))+
  geom_point(aes(y = youden_cutoff$sensitivity, x = youden_cutoff$specificity), size = 1.5, color = "red")+
  annotate(geom = "text", label = arred_fun(youden_cutoff$threshold),
           y = youden_cutoff$sensitivity, x = youden_cutoff$specificity,
           size = 4, hjust = -0.05, vjust = 1.5)+
  scale_x_reverse("Especificidade", breaks = seq(0, 1, by = 0.05), labels = scales::label_percent()) +
  scale_y_continuous("Sensibilidade", breaks = seq(0, 1, by = 0.05), labels = scales::label_percent())+
  theme_bw()+
  theme(text = element_text(size=13), axis.title.y = element_text(size = 12, colour = "black", face = "plain"), axis.text.x = element_text(size = 12, colour = "black"), axis.text = element_text(size = 13, colour = "black"), axis.text.y = element_text(size = 12, colour = "black"), plot.caption = element_text(size = 13, colour = "black"), plot.subtitle = element_text(size = 13, colour = "black"))
dev.off()