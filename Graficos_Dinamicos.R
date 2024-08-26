
library(readr)
library(ggplot2)
library(gganimate)
library(tidyverse)
library(RColorBrewer)

# Gráficos de barras 1
#####

#Preparando o banco
dados <- read.csv("aids14a23.csv", sep = ";",dec=".")

view(dados)

dados$ANT_REL_CA <- factor(dados$ANT_REL_CA)

dados <- dados %>%
  mutate(ANT_REL_CA = recode(ANT_REL_CA,
         "10" = "Homossexual",
         "20" = "Bissexual",
         "30" = "Heterossexual"
         ))
dados <- dados %>%
  mutate(CS_SEXO = recode(CS_SEXO,
         "F" = "Feminino",
         "M" = "Masculino",
                             
  ))

#Criando o gráfico por ggplot2
ggplot(dados, aes(x= ANT_REL_CA, fill = CS_SEXO)) + 
  geom_bar(width = 0.5, position = position_dodge() ) +
  labs(title = "Total de infectados por genero (Ano: {closest_state})",
       x = "Genero", y = "Quantidade de observações",
       fill = "Sexo de nascimento")  +
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x = element_text(size = 10),
         panel.grid.major = element_line(size = 0.5,
                                         color = "lightblue",
                                         linetype = "dashed"),
         
         panel.grid.minor = element_line(size = 0.5,
                                         color = "lightblue",
                                         linetype = "dashed")) +
  scale_fill_manual(values = c("#1ab2a3","#046434")) +
 
#Parte do gganimate
  transition_states(
    dados$NU_ANO,
    transition_length = 5,
    state_length = 5
  )

anim_save("grafico de infectados por genero.gif")


#####
# Gráficos de barras 2
#####

#Preparando o banco
dados <- read.csv("aids14a23.csv", sep = ";",dec=".")

view(dados)

dados$ANTRELSE_N <- factor(dados$ANTRELSE_N)

dados <- dados %>%
  mutate(ANTRELSE_N = recode(ANTRELSE_N,
                             "1" = "Homens",
                             "2" = "Mulheres",
                             "3" = "Ambos"
  ))

#Criando o gráfico por ggplot2
ggplot(dados, aes(x= ANTRELSE_N, fill = ANTRELSE_N)) + 
  geom_bar(position=position_dodge(0.5) ) +
  labs(title = "Total de infectados por relação (Ano: {closest_state})",
       x = "", y = "Quantidade relatada",
       fill = "Sexo dos(as) parceiros(as)")  +
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x = element_text(size = 12),
         panel.grid.major = element_line(size = 0.5,
                                         color = "lightblue",
                                         linetype = "dashed"),
         
         panel.grid.minor = element_line(size = 0.5,
                                         color = "lightblue",
                                         linetype = "dashed") 
  ) +
  scale_fill_manual(values = c("#146cb4","#1ab2a3","#046434")) +
  
  
  #Parte do gganimate
  transition_states(
    dados$NU_ANO,
    transition_length = 10,
    state_length = 1
  )

anim_save("grafico quantidade X tipo relação.gif")
##### 
#####