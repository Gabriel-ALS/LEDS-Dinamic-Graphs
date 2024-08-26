library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)

datinha <- read.csv("aids14a23.csv", sep = ";", dec = ".")
attach(datinha)

# Criar uma nova coluna para o mês e ano

datinha$DT_NOTIFIC <- as.Date(DT_NOTIFIC)
datinha$MES_ANO <- floor_date(datinha$DT_NOTIFIC, "month")

#-Serie-temporal-numero-casos-por-sexo-----

datinha <- datinha %>%
  mutate(SEXO = case_when(
    CS_SEXO == "M" ~ "Masculino",
    CS_SEXO == "F" ~ "Feminino",
    TRUE ~ "Outro"
  ))

datinha$SEXO <- factor(datinha$SEXO, levels = c("Masculino", "Feminino"))

attach(datinha)
# Agregar os dados por mês e sexo para obter o número total de casos por grupo
dados_aggregated <- datinha %>%
  group_by(MES_ANO, SEXO) %>%
  summarise(total_casos = sum(ORIGEM), percentual = sum(ORIGEM) / 3407)

# Criar o gráfico de série temporal
p<- ggplot(dados_aggregated, aes(x = MES_ANO, y = total_casos, color = SEXO)) +
geom_line(size = 1.0) +
geom_point() +
labs(title = "Número de casos absolutos mensais de AIDS no Espírito Santo de 2014 a 2023",
     subtitle = "Indivíduos com idade a partir de 13 anos por sexo.",
x = "Ano",
y = "Número de casos",
color = "Sexo") +
theme_minimal() + scale_color_manual(values=c('#046434','#146cb4'))
p

# Adicionar animação
anim <- p +
transition_reveal(MES_ANO)
# Ajustar a velocidade da animação e lrgura
anim_gif <- animate(anim, width = 700, duration = 40, fps = 10)  # 40 segundos de duração, 10 frames por segundo
# Salvar a animação como um GIF
anim_save("casos_AIDS_sexo.gif", anim_gif)
anim

#-Serie-temporal-raça-----

datinha$CS_RACA <- factor(datinha$CS_RACA, levels = c(1,2,3,4,5,9),
                                 labels = c("Branca","Preta",
                                            "Amarela","Parda",
                                            "Indigena","Ignorado"))


datinha$CS_RACA <- factor(datinha$CS_RACA, levels = c("Branca","Preta",
                                                      "Amarela","Parda",
                                                      "Indigena","Ignorado"))

attach(datinha)
# Agregar os dados por mês e sexo para obter o número total de casos por grupo
dados_aggregated <- datinha %>%
  group_by(MES_ANO, CS_RACA) %>%
  summarise(total_casos = sum(ORIGEM))

# Criar o gráfico de série temporal
p12<- ggplot(dados_aggregated, aes(x = MES_ANO, y = total_casos, color = CS_RACA)) +
  geom_line(size = 1.0) +
  geom_point() +
  labs(title = "Número de casos absolutos mensais de AIDS no Espírito Santo de 2014 a 2023",
       subtitle = "Indivíduos com idade a partir de 13 anos por raça.",
       x = "Ano",
       y = "Número de casos",
       color = "Raça") +
  theme_minimal() + scale_color_manual(values=c('#cd4c4c','#cdb84c','#63cd4c','#4ccdbd','#9e4ccd','#cd4cb0'))
p12

# Adicionar animação
anim12 <- p12 +
  transition_reveal(MES_ANO)
# Ajustar a velocidade da animação e lrgura
anim_gif12 <- animate(anim12, width = 700, duration = 40, fps = 10)  # 40 segundos de duração, 10 frames por segundo
# Salvar a animação como um GIF
anim_save("casos_AIDS_raca.gif", anim_gif12)

#-Serie-temporal-numero-casos-por-orientacao-------

datinha <- datinha %>%
mutate(EXPOSICAO = case_when(
ANT_REL_CA == "10" ~ "Homossexual",
ANT_REL_CA == "20" ~ "Bissexual",
ANT_REL_CA == "30" ~ "Heterossexual",
TRUE ~ "Outro"
))

datinha$EXPOSICAO <- factor(datinha$EXPOSICAO, levels = c("Heterossexual", 
                                                          "Homossexual",
                                                          "Bissexual"))

# Agregar os dados por mês e exposição para obter o número total de casos por grupo
dados_aggregated <- datinha %>%
  group_by(MES_ANO, EXPOSICAO) %>%
  summarise(total_casos = sum(ORIGEM))

# Criar o gráfico de série temporal
p2<- ggplot(dados_aggregated, aes(x = MES_ANO, y = total_casos, color = EXPOSICAO)) +
  geom_line(size = 1.0) +
  geom_point() +
  labs(title = "Número de casos absolutos mensais de AIDS no Espírito Santo de 2014 a 2023",
       subtitle = "Indivíduos com idade a partir de 13 anos por orientação sexual.",
       x = "Ano",
       y = "Número de Casos",
       color = "Orientação") +
  theme_minimal() + scale_color_manual(values=c('#050505','#ec4141', '#27c3c1'))
p2

# Adicionar animação
anim2 <- p2 +
  transition_reveal(MES_ANO)
# Ajustar a velocidade da animação e lrgura
anim_gif2 <- animate(anim2, width = 700, duration = 40, fps = 10)  # 40 segundos de duração, 10 frames por segundo
# Salvar a animação como um GIF
anim_save("casos_AIDS_orientacao.gif", anim_gif2)



#-Serie-temporal-incidencia-casos-po-sexo----

datinha <- datinha %>%
mutate(POPULACAO = case_when(
NU_ANO == "2013" ~ "3736386",
NU_ANO == "2014" ~ "3784361",
NU_ANO == "2015" ~ "3832826",
NU_ANO == "2016" ~ "3879376",
NU_ANO == "2017" ~ "3925341",
NU_ANO == "2018" ~ "3972388",
NU_ANO == "2019" ~ "4018650",
NU_ANO == "2020" ~ "4064052",
NU_ANO == "2021" ~ "4108508",
NU_ANO == "2022" ~ "4151923",
NU_ANO == "2023" ~ "4194277",
TRUE ~ "Outro"
))

datinha$POPULACAO <- as.numeric(POPULACAO)

attach(datinha)
dados_aggregated <- datinha %>%
group_by(SEXO, NU_ANO) %>%
summarise(casos = sum(ORIGEM), incidencia = sum((casos/POPULACAO) * 100000)/casos)

(dados_aggregated$casos / POPULACAO) * 1000

# Criar o gráfico de série temporal
p3<- ggplot(dados_aggregated, aes(x = NU_ANO, y = incidencia, color = SEXO)) +
  geom_line(size = 1.0) +
  geom_point() +
  labs(title = "Incidência de AIDS no Espírito Santo entre 2014 a 2023",
       subtitle = "Indivíduos com idade a partir de 13 anos por sexo.",
       x = "Ano",
       y = "Incidência(cem mil habitantes)",
       color = "Sexo") + scale_color_manual(values=c('#050505','#ec4141')) +
  theme_minimal()
p3

# Adicionar animação
anim3 <- p3 +
  transition_reveal(NU_ANO)
# Ajustar a velocidade da animação e lrgura
anim_gif3 <- animate(anim3, width = 700, duration = 40, fps = 10)  # 40 segundos de duração, 10 frames por segundo
# Salvar a animação como um GIF
anim_save("incidencia_AIDS_incid_sexo.gif", anim_gif3)

#-Serie-temporal-incidencia-orientacao-----
dados_aggregated <- datinha %>%
  group_by(EXPOSICAO, NU_ANO) %>%
  summarise(casos = sum(ORIGEM), incidencia = sum((casos/POPULACAO) * 100000)/casos)


# Criar o gráfico de série temporal
p4<- ggplot(dados_aggregated, aes(x = NU_ANO, y = incidencia, color = EXPOSICAO)) +
  geom_line(size = 1.0) +
  geom_point() +
  labs(title = "Incidência de AIDS no Espírito Santo entre 2014 a 2023",
       subtitle = "Indivíduos com idade a partir de 13 anos por orientação.",
       x = "Ano",
       y = "Incidência(cem mil habitantes)",
       color = "Orientação") +
  theme_minimal()  + scale_color_manual(values=c('#050505','#ec4141', '#27c3c1')) #meio, primeiro, ultimo
p4

# Adicionar animação
anim4 <- p4 +
  transition_reveal(NU_ANO)
# Ajustar a velocidade da animação e lrgura
anim_gif4 <- animate(anim4, width = 700, duration = 40, fps = 10)  # 40 segundos de duração, 10 frames por segundo
# Salvar a animação como um GIF
anim_save("incidencia_AIDS_incid_orientacao.gif", anim_gif4)

