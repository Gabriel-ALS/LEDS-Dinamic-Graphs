library(ggplot2)
library(tidyverse)
library(gganimate)
library(scales)


#Importar o banco de dados
pib_mundo <- read.csv2("countries_gdp_hist.csv")

#Dar uma olhada no banco de dados com str(), length(), sumary(), head()

#Queremos olhar para o Pib dos paises da America do Sul ao longo dos anos,
#então vamos utilizar a coluna year(), total_gdp_million, intermediate_region
#e country_name

#Aqui filtramos só o que utilizariamos do banco de dados
america_do_sul <- pib_mundo[pib_mundo$intermediate_region == "South America",
                            c("year","total_gdp_million","country_name")]

#Unique para ver se o nome dos paises está certo, e também precisamos
#transformar em factor e numeric algumas colunas.

america_do_sul <- america_do_sul %>%
  mutate(country_name = recode(country_name,
                               "Venezuela (Bolivarian Republic of)"
                               = "Venezuela",
                               "Bolivia (Plurinational State of)" =
                                  "Bolivia"))
america_do_sul$country_name <- factor(america_do_sul$country_name)
america_do_sul$total_gdp_million <- as.numeric((america_do_sul$total_gdp_million))


#Rank dos maiores PIBs para que quando mude o ano os paises subam e desçam
america_do_sul <- america_do_sul %>%
  arrange(year, desc(total_gdp_million)) %>%
  group_by(year) %>%
  mutate(rank = factor(row_number())) %>%
  ungroup()

america_do_sul$rank <-  as.numeric(america_do_sul$rank)

#Criar o grafico no GGplot2

grafico <- ggplot(america_do_sul, aes(x= factor(-rank),
                                      y = total_gdp_million,
                                      fill = country_name)) + 
  geom_bar(stat = "identity") +
  coord_flip(clip = "off") +
  geom_text(aes(y = 0, label = paste(country_name, " ")),
            vjust = 0.5, hjust = 1.3, size = 4)+
  labs(title = "PIB do País em: {closest_state}",
       x = "", y = "Milhões",
       fill = "Países") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 30, 10, 80)) +
  scale_x_discrete(labels = function(x) NULL) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values =colorRampPalette(c("#146cb4","#046434", "#1ab2a3"))(12)) +
  
transition_states(year,
  state_length = 5,
  transition_length = 2
) +
  view_follow(fixed_x = TRUE) +

  ease_aes('linear')

animate(grafico, nframes = 122, renderer = gifski_renderer(),fps = 5)

anim_save("PIB.gif")



 
