##### CONFIGURACOES INCIAIS #####

#Consultando diretório e instalando pacotes
setwd("C:\\Users\\paulo\\Documents\\GitHub\\covid_AM\\Dados")
#install.packages("tidyverse")
library(tidyverse)

#retirar notacao cientifica
options(scipen = 999)

#importando o dataset
covid_am <- read.csv2("covid_fiocruz.csv")
mobility_report_2020 <- read.csv("2020_BR_Region_Mobility_Report.csv")
mobility_report_2021 <- read.csv("2021_BR_Region_Mobility_Report.csv")
mobility_report_2022 <- read.csv("2022_BR_Region_Mobility_Report.csv")

df = apply(covid_am, 2, as.character) 
Encoding(df) = "UTF-8"
covid_am = as_tibble(df)

covid_am <- covid_am %>%  mutate(dia_semana = as.Date(dia_semana, "%d/%m/%Y"))

mobility_report_2020 <- mobility_report_2020 %>% rename( dia_semana = date,
                               descanso_recreacao = retail_and_recreation_percent_change_from_baseline,
                               mercado_farmacia = grocery_and_pharmacy_percent_change_from_baseline,
                               parques = parks_percent_change_from_baseline,
                               transito_estacoes = transit_stations_percent_change_from_baseline,
                               trabalho = workplaces_percent_change_from_baseline,
                               residencia = residential_percent_change_from_baseline) %>% 
                        mutate(dia_semana = as.Date(dia_semana)) %>% 
                        select(sub_region_1, dia_semana,  descanso_recreacao,mercado_farmacia,parques,
                               transito_estacoes,trabalho,residencia) %>% 
                        filter(sub_region_1 == "State of Amazonas" & !is.na(descanso_recreacao) & 
                                 !is.na(mercado_farmacia) & !is.na(parques) & !is.na(transito_estacoes)
                               & !is.na(trabalho) & !is.na(residencia))

mobility_report_2021 <- mobility_report_2021 %>% rename( dia_semana = date,
                               descanso_recreacao = retail_and_recreation_percent_change_from_baseline,
                               mercado_farmacia = grocery_and_pharmacy_percent_change_from_baseline,
                               parques = parks_percent_change_from_baseline,
                               transito_estacoes = transit_stations_percent_change_from_baseline,
                               trabalho = workplaces_percent_change_from_baseline,
                               residencia = residential_percent_change_from_baseline) %>%
                        mutate(dia_semana = as.Date(dia_semana)) %>% 
                        select(sub_region_1, dia_semana,  descanso_recreacao,mercado_farmacia,parques,
                               transito_estacoes,trabalho,residencia) %>% 
                        filter(sub_region_1 == "State of Amazonas" & !is.na(descanso_recreacao) & 
                                 !is.na(mercado_farmacia) & !is.na(parques) & !is.na(transito_estacoes)
                               & !is.na(trabalho) & !is.na(residencia))

mobility_report_2022 <-  mobility_report_2022 %>% rename( dia_semana = date,
                               descanso_recreacao = retail_and_recreation_percent_change_from_baseline,
                               mercado_farmacia = grocery_and_pharmacy_percent_change_from_baseline,
                               parques = parks_percent_change_from_baseline,
                               transito_estacoes = transit_stations_percent_change_from_baseline,
                               trabalho = workplaces_percent_change_from_baseline,
                               residencia = residential_percent_change_from_baseline) %>%
                        mutate(dia_semana = as.Date(dia_semana)) %>% 
                        select(sub_region_1, dia_semana,  descanso_recreacao,mercado_farmacia,parques,
                              transito_estacoes,trabalho,residencia) %>% 
                        filter(sub_region_1 == "State of Amazonas" & !is.na(descanso_recreacao) & 
                              !is.na(mercado_farmacia) & !is.na(parques) & !is.na(transito_estacoes)
                              & !is.na(trabalho) & !is.na(residencia))

mobility_report <- bind_rows(mobility_report_2022, mobility_report_2021, mobility_report_2020)

covid_am_t <- inner_join(covid_am, mobility_report, by = "dia_semana")

covid_am_t <- covid_am_t %>% mutate (indice_permanencia_domiciliar = residencia-(
  descanso_recreacao + mercado_farmacia + parques + transito_estacoes +
    trabalho) / 5) 

covid_am_t1 <-covid_am_t %>% 
  mutate(indice_permanencia_domiciliar = as.numeric(indice_permanencia_domiciliar)) %>% 
  mutate(obitos_novos = as.numeric(obitos_novos)) %>% 
  group_by(dia_semana)%>% 
  summarise(indice_permanencia_domiciliar = mean(indice_permanencia_domiciliar), 
            obitos_novos = sum(obitos_novos))

covid_am_t1 %>% 
  ggplot() +
  geom_line(aes(dia_semana, indice_permanencia_domiciliar), group = 1, color = "blue") +
  geom_line(aes(dia_semana, obitos_novos), group = 1, color = "dark red") +
  theme_bw() +
  labs( x = 'Semana Epidemiologica', y = '', caption = 'Fonte: Brasil.io; Google Mobility Report', 
        title = 'Evolução dos óbitos em relação ao IPD',
        subtitle ='Óbitos x Índice de Permanência Domiciliar')+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%W")

write.csv(covid_am_t, "covid_am_t.csv")


