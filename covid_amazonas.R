##### CONFIGURACOES INICIAS #####

#Consultando diretório e instalando pacotes
getwd()
setwd("C:\\Users\\paulo\\Documents\\GitHub\\covid_AM\\Dados")
#install.packages("tidyverse")
library(tidyverse)

#retirar notacao cientifica
options(scipen = 999)

#importando o dataset
covid_amazonas <- read.csv("Covid_amazonas_brasilio.csv")
mobility_report_2020 <- read.csv("2020_BR_Region_Mobility_Report.csv")
mobility_report_2021 <- read.csv("2021_BR_Region_Mobility_Report.csv")
mobility_report_2022 <- read.csv("2022_BR_Region_Mobility_Report.csv")
head(covid_amazonas)

#Ajustando codificação de caracters do dataset
df = apply(covid_amazonas, 2, as.character) 
Encoding(df) = "UTF-8"
covid_amazonas = as.data.frame(df)

##### TRATAMENTO DE DATASETS #####
#Renomeando as colunas de interesse
names(mobility_report_2022)

mobility_report_2020 <- rename(mobility_report_2020,
                               descanso_recreacao = retail_and_recreation_percent_change_from_baseline,
                               mercado_farmacia = grocery_and_pharmacy_percent_change_from_baseline,
                               parques = parks_percent_change_from_baseline,
                               transito_estacoes = transit_stations_percent_change_from_baseline,
                               trabalho = workplaces_percent_change_from_baseline,
                               residencia = residential_percent_change_from_baseline)

mobility_report_2021 <- rename(mobility_report_2021,
                               descanso_recreacao = retail_and_recreation_percent_change_from_baseline,
                               mercado_farmacia = grocery_and_pharmacy_percent_change_from_baseline,
                               parques = parks_percent_change_from_baseline,
                               transito_estacoes = transit_stations_percent_change_from_baseline,
                               trabalho = workplaces_percent_change_from_baseline,
                               residencia = residential_percent_change_from_baseline)

mobility_report_2022 <- rename(mobility_report_2022,
                               descanso_recreacao = retail_and_recreation_percent_change_from_baseline,
                               mercado_farmacia = grocery_and_pharmacy_percent_change_from_baseline,
                               parques = parks_percent_change_from_baseline,
                               transito_estacoes = transit_stations_percent_change_from_baseline,
                               trabalho = workplaces_percent_change_from_baseline,
                               residencia = residential_percent_change_from_baseline)

#Criando Data Frames para trabalho com mobilidade
mobility_am_2020 <- mobility_report_2020 %>% 
  select(sub_region_1, date,  descanso_recreacao,mercado_farmacia,parques,
         transito_estacoes,trabalho,residencia) %>% 
  filter(sub_region_1 == "State of Amazonas" & !is.na(descanso_recreacao) & 
           !is.na(mercado_farmacia) & !is.na(parques) & !is.na(transito_estacoes)
         & !is.na(trabalho) & !is.na(residencia))

mobility_am_2021 <- mobility_report_2021 %>% 
  select(sub_region_1, date,  descanso_recreacao,mercado_farmacia,parques,
         transito_estacoes,trabalho,residencia) %>% 
  filter(sub_region_1 == "State of Amazonas" & !is.na(descanso_recreacao) & 
           !is.na(mercado_farmacia) & !is.na(parques) & !is.na(transito_estacoes)
         & !is.na(trabalho) & !is.na(residencia))

mobility_am_2022 <- mobility_report_2022 %>% 
  select(sub_region_1, date,  descanso_recreacao,mercado_farmacia,parques,
         transito_estacoes,trabalho,residencia) %>% 
  filter(sub_region_1 == "State of Amazonas" & !is.na(descanso_recreacao) & 
           !is.na(mercado_farmacia) & !is.na(parques) & !is.na(transito_estacoes)
         & !is.na(trabalho) & !is.na(residencia))

#Unindo os Data frames
covid_am_mobility_2020 <- inner_join(covid_amazonas, mobility_am_2020, by = "date")
covid_am_mobility_2021 <- inner_join(covid_amazonas, mobility_am_2021, by = "date")
covid_am_mobility_2022 <- inner_join(covid_amazonas, mobility_am_2022, by = "date")
covid_am_mobility <- bind_rows(covid_am_mobility_2020, covid_am_mobility_2021, covid_am_mobility_2022)

##Definindo as Regiões de Saúde do Amazonas
#1. Região de Saúde de Manaus, Entorno e Alto Rio Negro
covid_am_rsmea <- covid_am_mobility %>% filter(!is.na(city) & city == "Manaus" 
                                              | city == "Autazes" 
                                              | city == "Barcelos"
                                              | city == "Careiro" 
                                              | city == "Careiro da Várzea"
                                              | city == "Iranduba" 
                                              | city == "Manaquiri"
                                              | city == "Nova Olinda do Norte" 
                                              | city == "Presidente Figueiredo"
                                              | city == "Rio Preto da Eva" 
                                              | city == "Santa Isabel do Rio Negro"
                                              | city == "São Gabriel da Cachoeira") %>% 
  mutate(Regiao_Saude = "Região de Saúde de Manaus, Entorno e Alto Rio Negro")

#2. Região de Saúde do Baixo Amazonas
covid_am_rsba <- covid_am_mobility %>% filter(!is.na(city) & city == "Parintins"  
                                              | city == "Maués" 
                                              | city == "Nhamundá"
                                              | city == "Boa Vista dos Ramos"
                                              | city == "Barreirinha") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Baixo Amazonas")

#3. Região de Saúde do Alto Solimões
covid_am_rsas <- covid_am_mobility %>% filter(!is.na(city) & city == "Amaturá" 
                                              | city == "Atalaia do Norte" 
                                              | city == "Benjamin Constant"
                                              | city == "Fonte Boa"
                                              | city == "Jutaí"
                                              | city == "Santo Antônio do Içá"
                                              | city == "São Paulo de Olivença"
                                              | city == "Tonantins"
                                              | city == "Tabatinga") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Alto Solimões")

#4. Região de Saúde do Médio Amazonas
covid_am_rsma <- covid_am_mobility %>% filter(!is.na(city) & city == "Itacoatiara" 
                                              | city == "Itapiranga" 
                                              | city == "São Sebastião do Uatumã"
                                              | city == "Silves"
                                              | city == "Urucará"
                                              | city == "Urucurituba") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Médio Amazonas")

#5. Região de Saúde do Rio Negro e Solimões
covid_am_rsrns <- covid_am_mobility %>% filter(!is.na(city) & city == "Manacapuru" 
                                              | city == "Novo Airão" 
                                              | city == "Anamã"
                                              | city == "Beruri"
                                              | city == "Caapiranga"
                                              | city == "Coari"
                                              | city == "Codajás") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Rio Negro e Solimões")

#6. Região de Saúde do Madeira
covid_am_rsm <- covid_am_mobility %>% filter(!is.na(city) & city == "Apuí" 
                                               | city == "Borba" 
                                               | city == "Humaitá"
                                               | city == "Manicoré "
                                               | city == "Novo Aripuanã") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Madeira")

#7. Região de Saúde do Rio Triângulo
covid_am_rsrt <- covid_am_mobility %>% filter(!is.na(city) & city == "Tefé" 
                                               | city == "Japurá" 
                                               | city == "Maraã"
                                               | city == "Juruá"
                                               | city == "Uarini"
                                               | city == "Alvarães") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Rio Triângulo")

#8. Região de Saúde do Rio Purus
covid_am_rsrp <- covid_am_mobility %>% filter(!is.na(city) & city == "Boca do Acre" 
                                               | city == "Canutama" 
                                               | city == "Lábrea"
                                               | city == "Pauini"
                                               | city == "Tapauá") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Rio Purus")

#9. Região de Saúde do Rio Juruá
covid_am_rsrj <- covid_am_mobility %>% filter(!is.na(city) & city == "Carauari" 
                                               | city == "Eirunepé" 
                                               | city == "Envira"
                                               | city == "Guajará"
                                               | city == "Ipixuna"
                                               | city == "Itamarati") %>% 
  mutate(Regiao_Saude = "Região de Saúde do Rio Juruá")

covid_am_rs <- bind_rows(covid_am_rsas, covid_am_rsba, covid_am_rsm,
                         covid_am_rsma, covid_am_rsmea, covid_am_rsrj,
                         covid_am_rsrns, covid_am_rsrp, covid_am_rsrt)

names(covid_am_rs)

covid_am_rs <- covid_am_rs %>% mutate (indice_permanencia_domiciliar = residencia-(
  descanso_recreacao + mercado_farmacia + parques + transito_estacoes +
    trabalho) / 5) %>% 
  mutate(last_available_confirmed = as.numeric(last_available_confirmed)) %>%
  mutate(estimated_population = as.numeric(estimated_population)) %>% 
  select(epidemiological_week, date, city, Regiao_Saude,order_for_place,
         estimated_population, last_available_confirmed,new_confirmed,
         last_available_confirmed_per_100k_inhabitants,new_deaths, 
         last_available_deaths, last_available_death_rate, 
         indice_permanencia_domiciliar,descanso_recreacao,mercado_farmacia, 
         parques, transito_estacoes, trabalho,residencia) %>% 
  filter(!is.na(epidemiological_week)|!is.na(date)|!is.na(city)|
           !is.na(Regiao_Saude)| !is.na(order_for_place)| !is.na(estimated_population)|
           !is.na(last_available_confirmed)| !is.na(new_confirmed)| 
           !is.na(last_available_confirmed_per_100k_inhabitants) | 
           !is.na(new_deaths) | !is.na(last_available_deaths) | 
           !is.na(last_available_death_rate) | !is.na(indice_permanencia_domiciliar)
         | !is.na(descanso_recreacao) | !is.na(mercado_farmacia) | !is.na(parques)
         | !is.na(transito_estacoes) | !is.na(trabalho) | !is.na(residencia)) %>% 
  rename(semana_epidemilogica = epidemiological_week, data = date, cidade = city,
        dias_primeiro_caso  = order_for_place, acumulo_confirmados = last_available_confirmed,
        acumulo_confirmados_100k = last_available_confirmed_per_100k_inhabitants, 
        confirmados_do_dia = new_confirmed, acumulo_obitos = last_available_deaths,
        taxa_obitos_ultimo_dia = last_available_death_rate,obitos_do_dia = new_deaths, 
        populacao_estimada_2020 = estimated_population) %>%
  mutate(semana_epidemilogica = fct_inorder(as.character(semana_epidemilogica)))

covid_am_rs1 <-covid_am_rs %>% 
  mutate(indice_permanencia_domiciliar = as.numeric(indice_permanencia_domiciliar)) %>% 
  mutate(obitos_do_dia = as.numeric(obitos_do_dia)) %>% 
  mutate(data = as.Date(data)) %>% 
  group_by(data)%>% 
  summarise(indice_permanencia_domiciliar = mean(indice_permanencia_domiciliar), 
            obitos_do_dia = sum(obitos_do_dia))

covid_am_rs1 %>% 
  ggplot() +
  geom_line(aes(data, indice_permanencia_domiciliar), group = 1, color = "blue") +
  geom_line(aes(data, obitos_do_dia), group = 1, color = "dark red") +
  theme_bw() +
  labs( x = 'Semana Epidemiologica', y = '', caption = 'Fonte: Brasil.io; Google Mobility Report', 
        title = 'Evolução dos óbitos em relação ao IPD',
        subtitle ='Óbitos x Índice de Permanência Domiciliar')+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%W")
