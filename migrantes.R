pacman::p_load(rio, 
               tidyverse, 
               summarytools)

migrantes <- import("C:/Users/laisr/OneDrive/Documentos/OPAS/Migrantes/RAIS_CTPS_CAGED_2021.csv",
                    encoding = "Latin-1")
cbo <- import("C:/Users/laisr/OneDrive/Documentos/ARQUIVOS_UTEIS/CBO2002 - Ocupacao.csv")

migrantes <- migrantes %>% 
  mutate(racacor = case_when(racacor==1 ~ "Indígena",
                             racacor==2 ~ "Branca",
                             racacor==3 ~ "Preta",
                             racacor==4 ~ "Amarela",
                             racacor==5 ~ "Parda",
                             TRUE ~ "Ignorado"),
         racacor = fct_infreq(racacor)) %>% 
  mutate(competenciamov = case_when(competenciamov==202101 ~ "Janeiro",
                                    competenciamov==202102 ~ "Fevereiro",
                                    competenciamov==202103 ~ "Março",
                                    competenciamov==202104 ~ "Abril",
                                    competenciamov==202105 ~ "Maio",
                                    competenciamov==202106 ~ "Junho",
                                    competenciamov==202107 ~ "Julho",
                                    competenciamov==202108 ~ "Agosto",
                                    competenciamov==202109 ~ "Setembro",
                                    competenciamov==202110 ~ "Outubro",
                                    competenciamov==202111 ~ "Novembro",
                                    competenciamov==202112 ~ "Dezembro",
                             TRUE ~ "Ignorado"),
         competenciamov = fct_infreq(competenciamov)) %>% 
  mutate(pais = abjutils::rm_accent(pais),
         pais = fct_infreq(pais)) %>% 
  mutate(continente = abjutils::rm_accent(continente),
         continente = fct_infreq(continente)) %>% 
  mutate(uf = case_when(uf==11	~ "Rondônia",
                        uf==12	~ "Acre",
                        uf==13	~ "Amazonas",
                        uf==14	~ "Roraima",
                        uf==15	~ "Pará",
                        uf==16	~ "Amapá",
                        uf==17	~ "Tocantins",
                        uf==21	~ "Maranhão",
                        uf==22	~ "Piauí",
                        uf==23	~ "Ceará",
                        uf==24	~ "Rio Grande do Norte",
                        uf==25	~ "Paraíba",
                        uf==26	~ "Pernambuco",
                        uf==27	~ "Alagoas",
                        uf==28	~ "Sergipe",
                        uf==29	~ "Bahia",
                        uf==31	~ "Minas Gerais",
                        uf==32	~ "Espírito Santo",
                        uf==33	~ "Rio de Janeiro",
                        uf==35	~ "São Paulo",
                        uf==41	~ "Paraná",
                        uf==42	~ "Santa Catarina",
                        uf==43	~ "Rio Grande do Sul",
                        uf==50	~ "Mato Grosso do Sul",
                        uf==51	~ "Mato Grosso",
                        uf==52	~ "Goiás",
                        uf==53	~ "Distrito Federal",
                        TRUE ~ "Ignorado"),
         uf = fct_infreq(uf)) %>% 
  mutate(sexo = case_when(sexo==1 ~ "Masculino",
                          sexo==3 ~ "Feminino",
                             TRUE ~ "Ignorado"),
         sexo = fct_infreq(sexo)) %>% 
  mutate(nivel_instrucao =  case_when(nivel_instrucao==1 ~ "Analfabeto",
                                      nivel_instrucao==2 ~ "1º ciclo do fundamental incompleto",
                                      nivel_instrucao==3 ~ "1º ciclo do fundamental completo",
                                      nivel_instrucao==4 ~ "2º ciclo do fundamental incompleto",
                                      nivel_instrucao==5 ~ "Fundamental completo",
                                      nivel_instrucao==6 ~ "Médio incompleto",
                                      nivel_instrucao==7 ~ "Médio completo",
                                      # não coloquei todos os niveis pra ganhar tempo, pois nao tinha na base
                                      TRUE ~ "Ignorado"),
         nivel_instrucao = fct_infreq(nivel_instrucao)) %>% 
  mutate(faixa_etaria = case_when(faixa_etaria==1 ~ "10 a 14 anos",
                                  faixa_etaria==2 ~ "15 a 17 anos",
                                  faixa_etaria==3 ~ "18 a 24 anos",
                                  faixa_etaria==4 ~ "25 a 29 anos",
                                  faixa_etaria==5 ~ "30 a 39 anos",
                                  faixa_etaria==6 ~ "40 a 49 anos",
                                  faixa_etaria==7 ~ "50 a 64 anos",
                                  faixa_etaria==8 ~ "65 anos ou mais",
                                  TRUE ~ "Ignorado"),
         faixa_etaria = fct_infreq(faixa_etaria)) %>% 
  mutate(faixa_horas_contrat = case_when(faixa_horas_contrat==1 ~ "Até 12 horas",
                                         faixa_horas_contrat==2 ~ "13 a 15 horas",
                                         faixa_horas_contrat==3 ~ "16 a 20 horas",
                                         faixa_horas_contrat==4 ~ "21 a 30 horas",
                                         faixa_horas_contrat==5 ~ "31 a 40 horas",
                                         faixa_horas_contrat==6 ~ "41 a 44 horas",
                                         TRUE ~ "Ignorado"),
         faixa_horas_contrat = fct_infreq(faixa_horas_contrat)) %>% 
  mutate(indtrabparcial=case_when(indtrabparcial==1 ~ "Sim",
                                  indtrabparcial==0 ~ "Não",
                                  TRUE ~ "Ignorado"),
         indtrabparcial=fct_infreq(indtrabparcial)) %>% 
  mutate(indtrabintermitente = case_when(indtrabintermitente==1 ~ "Sim",
                                         indtrabintermitente==0 ~ "Não",
                                  TRUE ~ "Ignorado"),
         indtrabintermitente = fct_infreq(indtrabintermitente)) %>% 
  mutate(status_migratorio = case_when(status_migratorio == "Sem informação" ~ "Ignorado",
                                       TRUE ~ status_migratorio),
         status_migratorio = fct_infreq(status_migratorio))
  
  



print(summarytools::dfSummary(migrantes %>% select(sexo, faixa_etaria, racacor, nivel_instrucao,
                                                   continente, pais, uf, competenciamov, status_migratorio,
                                                   salario, faixa_horas_contrat, indtrabparcial, indtrabintermitente), 
                              graph.col = T), 
      method = "viewer", 
      file = "C:/Users/laisr/OneDrive/Documentos/OPAS/Migrantes/migrantes_crude_dataselfie.html")

glimpse(migrantes)
migrantes %>%  
  count(tipomovimentacao)
summary(migrantes$faixa_horas_contrat)

# Frequência absoluta e relativa, acumulada e com separação de valores validos
# Ideal para variáveis categóricas
migrantes %>% 
  freq(racacor)

# Cross Table (Não sei pq, mas não funcionou com tidyverse)
#migrantes %>% 
 # ctable(racacor, sexo)

ctable(migrantes$racacor, 
       migrantes$sexo)

# Dá pra mudar a direção da porcentagem (vertical ou horizontal)
a <- ctable(migrantes$racacor, 
       migrantes$sexo, 
       prop = "t")


ctable(migrantes$racacor, 
       migrantes$sexo, 
       prop = "c")

# Explorar mais como passar para df e o summary dessa tabela, que já devolve um chiquadrado
b <- a$cross_table
c <- a$proportions
summary(b)

# A função que exporta o data_selfie, mas vendo no console  
summarytools::dfSummary(migrantes %>% select(sexo, faixa_etaria, racacor, nivel_instrucao,
                                             continente, pais, uf, competenciamov, status_migratorio,
                                             salario, faixa_horas_contrat, indtrabparcial, indtrabintermitente))

# Medidas sumárias para uma ou todas as variáveis do banco quantitativas (não numéricas não funcionam!!)
migrantes %>%
  select(salario, faixa_horas_contrat) %>% 
  mutate(salario1 = salario) %>% 
  descr()

freq(migrantes$racacor, 
     plain.ascii = F, 
     style = "rmarkdown")

salario <- migrantes$salario
freq(salario)

# o que a variavel saldomovimentacao, tipomovimentacao, ???
# como fazer a cbo??
# pq RS tem um asterisco?




# depois ####
#format(object.size(migrantes), units = "MB") # 28.9 Mb

print(summarytools::dfSummary(migrantes, 
                              graph.col = F), 
      method = "viewer", 
      file = "C:/Users/laisr/OneDrive/Documentos/OPAS/Migrantes/migrantes_crude_dataselfie.html")


View(summarytools::dfSummary(migrantes))

install.packages("basedosdados")
library("basedosdados")

# Defina o seu projeto no Google Cloud
set_billing_id("<YOUR_PROJECT_ID>")

# Para carregar o dado direto no R
query <- bdplyr("br_me_rais.dicionario")
df <- bd_collect(query)
