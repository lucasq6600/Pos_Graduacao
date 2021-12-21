################ Projeto TakeBlip::::::: #################

## 00. Importação das bibliotecas
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","cabootcrs",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","dplyr","ggrepel","sjPlot","FactoMineR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


## 01. biblioteca para leitura de Excel ==============
library(readxl)


## 02. importação dos arquivos em Excel ==============
Arq_01_Depara <- read_excel("C:/Users/Samsung/Desktop/Gerenciador/Freelancer_Projetos/PROJ_TAKEBLIP/Arq_01_Depara.xlsx",
                          sheet = "IMPORT_R")

Arq_02_Base <- read_excel("C:/Users/Samsung/Desktop/Gerenciador/Freelancer_Projetos/PROJ_TAKEBLIP/Arq_02_Base.xlsx",
                                  sheet = "IMPORT_R")

# View(Arq_01_Depara)
# View(Arq_02_Base)
# class(Arq_02_Base)

## 03. Trabalhando com as bases de dados importadas =================
save(Arq_02_Base, file = "Arq_02_Base.RData")

Arq_01_Depara_nodup <- Arq_01_Depara %>% distinct(DS_CNPJ_COM_SIMBOLOS, .keep_all = TRUE)

summary(Arq_01_Depara_nodup)

# Agrupando para contar a qtd de cada segmento:
total_line <- Arq_01_Depara_nodup %>% summarise(QTD_TOTAL = n())

Arq_01_Depara_nodup %>% 
    group_by(DS_SEGMENTO) %>% 
    summarise(QTD = n() / total_line)

# cria uma base nodup2 apenas com as 2 variáveis para cruzar.
Arq_01_Depara_nodup2 <- select(Arq_01_Depara_nodup,DS_CNPJ_COM_SIMBOLOS,DS_SEGMENTO)

# cruzamento
Arq_03_Cruzado <- left_join(Arq_02_Base,Arq_01_Depara_nodup2, by = "DS_CNPJ_COM_SIMBOLOS", copy = FALSE)

# funcionou assim:: names(Arq_03_Cruzado)[19] <- 'DS_FAIXA_FUNCIONARIOS'

save(Arq_03_Cruzado, file = "Arq_03_Base.RData")


# Qtd de registros totais da base após cruzamento: 
total_line2 <- Arq_03_Cruzado %>% summarise(QTD_TOTAL = n())

Arq_03_Cruzado %>% 
  group_by(DS_SEGMENTO) %>% 
  summarise(QTD = n() / total_line2)


# Check se o cruzamento trouxe alguém com o SEGMENTO vazio:
check_vazio <- Arq_03_Cruzado %>% 
  filter(DS_SEGMENTO == "")
check_vazio

# confirma dimensão da base: Qtd Linhas x Qtd Colunas
dim(Arq_03_Cruzado)
names(Arq_03_Cruzado)

# View(Arq_03_Cruzado)

class(Arq_03_Cruzado)
# teste <- Arq_03_Cruzado[1,9:10]
# View(teste)

load("Arq_03_Cruzado.RData")
# names(Arq_03_Cruzado)[19] <- 'DS_FAIXA_FUNCIONARIOS'
Arq_03_Cruzado$DS_IDADE_EMPRESA <- str_replace_all(Arq_03_Cruzado$DS_IDADE_EMPRESA," anos","")
Arq_03_Cruzado$DS_IDADE_EMPRESA <- as.integer(Arq_03_Cruzado$DS_IDADE_EMPRESA)


## 04. Separando cada coluna descritiva da base de dados =================
tipo_situacao_ativa <- Arq_03_Cruzado %>%
  filter(DS_TIPO_SITUACAO == 'ATIVA')

# Arq_04_Cruzado <- Arq_03_Cruzado %>%
#                  select(PC_INDICE_ADOCAO_TECNOLOGIA, PC_INDICE_PRESENCA_DIGITAL)
                


#############################################################################################
### 01. SEGMENTO = Analisando algumas colunas da base: 
QTD <- Arq_03_Cruzado %>% 
  filter(DS_SEGMENTO != "") %>% 
  group_by(DS_SEGMENTO) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
    filter(DS_SEGMENTO != "") %>% 
    group_by(DS_SEGMENTO) %>% 
    summarise(PC_TOTAL = n() / total_line2)

TT_SEGMENTO <- left_join(QTD,PC, by ="DS_SEGMENTO") 
TT_SEGMENTO  %>%  arrange(PC_TOTAL)

  #  distinct(DS_SEGMENTO) %>% 
  # summarise(PC_TOTAL = n() / total_line2)


### 02. PORTE DA EMPRESA  = Analisando algumas colunas da base: 
QTD <- Arq_03_Cruzado %>% 
  filter(DS_PORTE_EMPRESA != "") %>% 
  group_by(DS_PORTE_EMPRESA) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(DS_PORTE_EMPRESA != "") %>% 
  group_by(DS_PORTE_EMPRESA) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_PORTE_EMPRESA <- left_join(QTD,PC, by ="DS_PORTE_EMPRESA") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_PORTE_EMPRESA %>% arrange(PC_TOTAL)


###################### 02.B # Necessário criar factors do porta da empresa 
Arq_03_porte_empr <- factor(Arq_03_Cruzado,
                            levels = c("INDIVIDUAL","MICRO","PEQUENO","MEDIO","GRANDE"))


###################### 03. Faixa de Capital Social 
QTD <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_CAPITAL_SOCIAL != "") %>% 
  group_by(DS_FAIXA_CAPITAL_SOCIAL) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_CAPITAL_SOCIAL != "") %>% 
  group_by(DS_FAIXA_CAPITAL_SOCIAL) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_FAIXA_CAPITAL_SOCIAL <- left_join(QTD,PC, by ="DS_FAIXA_CAPITAL_SOCIAL") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_FAIXA_CAPITAL_SOCIAL %>% arrange(PC_TOTAL)


###################### 03.B # Necessário criar factors da faixa de capital social da empresa 
Arq_03_fx_social <- factor(Arq_03_Cruzado,
                          levels = c( "R$ 0 - R$ 50 K",         
                                      "R$ 51 K - R$ 250 K",   
                                      "R$ 251 K - R$ 500 K",    
                                      "R$ 501 K - R$ 2.5 M",   
                                      "R$ 2.5 M - R$ 10 M",     
                                      "R$ 10 M - R$ 50 M",      
                                      "R$ 50 M - R$ 100 M",
                                      "R$ 100 M - R$ 250 MI",
                                      "R$ 250 M - R$ 500 MI",   
                                      "+ R$ 1 B"))


###################### 04. Faixa de Faturamento
QTD <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_FATURAMENTO != "") %>% 
  group_by(DS_FAIXA_FATURAMENTO) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_FATURAMENTO != "") %>% 
  group_by(DS_FAIXA_FATURAMENTO) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_FAIXA_FATURAMENTO <- left_join(QTD,PC, by ="DS_FAIXA_FATURAMENTO") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_FAIXA_FATURAMENTO %>% arrange(PC_TOTAL)


###################### 04.B # Necessário criar factors da faixa de capital social da empresa 
Arq_03_fx_faturamento <- factor(Arq_03_Cruzado,
                           levels = c( "R$ 0 - R$ 80 K"
                                       ,"R$ 51 K - R$ 250 K"  
                                       ,"R$ 251 K - R$ 500 K" 
                                       ,"R$ 501 K - R$ 2.5 M" 
                                       ,"R$ 2.5 M - R$ 10 M"  
                                       ,"R$ 10 M - R$ 50 M"   
                                       ,"R$ 50 M - R$ 100 M"  
                                       ,"R$ 100 M - R$ 250 MI"
                                       ,"R$ 250 M - R$ 500 MI"
                                       ,"R$ 500 M - R$ 1 B"   
                                       ,"+ R$ 1 B"))


###################### 05. Faixa de Qtd de Funcionários
QTD <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_FUNCIONARIOS != "") %>% 
  group_by(DS_FAIXA_FUNCIONARIOS) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_FUNCIONARIOS != "") %>% 
  group_by(DS_FAIXA_FUNCIONARIOS) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_FAIXA_FUNCIONARIOS <- left_join(QTD,PC, by ="DS_FAIXA_FUNCIONARIOS") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_FAIXA_FUNCIONARIOS %>% arrange(PC_TOTAL)



###################### 06. Descrição do Tipo /Situação
QTD <- Arq_03_Cruzado %>% 
  filter(DS_TIPO_SITUACAO != "") %>% 
  group_by(DS_TIPO_SITUACAO) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(DS_TIPO_SITUACAO != "") %>% 
  group_by(DS_TIPO_SITUACAO) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_TIPO_SITUACAO <- left_join(QTD,PC, by ="DS_TIPO_SITUACAO") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_TIPO_SITUACAO %>% arrange(PC_TOTAL)


###################### 07. Descrição da Natureza Jurídica ::
QTD <- Arq_03_Cruzado %>% 
  filter(NM_NATUREZA_JURIDICA_GERAL != "") %>% 
  group_by(NM_NATUREZA_JURIDICA_GERAL) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(NM_NATUREZA_JURIDICA_GERAL != "") %>% 
  group_by(NM_NATUREZA_JURIDICA_GERAL) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_NATUREZA_JURIDICA_GERAL <- left_join(QTD,PC, by ="NM_NATUREZA_JURIDICA_GERAL") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_NATUREZA_JURIDICA_GERAL %>% arrange(PC_TOTAL)



###################### 08. Descrição da Atividade Econômica ::
QTD <- Arq_03_Cruzado %>% 
  filter(NM_ATIVIDADE_ECONOMICA_PRIMARIA != "") %>% 
  group_by(NM_ATIVIDADE_ECONOMICA_PRIMARIA) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(NM_ATIVIDADE_ECONOMICA_PRIMARIA != "") %>% 
  group_by(NM_ATIVIDADE_ECONOMICA_PRIMARIA) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_ATIVIDADE_ECONOMICA_PRIMARIA <- left_join(QTD,PC, by ="NM_ATIVIDADE_ECONOMICA_PRIMARIA") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_ATIVIDADE_ECONOMICA_PRIMARIA2 <- TT_ATIVIDADE_ECONOMICA_PRIMARIA %>% arrange(PC_TOTAL) 
TT_ATIVIDADE_ECONOMICA_PRIMARIA2 %>% 
  filter(PC_TOTAL >= 0.0191)





###################### 09. Descrição por Tipo de Unidade ::
QTD <- Arq_03_Cruzado %>% 
  filter(DS_TIPO_UNIDADE != "") %>% 
  group_by(DS_TIPO_UNIDADE) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(DS_TIPO_UNIDADE != "") %>% 
  group_by(DS_TIPO_UNIDADE) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_TIPO_UNIDADE <- left_join(QTD,PC, by ="DS_TIPO_UNIDADE") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_TIPO_UNIDADE %>% arrange(PC_TOTAL)





###################### 10. Descrição do Estado / Unidade Federativa ::
QTD <- Arq_03_Cruzado %>% 
  filter(CD_UF != "") %>% 
  group_by(CD_UF) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(CD_UF != "") %>% 
  group_by(CD_UF) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_CD_UF <- left_join(QTD,PC, by ="CD_UF") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_CD_UF %>% arrange(PC_TOTAL) 

TT_CD_UF2 <- TT_CD_UF %>% arrange(PC_TOTAL) 
TT_CD_UF2 %>% 
  filter(QTD_TOTAL < 10)



###################### 11. Descrição por Região do País ::
QTD <- Arq_03_Cruzado %>% 
  filter(NM_REGIAO != "") %>% 
  group_by(NM_REGIAO) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(NM_REGIAO != "") %>% 
  group_by(NM_REGIAO) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_REGIAO <- left_join(QTD,PC, by ="NM_REGIAO") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_REGIAO %>% arrange(PC_TOTAL) 


###################### 12. Descrição por CIDADE ::
QTD <- Arq_03_Cruzado %>% 
  filter(NM_CIDADE != "") %>% 
  group_by(NM_CIDADE) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(NM_CIDADE != "") %>% 
  group_by(NM_CIDADE) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_CIDADE <- left_join(QTD,PC, by ="NM_CIDADE") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_CIDADE %>% arrange(PC_TOTAL) %>% 
  filter(PC_TOTAL > 0.02)



###################### 13. Descrição por BAIRRO ::
QTD <- Arq_03_Cruzado %>% 
  filter(NM_BAIRRO != "") %>% 
  group_by(NM_BAIRRO) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(NM_BAIRRO != "") %>% 
  group_by(NM_BAIRRO) %>% 
  summarise(PC_TOTAL = n() / total_line2)

TT_BAIRRO <- left_join(QTD,PC, by ="NM_BAIRRO") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_BAIRRO %>% arrange(PC_TOTAL) %>% 
  filter(QTD_TOTAL > 10)

###################### 13. parte 02. filtrando UF(SP por ex..)
total_line3 <- Arq_03_Cruzado %>% 
      filter(CD_UF == "MG") %>% 
      summarise(QTD_TOTAL = n())

QTD <- Arq_03_Cruzado %>% 
  filter(NM_BAIRRO != "", CD_UF == "MG") %>% 
  group_by(NM_BAIRRO) %>% 
  summarise(QTD_TOTAL = n())

PC <- Arq_03_Cruzado %>% 
  filter(NM_BAIRRO != "", CD_UF == "MG") %>% 
  group_by(NM_BAIRRO) %>% 
  summarise(PC_TOTAL = n() / total_line3)

TT_BAIRRO <- left_join(QTD,PC, by ="NM_BAIRRO") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_BAIRRO %>% arrange(PC_TOTAL) %>% 
  filter(PC_TOTAL > 0.02)




###################### 14. PC de Índice de Presença Digital
class(Arq_03_Cruzado$DS_FAIXA_PERC_PRESENCA_DIGITAL)

total_line4 <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_PERC_PRESENCA_DIGITAL != "") %>% 
  summarise(QTD_TOTAL = n())


QTD <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_PERC_PRESENCA_DIGITAL != "") %>% 
  group_by(DS_FAIXA_PERC_PRESENCA_DIGITAL) %>% 
  summarise(QTD_TOTAL = n())

QTD$DS_FAIXA_PERC_PRESENCA_DIGITAL <- as.character(QTD$DS_FAIXA_PERC_PRESENCA_DIGITAL)
#QTD

PC <- QTD %>% 
  group_by(DS_FAIXA_PERC_PRESENCA_DIGITAL) %>% 
  summarise(PC_TOTAL = QTD_TOTAL / total_line4)

TT_FAIXA_PERC_PRESENCA_DIGITAL <- left_join(QTD,PC, by ="DS_FAIXA_PERC_PRESENCA_DIGITAL") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_FAIXA_PERC_PRESENCA_DIGITAL %>% arrange(PC_TOTAL) %>% 
  filter(PC_TOTAL > 0.02)




###################### 15. PC de Índice de Adoção/Adesão a Tecnologia
class(Arq_03_Cruzado$DS_FAIXA_PERC_ADOCAO_TECNOLOGIA)

total_line4 <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_PERC_ADOCAO_TECNOLOGIA != "") %>% 
  summarise(QTD_TOTAL = n())


QTD <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_PERC_ADOCAO_TECNOLOGIA != "") %>% 
  group_by(DS_FAIXA_PERC_ADOCAO_TECNOLOGIA) %>% 
  summarise(QTD_TOTAL = n())

QTD$DS_FAIXA_PERC_ADOCAO_TECNOLOGIA <- as.character(QTD$DS_FAIXA_PERC_ADOCAO_TECNOLOGIA)
#QTD

PC <- QTD %>% 
  group_by(DS_FAIXA_PERC_ADOCAO_TECNOLOGIA) %>% 
  summarise(PC_TOTAL = QTD_TOTAL / total_line4)

TT_FAIXA_PERC_ADOCAO_TECNOLOGIA <- left_join(QTD,PC, by ="DS_FAIXA_PERC_ADOCAO_TECNOLOGIA") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_FAIXA_PERC_ADOCAO_TECNOLOGIA %>% arrange(PC_TOTAL) %>% 
  filter(PC_TOTAL > 0.02)



###################### 16. Idade da Empresa
class(Arq_03_Cruzado$DS_FAIXA_IDADE_EMPRESA)

total_line4 <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_IDADE_EMPRESA != "") %>% 
  summarise(QTD_TOTAL = n())


QTD <- Arq_03_Cruzado %>% 
  filter(DS_FAIXA_IDADE_EMPRESA != "") %>% 
  group_by(DS_FAIXA_IDADE_EMPRESA) %>% 
  summarise(QTD_TOTAL = n())

QTD$DS_FAIXA_IDADE_EMPRESA <- as.character(QTD$DS_FAIXA_IDADE_EMPRESA)
#QTD

PC <- QTD %>% 
  group_by(DS_FAIXA_IDADE_EMPRESA) %>% 
  summarise(PC_TOTAL = QTD_TOTAL / total_line4)

TT_FAIXA_IDADE_EMPRESA <- left_join(QTD,PC, by ="DS_FAIXA_IDADE_EMPRESA") 
#sort(TT_PORTE_EMPRESA$QTD_TOTAL)
TT_FAIXA_IDADE_EMPRESA %>% arrange(PC_TOTAL) %>% 
  filter(PC_TOTAL > 0.02)
