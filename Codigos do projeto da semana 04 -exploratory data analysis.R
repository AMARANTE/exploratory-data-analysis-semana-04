##DATA SCIENCE
##Módulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Questão-01
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Questão  -------------------------------------------------------------------------------------------
##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
##make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
##-----------------------------------------------------------------------------------------------------------------
## Definição da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
## Leitura do arquivo
arq01 <- readRDS("summarySCC_PM25.rds")
## Agregar as emissões de todos os anos
EmissoesAgregadas <- aggregate(Emissions ~ year,arq01, sum)
## Construir um gráfico de todas as emissões por ano, usando o sistema "plotting"
barplot(
  (EmissoesAgregadas$Emissions)/10^6,
  names.arg=EmissoesAgregadas$year,
  xlab="Year (ano)",
  ylab="Emissions (Emissões)",
  col="blue",
  main="Total Emissions From PM2.5 in United States",
  legend = "Emissions X 10^6")
##----------------------------------------------------------------------------------------------------------------


##DATA SCIENCE
##Módulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Questão-02
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Questão  -------------------------------------------------------------------------------------------
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question
##-----------------------------------------------------------------------------------------------------------------
## Definição da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
## Leitura do arquivo
arq01 <- readRDS("summarySCC_PM25.rds")
##Seleção e agregação das informações de BALTIMORE
arqBaltimore <- arq01[arq01$fips=="24510",]
agregacaoBaltimore <- aggregate(Emissions ~ year, arqBaltimore,sum)
## Construir um gráfico de todas as emissões por ano, com as informações de BALTIMORE
barplot(
  agregacaoBaltimore$Emissions,
  names.arg=agregacaoBaltimore$year,
  xlab="Year (ano)",
  ylab="Emissions (Emissões)",
  main="Total Emissions From PM2.5 in Baltimore City",
  col="green")
##----------------------------------------------------------------------------------------------------------------


##DATA SCIENCE
##Módulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Questão-03
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Questão  -------------------------------------------------------------------------------------------
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
## Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
## a plot answer this question.
##-----------------------------------------------------------------------------------------------------------------
## Definição da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que será utilizada
library(ggplot2)
## Leitura do arquivo
arq01 <- readRDS("summarySCC_PM25.rds")
##Seleção e agregação das informações de BALTIMORE
arqBaltimore <- arq01[arq01$fips=="24510",]
##Construir o gráfico
print(
  ggplot(arqBaltimore,aes(factor(year),Emissions,fill=type)) +
    geom_bar(stat="identity") +
    theme_bw() + guides(fill=FALSE)+
    facet_grid(.~type,scales = "free",space="free") + 
    labs(x="year (ano)", y=expression("Total Emission From PM2.5")) + 
    labs(title=expression("Baltimore City - Emissions by Source Type")))
##-----------------------------------------------------------------------------------------------------

##DATA SCIENCE
##Módulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Questão-04
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Questão  ----------------------------------------------------------------------
## Across the United States, how have emissions from coal combustion-related sources changed 
##from 1999-2008?
##-------------------------------------------------------------------------------------------
## Definição da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que será utilizada
library(ggplot2)
## Leitura dos arquivos
arq01 <- readRDS("summarySCC_PM25.rds")
arq02 <- readRDS("Source_Classification_Code.rds")
##Tratamento dos dados
comb <- grepl("comb", arq02$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", arq02$SCC.Level.Four, ignore.case=TRUE)
coalComb <- (comb & coal)
combustaoarq02 <- arq02[coalComb,]$SCC
combustaoarq01 <- arq01[arq01$SCC %in% combustaoarq02,]
##Construir o gráfico
print (
ggplot(combustaoarq01,aes(factor(year),Emissions/10^5)) +
  geom_bar(stat="identity",fill="blue",width=0.60,color="yellow") +
  theme_bw() + guides(fill=FALSE) +
  labs(x="year [ano]", y=expression("Total Emission / (10^5) [Emissões]")) +
  labs(title=expression("Coal Combustion Source Emissions")))

##-----------------------------------------------------------------------------------------------------

##DATA SCIENCE
##Módulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Questão-05
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Questão  ----------------------------------------------------------------------
##How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
##--------------------------------------------------------------------------------------------
## Definição da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que será utilizada
library(ggplot2)
## Leitura dos arquivos
arq01 <- readRDS("summarySCC_PM25.rds")
arq02 <- readRDS("Source_Classification_Code.rds")
##Tratamento dos dados
veiculo <- grepl("vehicle", arq02$SCC.Level.Two, ignore.case=TRUE)
veiculoarq02 <- arq02[veiculo,]$SCC
veiculoarq01 <- arq01[arq01$SCC %in% veiculoarq02,]
veiculobaltimorearq01 <- veiculoarq01[veiculoarq01$fips==24510,]
##Construir o gráfico
print (
ggplot(veiculobaltimorearq01,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="green",width=0.60, color="blue") +
  theme_bw() + guides(fill=FALSE) +
  labs(x="year       [ano]", y=expression("Total Emission / (10^5)        [Emissões]")) +
  labs(title=expression("Motor Vehicle Emissions in Baltimore")))

##-----------------------------------------------------------------------------------------------------
##DATA SCIENCE
##Módulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Questão-06
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Questão  ----------------------------------------------------------------------------
##Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
##sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
##over time in motor vehicle emissions?
##-------------------------------------------------------------------------------------------------
## Definição da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que será utilizada
library(ggplot2)
## Leitura dos arquivos
arq01 <- readRDS("summarySCC_PM25.rds")
arq02 <- readRDS("Source_Classification_Code.rds")
##Tratamento dos dados
veiculo <- grepl("vehicle", arq02$SCC.Level.Two, ignore.case=TRUE)
veiculoarq02 <- arq02[veiculo,]$SCC
veiculoarq01 <- arq01[arq01$SCC %in% veiculoarq02,]
veiculobaltimorearq01 <- veiculoarq01[veiculoarq01$fips==24510,]
veiculobaltimorearq01$city <- "Cidade de Baltimore"
veiculolosangelesarq01 <- veiculoarq01[veiculoarq01$fips=="06037",]
veiculolosangelesarq01$city <- "Condado de Los Angeles"
juncaoarq01 <- rbind(veiculobaltimorearq01,veiculolosangelesarq01)
##Construir o gráfico
print (
ggplot(juncaoarq01, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(stat="identity",aes(fill=year),color="green",fill="green") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year       [ano]", y=expression("Emissions      [Emissões]")) +
  labs(title=expression("Motor Vehicle Emissions in Baltimore and Los Angeles")))
##----------------------------------------------------------------------------------------------------






