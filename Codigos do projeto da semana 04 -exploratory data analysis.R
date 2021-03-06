##DATA SCIENCE
##M�dulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Quest�o-01
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Quest�o  -------------------------------------------------------------------------------------------
##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
##make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
##-----------------------------------------------------------------------------------------------------------------
## Defini��o da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
## Leitura do arquivo
arq01 <- readRDS("summarySCC_PM25.rds")
## Agregar as emiss�es de todos os anos
EmissoesAgregadas <- aggregate(Emissions ~ year,arq01, sum)
## Construir um gr�fico de todas as emiss�es por ano, usando o sistema "plotting"
barplot(
  (EmissoesAgregadas$Emissions)/10^6,
  names.arg=EmissoesAgregadas$year,
  xlab="Year (ano)",
  ylab="Emissions (Emiss�es)",
  col="blue",
  main="Total Emissions From PM2.5 in United States",
  legend = "Emissions X 10^6")
##----------------------------------------------------------------------------------------------------------------


##DATA SCIENCE
##M�dulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Quest�o-02
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Quest�o  -------------------------------------------------------------------------------------------
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question
##-----------------------------------------------------------------------------------------------------------------
## Defini��o da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
## Leitura do arquivo
arq01 <- readRDS("summarySCC_PM25.rds")
##Sele��o e agrega��o das informa��es de BALTIMORE
arqBaltimore <- arq01[arq01$fips=="24510",]
agregacaoBaltimore <- aggregate(Emissions ~ year, arqBaltimore,sum)
## Construir um gr�fico de todas as emiss�es por ano, com as informa��es de BALTIMORE
barplot(
  agregacaoBaltimore$Emissions,
  names.arg=agregacaoBaltimore$year,
  xlab="Year (ano)",
  ylab="Emissions (Emiss�es)",
  main="Total Emissions From PM2.5 in Baltimore City",
  col="green")
##----------------------------------------------------------------------------------------------------------------


##DATA SCIENCE
##M�dulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Quest�o-03
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Quest�o  -------------------------------------------------------------------------------------------
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
## Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
## a plot answer this question.
##-----------------------------------------------------------------------------------------------------------------
## Defini��o da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que ser� utilizada
library(ggplot2)
## Leitura do arquivo
arq01 <- readRDS("summarySCC_PM25.rds")
##Sele��o e agrega��o das informa��es de BALTIMORE
arqBaltimore <- arq01[arq01$fips=="24510",]
##Construir o gr�fico
print(
  ggplot(arqBaltimore,aes(factor(year),Emissions,fill=type)) +
    geom_bar(stat="identity") +
    theme_bw() + guides(fill=FALSE)+
    facet_grid(.~type,scales = "free",space="free") + 
    labs(x="year (ano)", y=expression("Total Emission From PM2.5")) + 
    labs(title=expression("Baltimore City - Emissions by Source Type")))
##-----------------------------------------------------------------------------------------------------

##DATA SCIENCE
##M�dulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Quest�o-04
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Quest�o  ----------------------------------------------------------------------
## Across the United States, how have emissions from coal combustion-related sources changed 
##from 1999-2008?
##-------------------------------------------------------------------------------------------
## Defini��o da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que ser� utilizada
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
##Construir o gr�fico
print (
ggplot(combustaoarq01,aes(factor(year),Emissions/10^5)) +
  geom_bar(stat="identity",fill="blue",width=0.60,color="yellow") +
  theme_bw() + guides(fill=FALSE) +
  labs(x="year [ano]", y=expression("Total Emission / (10^5) [Emiss�es]")) +
  labs(title=expression("Coal Combustion Source Emissions")))

##-----------------------------------------------------------------------------------------------------

##DATA SCIENCE
##M�dulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Quest�o-05
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Quest�o  ----------------------------------------------------------------------
##How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
##--------------------------------------------------------------------------------------------
## Defini��o da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que ser� utilizada
library(ggplot2)
## Leitura dos arquivos
arq01 <- readRDS("summarySCC_PM25.rds")
arq02 <- readRDS("Source_Classification_Code.rds")
##Tratamento dos dados
veiculo <- grepl("vehicle", arq02$SCC.Level.Two, ignore.case=TRUE)
veiculoarq02 <- arq02[veiculo,]$SCC
veiculoarq01 <- arq01[arq01$SCC %in% veiculoarq02,]
veiculobaltimorearq01 <- veiculoarq01[veiculoarq01$fips==24510,]
##Construir o gr�fico
print (
ggplot(veiculobaltimorearq01,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="green",width=0.60, color="blue") +
  theme_bw() + guides(fill=FALSE) +
  labs(x="year       [ano]", y=expression("Total Emission / (10^5)        [Emiss�es]")) +
  labs(title=expression("Motor Vehicle Emissions in Baltimore")))

##-----------------------------------------------------------------------------------------------------
##DATA SCIENCE
##M�dulo : Exploratory Data Analysis
##Semana 4 - Tarefa avaliada por colega : Projeto-02
##Quest�o-06
##Geraldo Barbosa do Amarante
##--------------------------------------------------
##Enunciado da Quest�o  ----------------------------------------------------------------------------
##Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
##sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
##over time in motor vehicle emissions?
##-------------------------------------------------------------------------------------------------
## Defini��o da pasta de trabalho
setwd('C:/Amarante/ExploratoryDataAnalysis')
##Definir a livraria que ser� utilizada
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
##Construir o gr�fico
print (
ggplot(juncaoarq01, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(stat="identity",aes(fill=year),color="green",fill="green") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year       [ano]", y=expression("Emissions      [Emiss�es]")) +
  labs(title=expression("Motor Vehicle Emissions in Baltimore and Los Angeles")))
##----------------------------------------------------------------------------------------------------






