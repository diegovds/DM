library(stringr)
library(tidyr)
library(arules)
library(arulesViz)
options(max.print=1000000)

#csv de entrada
csv_entrada <- "C:\\local\\_ASSOC_FuteVoleiStars.csv"

#csv auxiliar
csv_auxiliar <- "C:\\local\\ndf.csv"

#mydata recebe os dados do csv
mydata <- read.csv(csv_entrada)

#data frame df recebe mydata
df <- data.frame(Partida = mydata$Partida, Equipes = mydata$Jogadore.a.s, Resultado = mydata$Resultado, Jogadores = mydata$Jogadore.a.s.1)

#padronização de nomos
df$Jogadores[which(df$Jogadores == "Ágata")] <- "Agata"
df$Jogadores[which(df$Jogadores == "Bárbara")] <- "Barbara"

#colunas recebe a quantidade maxima de nomes na coluna equipes
colunas <- paste0("Nome", 1:(max(str_count(df$Equipes, ',')) + 1))

#as novas colunas jogares recebem os nomes da coluna equipes separados 
df = separate(df, Equipes, into = colunas, sep = ',', remove = FALSE, extra = "merge")

#novo data frame sem as colunas desnecessarias
ndf = data.frame(Partida = df$Partida, Jogador1 = df$Nome1, Jogador2 = df$Nome2, Jogador3 = df$Nome3, Resultado = df$Resultado)

#padronização dos dados
ndf$Jogador1[which(ndf$Jogador1 == "?gata")] <- "Agata"
ndf$Jogador1[which(ndf$Jogador1 == "Rob'erto")] <- "Roberto"
ndf$Jogador1[which(substring(ndf$Jogador1, 4) == "bara")] <- "Barbara"
ndf$Jogador1[which(ndf$Jogador1 == "shelda")] <- "Shelda"
ndf$Jogador1[which(ndf$Jogador1 == "romario")] <- "Romario"
ndf$Jogador1 <- paste("", ndf$Jogador1, sep=" ")

ndf$Jogador2[which(ndf$Jogador2 == " ?gata")] <- " Agata"
ndf$Jogador2[which(ndf$Jogador2 == " Rob'erto")] <- " Roberto"
ndf$Jogador2[which(substring(ndf$Jogador2, 5) == "bara")] <- " Barbara"
ndf$Jogador2[which(ndf$Jogador2 == " roberto")] <- " Roberto"
ndf$Jogador2[which(ndf$Jogador2 == " ronaldo")] <- " Ronaldo"

ndf$Jogador3[which(ndf$Jogador3 == " ?gata")] <- " Agata"
ndf$Jogador3[which(ndf$Jogador3 == " Rob'erto")] <- " Roberto"
ndf$Jogador3[which(substring(ndf$Jogador3, 5) == "bara")] <- " Barbara"
ndf$Jogador3[which(ndf$Jogador3 == " ana")] <- " Ana"
ndf$Jogador3[which(ndf$Jogador3 == " shelda")] <- " Shelda"
ndf$Jogador3[which(is.na(ndf$Jogador3))] <- ""

#criação de csv auxiliar para usar na regra de associação
write.csv(ndf,csv_auxiliar, row.names = FALSE)

#leitura das transações do arquivo
base = read.transactions(csv_auxiliar, header = T, sep = ",", rm.duplicates = T)

#equipe com mais vitorias
regras = apriori(base, 
                 parameter = list(sup = 0.1, conf = 0.5), 
                 appearance = list(default='lhs',rhs='GANHOU'), 
                 control = list(verbose=F))
inspect(sort(regras, by='count', decreasing=T))

#equipe com mais derrotas
regras = apriori(base, 
                 parameter = list(sup = 0.06, conf = 0.5), 
                 appearance = list(default='lhs',rhs='Perdeu'), 
                 control = list(verbose=F))
inspect(sort(regras, by='count', decreasing=T))