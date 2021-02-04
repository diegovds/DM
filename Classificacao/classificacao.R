library(rpart)
library(rpart.plot)

# csv de entrada
csv_entrada <- "C:\\ local \\AgeOfMythology_work.csv"

# csv das novas unidades
csv_novas_unidades <- "C:\\ local \\AgeOfMythology_noClass.csv"

# Leitura do csv
mydata <- read.csv(csv_entrada)

# Leitura do csv de novas unidades
noClass <- read.csv(csv_novas_unidades)

# dataframes recebem os dados dos csv
df <- data.frame(mydata)
df_noClass <- data.frame(noClass)

# Tratamento dos dados

# Substitui "*" por ""
df$DmgPierce <- as.integer(gsub("\\*", "0", df$DmgPierce))
df$DmgCrush <- as.integer(gsub("\\*", "0", df$DmgCrush))
df$Range <- as.integer(gsub("none", "0", df$Range))

# Substitui "_" por ""
df$HP <- as.integer(gsub("_", "", df$HP))

# Substitui "%" por ""
df$ArmorHack <- as.integer(gsub("%", "", df$ArmorHack))
df$ArmorPierce <- as.integer(gsub("%", "", df$ArmorPierce))
df$ArmorCrush <- as.integer(gsub("%", "", df$ArmorCrush))

# Substitui "--" por "0"
df$Age <- as.integer(gsub("--", "0", df$Age))
df$Pop <- as.integer(gsub("--", "0", df$Pop))
df$DmgHack <- as.integer(gsub("--", "0", df$DmgHack))
df$DmgPierce <- as.integer(gsub("--", "0", df$DmgPierce))
df$DmgCrush <- as.integer(gsub("--", "0", df$DmgCrush))
df$Train.time <- as.integer(gsub("--", "0", df$Train.time))

# Substitui os NA por 0
df[is.na(df)] <- 0

# Cria um dataframe somente com as colunas necessárias para o treinamento dos dados
clean_df <- data.frame(Age = df$Age, Pop = df$Pop, DmgHack = df$DmgHack, DmgPierce = df$DmgPierce, DmgCrush = df$DmgCrush, Range = df$Range, HP = df$HP, ArmorHack = df$ArmorHack, ArmorPierce = df$ArmorPierce, ArmorCrush = df$ArmorCrush, Speed = df$Speed, Train.time = df$Train.time, Class = df$Class)

# Conversão de character para factor e definição de levels
clean_df$Class <- factor(clean_df$Class, levels = c('Animal', 'Cavalry', 'Hero', 'Melee Infantary', 'Myth Unit', 'Ranged Cavalry', 'Ranged Infantary', 'Ship', 'Siege', 'Spear Infantary')) 

# Construção da árvore de classificação
modelo <- rpart(Class ~ 
                  Age + 
                  Pop + 
                  DmgHack + 
                  DmgPierce + 
                  DmgCrush + 
                  Range + 
                  HP + 
                  ArmorHack + 
                  ArmorPierce + 
                  ArmorCrush + 
                  Speed + 
                  Train.time, 
                data = clean_df,
                parms = list(split = "gini"),
                cp = 0.0002,
                control = rpart.control(
                  minsplit = 1,
                  minbucket = 1,
                  maxdepth = 25))

# Mostra a árvore
prp(modelo)

# Mostra um gráfico com o grau de importância dos atributos para a definição da classe
barplot(modelo$variable.importance)

# Previsões das classes das novas unidades
predi <- predict(object = modelo, newdata = df_noClass, type = 'class')
predi

# Probabilidade de acerto das classes de novas unidades 
predi1 <- predict(object = modelo, newdata = df_noClass, type = 'prob')
predi1