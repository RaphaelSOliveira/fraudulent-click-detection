# Este trabalho tem o objetivo de prever se um usuário fará o download de um aplicativo depois de clicar em um anúncio de aplicativo para celular.

# A maior plataforma independente de serviço de big data da China, cobre mais de 70% dos dispositivos móveis ativos em todo o país. 
# Eles processam 3 bilhões de cliques por dia, dos quais 90% são potencialmente fraudulentos. Sua abordagem atual para evitar a 
# fraude de cliques para desenvolvedores de aplicativos é medir a jornada do clique de um usuário em seu portfólio e sinalizar 
# endereços IP que produzem muitos cliques, mas nunca acabam instalando aplicativos.

# Carregando bibliotecas
suppressMessages(library(readr))
suppressMessages(library(knitr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(gridExtra))
suppressMessages(library(randomForest))
suppressMessages(library(DMwR))
suppressMessages(library(ROCR))
suppressMessages(library(e1071))
library(readr)
library(knitr)
library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)
library(gridExtra)
library(randomForest)
library(DMwR)
library(ROCR)
library(e1071)

# Carregando objetos do script auxiliar
source("src/Tools.R")

# Carregando datasets
train_sample <- read_csv("train_sample.csv")

# Alguns dados do dataset
dim(train_sample)
str(train_sample)

# Feature Selection
train_sample <- train_sample %>% 
                  mutate(wday = as.factor(weekdays(click_time, abbreviate=T))) %>%
                  mutate(hour = hour(click_time)) %>%
                  select(-c(click_time)) %>%
                  add_count(ip, wday, hour) %>% rename("nip_day_h" = n) %>%
                  add_count(ip, hour, channel) %>% rename("nip_h_chan" = n) %>%
                  add_count(ip, hour, os) %>% rename("nip_h_osr" = n) %>%
                  add_count(ip, hour, app) %>% rename("nip_h_app" = n) %>%
                  add_count(ip, hour, device) %>% rename("nip_h_dev" = n) %>%
                  select(-c(ip, attributed_time))

# nip_day_h = número de cliques de um mesmo IP, no mesmo dia e na mesma hora
# nip_h_chan = número de cliques de um mesmo IP, no mesma hora e do mesmo
# canal de anúncio
# nip_h_osr = número de cliques de um mesmo IP, na mesma hora e de um mesmo OS
# nip_h_app = número de cliques de um mesmo IP,  na mesma hora e no mesmo APP
# nip_h_dev = número de cliques de um mesmo IP,  na mesma hora e no mesmo 
# dispositivo

# Análise Exploratória 

# Valores únicos
unique_values <- as.data.frame(lapply(train_sample, function(x)length(unique(x))))
unique_values

# Verificando valores missing
sapply(train_sample, function(x) sum(is.na(x)))

# Visualizando a distribuição e os outliers das variáveis criadas durante o processo de feature selection

# Os boxplots não nos traz tanta informação visual nesse caso,
# pois os valores de primeiro quartil, mediana e terceiro quartil
# estão muito próximo. Porém é interessante notar a presença
# dos outliers
boxplot(train_sample$nip_day_h)

# Podemos ver que a média de clicks por ip em um mesmo dia e hora é aproximadamente 1. Portanto os outliers podem indicar a ação de bots.
summary(train_sample$nip_day_h)
sd(train_sample$nip_day_h)

# O mesmo padrão se repete para as outra variáveis
summary(train_sample$nip_h_chan)
sd(train_sample$nip_h_chan)

summary(train_sample$nip_h_osr)
sd(train_sample$nip_h_osr)

summary(train_sample$nip_h_app)
sd(train_sample$nip_h_app)

summary(train_sample$nip_h_dev)
sd(train_sample$nip_h_dev)

# Plot de frequência das variáveis APP, Device, OS, Channel, Hora e Dia da semana
# filtrando o dataset para dados ondenão houve download

h1 <- train_sample %>% group_by(app) %>% filter(is_attributed==FALSE) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% mutate(app = as.character(app)) %>% head(15) %>% 
  ggplot(aes(x = reorder(app, -count), y=count)) + geom_bar(stat='identity', color='skyblue',
                                                            fill="steelblue", alpha=0.9) + 
  ggtitle("Top Apps") + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(x = "APP")

h2 <- train_sample %>% group_by(device) %>% filter(is_attributed==FALSE) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% mutate(app = as.character(device)) %>% head(15) %>% 
  ggplot(aes(x = reorder(device, -count), y=count)) + geom_bar(stat='identity', color='skyblue',
                                                               fill="steelblue", alpha=0.9) + 
  ggtitle("Top Devices") + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(x = "Device")

h3 <- train_sample %>% group_by(os) %>% filter(is_attributed==FALSE) %>%  summarise(count = n())  %>%
  arrange(desc(count)) %>% mutate(app = as.character(os)) %>% head(15) %>% 
  ggplot(aes(x = reorder(os, -count), y=count)) + geom_bar(stat='identity', color='skyblue',
                                                           fill="steelblue", alpha=0.9) + 
  ggtitle("Top OS") + theme(axis.text.x=element_text(angle=45, hjust=1))  + labs(x = "OS")

h4 <- train_sample %>% group_by(channel) %>% filter(is_attributed==FALSE) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% mutate(app = as.character(channel)) %>% head(15) %>% 
  ggplot(aes(x = reorder(channel, -count), y=count)) + geom_bar(stat='identity', color='skyblue',
                                                                fill="steelblue", alpha=0.9) + 
  ggtitle("Top Channel") + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(x = "Channel")

h5 <- train_sample %>% group_by(wday) %>% filter(is_attributed==FALSE) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% mutate(app = as.character(wday)) %>% head(15) %>% 
  ggplot(aes(x = reorder(wday, -count), y=count)) + geom_bar(stat='identity', color='skyblue',
                                                             fill="steelblue", alpha=0.9) + 
  ggtitle("Top week days") + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Dia da semana")

h6 <- train_sample %>% group_by(hour) %>% filter(is_attributed==FALSE) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% mutate(app = as.character(hour)) %>% head(15) %>% 
  ggplot(aes(x = reorder(hour, -count), y=count)) + geom_bar(stat='identity', color='skyblue',
                                                             fill="steelblue", alpha=0.9) + 
  ggtitle("Top Hours") + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(x = "Hours")

grid.arrange(h1, h2, h3, h4, h5, h6, nrow = 2)

# Plot de frequência das variáveis APP, Device, OS, Channel, Hora e dia da semana filtrando o dataset para dados onde houve o download

h7 <- train_sample %>% filter(is_attributed==TRUE) %>% group_by(app) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  mutate(app = as.character(app)) %>% head(15) %>% 
  ggplot(aes(x = reorder(app, -count), y=count)) + geom_bar(stat='identity', color='skyblue', fill="steelblue", alpha=0.9) + 
  ggtitle("Top Apps") + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(x = "APP")

h8 <- train_sample %>% filter(is_attributed==TRUE) %>% group_by(device) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  mutate(app = as.character(device)) %>% head(15) %>% 
  ggplot(aes(x = reorder(device, -count), y=count)) + geom_bar(stat='identity', color='skyblue', fill="steelblue", alpha=0.9) + 
  ggtitle("Top Devices") + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(x = "Device")

h9 <- train_sample %>% filter(is_attributed==TRUE) %>% group_by(os) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  mutate(app = as.character(os)) %>% head(15) %>% 
  ggplot(aes(x = reorder(os, -count), y=count)) + geom_bar(stat='identity', color='skyblue', fill="steelblue", alpha=0.9) + 
  ggtitle("Top OS") + theme(axis.text.x=element_text(angle=45, hjust=1))  + labs(x = "OS")

h10 <- train_sample %>% filter(is_attributed==TRUE) %>% group_by(channel) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  mutate(app = as.character(channel)) %>% head(15) %>% 
  ggplot(aes(x = reorder(channel, -count), y=count)) + geom_bar(stat='identity', color='skyblue', fill="steelblue", alpha=0.9) + 
  ggtitle("Top Channel") + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(x = "Channel")

h11 <- train_sample %>% filter(is_attributed==TRUE) %>% group_by(wday) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  mutate(app = as.character(wday)) %>% head(15) %>% 
  ggplot(aes(x = reorder(wday, -count), y=count)) + geom_bar(stat='identity', color='skyblue', fill="steelblue", alpha=0.9) + 
  ggtitle("Top week days") + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(x = "Dia da semana")

h12 <- train_sample %>% filter(is_attributed==TRUE) %>% group_by(hour) %>% summarise(count = n()) %>% arrange(desc(count)) %>%
  mutate(app = as.character(hour)) %>% head(15) %>% 
  ggplot(aes(x = reorder(hour, -count), y=count)) + geom_bar(stat='identity', color='skyblue', fill="steelblue", alpha=0.9) + 
  ggtitle("Top Hours") + theme(axis.text.x=element_text(angle=45, hjust=1)) + labs(x = "Hours")

grid.arrange(h7, h8, h9, h10, h11, h12, nrow = 2)

# Transformando variáveis em fator 
train_sample <- to.factors(train_sample, factColNames)

# Verificando balanceamento do dataset
table(train_sample$is_attributed)
prop.table(table(train_sample$is_attributed))

# Balanceamento do dataset
balanced_train_sample <- SMOTE(is_attributed ~ ., as.data.frame(train_sample), k = 3, perc.over = 400, perc.under = 150)
table(balanced_train_sample$is_attributed)
prop.table(table(balanced_train_sample$is_attributed))

# Machine Learning

# Dividino o dataset em dados de treino e dados e teste
set.seed(123)
smp_size <- floor(0.70 * nrow(balanced_train_sample))
train_ind <- sample(seq_len(nrow(balanced_train_sample)), size = smp_size)

train <- balanced_train_sample[train_ind, ]
test <- balanced_train_sample[-train_ind, ]

# Analisando relevância das variáveis para o modelo preditivo
# Aqui utilizo o algorítimo randomforest como ferramenta para averiguar a importância das variáveis para o modelo preditivo
modelo_rf1 <- randomForest(is_attributed ~ ., data=train, importance=TRUE)
varImpPlot(modelo_rf1)

# Treinando novamente o modelo com as 7 variáveis de maior relevância
# Foi adotado o método Gini para a escolha das variáveis
modelo_rf1 <- randomForest(is_attributed ~ app + channel + hour + device + os + nip_h_dev + nip_day_h, data=train)

# Análise da performance do modelo
# Criando dataframe com valores observados historicamente e com os valores previstos pelo modelo de machine learning
score_model <- data.frame(observado = test$is_attributed,
                          previsto = predictions <- predict(modelo_rf1, test[,-5]))

confusionMatrix(score_model$observado, score_model$previsto)

modelo_rf2 <- randomForest(is_attributed ~ app + channel + hour + device + os + nip_h_dev + nip_day_h, data=train, ntree = 300, nodesize = 3)

score_model2 <- data.frame(observado = test$is_attributed,
                          previsto = predictions <- predict(modelo_rf2, test[,-5]))

confusionMatrix(score_model2$observado, score_model2$previsto)

# Testando outro algorítimo de classificação SVM

# Utilizando kernel linear
modelo_svm_linear = tune.svm(is_attributed ~ app + channel + hour + device + os + nip_h_dev + nip_day_h, data=train,kernel="linear") 

# Performance do modelo
best.linear=modelo_svm_linear$best.model
best.test=predict(best.linear,newdata=test,type="class")
confusionMatrix(best.test,test$is_attributed)

# Utilizando kernel radial
modelo_svm_radial = tune.svm(is_attributed ~ app + channel + hour + device + os + nip_h_dev + nip_day_h, data=train,kernel="radial") 

# Performance do modelo
best.radial=modelo_svm_radial$best.model
best.test=predict(best.radial,newdata=test,type="class")
confusionMatrix(best.test,test$is_attributed)

# Chamando a Função para executar o registrar todos os resultados do modelo
mult_rf <- nb_multiple_runs(train, 20)
plot(mult_rf)
# Média de acurácia do modelo
summary(mult_rf)           

# Criando curvas ROC
# Gerando as classes de dados
class1 <- predict(modelo_rf2, newdata = test, type = 'prob')
class2 <- test$is_attributed

# Gerando a curva ROC
pred <- prediction(class1[,2], class2)
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))