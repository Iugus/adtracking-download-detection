#setando diretorio de trabalho
setwd("C:/Cursos/Razure-1/Projeto1")
getwd()

#importando pacotes
library(tidyverse)
library(data.table)
library(lubridate)

#importando dataset para treino
inicial_Data<- fread("train.csv")

#verificando proporcoes de positivos do dataset
nrow(inicial_Data[is_attributed == 1,])/nrow(inicial_Data)

#Extraindo o dataset utilizando todos os registros de download possiveis
numero.downloads <- nrow(inicial_Data[is_attributed == 1,])
indices.download <- inicial_Data[,.I[is_attributed == 1]]
indices.negativo <- sample(inicial_Data[is_attributed == 0,.I],size = numero.downloads)
WorkData<- inicial_Data[c(indices.download,indices.negativo)]
rm(inicial_Data)
Train_rows <- sample(1:nrow(WorkData),nrow(WorkData)*0.7)
Train_Data <- WorkData[Train_rows,]
Test_Data <- WorkData[-Train_rows,]

?sample
#salvando o dataset
fwrite(WorkData, file = "Work_data.csv")

#verificando resultados da importacap
str(Train_Data)
str(Test_Data)
max(WorkData$click_time)
min(WorkData$click_time)
lapply(Train_Data,function(dt){table(is.na(dt))})

#visto que existem apenas tres dias de data apenas hora e dia se tornam relevantes para o treinamento visto que demais variaveis nao tem amostra grande o suficiente


#selecionando tipos das variveis
Changing_types <- function(dt){
fatores <- c("app","device","os","channel","is_attributed")
Char <- c("ip")
dt[,fatores] <- dt[,lapply(.SD,as.factor), .SDcols = fatores]
dt[,Char] <- dt[,lapply(.SD,as.character), .SDcols = Char]
return(dt)
}

Train_Data<-Changing_types(Train_Data)
Test_Data<-Changing_types(Test_Data)


#Criando Novas Variaveis Temporais

Adding_dates_columns <- function(df){df %>%
  mutate(Hour = hour(click_time),
         day = day(click_time))
  }

Train_Data <-Adding_dates_columns(Train_Data)
Test_Data <-Adding_dates_columns(Test_Data)
#plotando Graficos para analise de dados

#proporcao de downloads por fator
lapply(colnames(Train_Data),function(x){
  if(unlist(Train_Data[,lapply(.SD,is.factor),.SDcols = c(x)])){
    ggplot(Train_Data, aes_string(x)) +
      geom_bar()+
      facet_grid(. ~ is_attributed)+
      ggtitle(paste("Relacao de Downloads por ",x))
  }
})

#numero de fatores muito grande para observar qualquer padrao aparente com este tipo de graficoo, aparentemente
#a unica relacao eh que apps mais clicados recebem mais downloads


#criando funcao para analise de quantidade de cliques por hora a cada dia da semana

Resumo <- Train_Data %>%
  count(Hour  ,day,is_attributed)

Ddays <- unique(Resumo$day)


tms.plot <- function(Ddays){
  ggplot(Resumo[Resumo$day == Ddays,], aes(x = Hour, y= n, group = is_attributed, color = is_attributed))+
    geom_line() +
    ylab("Quantidade de cliques")+
    labs(title = paste("cliques no Dia ", as.character(Ddays),sep = ""))+
    theme(text = element_text(size = 20))
}

lapply(Ddays, tms.plot)

#verificando quantidade de downloads por dia
ggplot(Train_Data, aes(x = day))+
  geom_bar() +
  ylab("Quantidade de Cliques")+
  facet_grid(. ~ is_attributed)+
  labs(title = paste("cliques semanais"))

#criando modelos
library(e1071)
library(kernlab)
library(kknn)


#treinamento do modelo naive bayes
modelo1_naive <- naiveBayes(is_attributed ~ . -attributed_time, data = Train_Data)

predicao1 <- predict(modelo1_naive, Train_Data[,c(1:6,9:10)])

Tabela1 <- table(predicao1,Train_Data$is_attributed)

Tabela1[1,2]

#teste inicial para verificar se treinamento teve resultados satisfatorios
Calc_Perf_measures <-function(table1,measure){
  tp <- table1[1,1]
  tf <- table1[2,2]
  fp <- table1[2,1]
  fn <- table1[1,2]
  if (measure == "accuracy") {
    accuracy <- (tp+tf)/(tp+tf+fp+fn)
    return(accuracy)
  } else {
    if (measure == "recall") {
      recall <- tp/(tp+fn)
      return(recall)
    } else {
      if (measure == "precision") {
        precision <- tp/(tp+fp)
        return(precision)
      } else {
        if (measure == "fscore") {
          fscore <- 2*tp/(2*tp+fp+fn)
          return(f-score)
        } else {
          if (measure == "all") {
            fscore <- 2*tp/(2*tp+fp+fn)
            precision <- tp/(tp+fp)
            recall <- tp/(tp+fn) 
            accuracy <- (tp+tf)/(tp+tf+fp+fn)
            calc_perf <- c(accuracy, recall, precision, fscore)
            names(calc_perf)<- c("accuracy", "recall", "precision", "fscore")
            return(calc_perf)
          }
        }
        
      }
      
    }

  }
}

Calc_Perf_measures(Tabela1,"all")

#teste para verificar como algoritmo se sai com novos datasets
predicao_test <- predict(modelo1_naive, Test_Data[,c(1:6,9:10)])

tabela_test1 <- table(predicao_test,Test_Data$is_attributed)

Calc_Perf_measures(tabela_test1,"all")

#algoritmo teve accuracia de 86% e fscore de 87% o que demonstra ser um bom algoritmo, apenas para verificar
#possibilidade de melhora vamos tenstar outros algoritmos

#testando algoritmo svm


#nao foi possivel usar o modelo devido a limitacoes de memoria


#testando algoritmo knn


#considerar primeiro resultado visto impossibilidade de grande mudancas nas variaveis e problemas em testes seguintes
