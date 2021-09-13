#Coleta de dados#
library(readxl)
voto_jornal <- read_excel("C:/Users/milen/Desktop/jornal.xlsx")
View(voto)

#Preparando os dados#
library(wordcloud)
library(tm)
library(RColorBrewer)
library(cluster)
library(ggplot2)

tweets<-as.character(voto_jornal$Texto)
head(tweets,5)
rm(voto_jornal)

CORPO<-VCorpus(VectorSource(tweets))#Colocando em vetor
remove(tweets)
CORPO<-tm_map(CORPO, content_transformer(tolower)) #Deixando tudo em minúsculo
CORPO<-tm_map(CORPO, removePunctuation) #tirando pontuação
CORPO<-tm_map(CORPO, removeWords, stopwords("pt")) #Tirando as stopwords
CORPO<-tm_map(CORPO, removeNumbers) #tirando números

#Primeira visualizaçao
wordcloud(CORPO,min.freq=2,max.words=100, scale = c(1.8,.2))
formatacao <- brewer.pal(8,"Dark2")
wordcloud(CORPO,min.freq=2,max.words=100, random.order=T, colors=formatacao, scale = c(1.8,.2))

#Mas ainda aparece muito lixo
CORPO<-tm_map(CORPO, removeWords, c("ter", "apenas", "nesta","sobre","algo", "quer", "aqui","ainda","via", "após","diz","já","ter", "estão", "pra","são","vai", "pra", "ser", "passaporte", "vacina")) #tirar mais palavras indesejadas

#Segunda visualizaçao
wordcloud(CORPO,min.freq=200,max.words=100, scale = c(1.8,.2))
formatacao <- brewer.pal(8,"Dark2")
wordcloud(CORPO,min.freq=200,max.words=100, random.order=T, colors=formatacao, scale = c(1.8,.2))

#####
#Limpeza do texto com a Document Term Matrix
tweets_dtm <- DocumentTermMatrix(CORPO)   
tweets_dtm
tweets_frequencia <- colSums(as.matrix(tweets_dtm))   
length(tweets_frequencia) 

#Removendo termos esparços
tweets_dtms <- removeSparseTerms(tweets_dtm, 0.98) 
tweets_dtms
tweets_frequencia <- colSums(as.matrix(tweets_dtms))   
length(tweets_frequencia) 
tweets_frequencia <- sort(colSums(as.matrix(tweets_dtms)), decreasing=TRUE) 
tweets_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
tweets_plot <- data.frame(word=names(tweets_frequencia), freq=tweets_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(tweets_plot, tweets_frequencia>10), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico   


#####
#Machine learning - Clustering
tweets_dtms2 <- removeSparseTerms(tweets_dtms, 0.70)
tweets_dtms2
distancia <- dist(t(tweets_dtms2), method="euclidian")   

#Elbow Method
set.seed(123) 
k.max <- 15  
wss <- sapply ( 1:k.max, function (k) {kmeans (distancia, k, nstart = 50 , iter.max = 15 ) $tot.withinss }) 
wss 
plot( 1:k.max , wss, type = "b" , pch = 19 , frame = FALSE , 
      xlab = "Número de clusters K", ylab = "Soma total dos quadrados dentro dos clusters") 

library(fpc)
pamk.best <- pamk(distancia)
cat("Número de clusters estimado pelo método silhouette width:", pamk.best$nc, "\n")
plot(pam(distancia, pamk.best$nc))

#Clustering - K-Means
kmeans_btc <- kmeans(distancia, 2)   
clusplot(as.matrix(distancia), kmeans_btc$cluster, color=T, shade=T, labels=3, lines=0,
         main = "K-Means Jornais Passaporte da Vacina",
         xlab = "PC1",
         ylab = "PC2") 

#Pegar cada cluster
cluster<-as.data.frame(kmeans_btc$cluster)
cluster<-cbind(names=row.names(cluster),cluster)
Grupo1<-cluster$names[cluster$`kmeans_btc$cluster`==1]
Grupo2<-cluster$names[cluster$`kmeans_btc$cluster`==2]
Grupo1
Grupo2
