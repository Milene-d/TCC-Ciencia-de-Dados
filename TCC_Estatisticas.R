library(readxl)
Save <- read_excel("C:/Users/milen/Desktop/Save.xlsx", sheet = "tt_totais")
View(Save)

#Passando o formato de caracter para data
Save$Time<-strptime(Save$Time, format = "%Y-%m-%d")

#Quantidade diária de Tweets, média de favoritos e média de rts totais
library(dplyr)
Media_diaria<- count(Save %>% group_by(Save$Time))
Qtd_rt<-count(Save %>% group_by(Save$`Is Retweet`))
md_FV<-mean(Save$Favorited)
md_rt<-mean(Save$Retweeted)

# % de RTs na listagem total
porc<-4722/5886

#Gráfico para análisar os retweets
plot(Save$Retweeted, xlab = 'Tweets', ylab = 'Quantidade')

#De onde os usuários eram
localização<- count(Save %>% group_by(Save$`User - Location`))
colnames(localização)<-c('Lugar','Qtd')
localização<-as.data.frame(localização)
localização<-na.omit(localização)
top_lugar<- head(localização[order(localização$Qtd, decreasing = TRUE),], n=10)
