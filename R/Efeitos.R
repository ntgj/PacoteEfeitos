#'Cria grafico de porcentagem de efeito
#' @export
#' @param X matrix
#' @param y vector
#' @param k numeric variable
#' @param PC numeric variable


Porcentagem <-function(X,y,k,PC){
  Ef<- X[,]
  tEf <- t(Ef)
  b<- inv(tEf%*%Ef)
  Efeito <- ((b)%*%(tEf%*%y))*2
  Q <- Efeito^2
  SQ <- sum(Q[1:length(Efeito),])
  porc <- (Efeito^2/(SQ))*100

  Efeitos <- c(1:length(Efeito))
  Porcentagem <- c(porc[1:length(Efeito),1])
  df <- data.frame(Efeitos,Porcentagem)
  ggplot(df, aes(x=Efeitos, y=Porcentagem,)) +  geom_bar(stat = "identity",color="Black", fill="Pink") +  theme_classic() + ylab("%") + 
  scale_x_continuous(breaks = seq(1,length(Efeito), by = 1))
}

#'Cria grafico de probabilidade de efeito
#' @export
#' @param X matrix
#' @param y vector
#' @param k numeric variable
#' @param PC numeric variable
#' @param t numeric variable


Probabilidade <- function(X,y,k,PC,t){
  Desvio <- sd(PC)
  Erro <- 2*Desvio/(3*2^k)^0.5
  T <- qt((t),length(PC)-1)
  Ef<- X[,]
  tEf <- t(Ef)
  b<- inv(tEf%*%Ef)
  Efeito <- ((b)%*%(tEf%*%y))*2
  Q <- Efeito^2
  SQ <- sum(Q[1:length(Efeito),])
  porc <- (Efeito^2/(SQ))*100
  Centro <- c((In+Fim)/2)
  Centro
  #_---------------#
  Z<-qnorm(Centro)
  mydf <- data.frame(Efeito)
  dfordem <- mydf %>% arrange((+Efeito))
  dfprobabilidade <- data.frame(dfordem,Z)
  label <- c(1:length(Efeito))
  dflabel <- data.frame(label,mydf)
  dflabel <- dflabel%>%arrange((+Efeito))
  #------------------------#
  ICE <- Erro*T
  ggplot(data = dfprobabilidade, aes_string(x = "Efeito", y = "Z")) +
    geom_vline(xintercept = ICE, color="Red") + geom_vline(xintercept = -ICE,color="Red") +
    geom_vline(xintercept = 0,color="Blue") +
    geom_label(label=dflabel[,1], size=3.5,color="Green 4") + theme_bw()
}
