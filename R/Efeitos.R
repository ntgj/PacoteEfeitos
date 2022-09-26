#' Funções baseadas na rotina fabi_efeito para Octave.
#'Cria grafico de porcentagem de efeito
#' @export
#' @param X matrix
#' @param y vector
#' @param k numeric variable


Porcentagem <-function(X,y,k){
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


Probabilidade <- function(X,y,k,PC,t=0.975){
  Desvio <- sd(PC)
  Erro <- 2*Desvio/(length(PC)*2^k)^0.5
  T <- qt((t),length(PC)-1)
  Ef<- X[,]
  tEf <- t(Ef)
  b<- inv(tEf%*%Ef)
  Efeito <- ((b)%*%(tEf%*%y))*2
  Q <- Efeito^2
  SQ <- sum(Q[1:length(Efeito),])
  porc <- (Efeito^2/(SQ))*100
  
  seq <- seq(0,length(Efeito)-1)
In <- character()
for (i in seq){
  my_out <- ((i/length(seq-1)))
  In <- c(In, my_out) 
}
Fim <- c(In[2:length(In)],as.numeric(last(In))+(1/length(In)))
Centro <-c((as.numeric(In)+as.numeric(Fim))/2)

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

#'Regressao
#' @export
#' @param X matrix
#' @param y vector
#' @param DF numeric variable
#' @param SSPE numeric variable

Regressao <- function(X,y,DF,SSPE){

X <- as.matrix(X)
y <- as.matrix(y)
        
        
tX <- t(X)
Coef<-inv(tX%*%X)%*%(tX%*%y)

DiagPrincipal <- diag(inv(tX%*%X))
Pred <- X%*%Coef
Errors<- (y-Pred)
n <- ncol(X)
m <- nrow(X)
DF <- DF

DFSSreg <- n-1 #DF SSReg
DFSSres <- m-n #DF SSres
DFSSTot <- m-1 #DF SSTot
DFSSPE <- DF #DF SSPE
DFSSLoF <- (m-n)-DF #DF SSLoF

SSREG <- sum((Pred-1*(mean(y)))^2)
SSres <- sum(Errors^2)
SSTot <- SSREG + SSres
SSPE <- SSPE
SSLoF <- SSres - SSPE
R2 <- SSREG/SSTot
R2Max <- (SSTot-SSPE)/SSTot
R <- sqrt(R2)
RMax <- sqrt(R2Max)


#Mean of Squares
MSREG <- SSREG/(n-1)
MSres <- SSres/(m-n)
MSTot <- SSTot/(m-1)
MSPE <- SSPE/DF
MSLoF <- SSLoF/((m-n)-DF)

#F tests
Ftest1 <- MSREG/MSres
F1tab <- qf(0.95,df1=DFSSreg,df2=DFSSres)
Ftest2 <- MSLoF/MSPE
F2tab <- qf(.95, df1=DFSSLoF, df2= DFSSPE)
qvec <- c("-",qt(0.975,(m-n)-1),"-","-",qt(0.975,((m-n)-DF)-1),"-","-") # FOR MSRES
qt(0.975,((m-n)-DF)-1) #FOR MSLOF


par(mfrow=c(2,2))
barplot(MSREG,1,main="MSREG",legend.text=round(MSREG,4),col = rainbow(20),ylim=range(pretty(c(0,MSREG+10))))
barplot(MSres,1,main="MSres e t",sub=qvec[2],legend.text=round(MSres,4),col="blue", ylim=range(pretty(c(0,MSres+0.5))))
barplot(MSPE,1,main="MSPE",legend.text=round(MSPE,4),col="green", ylim=range(pretty(c(0,MSPE+0.5))))
barplot(MSLoF,1,legend.text=round(MSLoF,4),
        col="black",main="MSLoF e t", sub=qvec[5], ylim=range(pretty(c(0,MSLoF+0.5))))

par(mfrow=c(1,3))
barplot(Ftest1,1,xlab="F1 Calculado",legend.text=round(Ftest1,4),col="blue", ylim=range(pretty(c(0,Ftest1+10))))
barplot(F1tab,1,xlab="F1 tabelado",legend.text=round(F1tab,4),col="blue", ylim=range(pretty(c(0,F1tab+10))))
barplot(Ftest1/F1tab,1,xlab="Calculado/Tabelado",legend.text=round(Ftest1/F1tab,4),col="green", 
        ylim=range(pretty(c(0,(Ftest1/F1tab)+5)))) + abline(h=10,col="red")

par(mfrow=c(1,3))
barplot(Ftest2,1,xlab="F2 Calculado",legend.text=round(Ftest2,4),col="purple", ylim=range(pretty(c(0,Ftest2+10))))
barplot(F2tab,1,xlab="F2 tabelado",legend.text=round(F2tab,4),col="purple", ylim=range(pretty(c(0,F2tab+10))))
barplot(Ftest2/F2tab,1,xlab="Calculado/Tabelado",legend.text=round(Ftest2/F2tab,4),col="red",
        ylim=range(pretty(c(0,(Ftest2/F2tab)+0.5)))) + abline(h=1,col="red")

par(mfrow=c(1,2))
barplot(R2,1,xlab="R²", legend.text=round(R2,4), col="yellow", ylim=range(pretty(c(0,1))))
barplot(R2Max, xlab="R²Max", legend.text=round(R2Max,4), col="yellow", ylim=range(pretty(c(0,1))))

Var <- readline(prompt="Insira o valor da variância")

t <- readline(prompt="Insira o valor de t")

DiagPrincipal<- as.matrix(DiagPrincipal)
Coef_a <- DiagPrincipal%*%as.numeric(Var)
Coef_e <- (Coef_a^0.5)*as.numeric(t)
Coef_e
Coef_L1 <- Coef - Coef_e
Coef_L2 <- Coef+ Coef_e
Pred_L1 <- X%*%Coef_L1
Pred_L2 <- X%*%Coef_L2

par(mfrow=c(1,1))
#Experimental x Previsto
plot(y, Pred, col="Red", xlab="Experimental", ylab="Previsto", 
     main="Experimental x Previsto",pch=19, 
     ylim = c(min(Pred_L1*1.05),max(Pred_L2*1.05)), xlim = c(min(y),max(y)))

points(y, Pred_L1, col="blue1", pch=4)
points(y, Pred_L2, col="darkorange4", pch=4)
reg <- lm(y~Pred)
legend("bottomright",legend=c("Previsto","Previsto Nível Baixo", "Previsto Nível Alto"),
       col=c("Red","blue1","darkorange4"), cex=0.8, pch=c(19,4,4),pt.cex=1)
clip(min(y),max(y),min(Pred + Pred_L1),max(Pred + Pred_L2))
abline(reg, col="blue", lwd=2)


#Histograma erros
par(mfrow=c(1,2))
hist(Errors,seq(min(Errors), max(Errors),length.out = 11),xlim=c(min(Errors),max(Errors)),
     main="Histograma Resíduos", xlab="Resíduos",ylab="Frequência", col="cadetblue3")

#Previsto x Resíduos
plot(Pred, Errors, col="darkgoldenrod1", main="Previsto x Resíduo", ylab="Resíduos", xlab="Previsto",pch=18)
clip(min(Pred),max(Pred),min(Errors),max(Errors))
abline(h=0, col="brown1")

par(mfrow=c(1,1))
#Coeficientes
plot(Coef, pch=15, xlab="", ylab="",ylim=c(min(1.25*Coef_L1),max(1.25*Coef_L2)),main="Coeficientes de Regressão",col="darkgoldenrod1")
points(Coef_L1, col="brown1", pch=3)
points(Coef_L2, col="blue3", pch=3)
clip(1,length(Coef),min(2*Coef_L1),max(2*Coef_L2))
abline(h=0, col="Red")
legend("topright",legend=c("Coeficientes","Coeficientes - Intervalo de Confiança", "Coeficientes +Intervalo de Confiança"),
       col=c("darkgoldenrod1","brown1","blue3"), cex=0.9, pch=c(15,3,3),pt.cex=1)
  Coeficientes <- cbind(Coef, Coef_L1, Coef_L2, Coef_e)
  colnames(Coeficientes) <- c("Coeficientes","-","+","IC")
return(Coeficientes)
}
