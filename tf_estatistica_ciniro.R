rm(list=ls())
library(car)
library(lmtest)

#CARREGAMENTO DOS DADOS INICIAIS------------------------------
entrada<-read.table(file="dadosiniciais.csv",header=T,sep=",")

#GERAÇÃO DAS AMOSTRAS INICIAIS---------------------------------
cwin <- entrada[1:30,]
iwin <- entrada[31:60,]
cubu <-  entrada[61:90,]
iubu <-  entrada[91:120,]
dadosiniciais <- rbind(cwin,iwin,cubu,iubu)

#ANÁLISE PRELIMINAR DE RESIDUOS--------------------------------
print(summary(dadosiniciais))

boxplot(tempo~base, 
        data = dadosiniciais,
        ylab = "Tempo (segundos)",
        names=c("Ubuntu Compilado","Windows Compilado","Ubuntu Interpretado","Windows Interpretado"))

library(car)
qqPlot(cwin$tempo, ylab = "Tempo (segundos) para Windows Compilado", xlab="")
qqPlot(iwin$tempo, ylab = "Tempo (segundos) para Windows Interpretado", xlab="")
qqPlot(cubu$tempo, ylab = "Tempo (segundos) para Ubuntu Compilado", xlab="")
qqPlot(iubu$tempo, ylab = "Tempo (segundos) para Ubuntu Interpretado", xlab="")

#CONFIGURAÇÃO DO EXPERIMENTO-----------------------------------
alpha <- 0.01
p <- 0.8
delta <- 0.1
a <- 4

#CALCULO DE TAMANHO AMOSTRAL-----------------------------------

#código compilado
varcwin = var(cwin$tempo)
varcubu = var(cubu$tempo)

#código interpretado
variwin = var(iwin$tempo)
variubu = var(iubu$tempo)

tau <- c(-delta/2, delta/2, rep(0, a-2))
vartau <- var(tau)

varmedia = (varcwin+varcubu+variwin+variubu)/a

testepot = power.anova.test(groups = a, 
                            between.var = vartau, 
                            within.var = varmedia, 
                            sig.level = alpha,
                            power = p)

n = ceiling(testepot$n)

print(testepot)

#ANALISE DE VARIÂNCIA------------------------------------------
amostrafinal<-read.table("amostrafinal.csv",header=T,sep=",")

cwin <- amostrafinal[1:193,]
iwin <- amostrafinal[194:386,]
cubu <-  amostrafinal[387:579,]
iubu <-  amostrafinal[580:772,]
amostra <- rbind(cwin,iwin,cubu,iubu)

print(summary(amostra))

boxplot(tempo~base, 
        data = amostra,
        ylab = "Tempo (segundos)",
        names=c("Ubuntu Compilado","Windows Compilado","Ubuntu Interpretado","Windows Interpretado"))

#Averiguação de diferenças com ANOVA
modelo <- aov(tempo~base,data = amostra)
print(summary.aov(modelo))

#AVALIAÇÃO DE PREMISSAS-----------------------------------------
#Averiguação de normalidade
sha = shapiro.test(modelo$residuals)
print(sha)

qqPlot(cwin$tempo, ylab = "Tempo (segundos) para Windows Compilado", xlab="")
qqPlot(iwin$tempo, ylab = "Tempo (segundos) para Windows Interpretado", xlab="")
qqPlot(cubu$tempo, ylab = "Tempo (segundos) para Ubuntu Compilado", xlab="")
qqPlot(iubu$tempo, ylab = "Tempo (segundos) para Ubuntu Interpretado", xlab="")

print(shapiro.test(amostra$tempo))
print(shapiro.test(cwin$tempo))
print(shapiro.test(iwin$tempo))
print(shapiro.test(cubu$tempo))
print(shapiro.test(iubu$tempo))


#Averiguação de homocedasticidade
fli = fligner.test(tempo~base, data = amostra)
print(fli)

plot(x    = modelo$fitted.values,
     y    = modelo$residuals,
     xlab = "Variâncias",
     ylab = "Amostras")

#Averiguação de Independência
durbinWatsonTest(modelo)

plot(x    = seq_along(modelo$residuals),
     y    = modelo$residuals,
     type = "l",
     las  = 1,
     lwd  = 2,
     lty  = 1,
     xlab = "Ordem dos resíduos",
     ylab = "Valor dos resíduos")

stop()
#DERIVAÇÃO DOS INTERVALOS DE CONFIANÇA----------------------------
#Comparação TODOS X TODOS
library(multcomp)
sistema_tukey <- glht(modelo, 
                    linfct = mcp(base = "Tukey"))
sistema_tukey_CI <- confint(sistema_tukey, 
                          level = (1-alpha))

print(sistema_tukey_CI)

par(mar = c(5,12,4,2))
plot(sistema_tukey_CI, 
     xlab       = "Tempos (segundos)",
     xlim       = c(-5,5),
     main=paste("Comparações com",(1-alpha)*100,"% de nível de confiança"))