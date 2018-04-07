#-----------------------------------------
#
# ANÁLISE COMPARATIVA DE TEMPOS DE EXECUÇÃO 
# DE CÓDIGO R INTERPRETADO E COMPILADO EM 
# SISTEMAS WINDOWS E LINUX
#
# Ciniro Aparecido Leite Nametala
# ciniro@gmail.com - IFMG Campus Bambui
#
#------------------------------------------


rm(list=ls())
cat('\014')
library("car")
library("lmtest")
library("agricolae")
library("multcomp")

fonte <- 0.7

#CARREGAMENTO DOS DADOS INICIAIS------------------------------
entrada<-read.table(file="dadosiniciais.csv",header=T,sep=",")

#GERAÇÃO DAS AMOSTRAS INICIAIS---------------------------------
cwin <- entrada[1:30,]
iwin <- entrada[31:60,]
cubu <-  entrada[61:90,]
iubu <-  entrada[91:120,]
dadosiniciais <- rbind(cwin,iwin,cubu,iubu)

#ANÁLISE PRELIMINAR--------------------------------
print("ANÁLISE PRELIMINAR DAS AMOSTRAS INICIAIS")
print("-----------------------------------------------")
print("Sumarizacao das amostras iniciais como um todo:")
print(summary(dadosiniciais))
print("Standard Deviation")
print(sd(dadosiniciais$tempo))
print("-----------------------------------------------")
print("Sumarizacao das amostras individualizadas:")
print("Windows compilado----------------")
print(summary(cwin$tempo))
print("Standard Deviation")
print(sd(cwin$tempo))
print("-----------------------------------------------")
print("Windows interpretado----------------")
print(summary(iwin$tempo))
print("Standard Deviation")
print(sd(iwin$tempo))
print("-----------------------------------------------")
print("Ubuntu compilado----------------")
print(summary(cubu$tempo))
print("Standard Deviation")
print(sd(cubu$tempo))
print("-----------------------------------------------")
print("Ubuntu interpretado----------------")
print(summary(iubu$tempo))
print("Standard Deviation")
print(sd(iubu$tempo))
print("-----------------------------------------------")


boxplot(tempo~base, 
        data = dadosiniciais,
        ylab = "Tempo (segundos)",
        names=c("Ubuntu Compilado","Windows Compilado","Ubuntu Interpretado","Windows Interpretado"), cex.lab=fonte, cex.axis=fonte)

par(cex.lab=fonte)
par(cex.axis=fonte)
qqPlot(cwin$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Windows Compilado")
qqPlot(iwin$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Windows Interpretado")
qqPlot(cubu$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Ubuntu Compilado")
qqPlot(iubu$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Ubuntu Interpretado")


print("-----------------------------------------------")
print("TESTE DE NORMALIDADE PARA CADA UMA DAS AMOSTRAS")
print("-----------------------------------------------")
print("Windows compilado----------------")
print(shapiro.test(cwin$tempo))
print("-----------------------------------------------")
print("Windows interpretado----------------")
print(shapiro.test(iwin$tempo))
print("-----------------------------------------------")
print("Ubuntu compilado----------------")
print(shapiro.test(cubu$tempo))
print("-----------------------------------------------")
print("Ubuntu interpretado----------------")
print(shapiro.test(iubu$tempo))
print("-----------------------------------------------")

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
print("-----------------------------------------------")
print("RESULTADO DO CALCULO DE TAMANHO AMOSTRAL VIA TESTE DE POTENCIA ANOVA")
print(testepot)
print("-----------------------------------------------")

#ANALISE DA AMOSTRA FINAL------------------------------------------
amostrafinal<-read.table("amostrafinal.csv",header=T,sep=",")

cwin <- amostrafinal[1:n,]
iwin <- amostrafinal[147:(146+n),]
cubu <-  amostrafinal[293:(292+n),]
iubu <-  amostrafinal[439:(438+n),]
amostra <- rbind(cwin,iwin,cubu,iubu)

print("ANÁLISE PRELIMINAR DAS AMOSTRAS FINAIS")
print("-----------------------------------------------")
print("Analise das amostras finais como um todo")
print(summary(amostra))
print("Standard Deviation")
print(sd(amostra$tempo))
print("-----------------------------------------------")
print("Sumarizacao das amostras individualizadas:")
print("Windows compilado----------------")
print(summary(cwin$tempo))
print("Standard Deviation")
print(sd(cwin$tempo))
print("-----------------------------------------------")
print("Windows interpretado----------------")
print(summary(iwin$tempo))
print("Standard Deviation")
print(sd(iwin$tempo))
print("-----------------------------------------------")
print("Ubuntu compilado----------------")
print(summary(cubu$tempo))
print("Standard Deviation")
print(sd(cubu$tempo))
print("-----------------------------------------------")
print("Ubuntu interpretado----------------")
print(summary(iubu$tempo))
print("Standard Deviation")
print(sd(iubu$tempo))
print("-----------------------------------------------")

boxplot(tempo~base, 
        data = amostra,
        ylab = "Tempo (segundos)",
        names=c("Ubuntu Compilado","Windows Compilado","Ubuntu Interpretado","Windows Interpretado"), cex.lab=fonte, cex.axis=fonte)

qqPlot(cwin$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Windows Compilado")
qqPlot(iwin$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Windows Interpretado")
qqPlot(cubu$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Ubuntu Compilado")
qqPlot(iubu$tempo, ylab = "Tempo (segundos)", xlab="", cex = 0.7, lwd = 0.5, grid = FALSE, main="Ubuntu Interpretado")

#AVERIGUACAO PRA USO DE ANOVA PARAMETRICA-----------
modelo <- aov(tempo~base,data = amostra)
print("-----------------------------------------------")
print("AVERIGUACAO DE PREMISSAS PARA APLICACAO DE ANOVA PARAMETRICA")
print("-----------------------------------------------")
print(summary.aov(modelo))
print("-----------------------------------------------")

#Averiguação de normalidade
print("Premissa de normalidade:")
sha = shapiro.test(modelo$residuals)
print(sha)
print("-----------------------------------------------")

#Averiguação de homocedasticidade
print("Premissa de homocedasticidade:")
fli = fligner.test(tempo~base, data = amostra)
print(fli)
print("-----------------------------------------------")

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

#TESTE COM KRUSKAL-WALLIS---------------------------------------
#averigua os p-valores entre as amostras
valores <- c(cwin$tempo, iwin$tempo, cubu$tempo, iubu$tempo)
aux <- c(
  rep(1,length(cwin$tempo)), 
  rep(2,length(iwin$tempo)),
  rep(3,length(cubu$tempo)),
  rep(4,length(iubu$tempo)) )
grupos <- factor(aux, labels=c("cwin","iwin","cubu","iubu"))
wilc <- pairwise.wilcox.test(valores, grupos, paired=FALSE, p.adjust.method = "bon", conf.level=1-alpha)

print("-----------------------------------------------")
print("APLICACAO DE KRUSKAL-WALLIS NAO PARAMETRICO")
print(wilc)
print("-----------------------------------------------")

#averigua a magnitude das diferencas
print("-----------------------------------------------")
print("AVERIGUACAO DAS MAGNITUDES DAS DIFERENCAS ENTRE AMOSTRAS")
kw<-with(amostra,kruskal(tempo,base,p.adj="bon",group=FALSE, main="amostra", alpha=alpha), console=TRUE)
print(kw$comparison)
print("-----------------------------------------------")

#TESTE COM ANOVA--------------------------------------------------
#DERIVAÇÃO DOS INTERVALOS DE CONFIANÇA----------------------------
#Comparação TODOS X TODOS
sistema_tukey <- glht(modelo, 
                    linfct = mcp(base = "Tukey"))
sistema_tukey_CI <- confint(sistema_tukey, 
                          level = (1-alpha))

print("-----------------------------------------------")
print("APLICACAO DE TUKEY TODOS CONTRA TODOS")
print(sistema_tukey_CI)
print("-----------------------------------------------")

#titulo = paste("Comparações com",(1-alpha)*100,"% de nível de confiança")
titulo=""
par(mar = c(5,12,4,2))
plot(sistema_tukey_CI, 
     xlab       = "Tempos (segundos)",
     xlim       = c(-0.5,1),
     main=titulo)
