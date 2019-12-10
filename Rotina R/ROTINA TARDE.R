########################################################################
###########           CURSO:INTRODUÇÃO AO SOFTWARE R         ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     11/12/2019                   ###########
########################################################################

##------------------------ ANÁLISE DE VARIÂNCIA ---------------------------##

setwd("C:\\Users\\Usuario\\Desktop\\CURSO R 2019")
dir()
dadosDIC<-read.table("dic.txt", h=T)
str(dadosDIC)  
dadosDIC<-transform(dadosDIC, Trat=factor(Trat))
str(dadosDIC)

# Saber o modelo dos delineamentos é importante, pois indicaremos para o R como é o
# modelo a ser utilizado.

#aov(formula, data = NULL)


## MODELO DIC:   Yij = m + Ti + eij

AOVDIC<-aov(Prod ~ Trat, data = dadosDIC) 

anova(AOVDIC)    # Quadro de Análise de Variância

library(agricolae)
cv.model(AOVDIC) # CV%

## MODELO DBC:   Yij = m + Ti + bj + eij
dadosDBC<-read.table("dbc.txt", h=T)
str(dadosDBC)
dadosDBC<-transform(dadosDBC, Gen=factor(Gen), Bloco=factor(Bloco))
str(dadosDBC) #Check!

AOVDBC<-aov(Prod ~ Gen + Bloco, data = dadosDBC) 

anova(AOVDBC)    #Quadro de Análise de Variância
cv.model(AOVDBC) #CV%

##------------------------ TESTE DE MÉDIAS ---------------------------##

#DIC
#Tukey
library(agricolae)
#HSD.test(y, trt, DFerror, MSerror, alpha=0.05)

anova(AOVDIC) 

y<-dadosDIC$Prod
trt<-dadosDIC$Trat
DFerror<-df.residual(AOVDIC)
MSerror<-((deviance(AOVDIC)/DFerror))

TukeyDIC<-HSD.test(y, trt, DFerror, MSerror, alpha=0.05)
TukeyDIC
TukeyDIC$groups

#ScottKnott
library(ScottKnott)
skDIC <- SK(x=dadosDIC, y=dadosDIC$Prod, model="y~Trat", which="Trat", sig.level=0.05, id.trim=10)
summary(skDIC)

#DBC
#Tukey
anova(AOVDBC)

y2<-dadosDBC$Prod
trt2<-dadosDBC$Gen
DFerror2<-df.residual(AOVDBC)
MSerror2<-((deviance(AOVDBC)/DFerror2))

TukeyDBC<-HSD.test(y=y2, trt=trt2, DFerror=DFerror2, MSerror=MSerror2, alpha=0.05)
TukeyDBC

           
#ScottKnott
library(ScottKnott)
skDBC <- SK(x=dadosDBC, y=dadosDBC$Prod, model="y~Gen", which="Gen", sig.level=0.05, id.trim=10)
summary(skDBC)   



##------------------------ PACOTE ExpDes.pt ---------------------------##

library(ExpDes.pt)

# DIC
# Exemplo - Trat Qualitativo

#   dic(trat=dados$LIN, resp=dados$DIAM, quali = TRUE, mcomp = "tukey", nl = FALSE,
#    hvar='bartlett', sigT = 0.05, sigF = 0.05)  

dic(trat=dadosDIC$Trat, resp=dadosDIC$Prod, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

# Dificil de ler -> Vamos criar um arquivo TXT
sink("Saida ANOVA DIC.txt", type = c ("output"))

dic(trat=dadosDIC$Trat, resp=dadosDIC$Prod, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)

sink()



# DBC
# Exemplo 1 - Trat Qualitativo

sink("Saida ANOVA DBC - QUALITATIVO.txt", type = c ("output"))

dbc(trat=dadosDBC$Gen, bloco=dadosDBC$Bloco, resp=dadosDBC$Prod, quali = TRUE, mcomp = "tukey", nl=FALSE,
    hvar='oneillmathews', sigT = 0.05, sigF = 0.05)

sink()

# DBC
# Exemplo 2 - Trat Quantitativo

dadosDBC2<-read.table("dbc2.txt", h=T)
str(dadosDBC2)
dadosDBC2<-transform(dadosDBC2, Bloco=factor(Bloco))
str(dadosDBC2)

sink("Saida ANOVA DBC - QUANTITATIVO.txt", type = c ("output"))

analiseDBC<-dbc(trat=dadosDBC2$Epoca, bloco=dadosDBC2$Bloco, resp=dadosDBC2$Brix, quali = FALSE)

sink()

# Linear
graficos(analiseDBC, grau = 1, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")

# Quadratica
graficos(analiseDBC, grau = 2, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "Epoca", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")



##------------------------ ESQUEMA FATORIAL ---------------------------##


#FATORIAL DUPLO -> y = m + Fator1 + Fator2 + Fator1*Fator2 + "Bloco" + erro
FATDUPLO=read.table("fatdbc.txt", h=T)
str(FATDUPLO)
FATDUPLO<-transform(FATDUPLO, BLOCO=factor(BLOCO), GEN=factor(GEN))
str(FATDUPLO)

attach(FATDUPLO)
sink("Saida ANOVA FATORIAL DBC.txt", type = c ("output"))
analiseFAT<-fat2.dbc(fator1=GEN, fator2=DOSE, bloco=BLOCO, resp=RESP, quali = c(TRUE, FALSE), mcomp = "tukey", 
         fac.names = c("GENÓTIPOS", "DOSES"), sigT = 0.05, sigF = 0.05)
sink()
detach(FATDUPLO)

DBC<-dbc(trat=FATDUPLO$DOSE, bloco=FATDUPLO$BLOCO, resp=FATDUPLO$RESP, quali = FALSE)

graficos(DBC, grau = 1, mod = TRUE, main = "Gráfico Regressão ", sub = " ",
         xlab = "DOSE", ylab = "RESP", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")


##------------------------ PARCELA SUBDIVIDIDA ---------------------------##

#PARCELA SUB -> y = m +Fator1 +Bloco +Fator1*Bloco(ERRO A) +Fator2 +Fator1*Fator2 +erro B

SUB=read.table("psubdiv.txt", h=T)
str(SUB)
SUB<-transform(SUB, Bloco=factor(Bloco), Genotipo=factor(Genotipo))
str(SUB)

modelo <- aov(Altura ~ Bloco + Genotipo + Error(Bloco:Genotipo) + Aplic + Genotipo:Aplic, data=SUB) # termo Error para declarar erro A
summary(modelo)


attach(SUB)
sink("Saida ANOVA PARCELA SUB.txt", type = c ("output"))
SUBDIC<-psub2.dic(Genotipo, Aplic, Bloco, Altura, quali = c(TRUE, TRUE), 
             mcomp = "tukey", fac.names = c("Genotipos", "Aplicação"), sigT = 0.05, 
             sigF = 0.05)
sink()
detach(SUB)


##--------------------------------- FIM ----------------------------------##




##--------------------------- Conteúdo extra -----------------------------##



##-------------------------- ANÁLISE CONJUNTA ---------------------------##

exp<-read.table("conjunta.txt", h = TRUE) #Leitura da tabela completa com todos os ambientes
exp<-transform(exp, GEN=factor(GEN), REP=factor(REP), AMB=factor(AMB))
str(exp)

#Amb 1
exp1<-subset(exp,AMB==1)
str(exp1)
aov1<-aov(PROD ~ GEN + REP, data=exp1)
shapiro.test(aov1$residuals)
bartlett.test(aov1$residuals ~ GEN, data=exp1) 
summary(aov1)

#Amb 2
exp2<-subset(exp,AMB==2)
str(exp2)
aov2<-aov(PROD ~ GEN + REP, data=exp2)
shapiro.test(aov2$residuals)
bartlett.test(aov2$residuals ~ GEN, data=exp2) 
summary(aov2)

#Amb 3
exp3<-subset(exp,AMB==3)
str(exp3)
aov3<-aov(PROD ~ GEN + REP, data=exp3)
shapiro.test(aov3$residuals)
bartlett.test(aov3$residuals ~ GEN, data=exp3) 
summary(aov3)

#Amb 4
exp4<-subset(exp,AMB==4)
str(exp4)
aov4<-aov(PROD ~ GEN + REP, data=exp4)
shapiro.test(aov4$residuals)
bartlett.test(aov4$residuals ~ GEN, data=exp4) 
summary(aov4)

#Amb 5
exp5<-subset(exp,AMB==5)
str(exp5)
aov5<-aov(PROD ~ GEN + REP, data=exp5)
shapiro.test(aov5$residuals)
bartlett.test(aov5$residuals ~ GEN, data=exp5) 
summary(aov5)

#Amb 6
exp6<-subset(exp,AMB==6)
str(exp6)
aov6<-aov(PROD ~ GEN + REP, data=exp6)
shapiro.test(aov6$residuals)
bartlett.test(aov6$residuals ~ GEN, data=exp6) 
summary(aov6)

#Ou automaticamente
aovgeral <- lapply(split(exp, f=exp$AMB), aov, formula=PROD~GEN+REP)
lapply(aovgeral, summary)
(glrs <- sapply(aovgeral, df.residual)) # graus de liberdade
(qmrs <- sapply(aovgeral, deviance)/glrs) # quadrados médios

(H <- max(qmrs)/min(qmrs))
#Se H < 7 ok!
pf(H,35,35)

#Análise conjunta
conjunta<-aov(PROD ~ REP%in%AMB + AMB + GEN + GEN:AMB, data=exp)#Se tiver NA usa função lm
#Conjunta lm conjunta<-lm(terms(resp~amb+amb:bloco+trat+amb:trat, keep.order=TRUE), data=exp)
anova(conjunta)
#ou conjunta1<-aov(PROD~ AMB/REP+GEN+AMB:GEN, data=exp)
summary(conjunta)
cv.model(conjunta)

#cASO SE DESEJE TESTAR OS TRATs e os AMBs COM A INTERAÇÃO, BASTA ADCIONAR Error(exp:trat)
conjunta2<-aov(PROD ~ AMB/REP+GEN+Error(GEN:AMB), data=exp)
summary(conjunta2)        

library("lsmeans")
lsmeans(conjunta)
(majus<-lsmeans(conjunta, "GEN")) #Caso a interação não fosse significativa


# Se a interação for significativa, deve-se realizar o teste de média dentro de cada ambiente
# para isso, utiliza-se os AMBs separados

library("ExpDes.pt")
amb1<-scottknott((exp1$PROD), exp1$GEN, df.residual(aov1),deviance(aov1), alpha = 0.05, group = TRUE, main = NULL)
amb2<-scottknott((exp2$PROD), exp2$GEN, df.residual(aov2),deviance(aov2), alpha = 0.05, group = TRUE, main = NULL)
amb3<-scottknott((exp3$PROD), exp3$GEN, df.residual(aov3),deviance(aov3), alpha = 0.05, group = TRUE, main = NULL)
amb4<-scottknott((exp4$PROD), exp4$GEN, df.residual(aov4),deviance(aov4), alpha = 0.05, group = TRUE, main = NULL)
amb5<-scottknott((exp5$PROD), exp4$GEN, df.residual(aov5),deviance(aov5), alpha = 0.05, group = TRUE, main = NULL)
amb6<-scottknott((exp6$PROD), exp4$GEN, df.residual(aov6),deviance(aov6), alpha = 0.05, group = TRUE, main = NULL)
Conj<-scottknott((exp$PROD), exp$GEN, df.residual(conjunta),deviance(conjunta), alpha = 0.05, group = TRUE, main = NULL)

##--------------------------------- FIM ----------------------------------##