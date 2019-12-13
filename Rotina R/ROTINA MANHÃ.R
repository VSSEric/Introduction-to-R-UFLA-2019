########################################################################
###########           CURSO:INTRODUÇÃO AO SOFTWARE R         ###########
###########             Eric Vinicius Vieira Silva           ###########
###########              ericvinicius.vs@gmail.com           ###########
###########                     11/12/2019                   ###########
########################################################################


##------------------------ PRIMEIROS PASSOS ---------------------------##

#1) COMANDOS IMPORTANTES


?TukeyHSD #Buscando o termo -> ou help("TukeyHSD")
??TukeyHSD #Buscando o termo dentro de pacotes -> help.search("TukeyHSD")

citation() #Como citar o software R

#No R, decimal é representado por ponto " . " Se desejamos escrever
#quatro virgula cinco, a representação correta é 4.5
#virgula será utilizada para separar valores, linhas, colunas, e funções


#2) OPERAÇÕES MATEMATICAS

2+2
3-1
2*3 
4/2
2^2
sqrt(100) #Raiz quadrada
sin(45) #Seno
cos(45) #Coseno
tan(45) #Tangente


4<7	  #x menor que y?
2<=3 	#x menor ou igual a y?
5>2	  #x maior que y?
4>=3	#x maior ou igual a y?
2==3	#x igual a y?
6!=7	#x diferente de y?


##-------------------------- CRIANDO OBJETOS -------------------------##


a<-7  # O comando <- indica que o objeto "a" receberá o valor 7; 
      #Esse objeto fica armazenado no ambiente de trabalho 

a     #Chamando o objeto "a" o R apresentará o valor atribuido ao objeto

b<-5

a+b   #Quando o comando "recebe" (<-) não é utilizado, o mesmo 
      #não será armazenado

c<-a+b  #Caso queira armazenar o resultado, é necessario criar um objeto 

c

#Atenção com sobrepossição. Sempre que um objetivo já existente
#recebe um novo valor, o anterior será substituido

a #O objeto "a" é igual a 7

a<-19 #Caso o mesmo objeto receba um novo valor, o mesmo substituirá o antigo

a

a<-7
a2<-19 #Caso não deseje apagar o objeto anterior. Deve ser criado um novo objeto, no caso "a2"
a
a2



ls() #Objetos armazenados 


rm(a2) #Remoção de objeto. Observe que somente a2 será removido do workspace



rm(list=ls()) #Deleta tudo que está armazenado no workspace



##---------------------- ENTRANDO COM DADOS NO R ----------------------##

#1) Entrando com dados diretamente no R
a<- c(10, 20, 30, 40, 50, 60) #criando Vetor A
a
b <- c(2, 4, 6, 10, 8, 12) #Criando Vetor B
b

a+b
a*b
a/b

sum(a) #Somatório de "a"
sum(a+b)

length(a+b) # número de elementos em a+b
media<-sum(a+b)/length(a+b); 
media 
mean(a+b) 

names(b)<-c("A","B","C","D", "E", "F");b #Nomeando o vetor b
mean(b)

(order(b, na.last = TRUE, decreasing = TRUE)) #Ordenando b

novob<-b[(order(b, na.last = TRUE, decreasing = TRUE))]; novob

b2<-b[1:3];b2  #retirar os números do vetor b nas posições 1, 2 e 3
b3<-b[-5]; b3  #retirar o quinta observação
b[6]<-100; b #Inserir o valor 100 na posição 6

a
names(a)<-c("A","B","C","D", "E", "F");a
mean(a)
a2<-a[a>mean(a)]; a2 #retirar os maiores que a média
mean(a2)

ab<-data.frame(a,b); ab #Criando data.frame composto pelos vetores a e b

names(ab)<-c("Amb1", "Amb2");ab #Renomeando as a e b para ambientes

ab$Amb1 #Observar somente o Ambiente 1

mean(ab$Amb1) #Média do ambiente 1

var(ab$Amb1) #Variância do ambiente 1

length(ab[1,])  #[x,y] Neste caso x representa as linhas, e y as colunas
                #Estamos interessados em verificar quantos valores temos
                #Para a linha 1, considerando todas as colunas.
                #Observa-se que temos 2 valores associados a essa linha

mean(ab$Amb1)


ab$Amb3<- c(30, 60, 90, 100, 120, 760); ab #Adicionando o Ambiente 3
mean(ab$Amb3) #média ambiente 3

#É possivel conectar comandos utilizando &
ab[ab$Amb1 > mean(ab$Amb1)
   & ab$Amb2 > mean(ab$Amb2)
   & ab$Amb3 > mean(ab$Amb3), ] #Genótipos acima da média em todos os Ambs

ab2<-ab[order(ab$Amb2, decreasing = TRUE), ]; ab2 #Ordenando de acordo
                                                  #com o ambiente 2



#2) Importando planilha de dados txt

setwd("C:\\Users\\Usuario\\Desktop\\CURSO R 2019")
#Ctrl+Shift+H -> DEFINIR DIRETORIO #Abrir a pasta diretamente

getwd()
dir()

ex1<-read.table("exemplo1.txt", h=T) #h=T informa que cabeçalho

ex1

head(ex1) #Primeiras linhas do txt
tail(ex1) #Ultimas linhas do txt
str(ex1) #Mostra a estrutura da tabela

rownames(ex1)<-c(LETTERS[1:20])#Nomeando as linhas. 
ex1

ex1$Amb1 #Observar dados somente do ambiente 1

mean(ex1$Amb2) #Média do ambiente 2

var(ex1$Amb3) #Variância do ambiente 3


ex1[ex1$Amb1 > mean(ex1$Amb1)
    & ex1$Amb2 > mean(ex1$Amb2)
    & ex1$Amb3 > mean(ex1$Amb3), ]  #Genótipos acima da média em 
                                    #todos os Ambientes

ex12<-ex1[order(ex1$Amb2, decreasing = TRUE), ]; ex12


##------------------- ESTATÍSTICA DESCRITIVA BÁSICA -------------------##

summary(ab)
var(ab)
apply(ab,1, summary) 
apply(ab,1,sd)
apply(ab,1,var) 



summary(ex1)
var(ex1)
apply(ex1,1, summary) 
apply(ex1,1,sd)
apply(ex1,1,var) 

##--------------------- INSTALAÇÃO DE PACOTES ---------------------##

install.packages("agricolae")
install.packages("ExpDes.pt")
install.packages("contrast")
install.packages("latticeExtra")
install.packages("fBasics")
install.packages("car")
install.packages("asbio")
install.packages("MASS")
install.packages("ScottKnott")
install.packages("lattice")
install.packages("laercio")
install.packages("lme4")


##------------------- PLANEJAMENTO EXPERIMENTAL -------------------##

#1) Repetição
#2) Casualização
#3) Controle local

library("agricolae") #Carregar o pacote

?design.X # Existem diversos delineamentos experimentais que podem/devem 
            # ser utilizados, delineamentos no inglês é conhecido como design
            # X deve ser substituido por:
            # crd para Inteiramente casualizado
            # rcbd para Blocos Completos casualizado
            # ab para Esquema fatorial
            # alpha para Alpha Latice
            # bib para Blocos Incompletos
            # split para Parcela Subdividida
            # dau para Blocos Aumentados
            # lsd para Quadrado Latino
            # lattice para Latice
            # graeco para Quadrado Greco-Latino
            # strip para experimentos em faixas


#DIC, X Tratamentos, X Repetições

trt<-(1:X) #Número de tratamentos
r<-X #Número de repetições

sorteioDIC<-design.crd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)


write.table(sorteioDIC$book, file='SorteioDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Trat"), sep =" ")


#DBCC, X Tratamentos, X Repetições

trt<-(1:X)
r<-X

sorteioDBCC<-design.rcbd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
sorteioDBCC

write.table(sorteioDBCC$book, file='SorteioDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Trat"), sep =" ")



#FATORIAL A por B, 

trt<-c(A,B) #Mais fatores -> separar por virgulas. Basta substituir A e B pelo nivel dos fatores, exemplo 5 x 4.

r<-3        #número de repetições


sorteioFATDIC<-design.ab(trt, r, serie = 3, design=c("crd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
sorteioFATDIC

sorteioFATDBCC<-design.ab(trt, r, serie = 3, design=c("rcbd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
sorteioFATDBCC

write.table(sorteioFATDIC$book, file='SorteioFATDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Fator A", "Fator B"), sep =" ")


write.table(sorteioFATDBCC$book, file='SorteioFATDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Fator A", "Fator B"), sep =" ")


#PARCELA SUBDIVIDIDA->PRIM.FAT. 3TRATS, SEG.FAT. 12TRATS->TOTAL 3x12=48 trats , 3 Repetições

#Semelhante ao fatorial.
trt1<-(1:3)
trt2<-(1:12)
r<-3

sorteioSPLITDIC<-design.split(trt1, trt2,r, design=c("crd"),serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
sorteioSPLITDIC

write.table(sorteioSPLITDIC$book, file='SorteioSPLITDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Subparcela", "Rep", "Fator A", "Fator B"), sep =" ")

sorteioSPLITDBCC<-design.split(trt1, trt2,r, design=c("rcbd"),serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
sorteioSPLITDBCC

write.table(sorteioSPLITDBCC$book, file='SorteioSPLITDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Subparcela", "Bloco", "Fator A", "Fator B"), sep =" ")




##------------------- PRESSUPOSTOS DA ANOVA -------------------##


# 1) Normalidade dos erros (Os erros devem seguir distribuição Normal)
# 2) Homocedasticidade dos erros (Os erros devem ser homogeneos)
# 3) Independência dos erros (Os erros devem ser independentes)
# 4) Aditividade do modelo (O modelo deve conter apenas efeitos aditivos)


# MODELOS

# DIC: 
# DBC: 

# PRESSUPOSTOS DIC
dados<-read.table("exemplo3.txt", h=T)
str(dados)
dados<-transform(dados, Trat=factor(Trat))
str(dados) 
summary(dados)

# Para realizar as análises dos pressupostos é necessário extrair 
# primeiramente os ERROS.

AOVDados<-aov(ABS ~ Trat, data = dados)
AOVDados$residuals #Extraindo os residuos/erros do nosso conjunto de dados


# NORMALIDADE
shapiro.test(AOVDados$residuals)  


install.packages("fBasics")
library(fBasics)

qqnormPlot(AOVDados$residuals) #Plotar o gráfico com os residuos. 

histPlot(x = as.timeSeries(AOVDados$residuals)) #observar a distribuição dos dados


# HOMOCEDASTICIDADE

bartlett.test(AOVDados$residuals~Trat,data=dados) 

# INDEPENDÊNCIA

install.packages("car")    
library("car")

dwt(lm(AOVDados)) # Neste caso será utilizado o teste de Durbin-Watson


# ADITIVIDADE

install.packages("asbio")
library(asbio)

tukey.add.test(dados$ABS, dados$Trat, dados$Rep) 

##------------------- TRANSFORMÇÃO DE DADOS -------------------##

# Caso os pressupostos da ANOVA não sejam atingindos, os dados do experimento
# não podem ser submetidos a Análise de Variância.

# Visando atender aos pressupostos algumas estrategias podem ser utilizadas

# Como a transformação de dados. 

dados<-read.table("exemplo2.txt", h=T)
str(dados) 
AOVDados<-aov(VarResp ~ Trat + Rep, data = dados)
AOVDados$residuals
shapiro.test(AOVDados$residuals) 
bartlett.test(residuals(AOVDados)~dados$Trat)
car::dwt(lm(AOVDados)) 
asbio::tukey.add.test(dados$VarResp,  dados$Rep, dados$Trat)

##------------------- Testando Transformações Comuns -------------------##

# Raiz Quadrada, Raiz Cubica, Log, Potência
dados$RQUAD<-dados$VarResp^(1/2) 
dados$RCUB<-dados$VarResp^(1/3)
dados$LOG<-log(dados$VarResp)
dados$POT2<-dados$VarResp^2  

AOVRQUAD<- aov(RQUAD~Trat+Rep, data=dados) # AOV dos dados transformados para
AOVRCUB<- aov(RCUB~Trat+Rep, data=dados)
AOVLOG<- aov(LOG~Trat+Rep, data=dados)
AOVPOT2<- aov(POT2~Trat+Rep, data=dados)

# VERIFICANDO OS PRESSUPOSTOS

shapiro.test(residuals(AOVRQUAD))
bartlett.test(residuals(AOVRQUAD)~dados$Trat)
car::dwt(lm(AOVRQUAD))
asbio::tukey.add.test(dados$RQUAD,  dados$Rep, dados$Trat)


##------------------------- VERIFICANDO VIA BOXCOX ---------------------##

install.packages("MASS")
library("MASS")

bc<-boxcox(AOVDados)
bc

locator(n=1) #Clicando

lambda <- boxcox(AOVDados)$x[which(boxcox(AOVDados)$y==max(boxcox(AOVDados)$y))]
# ou lambda <- bc$x[which(bc$y==max(bc$y))]
lambda

# usando a tranformação indicada -> 0.1818182 ~ 0.18
dados$BC0.18<-dados$VarResp^0.18

AOVBC0.18<- aov(BC0.18~Trat+Rep, data=dados)

shapiro.test(residuals(AOVBC0.18))
bartlett.test(residuals(AOVBC0.18)~dados$Trat)
car::dwt(lm(AOVBC0.18)) 
asbio::tukey.add.test(dados$BC0.18,  dados$Rep, dados$Trat)

##--------------------------- FIM -------------------------------##

