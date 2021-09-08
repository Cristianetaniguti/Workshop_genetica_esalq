# Meu primeiro comando
cat ("Hello world")

#Qual diretorio de trabalho
getwd()

setwd("~/cursoR/")

#Operacoes basicas
1+1.3
2*3
4/2
2**3 #ou
2^3

sqrt(4)
log(100, base = 10)
log(100)

(13+2+1.5)/3+log(96,base = 4)

?log
  
#vetor
c(1,3,2,5,2)

#sequencia
1:10

seq(from=0, to=100, by=5)
seq(0,100,5)
?seq

#Repetir sequencias de numeros
rep(3:5, 2)

c(1,4,3,2)*2
c(4,2,1,5)+c(5,2,6,1)
c(4,2,1,5)*c(5,2,6,1)

#Criando objetos
x = c(30.1, 30.4, 40, 30.2, 30.6, 40.1)
x <- c(30.1, 30.4, 40, 30.2, 30.6, 40.1)
c(30.1, 30.4, 40, 30.2, 30.6, 40.1) -> x

y = c(0.26, 0.3, 0.36, 0.24, 0.27, 0.35)

#Evitar usar
NA
NaN
Inf
NULL
TRUE
FALSE
sqrt() #nome da funcao

x+2
x+y
x*y
x*c(1,2)
x*2

z <- (x+y)/2
z

#soma
sum(z)

#media
mean(z)

#Variancia
var(z)

#Indexacao
z[3]
z[2:4]
z[c(1,3)]

#Estrutura
str(z)

clone <- c("GRA02", "URO01", "URO03","GRA02" ,"GRA01", "URO01")
clone

str(clone)

#Fatores - níveis
clone_fator <- as.factor(clone)
str(clone_fator)

#comprimento
length(clone_fator)

#vetores lógicos
vetor_logico <- x > 40

which(vetor_logico)
x[which(vetor_logico)]
x[which(x > 40)]

#Mais de um valor de caracter
clone %in% c("URO03", "GRA02")

#Warning 1
a <- 1:10
a

b <- seq(from = 0.1, to = 1, by = 0.1)
b

b <- b*10
b

a==b
a==round(b)

#Warning 2
errado <- c(TRUE, "vish", 1)
errado

#Matrizes
X <- matrix(1:12, nrow = 6, ncol = 2)
X

W <- matrix(c(x,y), nrow = 6, ncol = 2)
W

#Operacoes matriciais
X*2

X*X

#Algebra de matrizes
X%*%t(X)

W[4,2]
W

colnames(W) <- c("altura", "diametro")
colnames(W)
rownames(W) <- clone
W

#Data frame
logico <- x > 40

campo1 <- data.frame("clone" = clone,
                     "altura" = x,
                     "diametro" = y,
                     "idade" = rep(3:5,2),
                     "corte" = vetor_logico)

campo1

campo1[,4]

campo1$idade

volume = 3.14*(campo1$diametro/2)^2*campo1$altura
volume

campo1 <- cbind(campo1,volume)
campo1

str(campo1)

#Remove todas linhas da coluna 3 e todas colunas da linha 2 respectivamente
campo1[,-3]
campo1[-2,]
