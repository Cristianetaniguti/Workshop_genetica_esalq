# Curso 05/05
# Introdução ao R

# Vários comandos diferentes para a mesma coisa
# cat("Hello world")
cat("Qualquer outra coisa")
print("Outra forma")
paste("Mais outra")

print("Q")

# Estabelece diretório de trabalho
setwd("~/github/Workshop_genetica_esalq/CursoRBiometria_2021")

# Me perdi, usar:
getwd()

#####################
# Operações básicas
#####################

1 - 1.3  # Repare o que define o decimal "."
2*3
2^3
4/2

sqrt(9) # raíz quadrada
log(100) # base neperiana
log(100, 10) # base 10

# Exercício
(13+2+1.5)/3 + log(96, 4)

# Pedir ajuda
?log

########################
# Operações com vetores
########################

c(1,3,2,5,2)

# Criar sequência
1:10

# ou

seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, by = 2)
seq(from=0, to=100, by=5)

# Crie uma sequencia utilizando a função seq 
# que varie de 4 a 30, com intervalos de 3 em 3.

seq(from=4, by = 3, to = 30)
seq(4, 3, 30)
?seq

# ou ainda
rep(3:5,2)

# Operações com vetores
c(1,4,3,2)^2
c(4,2,1,5)+c(5,2,6,1)
c(4,2,1,5)*c(5,2,6,1)

x = c(30.1, 30.4, 40, 30.2, 30.6, 40.1)
# ou
x <- c(30.1,30.4,40,30.2,30.6,40.1)

# A linguagem é case sensitive
X

y = c(0.26,0.3,0.36,0.24,0.27,0.35)

x + y
x*y
x*2
x*c(1,2)
sqrt(x)

x*c(1,2,3,4)

# Comprimento do vetor
length(x)

z <- (x+y)/2
z

# Soma de todos os valores
x + y
sum((x + y)/2)
sum(z)
mean(z)
var(z)

# Indexação

z[3]

z[2:4]

str(z)

# Vetor caracteres
clone <- c("GRA02", "URO01", 
           "URO03", "GRA02", 
           "GRA01", "URO01")

str(clone)

# Fator
exemplo_fator <- as.factor(clone)
str(exemplo_fator)
length(exemplo_fator)

# Lógico

x >= 40 # numeros maiores ou igual a 40

logico <- x >= 40

# > maior
# >= maior igual
# < menor
# <= menor ou igual
# == igual
# != diferente
# ! negação

which(logico)

x[which(logico)]

!logico

x[!logico]

clone

clone %in% c("GRA02", "URO03", "URO01")
match(clone, c("GRA02", "URO03", "URO01"))

# Pergunta
LETTERS

which(LETTERS == "R")

any(logico)
all(logico)

# Atenção 1
a <- 1:10
a

b <- seq(from = 0.1, to = 1, 0.1)
b

a2 <- b*10
a2
a
a == round(a2,4) # Sempre use round para comparar!!!

# Atenção - não misture!
errado <- c(TRUE, "vish", 1)
errado

################################
# Matrizes
################################

X <- matrix(1:12, nrow = 6, ncol = 2, byrow = TRUE)
X

X <- matrix(1:12, nrow = 6, ncol = 2)
X

W <- matrix(c(x,y), nrow = 6, ncol = 2)
W

# ou
exemplo <- c(x,y)
W <- matrix(exemplo, nrow = 6, ncol = 2)
W

# Meus atalhos
# Shift + setas - seleciona 
# rodar - ctrl + enter

# Multiplicação elemento a elemento
X*X

# Multiplicaçao matricial
X%*%t(X)

x[3]

X[1:3,2]

# Links comentados
# https://rpubs.com/
