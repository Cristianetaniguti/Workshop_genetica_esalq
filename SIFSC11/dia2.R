### Dia 2 ###
# Vamos lá? #
#############

### Revisão ###
# Calculadora
1+1
2^2
sqrt(4)
log(16, base = 4)

# Criando um vetor
c(1,3,2,5,2)

# Vetores numéricos
x = c(30.1,30.4,40,30.2,30.6,40.1)

y = c(0.26,0.3,0.36,0.24,0.27,0.35)

# Vetor de caracteres
clone <- c("GRA02", "URO01", "URO03", "GRA02", "GRA01", "URO01")

# Logico
logico = x > 40
which(x > 40)
clone %in% c("URO03", "GRA02")

# Operadores lógicos
4>=3
50<100
4==4
!TRUE

# Matrizes
X <- matrix(1:12, nrow = 6, ncol = 2, byrow = TRUE)
X

# Vetores numéricos
x = c(30.1,30.4,40,30.2,30.6,40.1)

y = c(0.26,0.3,0.36,0.24,0.27,0.35)

# Vetor de caracteres
clone <- c("GRA02", "URO01", "URO03", "GRA02", "GRA01", "URO01")
logico = x > 40

# Data frames
(campo1 = data.frame("clone"=clone,
                    "altura"=x,
                    "diametro" = y,
                    "idade"= rep(3:5,2),
                    "corte"=logico))


campo1$altura + campo1$diametro
campo1$diametro

# Operacoes
volume = 3.14*((campo1$diametro/2)^2)*campo1$altura

# Ligar colunas
campo1 = cbind(campo1, volume)
campo1

str(campo1)

W <- matrix(c(x,y), nrow = 6, ncol =2)
# Lista
minha_lista = list(campo1 = campo1,
                   media_alt = tapply(campo1$altura, campo1$idade, mean),
                   matrix_ex = W)

str(minha_lista)

minha_lista$campo1$diametro
minha_lista[[1]][[3]]

# Importar e exportar
load("dia1.RData")

save.image(file="dia1.RData")
save(campo1, x, y, file = "campo1.RData")

rm(campo1)
campo1

load("dia1.RData")

# Write 
write.table(campo1, file="campo1.txt", sep = ";", dec=".", row.names = FALSE)
write.csv(campo1, file = "campo1.csv", dec = ",")

# Read
(campo1_txt = read.table(file = "campo1.txt", sep=";", dec=".", header = TRUE))
(campo1_csv = read.csv(file="campo1.csv"))

# Procurar ajuda
?read.table

dados = read.csv(file = "https://cristianetaniguti.github.io/Workshop_genetica_esalq/CursoRBiometria_2021/dados_alunos2021.csv",
                 stringsAsFactors = FALSE, 
                 na.strings="-", 
                 header = T,
                 dec = ",")

str(dados)
dim(dados)
colnames(dados) = c("Ocupacao", "Graduacao", "Conhecimentos_Genetica",
                    "Conhecimentos_Estatistica", "Conhecimento_Gen_Est", 
                    "Latitude", "Longitude")
colnames(dados)
str(dados)

nrow(dados)
ncol(dados)

table(dados$Ocupacao)

### Condicionais
if(2<3){
  cat("dois é menor do que três")
} else {
  cat("dois é maior do que três")
}

if(dados[2,3] == "Básico"){
  cat("Força, Mendel acredita em você!")
} else {
  cat("Mendel agradece a preferência")
}

if(dados[2,1] == "Mestrado"){
  cat("Força, o quinto dia útil esta chegando!")
} else if (dados[2,1] == "Doutorado"){
  cat("Assim como seus amigos do mestrado, acredite até sexta a bolsa cai!")
} else {
  cat("Esse já tem a carteira assinada, que beleza!")
}

(x <- 1:10)
any(x>9)
all(x>9)

### Repetição ###
## for ##
y = 11:20
y
for(i in 1:10){
  print(y[i])
}

(test = vector())
for(i in 1:10){
  test[i] = i+4
}
test 

for(i in 1:nrow(dados)){
  if(dados[i,1] == "Mestrado"){
    print("Força, o quinto dia útil esta chegando!")
  } else if (dados[i,1] == "Doutorado"){
    print("Assim como seus amigos do mestrado, acredite até sexta a bolsa cai!")
  } else {
    print("Esse já tem a carteira assinada, que beleza!")
  }
}

# Consertar o curso Agronomia
dados[,2]
grepl("Agro", dados[,2])
dados[grepl("Agro", dados[,2]),2]

for(i in 1:nrow(dados)){
  if(grepl("Agro", dados[i,2])){
    if(dados[i,2]!="Agronomia"){
      print("Por favor, substitua sua resposta por Agronomia.")
    }
  }
}

for(i in 1:nrow(dados)){
  if(grepl("Agro", dados[i,2]) & dados[i,2]!="Agronomia"){
      print("Por favor, substitua sua resposta por Agronomia.")
  }
}

# Substituição
dados_copia = dados
for(i in 1:nrow(dados_copia)){
  if(grepl("Agro", dados_copia[i,2], ignore.case = TRUE) & dados_copia[i,2]!="Agronomia"){
    dados_copia[i,2] = "Agronomia"
  }
}

head(table(dados_copia$Graduacao))
head(table(dados$Graduacao))

# Jeito mais simples
dados_copia = dados
gsub(pattern = ".*Agro.*", replacement = "Agronomia", ignore.case = TRUE, x = dados$Graduacao)


## While ##
x = 1
while(x<5){
  print(x)
  x = x+1
}

x = 1
while(x<5){
  x = x+1
  if(x==4){
    break
  }
  print(x)
}

x = 1
while(x<5){
  x = x+1
  if(x==4){
    next
  }
  print(x)
}

### Graficos ###
## Histogramas ##
#  Extraindo informações para novos objetos
latitude = dados$Latitude
latitude
hist(latitude)
# Ou usando o nome da coluna
hist(dados$Latitude)

# Ajuda da função
?hist

# Parâmetros
hist(dados$Latitude, breaks = 30, col = "red",
     border = "yellow")

# Cores no R
colors()
colours()

# Cor a partir de código hexadecimal
hist(dados$Latitude, breaks = 30, col = "#EE964B",
     border = "yellow")

barplot(dados$Latitude~dados$Ocupacao)

# Salvar
png(filename = "hist_rbase.png")
hist(dados$Latitude)
dev.off()

# Exemplo de gráfico com cores para variável categórica
# Dados e processamento dos dados
load("clima_lond.RData")
clima_lond
clima_lond$dia <- as.factor(clima_lond$dia)
clima_lond$prec.mm <- as.numeric(as.character(clima_lond$prec.mm))

# Barplot
barplot(tapply(clima_lond$prec.mm, clima_lond$Mes, sum), 
        main="Total Mensal",
        xlab = "Meses do ano de 2017",
        ylab = "Precipitação em mm", col=c("red", "blue",
                                           "green", "yellow",
                                           "gray", "white"))







