# Criando vetores para armazenar todas as linhas em que a Atura de Plantas tiver um valor dentro de um determinado intervalo
pequeno = vector() # Plantas menores que 1.5m
medio = vector() # Plantas maiores que 1.5m e menores que 2.0m
grande = vector() # Plantas maiores que 2.0m

### Fazer o loop percorrer todas as linhas da tabela
for(i in 1:nrow(dados)){
  if(dados[i,5] < 1.5){
    
    pequeno = i #falta indexação []
    
  } else if (dados[i,5] > 1.5 && dados[i,5] < 2.0){
    
    medio = i #falta indexação []
    
  } else {
    
    grande = i #falta indexação []
    
  }
}

pequeno
medio
grande

# Observe que quando eu não coloco indexação, a cada repetição da função, os objetos "pequeno", "médio" e grande registram 
# apenas o último valor de "i" recebido

# Seria o mesmo que fazer isso:

pequeno = 1
pequeno = 2
pequeno = 3

pequeno

# Se eu coloco indexação:
pequeno = vector()

pequeno[1] = 1
pequeno[2] = 2
pequeno[3] = 3

pequeno

### Colocando indexação  
### Fazer o loop percorrer todas as linhas da tabela
pequeno = vector()
medio = vector()
grande = vector()

for(i in 1:nrow(dados)){
  if(dados[i,5] < 1.5){
    
    pequeno[i] = i ## O "i" será inserido em uma posição no vetor igual ao valor de "i"
    
  } else if (dados[i,5] > 1.5 && dados[i,5] < 2.0){
    
    medio[i] = i ## Se o "i" for igual a 16, ele será inserido na posição "16" do vetor
    
  } else {
    
    grande[i] = i  ## Cada linha da tabela vai pertencer a apenas uma das categorias: "pequeno", "médio" ou "grande"
    
  }
}

# Observe como ficou o vetor "pequeno"
pequeno

## O "NA", na linguagem R, corresponde a células vazias no vetor, nenhum valor ficou armazenado nelas
## Então, da forma que a indexação foi feita, na categoria "pequeno" por exemplo: 
## Nas posições do vetor em que o valor na linha correspondente do data frame não se encaixa na categoria "pequeno"
## O vetor não recebe nenhum valor


## Por exemplo, nas linhas 5 e 6 do data frame (ou na quinta e sexta rodada do loop) o que acontece:
pequeno = c(1,2,3,4) # Observamos anteriormente que nas quatro primeiras posições ele recebe os valores 1,2,3 e 4

## Loop na quinta linha:

if(dados[5,5] < 1.5){
  
  pequeno[5] = 5 ## A condição é FALSA e "pequeno" não vai receber nenhum valor (essa linha de código não é considerada) 
  
}

## Loop na sexta linha:

if(dados[6,5] < 1.5){
  
  pequeno[6] = 6 ## O valor será inserido na posição 6, não na posição 5
  
}

pequeno

# A posição 5 ficou com um NA

### Como resolver? 
### Criando variáveis diferentes de "i" para fazer a indexação, "i" vai conter apenas a identificação das linhas do data frame  

## Vamos rodar o loop apenas para as primeiras quatro linhas e observar o que acontece nas seguintes 
pequeno = vector()
medio = vector()
grande = vector()
x = 0 #variavel de indexação para "pequeno"
y = 0 #variável de indexação para "medio"
z = 0 #variável de indexação para "grande"

for(i in 1:4){ ## Apenas as primeiras 4 linhas 
  
  if(dados[i,5] < 1.5){
    
    x = x+1 # A cada vez que a linha "i" satisfizer a condição na categoria "pequeno", "x" aumenta uma unidade
    pequeno[x] = i 
    
  } else if (dados[i,5] > 1.5 && dados[i,5] < 2.0){
    
    y = y+1 # A cada vez que a linha "i" satisfizer a condição na categoria "medio", "y" aumenta uma unidade
    medio[y] = i
    
  } else {
    
    z= z+1 #A cada vez que a linha "i" satisfizer a condição na categoria "grande", "z" aumenta uma unidade
    grande[z] = i  
    
  }
}

pequeno
x
y
z

## Loop na quinta linha:

if(dados[5,5] < 1.5){
  
  x = x+1 ## A condição é FALSA, esse código é desconsiderado e "x" continua valendo quatro 
  pequeno[x] = 5 ##Esse código também é desconsiderado porque a condição é falsa
  
}

x  # "x" ainda é igual  4

## Loop na sexta linha:

if(dados[6,5] < 1.5){
  
  x = x+1 ## A condição é VERDADEIRA, "x" recebe o valor de 5 (4+1) 
  pequeno[x] = 6 ## O valor será inserido na posição 5, não na posição 6
  
}

pequeno

### Agora fazendo o loop percorrer todas linhas

pequeno = vector()
medio = vector()
grande = vector()
x = 0 #variavel de indexação para "pequeno"
y = 0 #variável de indexação para "medio"
z = 0 #variável de indexação para "grande"

for(i in 1:nrow(dados)){
  
  if(dados[i,5] < 1.5){
    
    x = x+1 # A cada vez que a linha "i" satisfizer a condição na categoria "pequeno", "x" aumenta uma unidade
    pequeno[x] = i
    
  } else if (dados[i,5] > 1.5 && dados[i,5] < 2.0){
    
    y = y+1 # A cada vez que a linha "i" satisfizer a condição na categoria "medio", "y" aumenta uma unidade
    medio[y] = i
    
  } else {
    
    z= z+1 #A cada vez que a linha "i" satisfizer a condição na categoria "grande", "z" aumenta uma unidade
    grande[z] = i  
    
  }
}

pequeno

## As variáveis "x", "y" e "z" também são úteis. Elas contêm a contagem do número de elementos em cada categoria: 
x #111 linhas na categoria "pequeno"
y #50 linhas na categoria "médio"
z #39 linhas na categoria "grande"

Hpeq = dados[pequeno,] ## Somente as linhas do data frame com híbridos pequenos
Hmedio = dados[medio,] ## Somente as linhas do data frame com híbridos medios
Hgrande = dados[grande,] ## Somente as linhas do data frame com híbridos grandes
