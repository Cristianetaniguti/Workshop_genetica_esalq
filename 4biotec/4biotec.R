# Gerando o arquivo de genótipo

fen <- read.csv("http://cristianetaniguti.github.io/Workshop_genetica_esalq//praticas_2017/Feijao_Lavras_para_Genotipos.csv")
str(fen)

media_cultivar <- tapply(fen$Produtividade, fen$Cultivar, mean)
media_cultivar

media_cultivar_ordenada <- sort(media_cultivar)

desordem <- match(media_cultivar, media_cultivar_ordenada)

ligados <- matrix(c(sample(c(rep(0,19), rep(1,1))),
                    sample(c(rep(0,17), rep(1,3))),
                    sample(c(rep(0,15), rep(1,5))),
                    sample(c(rep(0,13), rep(1,7))),
                    sample(c(rep(0,10), rep(1,10))),
                    sample(c(rep(0,10), rep(1,10))),
                    sample(c(rep(0,6), rep(1,14))),
                    sample(c(rep(0,4), rep(1,16))),
                    sample(c(rep(0,3), rep(1,17))),
                    sample(c(rep(0,1), rep(1,19)))),
                  nrow = 20, byrow = T)

nao_ligados <- matrix(sample(c(rep(1,147), rep(0,153))), nrow = 20)


genotipos <- matrix(c(nao_ligados[,1:5], ligados[,1:5], nao_ligados[,6:12], ligados[,6:10],nao_ligados[,13:15]), nrow = 20)

desordem <- sample(1:20)

genotipos <- genotipos[desordem,]

rownames(genotipos) <-  paste0("Cult",1:20)
colnames(genotipos) <-  paste0("MK", 1:25)

write.csv(genotipos, file = "genotipos.csv")

fen <- rnorm(20, mean = 4, sd = 2)
fen <- sort(fen)
fen <- fen[desordem]

fenotipos <- data.frame("cultivar" = paste0("Cult", 1:20), "tamanho da lesão" = fen)

write.csv(fenotipos, file = "fenotipos.csv")

####

fenotipos <- read.csv("https://raw.githubusercontent.com/Cristianetaniguti/Workshop_genetica_esalq/gh-pages/4biotec/fenotipos.csv", 
                      stringsAsFactors = F)

str(fenotipos)
head(fenotipos)

#rownames(fenotipos) <- fenotipos$cultivar
#fenotipos <- fenotipos[,-c(1,2)]

genotipos <- read.csv("https://raw.githubusercontent.com/Cristianetaniguti/Workshop_genetica_esalq/gh-pages/4biotec/genotipos.csv", 
                      stringsAsFactors = F)

str(genotipos)
head(genotipos)

rownames(genotipos) <- genotipos[,1]
head(genotipos)
genotipos <- genotipos[,-1]
head(genotipos)

heatmap(genotipos, Colv = NA, Rowv = NA)

genotipos <- as.matrix(genotipos)

heatmap(genotipos, Colv = NA, Rowv = NA)

geno.cores <- heat.colors(2)

heatmap(genotipos, Colv = NA, Rowv = NA,  col=geno.cores)
head(genotipos)

fenotipos_ordenado <- sort(fenotipos)

desordem <- match(fenotipos, fenotipos_ordenado)

colfunc <- colorRampPalette(c("royalblue", "red"))
cores <- colfunc(20)[desordem]

heatmap(genotipos, Colv = NA, Rowv = NA, col=heat.colors(2), RowSideColors = cores)
head(genotipos)

ordem <- match(fenotipos_ordenado, fenotipos)

heatmap(genotipos[ordem,], Rowv = NA, Colv = NA, col=geno.cores, RowSideColors = cores[ordem], scale = "column")

heatmap(genotipos[ordem,c(6:10,17:20)], Rowv = NA, Colv = NA, col=geno.cores, RowSideColors = cores[ordem], scale = "column")
heatmap(genotipos[ordem,-c(6:10,17:20)], Rowv = NA, Colv = NA, col=geno.cores, RowSideColors = cores[ordem], scale = "column")

# não ligados
par(mfrow=c(2,2))
boxplot(fenotipos ~ genotipos[,11])
boxplot(fenotipos ~ genotipos[,12])
boxplot(fenotipos ~ genotipos[,13])
boxplot(fenotipos ~ genotipos[,14])


# ligados
par(mfrow=c(2,2))
boxplot(fenotipos ~ genotipos[,7])
boxplot(fenotipos ~ genotipos[,8])
boxplot(fenotipos ~ genotipos[,9])
boxplot(fenotipos ~ genotipos[,10])


mk <- genotipos[,11]
mk <- gsub(pattern = 0, replacement = "aa", x = mk)
mk <- gsub(pattern = 1, replacement = "Aa", x = mk)
mk <- gsub(pattern = 2, replacement = "AA", x = mk)

mk <- as.factor(mk)

modelo <- lm(fenotipos ~ mk)
summary(modelo)


mk <- genotipos[,10]
mk <- gsub(pattern = 0, replacement = "aa", x = mk)
mk <- gsub(pattern = 1, replacement = "Aa", x = mk)
mk <- gsub(pattern = 2, replacement = "AA", x = mk)

mk <- as.factor(mk)

modelo <- lm(fenotipos ~ mk)
summary(modelo)

# Loop para identificar p-valor de todos os marcadores

p <- vector()
for(i in 1:dim(genotipos)[2]){
  mk <- genotipos[,i]
  mk <- gsub(pattern = 0, replacement = "aa", x = mk)
  mk <- gsub(pattern = 1, replacement = "Aa", x = mk)
  mk <- gsub(pattern = 2, replacement = "AA", x = mk)
  
  mk <- as.factor(mk)
  
  modelo <- lm(fenotipos ~ mk)
  f <- summary(modelo)$fstatistic
  p[i] <- pf(f[1],f[2],f[3],lower.tail=F)
}

which(p < 0.05)



