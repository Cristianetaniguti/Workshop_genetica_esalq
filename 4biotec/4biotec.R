# Gerando o arquivo de genótipo

fen <- read.csv("http://cristianetaniguti.github.io/Workshop_genetica_esalq//praticas_2017/Feijao_Lavras_para_Genotipos.csv")
str(fen)

media_cultivar <- tapply(fen$Produtividade, fen$Cultivar, mean)
media_cultivar

media_cultivar_ordenada <- sort(media_cultivar)

desordem <- match(media_cultivar, media_cultivar_ordenada)

ligados <- matrix(c(sample(c(rep(0,17), rep(1,3))),
                    sample(c(rep(0,15), rep(1,5))),
                    sample(c(rep(0,12), rep(1,8))),
                    sample(c(rep(0,13), rep(1,7))),
                    sample(c(rep(0,10), rep(1,10))),
                    sample(c(rep(0,10), rep(1,7), rep(2,3))),
                    sample(c(rep(0,8), rep(1,7), rep(2,5))),
                    sample(c(rep(0,6), rep(1,4), rep(2,10))),
                    sample(c(rep(1,9), rep(2,11))),
                    sample(c(rep(1,5), rep(2,15)))),
                  nrow = 20, byrow = T)

nao_ligados <- matrix(sample(c(rep(2,97), rep(1,100), rep(0,103))), nrow = 20)


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

geno.cores <- heat.colors(3)

heatmap(genotipos, Colv = NA, Rowv = NA,  col=geno.cores)
head(genotipos)

geno.cores <- rev(geno.cores)

heatmap(genotipos, Colv = NA, Rowv = NA,  col=geno.cores)

fen_ordenado <- sort(fen)

desordem <- match(fen, fen_ordenado)

colfunc <- colorRampPalette(c("royalblue", "red"))
cores <- colfunc(20)[desordem]

heatmap(genotipos, Colv = NA, Rowv = NA, col=rev(heat.colors(3)), RowSideColors = cores, scale = "column")

ordem <- match(fen_ordenado, fen)

heatmap(genotipos[ordem,], Rowv = NA, Colv = NA, col=geno.cores, RowSideColors = cores[ordem], scale = "column")

heatmap(genotipos[ordem,c(6:10,17:20)], Rowv = NA, Colv = NA, col=geno.cores, RowSideColors = cores[ordem], scale = "column")
heatmap(genotipos[ordem,-c(6:10,17:20)], Rowv = NA, Colv = NA, col=geno.cores, RowSideColors = cores[ordem], scale = "column")

# não ligados
par(mfrow=c(2,2))
boxplot(fen ~ genotipos[,11])
boxplot(fen ~ genotipos[,12])
boxplot(fen ~ genotipos[,13])
boxplot(fen ~ genotipos[,14])


# ligados
par(mfrow=c(2,2))
boxplot(media_cultivar ~ genotipos[,7])
boxplot(media_cultivar ~ genotipos[,8])
boxplot(media_cultivar ~ genotipos[,9])
boxplot(media_cultivar ~ genotipos[,10])


mk <- genotipos[,11]
mk <- gsub(pattern = 0, replacement = "aa", x = mk)
mk <- gsub(pattern = 1, replacement = "Aa", x = mk)
mk <- gsub(pattern = 2, replacement = "AA", x = mk)

mk <- as.factor(mk)

modelo <- lm(media_cultivar ~ mk)
summary(modelo)


mk <- genotipos[,10]
mk <- gsub(pattern = 0, replacement = "aa", x = mk)
mk <- gsub(pattern = 1, replacement = "Aa", x = mk)
mk <- gsub(pattern = 2, replacement = "AA", x = mk)

mk <- as.factor(mk)

modelo <- lm(media_cultivar ~ mk)
summary(modelo)

# Loop para identificar p-valor de todos os marcadores

p <- vector()
for(i in 1:dim(genotipos)[2]){
  mk <- genotipos[,i]
  mk <- gsub(pattern = 0, replacement = "aa", x = mk)
  mk <- gsub(pattern = 1, replacement = "Aa", x = mk)
  mk <- gsub(pattern = 2, replacement = "AA", x = mk)
  
  mk <- as.factor(mk)
  
  modelo <- lm(fen ~ mk)
  f <- summary(modelo)$fstatistic
  p[i] <- pf(f[1],f[2],f[3],lower.tail=F)
}

which(p < 0.05)



