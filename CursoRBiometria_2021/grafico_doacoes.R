df <- read.csv("Alunos - Página1 (1).csv", na.strings = "-", stringsAsFactors = F)

head(df)
str(df)

library(ggplot2)

df_rm <- df[-which(is.na(df$Instituição.escolhida)),]

gsub(pattern = "Articulação dos Povos Indígenas do Brasil (APIB)" , replacement = "APIB", as.character(df_rm$Instituição.escolhida))

df_rm$Instituição.escolhida[which(df_rm$Instituição.escolhida == "Articulação dos Povos Indígenas do Brasil (APIB)")] <- "APIB"
df_rm$Instituição.escolhida[which(df_rm$Instituição.escolhida == "Associação Terra Indígena Xingu (Atix)")] <- "Atix"
df_rm$Instituição.escolhida[which(df_rm$Instituição.escolhida == "Rio Negro, Nós Cuidamos!" )] <- "FOIRN"
df_rm$Instituição.escolhida[which(df_rm$Instituição.escolhida == "Coordenação das Organizações Indígenas da Amazônia Brasileira" )] <- "COIAB"


ggplot(data=df_rm, aes(x=Instituição.escolhida, y=valor.doado, fill=Categoria)) +
  geom_bar(stat="identity") +
  theme_bw() + theme(axis.text.x = element_text(angle = 15,  hjust=1)) + 
  ylab("Total doado (R$)") + xlab("Instituição") + scale_y_continuous(breaks = seq(0, 1400, by = 100)) +
  scale_fill_viridis_d(begin = 0, end = 0.7) + labs(caption = "Padrinho: Pessoas que doaram valor superior ao valor da  \n inscrição para pagar pela inscrição de estudantes")


tapply(df_rm$valor.doado, df_rm$Instituição.escolhida, sum)

table(df_rm$Instituição.escolhida)
table(df_rm$Categoria, df_rm$Instituição.escolhida)
table(df_rm$valor.doado, df_rm$Instituição.escolhida)
