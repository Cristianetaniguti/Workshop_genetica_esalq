df <- read.csv("Alunos - Página1.csv", na.strings = "-")

head(df)
str(df)

library(ggplot2)

df_rm <- df[-which(is.na(df$Instituição.escolhida)),]

levels(df_rm$Instituição.escolhida)

ggplot(data=df_rm, aes(x=Instituição.escolhida, y=valor.doado, fill=Categoria)) +
  geom_bar(stat="identity") +
  theme_bw() + theme(axis.text.x = element_text(angle = 15,  hjust=1)) + 
  ylab("Total doado (R$)") + xlab("Instituição") + scale_y_continuous(breaks = seq(0, 1400, by = 100)) +
  scale_fill_viridis_d(begin = 0, end = 0.7)
