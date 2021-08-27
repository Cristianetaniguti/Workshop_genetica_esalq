localizacao <- function(latitude, 
                        longitude, 
                        genetica, 
                        estatistica, 
                        genetica_estatistica){
  
  if(any(latitude < -180 | latitude > 180)) stop("Valores de latitude inválidos")
  if(any(longitude < -180 | longitude > 180)) stop("Valores de longitude inválidos")
  
  hemisferio <- vector()
  hemisferio[which(latitude > 0)] <- "Norte"
  hemisferio[which(latitude < 0)] <- "Sul"
  hemisferio[which(latitude == 0)] <- "Equador"
  
  meridiano <- vector()
  meridiano[which(longitude > 0)] <- "Oriente"
  meridiano[which(longitude < 0)] <- "Ocidente"
  meridiano[which(longitude == 0)] <- "Meridiano de Greenwich"
  
  medias <- (as.numeric(genetica) + 
               as.numeric(estatistica) + 
               as.numeric(genetica_estatistica))/3
  
  df <- data.frame(hemisferio, meridiano, round(medias,2))
  return(df)
}
