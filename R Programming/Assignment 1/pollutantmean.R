pollutantmean <- function(directory,pollutant,id=1:332){
 
  files <- list.files(directory)
  sum <- 0
  n <- 0
  for (i in 1:length(id)){
    dados<-read.csv(paste(directory,"/",files[id[i]],sep=""))
    sum <- sum + sum(dados[,pollutant],na.rm=T)
    n <- n + sum(!is.na(dados[,pollutant]))
  }
  return(sum/n)
}
