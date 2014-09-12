corr <- function(directory,treshold = 0){
  files <- list.files(directory)
  
  table <- complete(directory,1:length(files))
  
  table <- table[table$nobs>treshold,]
  
  if (nrow(table)>0){
  
      for (i in 1:nrow(table)){
          if (i == 1){
              dados <- read.csv(paste(directory,"/",files[table$id[i]],sep=""))
              dados <- na.omit(dados)
              cr <- cor(dados$nitrate,dados$sulfate)
          }else{
              dados <- read.csv(paste(directory,"/",files[table$id[i]],sep=""))
              dados <- na.omit(dados)
              cr <- c(cr,cor(dados$nitrate,dados$sulfate,))
          }
      }
      return(cr)
  }else{
    return(as.numeric(NULL))
  }
}