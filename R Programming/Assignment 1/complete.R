complete <- function(directory, id = 1:332) {
  
    files <- list.files(directory)
    for (i in 1:length(id)){
        if (i == 1){
            table <- data.frame(id=id[i],
                                nobs= sum(complete.cases(read.csv(paste(directory,"/",files[id[i]],sep="")))))
        }else{
            table[i,] <- c(id[i],sum(complete.cases(read.csv(paste(directory,"/",files[id[i]],sep="")))))
        }
    }
    return(table)
}