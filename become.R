nummat <- function(file, ...){
  if (grepl(".sav$",file)){
    cat ("read SPSS\n")
    require(foreign)
    data <- read.spss(file) -> data
  } else {
    cat ("read CSV\n")
    data <- read.csv(file)
  }
  df.data <- as.data.frame(data)
  num.col <- unlist(lapply(df.data,typeof)) != 'character'
  numdd <- df.data[,as.integer(which(num.col))]
  dims <- dim(numdd)
  nm <-  matrix(as.numeric(as.matrix(numdd)), dims[1],dims[2]) 
  colnames(nm) <- colnames(numdd)
  return(nm)
}


my.cor <- function(x, use="pairwise.complete.obs"){
  cx <- cor(x, use=use)
  diag(cx) <- 0
  return(cx)
}