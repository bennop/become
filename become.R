#' numeric matrix
#' 
#' read SPSS SAV or corresponding CSV file, remove all non-numeric columns and return as matrix.
#' 
#' The file type is determined by the extension.
#'
#' @param file path to input file
#' @param ... extra arguments, not used
#'
#' @returns matrix corresponding to the numeric columns of the input data. 
#'          All attributes other than column names and dimensionare discarded.
#' @export
#'
#' @examples
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
  nm <-  matrix(as.numeric(as.matrix(numdd)), 
                dims[1],dims[2]) 
  colnames(nm) <- colnames(numdd)
  return(nm)
}


#' Adapted correlation
#' 
#' the diagonal of the correlation martix is set to zero.
#'
#' @param x 
#' @param y optional second argument to cor defaults to \var{x}
#' @param use passed on to cor, default: "pairwise.complete.obs"
#'
#' @returns correlation matrix with zeroed diagonal
#' @export
#'
#' @examples
my.cor <- function(x, y=x, use="pairwise.complete.obs"){
  cx <- cor(x, use=use)
  diag(cx) <- 0
  return(cx)
}