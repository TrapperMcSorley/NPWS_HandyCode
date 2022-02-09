####calculate the number of days since a detection####


TimeToDetection <- function(MyMatrix){
  
  results.matrix <- matrix(NA,nrow(MyMatrix),ncol(MyMatrix) , dimnames = dimnames(MyMatrix))
  matrix.rev <- MyMatrix

for(i in 1:length(MyMatrix[,1])) matrix.rev[i,] <- rev(MyMatrix[i,])

for(j in 1:length(matrix.rev[1,])){
  for (i in 1:length(matrix.rev[,1])){
    if(is.na(matrix.rev[i,j])) results.matrix[i,j] <- NA
    else{
      if(rle(matrix.rev[i,j :length(matrix.rev[i,])])$values[1] == 0) {
        results.matrix[i,j] <- rle(matrix.rev[i,j :length(matrix.rev[i,])])$length[1]}
      else{results.matrix[i,j] <-  0}
    }}}

for(i in 1:length(results.matrix[,1])) results.matrix[i,] <- rev(results.matrix[i,])

return(results.matrix)
}

testmatrix <- matrix(data = NA ,ncol = 100 , nrow = 100)

for (i in 1:100){
  testmatrix[i,]  <-  rbinom(100 ,1, .1)
}


testTTD <- TimeToDetection(MyMatrix = testmatrix)
