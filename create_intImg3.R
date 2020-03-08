create_intImg3 <- function(in_img){
  h <- length(in_img[1,1,])
  l <- length(in_img[,1,1])
  w <- length(in_img[1,,1])
  
  intImg3 <- array(dim = c(l, w, h))
  
  for (k in 1:h){
  intImg <- matrix(nrow = l, ncol = w)
  # create the kth plane with same mehtod for 2d algorithm
     for(i in 1:l){
      sumImg <- 0
      for (j in 1:w){
        sumImg <- sumImg + in_img[i, j, k]
        if (i == 1){
          intImg[i, j] <- sumImg 
        } else{
          intImg[i, j] <- sumImg + intImg[i-1, j] 
        }
    # create (i,j,k)
        if (k == 1){
          intImg3[i, j, k] <- intImg[i, j]
        } else {
          intImg3[i, j, k] <- intImg[i, j] + intImg3[i, j, k-1]
        }
      }
    }
  }
  return(intImg3)
}