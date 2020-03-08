create_intImg <- function(in_img){
  intImg <- matrix(nrow = nrow(in_img), ncol = ncol(in_img))
  for(i in 1:nrow(in_img)){
    sumImg <- 0
    for (j in 1:ncol(in_img)){
      sumImg <- sumImg + in_img[i, j]
      if (i == 1){
        intImg[i, j] <- sumImg
      } else{
        intImg[i, j] <- sumImg + intImg[i-1, j]
      }
    }
  }
  return(intImg)
}
