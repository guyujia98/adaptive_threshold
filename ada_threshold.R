#' This function is used to creating 2d binary image
#'
#' @param in_img the input grey image
#' @param Intimg the integral image created by \code{create_intImg}
#' @param s the expected length of the moving square
#' @param t the threshold, should be in a range of 0 to 100
#'
#' @seealso \code{create_intImg}, \code{ada_threshold3}


ada_threshold <- function(in_img, Intimg, s, t = 15){
  w <- ncol(Intimg)
  h <- nrow(Intimg)
  out_img <- matrix(nrow = h, ncol = w)
  for(i in 1:w){
    for(j in 1:h){
      x_1 <- floor(i - s/2)
      x_2 <- floor(i + s/2)
      y_1 <- floor(j - s/2)
      y_2 <- floor(j + s/2)
      if (x_1 < 1){
        x_1 <- 1
      }
      if(x_2 > w){
        x_2 <- w
      }
      if(y_1 < 1){
        y_1 <- 1
      }
      if(y_2 > h){
        y_2 <- h
      }
      count <- (x_2 - x_1) * (y_2 - y_1)
      if(x_1 != 1 && y_1 != 1){
        sum <- Intimg[y_2, x_2] - Intimg[y_2, x_1-1] - Intimg[y_1-1, x_2] + Intimg[y_1-1, x_1-1]
      } else{
        if(x_1 == 1 && y_1 == 1){
          sum <- Intimg[y_2, x_2]
        }
        if(x_1 == 1 && y_1 > 1){
          sum <- Intimg[y_2, x_2] - Intimg[y_1-1, x_2]
        }
        if(x_1 > 1 && y_1 == 1){
          sum <- Intimg[y_2, x_2] - Intimg[y_2, x_1-1]
        }
      }
      if(in_img[j, i] * count <= sum * (100-t)/100){
        out_img[j, i] <- 0
      } else{
        out_img[j, i] <- 1
      }
    }
  }
  return(out_img)
}
