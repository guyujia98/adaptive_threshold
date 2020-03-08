ada_threshold3 <- function(in_img3, intImg3, s, t){
  h <- length(in_img3[1,1,])
  l <- length(in_img3[,1,1])
  w <- length(in_img3[1,,1])
  
  out_img3 <- array(dim = c(l, w, h))
  for(i in 1:l){
    for(j in 1:w){
      for(k in 1:h){
        x_1 <- floor(i - s/2)
        x_2 <- floor(i + s/2)
        y_1 <- floor(j - s/2)
        y_2 <- floor(j + s/2)
        z_1 <- floor(k - s/2)
        z_2 <- floor(k + s/2)
        # border condition
        if (x_1 < 1){
          x_1 <- 1
        } 
        if(x_2 > l){
          x_2 <- l
        } 
        if(y_1 < 1){
          y_1 <- 1
        } 
        if(y_2 > w){
          y_2 <- w
        }
        if(z_1 < 1){
          z_1 <- 1
        }
        if(z_2 > h){
          z_2 <- h
        }
        
        count <- (x_2 - x_1) * (y_2 -y_1) * (z_2 - z_1)
        # calculate the volume of the cube
        
        v_1 <- intImg3[x_2, y_2, z_2]
        if(x_1 > 1){
          v_2 <- intImg3[x_1 -1, y_2, z_2]
          if(y_1 > 1){
            v_5 <- intImg3[x_1 -1, y_1 -1, z_2]
            v_3 <- intImg3[x_2, y_1 -1, z_2]
            if(z_1 > 1){
              v_4 <- intImg3[x_2, y_2, z_1-1]
              v_6 <- intImg3[x_1 -1, y_2, z_1 -1]
              v_7 <- intImg3[x_2, y_1 -1, z_1 -1]
              v_8 <- intImg3[x_1 -1, y_1 -1, z_1 -1]
            }else{
              v_4 <- 0
              v_6 <- 0
              v_7 <- 0
              v_8 <- 0
            }
          }else{
            v_3 <- 0
            v_5 <- 0
            v_7 <- 0
            v_8 <- 0
            if(z_1 > 1){
              v_4 <- intImg3[x_2, y_2, z_1 -1]
              v_6 <- intImg3[x_1 -1, y_2, z_1 -1]
            }else{
              v_4 <- 0
              v_6 <- 0
            }
          }
        } else {
          v_2 <- 0
          v_5 <- 0
          v_6 <- 0
          v_8 <- 0
          if(y_1 > 1){
            v_3 <- intImg3[x_2, y_1-1, z_2]
            if(z_1 > 1){
              v_4 <- intImg3[x_2, y_2, z_1-1]
              v_7 <- intImg3[x_2, y_1-1, z_1-1]
            }else{
              v_4 <- 0
              v_7 <- 0
            }
          }else{
            v_3 <- 0
            v_7 <- 0
            if(z_1 > 1){
              v_4 <- intImg3[x_2, y_2, z_1-1]
            }else{
              v_4 <- 0
            }
          }
        }
        sumvol <- v_1 - v_2 - v_3 - v_4 + v_5 + v_6 + v_7 - v_8
        
        if(in_img3[i, j, k] * count <= sumvol * (100-t)/100){
          out_img3[i, j, k] <- 0
        } else{
          out_img3[i, j, k] <- 1
        }
      }
    }
  }
  return(out_img3)
}