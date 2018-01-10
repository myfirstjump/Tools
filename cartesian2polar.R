
### Transform functions
cart2polar <- function(x,y) {
  r <- (x**2+y**2)**0.5
  theta = atan2(y, x)   # same as atan(y/x)
  return(c(r, theta))
}

polar2cart <- function(r, theta) {
  return(c(r*cos(theta), r*sin(theta)))
}

### Example Data
angle_seq <- pi*seq(0.00, 0.5,0.05)
cart_coor <- polar2cart(1, angle_seq)

# target AB data frame
df <- data.frame(
  cart_A = cart_coor[1:(length(cart_coor)/2)],
  cart_B = cart_coor[(length(cart_coor)/2 + 1): length(cart_coor)]
)


### Plotting function
plottt <- function(df, r, mag) {
  x = polar2cart(r,mag*angle_seq)[1:(length(polar2cart(r,mag*angle_seq))/2)]
  y = polar2cart(r,mag*angle_seq)[(length(polar2cart(r,mag*angle_seq))/2 + 1): length(polar2cart(r,mag*angle_seq))]
  plot(x, y, xlim = c(-2,2), ylim = c(-2,2), xlab = "A", ylab = "B", title("ScattePlot")) 
}


plottt(df, 1, 1) 


### 以45度角去擴散
library("spdep")
df  # point 6剛好在45度，不需要移動
"
cart_A    cart_B
1  0.9238795 0.3826834
2  0.8910065 0.4539905
3  0.8526402 0.5224986
4  0.8090170 0.5877853
5  0.7604060 0.6494480
6  0.7071068 0.7071068
7  0.6494480 0.7604060
8  0.5877853 0.8090170
9  0.5224986 0.8526402
10 0.4539905 0.8910065
11 0.3826834 0.9238795
"

transform_by_polar <- function(df, A_name, B_name, mag) {
  df[, c("R1","R2")] <- Rotation(df[,c(A_name, B_name)], -pi/4)
  for (i in 1:(dim(df)[1])) {
    polar_tmp <- cart2polar(df$R1[i], df$R2[i])
    
    cart_tmp <- polar2cart(polar_tmp[1],mag*polar_tmp[2])
    df$tmp_A[i] <- cart_tmp[1]
    df$tmp_B[i] <- cart_tmp[2]
  }
  df[, c("modify_A","modify_B")] <- Rotation(df[,c("tmp_A", "tmp_B")], pi/4)
  return(df)
}

df_m <- transform_by_polar(df, "cart_A", "cart_B", mag = 2)

plot(df_m$modify_A, df_m$modify_B, xlim = c(-2,2), ylim = c(-2,2), xlab = "A", ylab = "B", title("ScattePlot")) 


# Rotation matrix
t <- pi/4
mt <- matrix(c(cos(t), -sin(t), sin(t), cos(t)), nrow = 2, byrow = TRUE)
mt
mt%*%c(2**0.5, 0)
