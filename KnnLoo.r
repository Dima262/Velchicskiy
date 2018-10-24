eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

l <- dim(iris)[1] # 150

knn <- function(iris_new, point, k){
  xl <- iris_new[, 3:5]
  n <- dim(xl)[2] - 1 # 2
  
  distances <- matrix(NA, l, 2)
  for(p in 1:l) {
    distances[p, ] <- c(p, eDist(xl[p, 1:n], point))
  }
  orderedxl <- xl[order(distances[ , 2]), ]
  classes <- orderedxl[1:k, n + 1]  
  counts <- table(classes)
  return(names(which.max(counts)))
}

plot(NULL, NULL, type = "l", xlim = c(0, 150), ylim = c(0, 1), xlab = 'k', ylab = 'LOO')
step <- 5
Ox <- seq(from = 1, to = 150, by = step) # k
Oy <- c() # LOO

LOO_opt <- 1
k_opt <- 1
for(k in Ox) {
  Q <- 0
  for(i in 1:l) {
    iris_new <- iris[-i, ] # iris without point x_i
    point <- iris[i, 3:4]
    if(knn(iris_new, point, k) != iris[i, 5]) {
      Q <- Q + 1
    } 
  }
  LOO <- Q/l
  Oy <- c(Oy, LOO)
  
  if(LOO < LOO_opt) {
    LOO_opt <- LOO
    k_opt <- k
  }
}

print(Ox)
print(Oy)
print(k_opt)

lines(Ox, Oy, pch = 8, bg = "black", col = "blue")
points(k_opt, LOO_opt, pch = 21, bg = "black", col = "black")