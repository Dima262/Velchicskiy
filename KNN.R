colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(NULL, NULL, type = "l", xlim = c(min(iris[, 3]), max(iris[, 3])), ylim = c(min(iris[, 4]), max(iris[, 4])), xlab = 'Petal.Length', ylab = 'Petal.Width')

eDist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

k <- 5 
xl <- iris[, 3:5]
l <- dim(xl)[1]
n <- dim(xl)[2] - 1

col3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.1) 
col4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.1)
for(i in col3) {
  for(j in col4) {
    point <- c(i, j)
    distances <- matrix(NA, l, 2) # расстояния от классифицируемого объекта u до каждого i-го соседа
    for(p in 1:l) {
      distances[p, ] <- c(p, eDist(xl[p, 1:n], point))
    }
    orderedxl <- xl[order(distances[ , 2]), ] # сортировка расстояний
    classes <- orderedxl[1:k, n + 1] # названия первых k классов (k ближайших соседей) в classes 
    counts <- table(classes) # количество каждого класса в классах
    classname <- which.max(counts) #имя класса содержащего max количество
    points(point[1], point[2],  pch = 21, bg = "white", col = colors[classname])
  }
}

for (i in 1:l) {
  points(iris[i, 3], iris[i, 4],  pch = 21, bg = colors[iris$Species[i]], col = colors[iris$Species[i]])
}

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))
