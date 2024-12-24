N = 10 #Количество узлов в сетке
grid = matrix(nrow = N, ncol = N, 1:(N^2), byrow = TRUE)
finish = c(3, 3) #Конечный узел
prob_matrix = function(n) {
  P = matrix(0, nrow = n^2, ncol = n^2)
  for(i in 1:n) {
    for(j in 1:n) {
      up = i - 1
      right = j + 1
      down = i + 1
      left = j - 1
      directions = c(up, right, down, left)
      count_val = sample(sum(directions > 0), 1)
      rand_val = runif(count_val) # Используем равномерное распределение
      prob = rand_val / sum(rand_val) # Нормализуем
      k = 1
      if(up > 0 && up <= n && k <= count_val) {
        P[grid[i, j], grid[up, j]] = prob[k]
        k = k + 1
      }
      if(right > 0 && right <= n && k <= count_val) {
        P[grid[i, j], grid[i, right]] = prob[k]
        k = k + 1
      }
      if(down > 0 && down <= n && k <= count_val) {
        P[grid[i, j], grid[down, j]] = prob[k]
        k = k + 1
      }
      if(left > 0 && left <= n && k <= count_val) {
        P[grid[i, j], grid[i, left]] = prob[k]
        k = k + 1
      }
    }
  }
  return(P)
}

probability_matrix = prob_matrix(N)

rand_walk = function(start_pos, p_matr, fin_pos) {
  path = matrix(0,nrow = 1, ncol = 2)
  path[1,] = start_pos
  knot = start_pos
  prev_knot = knot
  k = 1
  repeat {
    col_prob_trans = which.max(p_matr[grid[knot[1], knot[2]],])
    next_knot = which(grid == col_prob_trans, arr.ind = TRUE)
    prev_knot = knot
    p_matr[grid[knot[1], knot[2]], col_prob_trans] = p_matr[grid[knot[1], knot[2]], col_prob_trans]/2
    p_matr[col_prob_trans, grid[knot[1], knot[2]]] = p_matr[col_prob_trans, grid[knot[1], knot[2]]]/2
    knot = next_knot
    path = rbind(path, knot)
    k = k + 1
    if(knot[1] == fin_pos[1] && knot[2] == fin_pos[2]) break;
    #if(k == (N^2)) break;
  }
  return(path)
}

# Функция для отображении поля переходов всех овец, которые не находятся на финише, в параметре указывается матрица переходов probability_matrix
all_pole = function(p_matr) {
  for(j in 1:(N^2)) {
    start = which(grid == j, arr.ind = TRUE)
    if(start[1] != finish[1] || start[2] != finish[2]) {
      res = rand_walk(start, p_matr, finish)
      plot(1:N, 1:N, asp = 1, xlim = c(1, N), ylim = c(1, N), type = "n", xlab = "Ось X", ylab = "Ось Y", main = paste("График блужданий", j))
      for(i in 2:length(res[,1])) {
        lines(c(res[i - 1,1], res[i,1]), c(res[i - 1,2], res[i,2]), col = 'red', type = 'l', lwd = 2)
        arrows(res[i - 1,1], res[i - 1,2], res[i,1], res[i,2], length = 0.1, col = "red", lwd = 2) 
      }
      for(i in 2:length(res[,1])) points(res[i,1], res[i,2], col = "blue", pch = 19, cex = 1.5)
      points(start[1], start[2], col = "green", pch = 19, cex = 2)
      points(finish[1], finish[2], col = "magenta", pch = 19, cex = 2)
      axis(1, at = 1:N)
      axis(2, at = 1:N)
      grid(nx = NA, ny = NULL, col = 1, lty = "dotted")
      grid(nx = NULL, ny = NA, col = 1, lty = "dotted")
    }
  }
}

# Функция для отображении поля переходов одной овцы, в параметрах указывается матрица переходов probability_matrix
# И стартовая позиция, например c(1,1) в зависимости от N, указанного в начале кода
pole = function(p_matr, start_pos = c(1,1)) {
  start = start_pos
  if(start[1] != finish[1] || start[2] != finish[2]) {
    res = rand_walk(start, p_matr, finish)
    plot(1:N, 1:N, asp = 1, xlim = c(1, N), ylim = c(1, N), type = "n", xlab = "Ось X", ylab = "Ось Y", main = paste("График блужданий"))
    for(i in 2:length(res[,1])) {
      lines(c(res[i - 1,1], res[i,1]), c(res[i - 1,2], res[i,2]), col = 'red', type = 'l', lwd = 2)
      arrows(res[i - 1,1], res[i - 1,2], res[i,1], res[i,2], length = 0.1, col = "red", lwd = 2) 
    }
    for(i in 2:length(res[,1])) points(res[i,1], res[i,2], col = "blue", pch = 19, cex = 1.5)
    points(start[1], start[2], col = "green", pch = 19, cex = 2)
    points(finish[1], finish[2], col = "magenta", pch = 19, cex = 2)
    grid(col = 1)
  }
  return(res)
}

# Функция для отображении гистограммы, в параметре указывается матрица переходов probability_matrix
histogram_directions = function(p_matr) {
  counts = 0
  k = 1
  for(j in 1:(N^2)) {
    start = which(grid == j, arr.ind = TRUE)
    if(start[1] != finish[1] || start[2] != finish[2]) {
      res = rand_walk(start, p_matr, finish)
      if(finish[1] == res[length(res[,1]), 1] && finish[2] == res[length(res[,2]), 2]) {
        if(counts[1] == 0) {
          counts[1] = length(res[,1]) - 1
          k = k + 1
        } else {
          counts[k] = length(res[,1]) - 1
          k = k + 1
        }
      }
    }
  }
  hist(counts, breaks = N, col = "lightblue", xlab = "Кол-во попыток", ylab = "Частота")
  return(counts)
}

#all_pole(probability_matrix)
histogram_directions(probability_matrix)
#res = pole(probability_matrix, c(1,1))

