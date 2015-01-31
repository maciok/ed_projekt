# Plik: skrypt.R
#
# Autorzy: Mateusz Supronowicz, 252566
#		   Maciej Pietrzak, 252552
#
# Opis: Skrypt opisujacy tok postepowan przy pracy nad projektem z zajec Eksploracja Danych.
#       Praca z danymi "szklo_B.mat".
source("eksploracja.r")
# Biblioteki
#library(R.matlab)
#library(psych)
#library(stats)
#library(matrixStats)

# Wczytanie danych z pliku
file <- readMat("szklo_B.mat")

# Wczytanie macierzy
data.mx <- file$szklo.B 

# Liczba kolumn i wierszy
data.row <- nrow(data.mx)
data.col <- ncol(data.mx)

data.mx[,1]
data.mx[,2]
data.mx[,3]
data.mx[,4]
data.mx[,5]
data.mx[,6]
data.mx[,7]
data.mx[,8]
data.mx[,9]
#############
# Zadanie 5 # 
#############
#dev.new()
#plot(data.mx[,1],data.mx[,2],col=data$c,pch=16)
# dane oryginalne (kolor - wartosc atrybutu decyzyjnego)
#dev.new()
#plot(data.mx[,1],data.mx[,2]) # zapominamy o atrybucie decyzyjnym
dev.new()
g <- kmeans(data.mx,9) # grupowanie (9 grup)

  plot(data.mx[ ,1],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,2],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,3],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,4],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,5],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,6],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,7],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")

  plot(data.mx[ ,8],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Tmp")
plot(data.mx[,1],data.mx[,2],pch=16,col=g$cluster)
# wynik grupowania (kolor - wyn.grupowania)
points(g$centers) # srodki skupien
table(data$c,g$cluster) # macierz kontyngencji

#############
# Zadanie 1 # 
#############

summary(data.mx)

# Obliczanie srednich
mean.mx <- matrix( , nrow = data.col, ncol = 3)
for(i in 1 : data.col) {
  mean.mx[i, ] <- c(mean(data.mx[ ,i]), harmonic.mean(data.mx[ ,i]), geometric.mean(data.mx[ ,i]))
}

# Odchylenie standardowe atrybutow
stddev.mx <- matrix( ,nrow = data.col, ncol = 1)
for(i in 1 : data.col) {
  stddev.mx[i, ] = sd(data.mx[ ,i])
}

# Miara rozstepu atrybutow
range.mx <- matrix( , nrow = data.col, ncol = 2) # Wartosci min, max
diff.mx <- matrix( , nrow = data.col, ncol = 1) # Rozstep

for(i in 1 : data.col) {
  range.mx[i, ] <- range(data.mx[ ,i])
  diff.mx[i, ] <- diff(range.mx[i, ])
}

# Miara zmiennosci poszczegolnych atrybutow
cov.mx <- matrix( , nrow = data.col, ncol = 1) 
for(i in 1 : data.col) {
  cov.mx[i, ] <- stddev.mx[i, ] / mean.mx[i,1]
}

#############
# Zadanie 2 # 
#############

# Macierz klasyfikujaca obiekty oddalone
pts.odd.mx <- matrix( , nrow = data.row, ncol = data.col) # Klasyfikikacja ob. oddalonych
for(i in 1 : data.col) {
  pts.odd.mx[ ,i] <- abs(data.mx[ ,i] - mean.mx[i,1]) < (2 * stddev.mx[i,1])
  dev.new()
  plot(seq(1, data.row, by = 1),
       data.mx[ ,i],
       pch = 16,
       xlab = "Probki",
       ylab = "Wartosci",
       col = pts.odd.mx[ ,i] + 1,
       main = paste("Atrybut Nr.", toString(i)))
}

#############
# Zadanie 3 # 
#############

# Pokazanie wszystkich danych
dev.new()
plot(data.mx[ ,1],
     pch = 16,
     col = 1,
     xlim = c(0, data.row), # Liczba probek
     ylim = c(min(data.mx), max(data.mx)),  # [min,max]
     xlab = "Indeks",
     ylab = "Wartosc",
     main = "Wartosci wszystkich atrybutow")

for(i in 2 : data.col) {
  points(data.mx[ ,i], pch = 16, col = i)
}

# Rozpoznanie przedzialow w ktorych wystepuja punkty oddalone
# w atrybutach nr. 1,4,6,8,9
dev.new()
plot(data.mx[ ,1], 
     pch = 16, 
	 col = pts.odd.mx[ ,1] + 1, 
	 xlim = c(0, data.row), 
	 ylim = c(0, 2),
	 xlab = "Indeks", 
	 ylab = "Wartosci", 
	 main = "Wartosci punktow oddalonych przyjmowane w Atrybutach Nr. 1,4,6,8,9")
points(data.mx[ ,9], pch = 16, col = pts.odd.mx[ ,9] + 1, xlim = c(0, data.row), ylim = c(0, 2))
points(data.mx[ ,8], pch = 16, col = pts.odd.mx[ ,8] + 1, xlim = c(0, data.row), ylim = c(0, 2))
points(data.mx[ ,6], pch = 16, col = pts.odd.mx[ ,6] + 1, xlim = c(0, data.row), ylim = c(0, 2))
points(data.mx[ ,4], pch = 16, col = pts.odd.mx[ ,4] + 1, xlim = c(0, data.row), ylim = c(0, 2))
axis(2, at = c(0.0, 0.2, 0.3, 0.4, 0.5, 1.0, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 2.0))

# Macierz wykresow punktowych
dev.new()
pairs(~mean.mx[ ,1] + stddev.mx + diff.mx + cov.mx, 
      pch = 22,
      bg = rainbow(9), 
	  oma = c(4,4,6,12), 
	  main = "Macierz wykresow punktowych")

par(xpd = TRUE)
# Numery atrybutow i odpowiadajace im kolory
legend(0.9, 0.7, c("1","2","3","4","5","6","7","8","9"), fill = rainbow(9))

#############
# Zadanie 4 # 
#############

# Standaryzacja
data.std.mx <- matrix( , nrow = data.row, ncol = data.col)
for(i in 1 : data.row) {
  for(j in 1 : data.col) {
    data.std.mx[i,j] <- (data.mx[i,j] - mean.mx[j,1]) / stddev.mx[j,1] 
  }
}

# Graficzne przedstawienie danych std. za pomoca wykresu pudelkowego
dev.new()
boxplot(data.std.mx,
        xlab = "Atrybut",
        ylab = "Std. wartosc",
        main = "Graficzne przedstawienie wartosci po standaryzacji")

#############
# Zadanie 5 # 
#############

# Grupowanie - 5 grup po wybranych atrybutach: 2,3,4,5,6,7
g <- kmeans(data.std.mx[,c(2,3,4,5,6,7)],5 ) 
g

dev.new()
  plot(data.mx[ ,1],
       pch = 16,
       xlab = "Probki",
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,2],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ sodu",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,3],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ magnezu",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,4],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ aluminium",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,5],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ krzemu",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,6],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ potasu",
       col = g$cluster,
       main = "Tmp")

dev.new()
  plot(data.mx[ ,7],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ wapnia",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,8],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ baru",
       col = g$cluster,
       main = "Grupowanie")

dev.new()
  plot(data.mx[ ,9],
	 data.mx[ ,1],
       pch = 16,
       ylab = "Wspó³czynnik za³amiania œwiat³a",
       xlab = "Zawartoœæ ¿elaza",
       col = g$cluster,
       main = "Grupowanie")

#############
# Zadanie 6 # 
#############

wektor <- list()
wektor$m <- cbind(data.mx[ ,2], data.mx[ ,3], data.mx[ ,4], data.mx[ ,5], data.mx[ ,6], data.mx[ ,7])
# Wektor atrybutow decyzyjnych
wektor$c <- g$cluster

#############
# Zadanie 7 # 
#############

md1 = podziel_md(wektor,0.7,1)
pokaz_md(md1)



