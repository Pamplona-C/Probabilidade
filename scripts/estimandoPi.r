N <- 1000000  # Número de iterações
n <- 0

# Gerando as coordenadas dos pontos no quadrado unitário 
#seguindo uma distribuição uniforme entre 0 e 1.
x <- runif(N)
y <- runif(N)

# Percorrendo as listas geradas de x e y, se o ponto gerado estiver 
#dentro do primeiro quadrante do círculo, somamos 1 no contador n.
for (i in 1:N) {
  if (x[i]^2 + y[i]^2 < 1) {
    n <- n + 1
  }
}

# Como a área de um quadrante do círculo é pi/4, multiplicamos a razão n/N por 4.
pi_estimado <- 4 * n / N

pi_estimado
