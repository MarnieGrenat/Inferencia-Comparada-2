.Machine
library(EnvStats)
# https://search.r-project.org/CRAN/refmans/extraDistr/html/Triangular.html
# https://cran.r-project.org/web/packages/EnvStats/EnvStats.pdf
# Método da Transformação Inversa:
# a < b | a ≤ c ≤ b.
a <- 0
b <- 2
c <- 1

# Obter u de uma U(0, 1)
# Repetir 5 mil valores

set.seed(2)
u <- runif(5000)

# Calcular F^-1(u)
x <- qtri(p=u, min=a, max=b, mode=c)

# Plotar Gráfico
hist(x, 40, freq = FALSE, main = 'Triangular(0, 1, 2)')

# Verificação extra 
tri_inv <- function(u, a, c, b) {
  
  # Valor da função de distribuição cumulativa no ponto c
  fdc <- (c - a) / (b - a)  
  
  # Aplicar a fórmula inversa para a distribuição triangular
  ifelse(u < fdc,
         a + sqrt(u * (b - a) * (c - a)),
         b - sqrt((1 - u) * (b - a) * (b - c)))
}


x <- tri_inv(u, a, c, b)
hist(x, 40, freq = FALSE, main = 'Triangular(0, 1, 2)')