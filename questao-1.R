#******************************************************************************************************
#                   TDE 2 - Inferência Comparada.
#
#     Questão: 1
#       Autor: Gabriela Dellamora Paim
#      Versão: 2024/06/05
#
#******************************************************************************************************
# Imports
library(EnvStats)
library(extraDistr)
library(stats)

# Parâmetros 
a <- 0
b <- 2
m <- 1
n <- 5000
set.seed(222)
u <- runif(n)

# Função para inversão da distribuição triangular. Calcula F^-1(u)

itriang <- function(u, a = -1, b = 1, m = (a + b)/2)
{
  fdc <- (m - a) / (b - a)
  ifelse(u < fdc,
         a + sqrt(u * (m - a) * (b - a)),
         b - sqrt((1 - u) * (b - m) * (b - a)))
}

#******************************************************************************************************
# a.Simule a densidade de uma Triangular(0, 1, 2) a partir do Método da Transformação Inversa
#******************************************************************************************************

## Execução exercício
tarefa_a <- itriang(u, a, b, m)

## Plotando gráfico
hist(x,
     40,
     freq = FALSE,
     col = rgb(1, 0.3, 0),
     xlim = c(a, b),
     main = 'Transformação Inversa - Triangular(0, 1, 2)'
     )

## "Prova real"
prova_real <- qtri(p=u, min=a, max=b, mode=m)

par(mfrow = c(1, 2))
hist(tarefa_a,
     40,
     freq = FALSE,
     col = rgb(1, 0.3, 0),
     xlim = c(a, b),
     main = 'Transformação Inversa - Triangular(0, 1, 2)'
     )

hist(prova_real,
     40,
     freq = FALSE,
     col = rgb(0, 0.3, 1),
     xlim = c(a, b),
     main = 'Prova Real - Triangular(0, 1, 2)'
     )
par(mfrow = c(1, 1))

#******************************************************************************************************
# b.Compare com os resultados de extraDistr::rtriang do R [...]. Dica: use o teste de KS
#******************************************************************************************************
## Execução exercício
x <- itriang(u, a, b, m)
y <- rtriang(n, a, b, m)

## Plotando gráfico
par(mfrow = c(1, 2))
hist(x, 40,
     freq = FALSE,
     col = rgb(1, 0.3, 0), 
     xlim = c(a, b),
     main = 'ITRIANG(0, 1, 2)',
     xlab = 'Valor',
     ylab = 'Densidade'
     )

hist(y, 40,
     freq = FALSE,
     col = rgb(0, 0.3, 1),
     xlim = c(a, b),
     main = 'RTRIANG(0, 1, 2)',
     xlab = 'Valor', ylab = 'Densidade'
     )
# Overlap (para identificar D)
hist(x, 40,
     freq = FALSE,
     col = rgb(1, 0.3, 0, 0.3), 
     xlim = c(a, b),
     main = 'ITRIANG(0, 1, 2)',
     xlab = 'Valor',
     ylab = 'Densidade',
     add = TRUE
)
par(mfrow = c(1, 1))

## Teste KS
ks.test(x, y)


# Análise do Retorno:
#
# [ D <- 0.00406 ]
# D representa a maior diferença absoluta entre as funções.
# Sendo um valor pequeno (próximo de zero), indica que as distribuições são muito semelhantes.

# [ p-value <- 0.8044 , alpha <- 0.05 (5%) ]
# H0 <- x = y   (as duas amostras provêm da mesma distribuição)
# H1 <- x <> y  (As duas amostras não provêm da mesma distribuição)
# Ou seja: Não existem evidências para rejeitar H0.

# O resultado do teste de KS indica que provavelmente as duas amostras provêm da mesma distribuição.
