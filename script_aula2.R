##======================================================================
## Script com aplicações dos conceitos da aula 2. Estimativas de pi por
## simulação e simulação estocástica.
##======================================================================

##======================================================================
## Agulha de Buffon
##======================================================================

##----------------------------------------------------------------------
## Definindo as variaveis envolvidas
##----------------------------------------------------------------------

## l = comprimento da agulha
## a = distancia entre as paralelas
## D = distancia do centro da agulha até a paralela mais proxima. Fica
## no intevalo D \in [0, a/2]
## d = distancia do centro ate a ponta da agulha. Define se a agulha
## toca ou nao na paralela (se D <= d)
## \theta = angulo formado entre a linha a linha que passa pelo centro
## da agulha e sua posicao. Fica no intervalo \theta \in [0, pi]

## Por trigonometria,
## d = (l/2) * \sin(\theta)

##----------------------------------------------------------------------
## Visualizacao grafica
##----------------------------------------------------------------------

# define as variaveis
a <- 1
l <- 1
Dist <- seq(0, a/2, length = 100)       # Dist = D
theta <- seq(0, pi, length = 100)
# calcula o sub-espaco
d <- (l/2) * sin(theta)
# grafico - qualquer ponto dentro da area sombreada significa que a
# agulha tocou uma paralela
plot(theta, d, type = "l", xaxs = "i", yaxs = "i",
     xlab = expression(theta),
     ylab = expression(d == (l/2) %*% sin(theta)),
     axes = FALSE)
polygon(theta, d, density = 10)
axis(1, at = round(seq(0, pi, length = 10), 2),
     labels = round(seq(0, pi, length = 10), 2))
axis(2, at = round(seq(0, a/2, length = 6), 1),
     labels = round(seq(0, a/2, length = 6), 1))
box()

## Grafico 3D
# gera a matriz com todos os valores possiveis para a amplitude de D e
# \theta
mat <- matrix(Dist %*% t(sin(theta)), nrow = length(Dist),
              ncol = length(theta))
# para fazer o wireframe eh mais facil criar um dataframe para usar no
# formato de formula
df <- expand.grid(Dist=Dist, theta=theta)
# aloca os valores calculados de d da matriz
df$d <- as.vector(mat)
# wireframe
wireframe(d ~ Dist + theta, data = df,
          scales = list(arrows = FALSE,
          x = list(tick.number = 5),
          y = list(tick.number = 5),
          z = list(tick.number = 5)),
          xlab = "D", ylab = expression(theta),
          zlab = list(expression(d == D %*% sin(theta)), rot=90))

##----------------------------------------------------------------------
## Simulacao do experimento de Buffon
##----------------------------------------------------------------------

## Numero de repeticoes
n <- 1000

## D ~ U[0, a/2]
D.random <- runif(n, 0, a/2)

## \theta ~ U[0, pi]
theta.random <- runif(n, 0, pi)

## d = (l/2) * \sin(\theta)
d <- (l/2) * sin(theta.random)

## var. aleatoria H, onde
## H = 1, se D <=d
## H = 0, c.c.
H <- numeric(n)
H[D.random <= d] <- 1

## h = numero de sucessos
h <- sum(H)

## \pi ~ (2ln)/(ah)
pi <- (2*l*n)/(a*h)

## Funcao
buffon <- function(n, a, l){
    D.random <- runif(n, 0, a/2)
    theta.random <- runif(n, 0, pi)
    d <- (l/2) * sin(theta.random)
    H <- numeric(n)
    H[D.random <= d] <- 1
    h <- sum(H)
    pi <- (2*l*n)/(a*h)
    return(pi)
}

system.time(
            buffon(10000000, 1, 1)
            )
