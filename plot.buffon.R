plot.buffon <- function(x, xlab = "Número de jogadas da agulha",
                        ylab = expression(paste("Estimativa de ", pi)),
                        ...){
    plot(x$n, x$pi.est, type = "l", xlab = xlab, ylab = ylab, ...)
    abline(h = pi, col = "lightgrey")
}
