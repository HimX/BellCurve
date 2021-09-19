print_bell_curve <- function(media, dt, lower_limit,
                             upper_limit, x, y, i, pdf_name, formula) {
    pdf(pdf_name)
    plot(x, y,
        type = "n", xlab = "", ylab = "",
        main = "Distribuci\uF3n Normal"
    )

    lines(x, y)
    # Join the points
    polygon(c(lower_limit, x[i], upper_limit), c(0, y[i], 0),
        col = "paleturquoise"
    )
    area <- pnorm(upper_limit, media, dt) - pnorm(lower_limit, media, dt)
    result <- paste(
        formula, signif(area, digits = 4)
    )
    mtext(result, 3)
}

# Ejemplo
media <- 100
dt <- 12
lower_limit <- 85
upper_limit <- 115
x <- seq(-4, 4, length = 100) * dt + media
y <- dnorm(x, media, dt)
i <- x > lower_limit & x < upper_limit
print_bell_curve(
    media, dt, lower_limit, upper_limit, x, y, i,
    "outputs/Ejemplo.pdf", "P(85 < X < 115) = "
)

# Punto 1
media <- 15
dt <- 2
lower_limit <- 16
upper_limit <- 20
x <- seq(-4, 4, length = 100) * dt + media
y <- dnorm(x, media, dt)
i <- x > lower_limit & x < upper_limit
print_bell_curve(
    media, dt, lower_limit, upper_limit, x, y, i,
    "outputs/Punto1.pdf", "P(16 < X < 20) = "
)

# Punto 2
media <- 10
dt <- 3
lower_limit <- 0
upper_limit <- 8
x <- seq(-4, 4, length = 100) * dt + media
y <- dnorm(x, media, dt)
i <- x < upper_limit
print_bell_curve(
    media, dt, lower_limit, upper_limit, x, y, i,
    "outputs/Punto2.pdf", "P(X < 8) = "
)

# Punto 3
media <- 50
dt <- 4
lower_limit <- 0
upper_limit <- 60
x <- seq(-4, 4, length = 100) * dt + media
y <- dnorm(x, media, dt)
i <- x < upper_limit
print_bell_curve(
    media, dt, lower_limit, upper_limit, x, y, i,
    "outputs/Punto3.pdf", "P(X < 60) = "
)

# Punto 4
media <- 250
dt <- 20
lower_limit <- 215
upper_limit <- 400
x <- seq(-4, 4, length = 100) * dt + media
y <- dnorm(x, media, dt)
i <- x > lower_limit
print_bell_curve(
    media, dt, lower_limit, upper_limit, x, y, i,
    "outputs/Punto4.pdf", "P(X > 215)"
)