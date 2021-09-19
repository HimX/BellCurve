media <- 100
dt <- 12
lower_limit <- 85
upper_limit <- 115

x <- seq(-4, 4, length = 100) * dt + media
y <- dnorm(x, media, dt)

plot(x, y,
    type = "n", xlab = "", ylab = "",
    main = "Distribuci\uF3n Normal"
)
i <- x >= lower_limit & x <= upper_limit

lines(x, y)
# Join the points
polygon(c(lower_limit, x[i], upper_limit), c(0, y[i], 0), col = "paleturquoise")
area <- pnorm(upper_limit, media, dt) - pnorm(lower_limit, media, dt)
result <- paste(
    "P(", lower_limit, "< X <", upper_limit,
    ")=", signif(area, digits = 4)
)
mtext(result, 3)