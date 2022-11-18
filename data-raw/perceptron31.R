set.seed(42)
x1red <- round(runif(16, 4, 16), 2)
x1blue <- round(runif(14, 0, 15), 2)
x1extra <- 5
x2red <- round(5*x1red - runif(16, 0, 5*x1red))
x2blue <- round(5*x1blue + runif(14, 0, 30))
x2extra <- 50
perceptron31 <- rbind(
  data.frame(x1=x1red, x2=x2red, col="red"),
  data.frame(x1=x1blue, x2=x2blue, col="blue"),
  data.frame(x1=x1extra, x2=x2extra, col="red")
)
perceptron31$col <- factor(perceptron31$col, levels = c("red", "blue"))
save(perceptron31, file = here::here("data", "perceptron31.rda"),
     version = 2, compress = "bzip2")
