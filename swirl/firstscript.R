plot(rnorm(100))

df <- data.frame(sibling = c("Ann", "Marie", "Joe"),
                 age = c(12, 34, 7)
)
df

ds <- tibble::tibble(
  sibling = c("Ann", "Marie", "Joe"),
  age = c(12, 34, 7)
)

a <- c("A","B")
b <- c("a","b")
expand.grid(a,b)

x <- c(1,3,5,6,3)
y <- c(2,2,7,3,2)
diff(x)
prod(x)
rank(x)
rev(rank(x))
cumsum(x)
cumprod(x)
cummin(x)
cummax(x)

pmin(x,y)

which.max(x)
split(x,y)
