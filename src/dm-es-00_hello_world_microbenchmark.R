# You can install packages using
# install.packages("<package_name>")
library(microbenchmark)

example <- function(string) {
  print(string)
}
microbenchmark(example("Hello World"))