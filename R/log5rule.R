log5rule <- function(a, b) {
  stopifnot(a>=0, a<=1, b>=0, b<=1)
  (a-a*b) / (a+b - 2*a*b)
}
if (F) {
  curve(log5rule(x, .5))
  curve(log5rule(x, .6))
  curve(log5rule(x, .4))
  curve(log5rule(.5, x))
  curve(log5rule(.6, x))
  curve(log5rule(.4, x))
}
