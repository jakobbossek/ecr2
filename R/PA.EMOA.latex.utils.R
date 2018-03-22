toScientificLaTeXNotation = function(x, digits = 2) {
  sign = ""
  if (x < 0) {
    sign = "-"
    x = -x
  }

  exponent = floor(log10(x))
  if (exponent) {
    y = round(x / (10^exponent), digits = digits)
    e = paste0("\\times 10^{", as.integer(exponent), "}")
  } else {
    y = round(x, digits = digits)
    e = ""
  }
  return(paste0(sign, y, e))
}
