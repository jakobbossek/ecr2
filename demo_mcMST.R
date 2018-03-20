library(devtools)

load_all(".")

# data(mcMST)
# obj.cols = c("f1", "f2")

# # normalize to [1, 1] x [1, 2]
# mcMST = ecr::normalize(mcMST, obj.cols = obj.cols, offset = c(1, 1))

# # check ref-points/sets
# #ref.points = approximateRefPoints(mcMST, obj.cols = obj.cols, offset = 0.1, as.df = TRUE)
# #ref.sets = approximateRefSets(mcMST, obj.cols = obj.cols, as.df = FALSE)

# # select subset of instances for examples
# mcMST = dplyr::filter(mcMST,
#   prob %in% c("instance-100-1", "instance-100-2", "instance-100-3"))

# #stop()
# # visualize approximation sets

# pl = plotScatter2d(dplyr::filter(mcMST, repl <= 2), title = "Test", colour = "algorithm", facet.type = "grid", facet.args = list(facets = formula(repl ~ prob)))
# print(pl)

# # define which unary indicators to use
# unary.inds = list(
#   HV = list(fun = ecr::computeHV),
#   HVIND = list(fun = ecr::emoaIndHV),
#   EPS  = list(fun = ecr::emoaIndEps),
#   #R2   = list(fun = ecr::emoaIndR2),
#   ONVG = list(fun = ecr::emoaIndONVG),
#   DELTA = list(fun = ecr::emoaIndDelta)
# )

# inds = computeIndicators(dplyr::filter(mcMST, grepl("NSGA", algorithm)), unary.inds = unary.inds)

# unary.inds = inds$unary

# plotIndicatorDistribution(unary.inds, plot.type = "boxplot")

# load_all()
unary.inds$algorithm = gsub("NSGA2.", "", unary.inds$algorithm, fixed = TRUE)
test.res = applyStatisticalTests(unary.inds, ind.names = c("HV", "EPS"))


library(kableExtra)
library(knitr)

probs = names(test.res)
n.probs = length(probs)
n.algos = 4L


scinot <- function(x, digits=2, showDollar=FALSE)
{
    sign <- ""
    if (x < 0) {
        sign <- "-"
        x <- -x
    }
    exponent <- floor(log10(x))
    if (exponent) {
        xx <- round(x / 10^exponent, digits=digits)
        e <- paste("\\times 10^{", as.integer(exponent), "}", sep="")
    } else {
        xx <- round(x, digits=digits)
        e <- ""
    }
    if (showDollar) paste("$", sign, xx, e, "$", sep="")
    else paste(sign, xx, e, sep="")
}

niceCellFormater = function(cell) {
  if (is.na(cell))
    return ("-")
  else if (as.numeric(cell) > 0.05)
    return ("$> 0.05$")
  else
    return (sprintf("$\\mathbf{%s}$", scinot(cell)))
}

HV.res = do.call(cbind, lapply(test.res, "[[", "HV"))
EPS.res = do.call(cbind, lapply(test.res, "[[", "EPS"))
comb.res = rbind(HV.res, EPS.res)

comb.res = apply(comb.res, 2L, function(x) {
  sapply(x, niceCellFormater)
})


# mtcars[1:10, 1:2] %>%
# mutate(
# car = row.names(.),
# # You don't need format = "latex" if you have ever defined options(knitr.table.format) mpg = cell_spec(mpg, "latex", color = ifelse(mpg > 20, "red", "blue")),
# cyl = cell_spec(cyl, "latex", color = "white", align = "c", angle = 45,
# background = factor(cyl, c(4, 6, 8),
# c("#666666", "#999999", "#BBBBBB")))
# ) %>%

toLatex = function(stats, probs = NULL, inds = NULL, by.instance = TRUE, cell.formatter = NULL) {
  assertList(stats)
  assertFlag(by.instance)

  if (is.null(probs))
    probs = unique(names(stats))

  if (is.null(inds))
    inds = unique(names(stats[[1L]]))

  if (is.null(cell.formatter))
    cell.formatter = niceCellFormater

  all.probs = names(stats)
  all.inds  = names(stats[[1L]])

  assertSubset(probs, choices = all.probs)
  assertSubset(inds, choices = all.inds)
  assertFunction(cell.formatter, args = "cell")

  # now filter relevant stats, i.e., for selected problems
  stats = stats[which(all.probs %in% probs)]

  if (by.instance)

  for (prob in probs) {
    catf("Problem: %s", prob)
    res.stats = stats[[prob]][which(names(stats[[prob]]) %in% inds)]
    inds = names(res.stats)
    res.stats = do.call(cbind, res.stats)
    res.stats = apply(res.stats, 2L, function(column) sapply(column, cell.formatter))

    n.inds = length(inds)
    n.algos = nrow(res.stats)

    dd = kable(res.stats, format = "latex", booktabs = TRUE, escape = FALSE)
    dd = kable_styling(dd, latex_options = "striped", position = "center")
    header.probs = c(1, rep(n.algos, n.inds))
    inds.latex = c("$I_{HV}$", "$I_{\\\\epsilon}^{+}$")
    group.titles = paste(rep(prob, n.inds), "/", inds.latex)
    names(header.probs) = c(" ", group.titles)

    dd = kableExtra::add_header_above(dd, header.probs, bold = TRUE, escape = FALSE)
    print(dd)
    # dd = group_rows(dd, "$I_{HV}^{+}$", 1, 4, escape = FALSE)
    # dd = group_rows(dd, "$I_{\\eps}^b$", 5, 8, escape = FALSE)
  }
}

toLatex(test.res, probs = c("instance-100-2", "instance-100-3"), by.instance = TRUE)


# dd = kable(comb.res, format = "latex", booktabs = TRUE, escape = FALSE)
# dd = kable_styling(dd, latex_options = "striped", position = "center", font_size = 7)
# header.probs = c(1, rep(n.algos, n.probs))
# names(header.probs) = c(" ", probs)

# dd = add_header_above(dd, header.probs)
# dd = group_rows(dd, "$I_{HV}^{+}$", 1, 4, escape = FALSE)
# dd = group_rows(dd, "$I_{\\eps}^b$", 5, 8, escape = FALSE)
#print(dd)







stop("computed indicators")

binary.inds = inds$binary

plotHeatmap(binary.inds[[1L]])


stop()
# 3D EXAMPLE

mcMST3d = mcMST
mcMST3d$f3 = mcMST3d$f2
plotScatter3d(mcMST3d, package = "plot3D")
