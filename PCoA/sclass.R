library(data.table)
library(GUniFrac)
library(ade4)
library(vegan)
library(purrr)

cols = c("#ff00dd", "#014201", "#0000a1", "#d80000")[c(4, 3, 2, 1)]

label = c("AR", "Asthma", "Asthma + AR", "HC")

# uwt = fread("~/data/beta/a-b-c-d.weighted_unifrac.coordinate.xls")
dis = fread("data/weighted_unifrac_a-b-c-d.matrix")

# group = rio::import("data/beta/分组及编号.xlsx")
group = rio::import("data/新的分组及编号.xlsx")
group = as.data.table(group)
names(group) = c("old", "raw", "new")


# goal = c("B", "NB", "D")
# goal = c("C", "NC", "D")

# indics = group[new %in% goal, old]
# dis = dis[order(sample)][sample %in% indics, ..indics]

index = sort(names(dis)[-1])
index
dist = dis[order(sample)][, ..index]

dist = as.matrix(dist)
row.names(dist) = index

groups = substr(index, 1, 1)
# groups = as.factor(group[old %in% indics, new])

groups = as.factor(groups)

test = adonis3(as.dist(dist) ~ groups)
print(test)
p = test$aov.tab$`Pr(>F)`[1]

# test = anosim(dist, groups, permutations = 999, distance = "bray", strata = NULL, parallel = getOption("mc.cores"))
# print(test)
# png("anosim.test.png", width = 960, height = 960)
# plot(test, xlab = "groups", ylab = "")
# dev.off()

res = pcoa(dist)
result = res$values[, "Relative_eig"]
pro1 = as.numeric(sprintf("%.4f", result[1])) * 100
pro2 = as.numeric(sprintf("%.4f", result[2])) * 100

# 权重矩阵
fac = groups
dfdistri <- ade4:::fac2disj(fac) * rep(1, length(fac))

# col = rep(1, length(levels(fac)))
coul <- rep(1, length(levels(fac)))

w1 <- unlist(lapply(dfdistri, sum))
dfdistri <- t(t(dfdistri) / w1)

# x yax 1,2
coox <- as.matrix(t(dfdistri)) %*% res$vectors[, 1]
cooy <- as.matrix(t(dfdistri)) %*% res$vectors[, 2]

# 计算边界

source("get_ellipse.R")
es = list()
for (i in 1:ncol(dfdistri)) {
  es[[i]] = get_ellipse(res$vectors[, 1], res$vectors[, 2], dfdistri[, i],
                        cellipse = 1.5, axesell = TRUE, coul = "black")
}

xs = map(es, "x")
xs = c(xs, res$vectors[, 1])
xs = range(unlist(xs)) * 1.05

ys = map(es, "y")
ys = c(ys, res$vectors[, 2])
ys = range(unlist(ys)) * 1.05

par(family = "Arial", mar = c(5, 5, 4, 2))

plot.new()

plot(
  x = res$vectors[, 1],
  y = res$vectors[, 2],
  col = "white",
  xlim = xs, ylim = ys,
  xlab = glue::glue("PCoA1 {pro1}%"), ylab = glue::glue("PCoA2 {pro2}%"),
  cex.lab = 1.35)

col <- "lightgray"
lty <- 1
xaxp <- par("xaxp")
ax <- (xaxp[2] - xaxp[1]) / xaxp[3] / 2
yaxp <- par("yaxp")
ay <- (yaxp[2] - yaxp[1]) / yaxp[3] / 2
a <- min(ax, ay)
v0 <- seq(xaxp[1], xaxp[2], by = a)
h0 <- seq(yaxp[1], yaxp[2], by = a)
abline(v = v0, col = col, lty = lty)
abline(h = h0, col = col, lty = lty)

abline(v = 0, h = 0, col = "black")

pch = 20

for (i in 1:ncol(dfdistri)) {
  pch <- rep(pch, length = nrow(res$vectors ))
  points(res$vectors[, 1][dfdistri[, i] > 0], res$vectors[, 2][dfdistri[, i] > 0],
  pch = pch[dfdistri[, i] > 0], cex = par("cex"), col = cols[i])
}

for (i in 1:ncol(dfdistri)) {
  scatterutil.star(res$vectors[, 1], res$vectors[, 2], dfdistri[, i], cstar = 1,
    cols[i])
}


for (i in seq_along(es)) {
  polygon(x = es[[i]][["x"]],
    y = es[[i]][["y"]], border = cols[i])

  segments(es[[i]][["seg1"]][1],
    es[[i]][["seg1"]][2],
    es[[i]][["seg1"]][3],
    es[[i]][["seg1"]][4],
    lty = 2)

  segments(es[[i]][["seg2"]][1],
    es[[i]][["seg2"]][2],
    es[[i]][["seg2"]][3],
    es[[i]][["seg2"]][4],
    lty = 2)
}

scatterutil.eti(coox, cooy, label, clabel = 1.5, coul = cols)

text(x = xs[1], y = ys[1],
  adj = c(0, 0),
  labels = glue::glue("p.value = {p}"),
  cex = 1.5)
