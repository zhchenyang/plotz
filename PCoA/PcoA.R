library(data.table)
library(GUniFrac)
library(ade4)
library(vegan)

# uwt = fread("~/data/beta/a-b-c-d.weighted_unifrac.coordinate.xls")
dis = fread("~/data/beta/weighted_unifrac_a-b-c-d.matrix")

# group = rio::import("~/data/beta/分组及编号.xlsx")
group = rio::import("~/data/新的分组及编号.xlsx")
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

test = anosim(dist, groups, permutations = 999, distance = "bray", strata = NULL, parallel = getOption("mc.cores"))
print(test)
# png("anosim.test.png", width = 960, height = 960)
# plot(test, xlab = "groups", ylab = "")
# dev.off()

res = pcoa(dist)
result = res$values[, "Relative_eig"]
pro1 = as.numeric(sprintf("%.4f", result[1])) * 100
pro2 = as.numeric(sprintf("%.4f", result[2])) * 100

# png("PCoA2.png", width = 960, height = 960)
png("PCoA.png", width = 960, height = 960)
plot.new()
plot(x = res$vectors[, 1] * 1.05,
  y = res$vectors[, 2] * 1.05, col = "white",
  xlab = glue::glue("PCoA1 {pro1}%"), ylab = glue::glue("PCoA2 {pro2}%"),
  cex.lab = 1.35)
abline(v = -4:5 / 10, h = -2:6 / 10, col = "gray75")
abline(h = 0, v = 0)
s.class(res$vectors[, 1:2], fac = groups, clabel = 1.5,
  label = c("AR", "Asthma", "Asthma + AR", "HC"),
  col = c("darkblue", "darkgreen", "darkorchid4", "#ff0800"),
  # col = c("darkblue", "darkgreen", "#ff0800"),
  add.plot = TRUE)
text(x = -0.5, y = -0.25,
  adj = c(0, 0),
  labels = glue::glue("p.value = {p}"),
  cex = 1.5)
dev.off()
