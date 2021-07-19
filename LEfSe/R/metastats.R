library(data.table)
data = rio::import("~/data/fengdu.xlsx", header = TRUE, row.names = 1)
data2 = read.csv("~/data/OTU_shared_final.csv", header = TRUE, row.names = 1)

group = rio::import("~/data/新的分组及编号.xlsx")
names(group) = c("raw", "old", "new")
group = as.data.table(group)

# 选出 a b c d 四组
abcd = group[old %in% letters[1:4], .(raw, old)]

# a b
ad = group[old %in% c("a", "d"), .(raw, old)]

groups = abcd$old

names(groups) = abcd$raw

# otu 表中的 a b c d 组
row.names(data) = data[, 1]
data = data[, names(groups)]
otu = t(data[, -1])
str(otu)


index = rownames(otu)

groups = groups[index]
groups = as.factor(groups)
newotu = data.frame(group = groups, otu)

pvalue = t(otu)[, 1:2]
colnames(pvalue) = c("p-value", "FDR")

for (i in 2:ncol(newotu)) {
  t = kruskal.test(newotu[, i] ~ newotu[, 1])
  pvalue[i - 1, 1] = t$p.value
}
tmp = pvalue
pvalue = pvalue[order(pvalue[, 1]), ]
pvalue[, 2] = p.adjust(pvalue[, 1], method = "BH", n = nrow(pvalue))
tmp[, 2] = p.adjust(tmp[, 1], method = "BH", n = nrow(tmp))
tmp = tmp[order(tmp[, 1]), ]
pvalue
tmp

top = pvalue[pvalue[, 2] < 0.05, ]
topdata = cbind(group = newotu[, 1],
  newotu[, rownames(top)[!is.na(rownames(top))]])

library(gplots)
cols = c("darkblue", "darkgreen", "darkorchid4", "#ff0800")
mycol = colorRampPalette(c("white", "blue", "green", "red", "red"))(100)
sidecol = c(rep(cols[1], 30), rep(cols[2], 30),
  rep(cols[3], 30), rep(cols[4], 20))

cairo_pdf("heatmap.pdf")

topdata = topdata[order(topdata[, 1]), ]

heatmap.2(log(as.matrix(topdata[, -1]) + 1), Rowv = FALSE,
  dendrogram = "column", trace = "none",
  RowSideColors = sidecol,
  keysize = 1.2, key.title = "",
  cexRow = 1.2, cexCol = 0.5)
dev.off()

write.csv(pvalue, "pvalue-abcd.csv")


# a d part
groups = ad$old

names(groups) = ad$raw

# otu 表中的 a d 组
# row.names(data) = data[, 1]
data = data[, names(groups)]

otu = t(data[, -1])



index = rownames(otu)

groups = groups[index]
groups = as.factor(groups)
newotu = data.frame(group = groups, otu)

pvalue_t = t(otu)[, 1:2]
pvalue_kw = t(otu)[, 1:2]
colnames(pvalue_t) = c("p-value-t.test", "FDR-t.test")
colnames(pvalue_kw) = c("p-value-kw.test", "FDR-kw.test")

for (i in 2:ncol(newotu)) {
  t = t.test(newotu[, i] ~ newotu[, 1])
  t1 = kruskal.test(newotu[, i] ~ newotu[, 1])
  pvalue_t[i - 1, 1] = t$p.value
  pvalue_kw[i - 1, 1] = t1$p.value
}
tmp = pvalue_t
# pvalue_t[, 2] = p.adjust(pvalue[, 1], method = "BH", n = nrow(pvalue))
pvalue_t = pvalue_t[order(pvalue_t[, 1]), ]
pvalue_t[, 2] = p.adjust(pvalue_t[, 1], method = "BH", n = nrow(pvalue_t))
pvalue_t

pvalue_kw = pvalue_kw[order(pvalue_kw[, 1]), ]
pvalue_kw[, 2] = p.adjust(pvalue_kw[, 1], method = "BH", n = nrow(pvalue_kw))
out = cbind(pvalue_t, pvalue_kw)
out
top = pvalue[pvalue[, 2] < 0.05, ]
topdata = cbind(group = newotu[, 1],
  newotu[, rownames(top)[!is.na(rownames(top))]])

library(gplots)
mycol = colorRampPalette(c("white", "blue", "green", "red", "red"))(100)
sidecol = c(rep("red", 7), rep("green", 12), rep("blue", 3))
cairo_pdf("heatmap-ad.pdf")
heatmap.2(log(as.matrix(topdata[, -1]) + 1), Rowv = FALSE,
  dendrogram = "column", trace = "none",
  keysize = 1.2, key.title = "",
  cexRow = 1.2, cexCol = 0.5)
dev.off()

write.csv(pvalue_t, "pvalue-ad-t.test.csv")
write.csv(pvalue_kw, "pvalue-ad.kw.test.csv")
