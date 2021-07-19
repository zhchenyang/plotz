library(data.table)
library(ggplot2)
# library(microbiomeMarker)
# library(microbiomeViz)
# df = fread("~/data/LDA_202107021352.xls")


# data("SRS014459_Stool_profile")
# tr <- parseMetaphlanTSV(SRS014459_Stool_profile)
# tr
# p <- tree.backbone(tr)
# p

dt = rio::import("data/LDA-a-b-c-d.xlsx")
dt = as.data.table(dt)

x = c(
  "d" = "HC",
  "a" = "AR",
  "b" = "Asthma",
  "c" = "Asthma+AR")

dt[, 比较组 := forcats::fct_relevel(x[比较组], "HC")]

dt[, LDA值 := as.numeric(LDA值)]
dt[, 物种 := stringr::str_extract(物种, "(?<=\\.)\\w+$")]


orders = dt[order(比较组, - LDA值)][, 物种]

png("plot.png", width = 800, height = 850)
ggplot(dt, aes(x = LDA值, y = 物种, fill = 比较组)) +
  geom_col(width = 0.8, color = "black", alpha = 0.8) +
  scale_y_discrete(limits = rev(orders)) +
  scale_x_continuous(breaks = 0:4 * 1.2) +
  scale_fill_manual(values = c("#ff00dd", "#014201", "#0000a1", "#d80000")[c(1, 4, 3, 2)]) +
  # coord_flip() +
  labs(x = "LDA SCORE(log 10)", y = "") +
  coord_cartesian(expand = FALSE, xlim = c(0, 5)) +
  theme(
    text = element_text(family = "Arial"),
    axis.title.x = element_text(size = 24),
    axis.text = element_text(size = 16),
    legend.title = element_blank(), legend.position = c("top"),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 18),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "#4e4c4c", linetype = 2, size = 1.2))

dev.off()
