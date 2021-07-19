library(data.table)
library(ggplot2)
dt = rio::import("data/lda.csv")
dt = as.data.table(dt)


dt[, 比较组 := forcats::fct_relevel(比较组, "B")]

dt[, LDA值 := as.numeric(LDA值)]
dt[比较组 == "NB", LDA值 := -LDA值]
# dt[, 物种 := stringr::str_extract(物种, "(?<=\\.)\\w+$")]


orders = dt[order(比较组, - LDA值)][, 属]

png("plot2.png", width = 800, height = 850)
ggplot(dt, aes(x = LDA值, y = 属, fill = 比较组)) +
  geom_col(width = 0.8, color = "black", alpha = 0.8) +
  scale_y_discrete(limits = rev(orders)) +
  scale_x_continuous(breaks = -3:3 * 1.2) +
  scale_fill_manual(values = c("#00ffff", "#ffa500")[c(2, 1)]) +
  # coord_flip() +
  labs(x = "LDA SCORE(log 10)", y = "") +
  geom_text(aes(x = 0 + 0.1, label = 属), hjust = 0, size = 5, data = dt[比较组 == "NB"]) +
  geom_text(aes(x = 0 - 0.1, label = 属), hjust = 1, size = 5, data = dt[比较组 == "B"]) +
  coord_cartesian(expand = FALSE, xlim = c(-4, 4)) +
  theme(
    text = element_text(family = "Arial"),
    axis.title.x = element_text(size = 24),
    axis.text.x = element_text(size = 16),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_blank(), legend.position = c("top"),
    legend.justification = c(0.1, 1),
    legend.text = element_text(size = 18),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "#4e4c4c", linetype = 2, size = 1))

dev.off()
