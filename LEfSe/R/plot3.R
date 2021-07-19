library(data.table)

dt = rio::import("data/lda2.csv")
dt = as.data.table(dt)

x = c(
  "d" = "HC",
  "a" = "AR",
  "b" = "Asthma",
  "c" = "Asthma+AR")

# dt[, 比较组 := forcats::fct_relevel(x[比较组], "HC")]

dt[, LDA值 := as.numeric(LDA值)]
dt[, 物种 := stringr::str_extract(物种, "(?<=\\.)\\w+$")]


orders = dt[order(比较组, - LDA值)][, 属]



png("plot3.png", width = 800, height = 350)
ggplot(dt, aes(x = LDA值, y = 属, fill = 比较组)) +
  geom_col(width = 0.6, color = "black", alpha = 0.8) +
  scale_y_discrete(limits = rev(orders)) +
  scale_x_continuous(breaks = 0:3 * 1.2) +
  scale_fill_manual(values = "#800080") +
  # coord_flip() +
  labs(x = "LDA SCORE(log 10)", y = "", subtitle = "") +
  coord_cartesian(expand = FALSE, xlim = c(0, 4)) +
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

