library(ade4)
library(GUniFrac)

data(throat.otu.tab)
data(throat.tree)
data(throat.meta)

groups = throat.meta$SmokingStatus

otu.tab.rff <- Rarefy(throat.otu.tab)$otu.tab.rff

str(otu.tab.rff)
unifracs <- GUniFrac(otu.tab.rff, throat.tree, alpha=c(0, 0.5, 1))$unifracs
str(unifracs)

dw <- unifracs[, , "d_1"]		# Weighted UniFrac
du <- unifracs[, , "d_UW"]		# Unweighted UniFrac	
dv <- unifracs[, , "d_VAW"]		# Variance adjusted weighted UniFrac
d0 <- unifracs[, , "d_0"]     		# GUniFrac with alpha 0  
d5 <- unifracs[, , "d_0.5"]   		# GUniFrac with alpha 0.5 

adonis(as.dist(d5) ~ groups)
s.class(cmdscale(d5, k=2), fac = groups) 
