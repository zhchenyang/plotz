## PCoA 绘图流程

PCoA 的作图主要分为三个步骤：选择特定的相似性距离并计算距离矩阵。距离的选择可以有 Bray-curits、Unifrac 等，不同的距离有不同的作用和意义（具体可以参考微生物 β 多样性常用计算方法比较）。

相似性距离可以利用 R 的 GUniFrac 和 vegan 等包计算，也可以利用 QIIME 计算。

进行 PCoA 分析，也就是利用表征分析选择最能表示样本距离的坐标轴。这个可以利用 R 的 ape 包的 pcoa（） 命令完成。

## 补充内容

PCA（Principal Components Analysis）即主成分分析，也称主分量分析或主成分回归分析法，首先利用线性变换，将数据变换到一个新的坐标系统中；然后再利用降维的思想，使得任何数据投影的第一大方差在第一个坐标(称为第一主成分)上，第二大方差在第二个坐标(第二主成分)上。这种降维的思想首先减少数据集的维数，同时还保持数据集的对方差贡献最大的特征，最终使数据直观呈现在二维坐标系。

PCoA（Principal Co-ordinates Analysis）分析即主坐标分析，可呈现研究数据相似性或差异性的可视化坐标，是一种非约束性的数据降维分析方法，可用来研究样本群落组成的相似性或相异性。它与PCA类似，通过一系列的特征值和特征向量进行排序后，选择主要排在前几位的特征值，找到距离矩阵中最主要的坐标，结果是数据矩阵的一个旋转，它没有改变样本点之间的相互位置关系，只是改变了坐标系统。两者的区别为PCA是基于样本的相似系数矩阵（如欧式距离）来寻找主成分，而PCoA是基于距离矩阵（欧式距离以外的其他距离）来寻找主坐标。

