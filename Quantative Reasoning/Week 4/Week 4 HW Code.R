#Week 4 HW

data(PlantGrowth)
summary(PlantGrowth)

hist(PlantGrowth$weight[PlantGrowth$group=="ctrl"])
hist(PlantGrowth$weight[PlantGrowth$group=="trt1"])
hist(PlantGrowth$weight[PlantGrowth$group=="trt2"])

mean(PlantGrowth$weight[PlantGrowth$group=="ctrl"])
mean(PlantGrowth$weight[PlantGrowth$group=="trt1"])
mean(PlantGrowth$weight[PlantGrowth$group=="trt2"])

t.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"],PlantGrowth$weight[PlantGrowth$group=="trt1"])
t.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"],PlantGrowth$weight[PlantGrowth$group=="trt2"])

boxplot(PlantGrowth$weight ~ PlantGrowth$group)
