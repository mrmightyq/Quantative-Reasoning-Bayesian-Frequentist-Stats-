toast <- matrix(c(33,47,17,3),ncol=2,byrow=TRUE)
colnames(toast)<- c("Pass","Fail")
rownames(toast)<- c("Default","No Default")
toast <- as.table(toast)
toast
toast/margin.table(toast)
