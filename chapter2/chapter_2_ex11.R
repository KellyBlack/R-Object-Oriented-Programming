a = as.factor(c("a","b","a","c","b","a"))
b = as.factor(c("z","x","z","y","z","z"))
twoTab <- table(a,b)
twoTab
rownames(twoTab) <- c("A response","B Response","C Response")
colnames(twoTab) <- c("said X","said Y","said Z")
twoTab
