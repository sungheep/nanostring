library(edgeR)

################################### nanostring ########################################
a <- read.csv('/data/users/gb/Asan/survival/nanostring/Rawdata/nano_count.csv')
b <- read.csv('/data/users/gb/Asan/survival/nanostring/Nanostring_all.csv')

a <- a[-1]
a2 <- a[1:579]

group <- cbind(b$RELAPSE,b$pCR)
colnames(group) <- c('RELAPSE','pCR')

a2 <- data.frame(t(a2))
group <- b$RELAPSE


y <- DGEList(counts = a2 , group = group)
y$samples

keep <- rowSums(cpm(y)>1) >=2
keep

y <- y[keep, , keep.lib.sizes=T]

y <- calcNormFactors(y)

head(y$samples)
head(y$counts)


levels(y$samples$group)

y <- estimateDisp(y)
y <- estimateCommonDisp(y)
y <- estimateTagwiseDisp(y)

design <- model.matrix(~0+group,data=y$samples)

colnames(design) <- levels(y$samples$group)

design

fit <- glmQLFit(y,design)
qlf <- glmQLFTest(fit,contrast = c(-1,1))

summary(decideTestsDGE(qlf, p.value = 0.05))

topTags(qlf)

