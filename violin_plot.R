############# violine plot #####################

library(ggplot2)
library(devtools)
library(easyGgplot2)

data <- read.csv('/data/users/gb/Asan/survival/nanostring/Rawdata/nano_log_value.csv',header=T)
info <- read.csv('/data/users/gb/Asan/survival/nanostring/Nanostring_info_2.csv',header = T)

rownames(data) <- data$X
rownames(info) <- info$X

data <- data[-1]
info <- info[-1]

pcr.gene <- c('IL2RA','GNLY','SELE','S100A9','CCL5','CCL20','FCER1A','CD1A','C4A.B') #pCR
relapse.gene <- c('CCL5','CCL7','TNFSF13B','CSF2RB','CLEC4E','CCL8','SELE','EDNRB','IL17B','IL2RA','FCER1A','TGFBI','GZMB')#RELAPSE

data.pcr <- data[pcr.gene]
data.relapse <- data[relapse.gene]

data.info <- cbind(data.relapse,info$RELAPSE)
#colnames(data.info)[10] <- 'pCR'
colnames(data.info)[14] <- 'RELAPSE'

pdf(file = "/data/users/gb/Asan/survival/nanostring/DEG/DEG_violin_plot_RELAPSE.pdf")
for (i in (1:length(relapse.gene))){# DEG ÇÑ°Å 
  print(i)
  #plot(ggplot2.violinplot(data=mmi.ccr.pcr, xName='pCR', yName=colnames(m5.ccr[i]),
  #                        trim=T,groupName = 'pCR',addDot = T,addMean = T))
  plot(ggplot2.violinplot(data=data.info, xName='RELAPSE', yName=relapse.gene[i],
                          trim=T,groupName = 'RELAPSE',addDot = F,addMean = T))
  
}
dev.off()
