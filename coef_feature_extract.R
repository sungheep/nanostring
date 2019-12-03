c <- data.frame()

for (i in (1:length(lasso_coef))){
  #print(i)
  #c <- data.frame()
  if(lasso_coef[i] != 0){
    c[i,1] <- rownames(lasso_coef)[i]
    c[i,2] <- lasso_coef[i]
  }
  c <- na.omit(c)
}

nrow(c)
#write.csv(x = c,file = 'lasso_coef_Miller.csv')
#write.table(c,file = 'lasso_coef_Miller.txt',sep = '\t',quote = F,row.names = F)

c <- data.frame()

for (i in (1:length(elastic_coef))){
  #print(i)
  #c <- data.frame()
  if(elastic_coef[i] != 0){
    c[i,1] <- rownames(elastic_coef)[i]
    c[i,2] <- elastic_coef[i]
  }
  c <- na.omit(c)

}

nrow(c)
#write.csv(x = c,file = 'elastic_coef_RCB.csv')
#write.table(c,file = '/data/gb/Asan/survival/nanostring/Rawdata/tr-te-split/elastic_coef_pCR.txt',sep = '\t',quote = F,row.names = F)


