


met1 <- c(12,16, 16)
met2 <- c(14,15, 18)
met3 <- c(9,10, 11)




dados<- as.data.frame(cbind(met1,met2,met3,met4))

y <- c(dados$met1, dados$met2, dados$met3, dados$met4)

aux <- c( rep(1,length(dados$met1)), rep(2,length(dados$met2)), rep(3,length(dados$met3)), rep(4,length(dados$met4)) )

metodos <- factor(aux, labels=c("Met 1", "Met 2", "Met 3","Met 4"))
pairwise.wilcox.test(y,metodos, paired=F, p.adjust.method = 'none')



met1 <- c(cas$MT_R_0_30)
met2 <- c(cas$MT_R_100_30)
met3 <- c(cas$MT_R_0_1h)
met4 <- c(cas$MT_R_100_1h)

