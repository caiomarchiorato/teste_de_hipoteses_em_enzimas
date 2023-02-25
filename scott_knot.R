{library("ggplot2")
library("ExpDes")
library("ScottKnott")}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
cas <- read.csv("cas.csv", header = T, sep = ';')
ldes <- read.csv("ldes.csv", header = T, sep = ';')

folha_cas <- read.csv("FOLHA_CAS.csv", header = T, sep = ';')
folha_ldes <- read.csv("FOLHA_LDES.csv", header = T, sep = ';')

raiz_cas <- read.csv("RAIZ_CAS.csv", header = T, sep = ';')
raiz_ldes <- read.csv("RAIZ_LDES.csv", header = T, sep = ';')

folha_arg <- read.csv("",header= T, sep= ";")
raiz_arg <- 

##################################
#teste de shapiro-wilk
#testes com p<0,05 s?o n?o param?tricos
shapiro_cas <- shapiro.test(cas$B)
shapiro_ldes <- shapiro.test(ldes$B)
shapiro_folha_cas <-shapiro.test(folha_cas$B)
shapiro_folha_ldes <-shapiro.test(folha_ldes$B)
shapiro_raiz_cas <-shapiro.test(raiz_cas$B)
shapiro_raiz_ldes <-shapiro.test(raiz_ldes$B)

dados <- c('cas','ldes','folha_cas','folha_ldes','raiz_cas','raiz_ldes')
pvalor <- c(shapiro_cas$p.value,
            shapiro_ldes$p.value,
            shapiro_folha_cas$p.value,
            shapiro_folha_ldes$p.value,
            shapiro_raiz_cas$p.value,
            shapiro_raiz_ldes$p.value)
normalidade <- cbind(dados, pvalor)


write.table(normalidade,'shapiro.csv',sep=',')
######################################
krusk_cas <- kruskal.test(B ~ A, data = cas)
krusk_ldes <- kruskal.test(B ~ A, data = ldes)
krusk_folha_cas <- kruskal.test(B ~ A, data = folha_cas)
krusk_folha_ldes <- kruskal.test(B ~ A, data = folha_ldes)
krusk_raiz_cas <- kruskal.test(B ~ A, data = raiz_cas)
krusk_raiz_ldes <- kruskal.test(B ~ A, data = raiz_ldes)

pvalor_k <- c(krusk_cas$p.value,
            krusk_ldes$p.value,
            krusk_folha_cas$p.value,
            krusk_folha_ldes$p.value,
            krusk_raiz_cas$p.value,
            krusk_raiz_ldes$p.value)
teste_krusk <- cbind(dados, pvalor_k)

write.table(teste_krusk,'krusk.csv',sep=',')
######################################
#construindo os testes de scottknott
teste_cas_folha <- SK(B ~ A, data= folha_cas, sig.level = 0.05)
teste_ldes_folha <- SK(B ~ A, data= folha_ldes, sig.level = 0.05)

teste_cas_raiz <- SK(B ~ A, data= raiz_cas, sig.level = 0.05)
teste_ldes_raiz <- SK(B ~ A, data= raiz_ldes, sig.level = 0.05)

teste_cas <- SK(B ~ A, data= cas, sig.level = 0.05 )
teste_ldes <- SK(B ~ A, data= ldes, sig.level = 0.05)

summary(teste_cas)
summary(teste_ldes)

write.csv(summary(teste_cas), 'teste_cas.csv')
write.csv(summary(teste_ldes), 'teste_ldes.csv')

write.csv(summary(teste_cas_raiz), 'teste_cas_raiz.csv')
write.csv(summary(teste_ldes_raiz), 'teste_ldes_raiz.csv')

write.csv(summary(teste_cas_folha), 'teste_cas_folha.csv')
write.csv(summary(teste_ldes_folha), 'teste_ldes_folha.csv')


#calculando as m?dias
mean_cas <- aggregate(B ~ A, data= cas, FUN = mean)
mean_ldes <- aggregate(B ~ A, data=ldes, FUN = mean)

mean_folhas_cas <- aggregate(B ~ A, data= folha_cas, FUN = mean)
mean_folhas_ldes <- aggregate(B ~ A, data= folha_ldes, FUN = mean)

mean_raiz_cas <- aggregate(B ~ A, data= raiz_cas, FUN = mean)
mean_raiz_ldes <- aggregate(B ~ A, data= raiz_ldes, FUN = mean)

###################################################
#teste de wilcoox ##################################
cas <- read.csv("wilx/cas.csv", header = T, sep = ';')
ldes <- read.csv("wilx/ldes.csv", header = T, sep = ';')

folha_cas <- read.csv("wilx/FOLHA_CAS.csv", header = T, sep = ';')
folha_ldes <- read.csv("wilx/FOLHA_LDES.csv", header = T, sep = ';')

raiz_cas <- read.csv("wilx/RAIZ_CAS.csv", header = T, sep = ';')
raiz_ldes <- read.csv("wilx/RAIZ_LDES.csv", header = T, sep = ';')

nomes <- read.csv('wilx/nomes.csv', header=  F, sep= ';')
aux <- c( rep(1,length(cas$MT_R_0_30)),
          rep(2,length(cas$MT_R_100_30)),
          rep(3,length(cas$MT_R_0_1h)),
          rep(4,length(cas$MT_R_100_1h)),
          rep(5,length(cas$MT_R_0_2h)),
          rep(6,length(cas$MT_R_100_2h)),
          rep(7,length(cas$MT_R_0_3h)),
          rep(8,length(cas$MT_R_100_3h)),
          rep(9,length(cas$yg2_R_0_30)),
          rep(10,length(cas$yg2_R_100_30)),
          rep(11,length(cas$yg2_R_0_1h)),
          rep(12,length(cas$yg2_R_100_1h)),
          rep(13,length(cas$yg2_R_0_2h)),
          rep(14,length(cas$yg2_R_100_2h)),
          rep(15,length(cas$yg2_R_0_3h)),
          rep(16,length(cas$yg2_R_100_3h)))


aux2 <- c( rep(1,length(cas$MT_R_0_30)),
          rep(2,length(cas$MT_R_100_30)),
          rep(3,length(cas$MT_R_0_1h)),
          rep(4,length(cas$MT_R_100_1h)),
          rep(5,length(cas$MT_R_0_2h)),
          rep(6,length(cas$MT_R_100_2h)),
          rep(7,length(cas$MT_R_0_3h)),
          rep(8,length(cas$MT_R_100_3h)),
          rep(9,length(cas$yg2_R_0_30)),
          rep(10,length(cas$yg2_R_100_30)),
          rep(11,length(cas$yg2_R_0_1h)),
          rep(12,length(cas$yg2_R_100_1h)),
          rep(13,length(cas$yg2_R_0_2h)),
          rep(14,length(cas$yg2_R_100_2h)),
          rep(15,length(cas$yg2_R_0_3h)),
          rep(16,length(cas$yg2_R_100_3h)),
          rep(17,length(cas$MT_F_0_30)),
          rep(18,length(cas$MT_F_100_30)),
          rep(19,length(cas$MT_F_0_1h)),
          rep(20,length(cas$MT_F_100_1h)),
          rep(21,length(cas$MT_F_0_2h)),
          rep(22,length(cas$MT_F_100_2h)),
          rep(23,length(cas$MT_F_0_3h)),
          rep(24,length(cas$MT_F_100_3h)),
          rep(25,length(cas$yg2_F_0_30)),
          rep(26,length(cas$yg2_F_0_30)),
          rep(27,length(cas$yg2_F_0_30)),
          rep(28,length(cas$yg2_F_0_30)),
          rep(29,length(cas$yg2_F_0_30)),
          rep(30,length(cas$yg2_F_0_30)),
          rep(31,length(cas$yg2_F_0_30)),
          rep(32,length(cas$yg2_F_0_30)))

#y <- c(cas$B)

#metodos <- factor(aux2, labels=c(nomes))

#wil <- pairwise.wilcox.test(x= y,g= metodos, paired= F, p.adjust.method = 'none', conf.level= 0.05)

#write.table('wil.csv', wil$p.value, sep=',')
#print(wil$p.value)



folha_cas_graf <- read.csv("teste_cas_folha.csv", header = T, sep = ';')
folha_ldes_graf <- read.csv("teste_ldes_folha.csv", header = T, sep = ';')

raiz_cas_graf <- read.csv("teste_cas_raiz.csv", header = T, sep = ';')
raiz_ldes_graf <- read.csv("teste_ldes_raiz.csv", header = T, sep = ';')

cas_graf <- read.csv("teste_cas.csv", header = T, sep = ';')
ldes_graf <- read.csv("teste_ldes.csv", header = T, sep = ';')

#criando gr?ficos
#folhas ####################################
ggplot(folha_cas_graf, (aes(x= A,
                   y= B,
                   fill = A))) +
  geom_col(position="dodge", show.legend = TRUE) +
  scale_fill_discrete(name="Tratamento") +
  labs(title= 'Atividades - CAS - Folhas',
       x = 'Tratamento com NaCl',
       y = 'Atividade da enzima CAS \n (pmol min-1 mg prot-1)') +
  scale_x_discrete(breaks = NULL) +
  labs() +
  geom_text(aes(label = G1), vjust= -0.3)

ggplot(folha_ldes_graf, (aes(x= A,
                             y= B,
                             fill = A))) +
  geom_col(position="dodge", show.legend = TRUE) +
  scale_fill_discrete(name="Tratamento") +
  labs(title= 'Atividades - L/D-DES - Folhas',
       x = 'Tratamento com NaCl',
       y = 'Atividade da enzima CAS \n (pmol min-1 mg prot-1)') +
  scale_x_discrete(breaks = NULL) +
  labs() +
  geom_text(aes(label = G1), vjust= -0.3)
#############################################
#raizes ####################################
ggplot(raiz_cas_graf, (aes(x= A,
                             y= B,
                             fill = A))) +
  geom_col(position="dodge", show.legend = TRUE) +
  scale_fill_discrete(name="Tratamento") +
  labs(title= 'Atividades - CAS - Raiz',
       x = 'Tratamento com NaCl',
       y = 'Atividade da enzima CAS \n (pmol min-1 mg prot-1)') +
  scale_x_discrete(breaks = NULL) +
  labs() +
  geom_text(aes(label = G1), vjust= -0.3)

ggplot(raiz_ldes_graf, (aes(x= A,
                             y= B,
                             fill = A))) +
  geom_col(position="dodge", show.legend = TRUE) +
  scale_fill_discrete(name="Tratamento") +
  labs(title= 'Atividades - L/D-DES - Raiz',
       x = 'Tratamento com NaCl',
       y = 'Atividade da enzima L/D-DES \n (pmol min-1 mg prot-1)') +
  scale_x_discrete(breaks = NULL) +
  labs() +
  geom_text(aes(label = G1), vjust= -0.3)
####################################
#enzimas
ggplot(cas_graf, (aes(x= A,
                            y= B,
                            fill = A))) +
  geom_col(position="dodge", show.legend = TRUE) +
  scale_fill_discrete(name="Tratamento") +
  labs(title= 'Atividades - Cas',
       x = 'Tratamento com NaCl',
       y = 'Atividade da enzima CAS \n (pmol min-1 mg prot-1)') +
  scale_x_discrete(breaks = NULL) +
  labs() +
  geom_text(aes(label = G1), vjust= -0.3)


ggplot(ldes_graf, (aes(x= A,
                            y= B,
                            fill = A))) +
  geom_col(position="dodge", show.legend = TRUE) +
  scale_fill_discrete(name="Tratamento") +
  labs(title= 'Atividades - L/D-DES',
       x = 'Tratamento com NaCl',
       y = 'Atividade da enzima L/D-DES \n (pmol min-1 mg prot-1)') +
  scale_x_discrete(breaks = NULL) +
  labs() +
  geom_text(aes(label = G1), vjust= -0.3)


pairwise.wilcox.test(B ~ A, data= cas, paired= TRUE)
