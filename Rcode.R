library("plot3D")
library(readxl)
library(dplyr)
library(rio)
library(ggplot2)
library(ggpattern)
library(tidyverse)
library(lubridate)
require(nnet)
library(ggpubr)
library(sampleSelection)

dataObs=data.Ready %>% filter(GPA>0)
mean.gpaP1=lm(GPA~matem,
              data=data.Ready %>% filter(Zsum==1))

LogitZ=multinom(as.factor(Zsum)~matem,
                data=data.Ready) #P(Z=1|X)
fittedZ=LogitZ$fitted.values
pje.mate=data.frame(matem=data.Ready%>% filter(Zsum==1) %>% 
                      select(matem))

predGPA=predict(mean.gpaP1,newdata = pje.mate) #E(Y|X,Z=1)
data.fullP1=data.frame(pje.mate=pje.mate[,1],
                       GPA=data.Ready%>% filter(Zsum==1) %>% 
                         select(GPA),
                       pred.GPA=predGPA,
                       prob.Z=fittedZ[1:125,]) %>% 
  arrange(pje.mate) %>% 
  mutate(LB_p11=pred.GPA*prob.Z+(1-prob.Z),
         UB_p11=pred.GPA*prob.Z+7*(1-prob.Z))


## WIDER BOUNDS
# Fig 3.1a
library(grid)
ggplot()+
  geom_point(data=data.fullP1,
             aes(x=pje.mate,
                 y=GPA,
                 shape="Observed GPAs")
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.fullP1,
            aes(x=pje.mate,
                y=pred.GPA,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.fullP1,
            aes(x=pje.mate,
                y=LB_p11,
                color="Identificaction bounds under WIA",
                linetype = "Identificaction bounds under WIA"
            ),
            size=1.1)+
  geom_line(data=data.fullP1,
            aes(x=pje.mate,
                y=UB_p11,
                color="Identificaction bounds under WIA",
                linetype = "Identificaction bounds under WIA"),
            size=1.1)+
  geom_ribbon(data=data.fullP1,
              aes(x=pje.mate,
                  ymin = LB_p11, ymax = UB_p11), 
              fill = "#2ca25f", alpha = 0.1)+
  scale_shape(name = "",
              breaks="Observed GPAs")+
  scale_color_manual(name='',
                     breaks=c('Identificaction bounds under WIA', 
                              'Predicted GPAs in selected applicants', 
                              'Lower bound for predicted GPAs'
                     ),
                     values=c('Identificaction bounds under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594', 
                              'Identificaction bounds under WIA'='#2ca25f')
  )+
  scale_linetype_manual(name='',
                        breaks=c('Identificaction bounds under WIA', 
                                 'Predicted GPAs in selected applicants', 
                                 'Identificaction bounds under WIA'
                        ),
                        values=c('Identificaction bounds under WIA'='solid', 
                                 'Predicted GPAs in selected applicants'='solid', 
                                 'Identificaction bounds under WIA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.75, 0.2),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))


# width of the bounds univariate Math score
o=550
predict(mean.gpaP1,newdata=list(matem=o))*
  predict(LogitZ,newdata=list(matem=o),"probs")+
  7*(1-predict(LogitZ,newdata=list(matem=o),"probs"))-
  (predict(mean.gpaP1,newdata=list(matem=o))*
     predict(LogitZ,newdata=list(matem=o),"probs")+
     1*(1-predict(LogitZ,newdata=list(matem=o),"probs")))


#### with X1 and X2
fit=lm(GPA~matem+`leng y Com`,
       data=dataObs)
prob=glm(Z ~ matem + `leng y Com`,
         data = data.Ready,
         family = "binomial")
grid.lines = 20

matem <- seq(min(dataObs$matem), max(dataObs$matem), length.out = grid.lines)
`leng y Com` <- seq(min(dataObs$`leng y Com`), max(dataObs$`leng y Com`), length.out = grid.lines)
xy <- expand.grid( x = matem, y = `leng y Com`)

z.pred <- matrix(predict(fit, newdata = list(matem = xy$x, `leng y Com`=xy$y)), 
                 nrow = grid.lines, ncol = grid.lines)

pred.probabilities=data.frame(matem = xy$x, `leng y Com`=xy$y)
colnames(pred.probabilities)=c("matem", "leng y Com")
probs.pred= matrix(predict(prob, newdata = pred.probabilities, type="response"), 
                   nrow = grid.lines, ncol = grid.lines)

LB=z.pred*probs.pred+1*(1-probs.pred)
UB=z.pred*probs.pred+7*(1-probs.pred)

## With of the bounds for table 3.1
m=550
l=520
data.predict=data.frame(a=m,b=l)
colnames(data.predict)=c("matem","leng y Com")
predict(fit,newdata=list(matem=m,`leng y Com`=l))*predict(prob, newdata = data.predict, type="response")+
  7*(1-predict(prob, newdata = data.predict, type="response"))-
  (predict(fit,newdata=list(matem=m,`leng y Com`=l))*predict(prob, newdata = data.predict, type="response")+
     1*(1-predict(prob, newdata =data.predict, type="response")))


## Fig 3.1b
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$`leng y Com`, dataObs$GPA,
          pch = 19, cex = 0.5, colvar = NULL, col = "black",
          theta = 20, phi = 20, bty = "b", zlim = c(1, 7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = `leng y Com`, z = z.pred,  
                      facets = TRUE,
                      col = ramp.col(col = c("#084594", "#084594"), alpha = 0.7)), 
          main = "Predicted GPA"
)

par(mar = c(5, 4, 4, 2) + 0.1)
persp3D(x = matem, y = `leng y Com`, z = LB,  
        colvar = NULL, add = TRUE, col = "#2ca25f", alpha = 0.5)

persp3D(x = matem, y = `leng y Com`, z = UB,  
        add = TRUE, colvar = NULL, col = "#2ca25f", alpha = 0.5)

legend("bottomleft", 
       legend=c("Identification bounds under WIA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")

### A MORE INFORMATIVE ASSUMPTIONS
### completo
mean.gpa=lm(PGA_1ERsemestre~matem,
            data=data.biol.mat)

## Multinomial regression
multinom.carreras=multinom(as.factor(nom_carrera)~matem,
                           data=data.biol.mat)
fitted.carreras=multinom.carreras$fitted.values

biologia=data.biol.mat %>% filter(nom_carrera=="BIOLOGÍA")
biologia.M=data.biol.mat %>% filter(nom_carrera=="BIOLOGÍA MARINA")
bioquimica=data.biol.mat %>% filter(nom_carrera=="BIOQUÍMICA")

## linear regression by program
lm.biologia.M=lm(PGA_1ERsemestre~matem,
                 data=biologia.M) ## #E(Y|X,G=1)
lm.biologia=lm(PGA_1ERsemestre~matem,
               data=biologia) ## #E(Y|X,G=2)
lm.bioquimica=lm(PGA_1ERsemestre~matem,
                 data=bioquimica) ## #E(Y|X,G=3)

pje.mate=data.frame(matem=data.biol.mat$matem)

pred.biologia.M=predict(lm.biologia.M,newdata = pje.mate) #E(Y|X,G=1)
pred.biologia=predict(lm.biologia,newdata = pje.mate) #E(Y|X,G=2)
pred.bioquimica=predict(lm.bioquimica,newdata = pje.mate) #E(Y|X,G=3)


data.full=data.frame(pje.mate=pje.mate[,1],
                     pred.biol=pred.biologia,
                     pred.biol.M=pred.biologia.M,
                     pred.bioq=pred.bioquimica,
                     prob.biol=fitted.carreras[,1],
                     prob.biol.M=fitted.carreras[,2],
                     prob.bioq=fitted.carreras[,3],
                     ltp=pred.biologia*fitted.carreras[,1]+
                       pred.biologia.M*fitted.carreras[,2]+
                       pred.bioquimica*fitted.carreras[,3]) %>% 
  arrange(pje.mate)

data.full.obs=data.full

### probabilidties

## P(Z=1|X)
pz1=multinom(as.factor(Zsum)~matem,
             data=data.Ready)

## P(G=g|X,Z=0)
data.Z0=data.Ready %>% filter(Zsum==0)
pS=multinom(as.factor(S)~matem,
            data=data.Z0)

ptje_mate=data.frame(matem=seq(min(data.full$pje.mate),
                               max(data.full$pje.mate),
                               length=1000))

min_max.gpa=data.biol.mat %>% group_by(nom_carrera) %>% 
  summarise(min.gpa=min(PGA_1ERsemestre),
            max.gpa=max(PGA_1ERsemestre))

data.lm=data.frame(x=ptje_mate,
                   y=predict(mean.gpa,newdata=ptje_mate))

data.bounds=data.frame(
  Mate=ptje_mate,
  EG1=predict(lm.biologia.M,newdata=ptje_mate),
  EG2=predict(lm.biologia,newdata=ptje_mate),
  EG3=predict(lm.bioquimica,newdata=ptje_mate),
  pred.lm=data.lm$y,
  pG=predict(multinom.carreras,newdata=ptje_mate,type="prob"),
  pz1=predict(pz1,newdata=ptje_mate,probs,type="prob"),
  pS=predict(pS,newdata=ptje_mate,probs,type="prob"))  %>% 
  mutate(B1=(EG1*pG.BIOLOGÍA.MARINA+
               EG2*pG.BIOLOGÍA+
               EG3*pG.BIOQUÍMICA)*pz1+(1-pz1),
         B2=(EG1*pG.BIOLOGÍA.MARINA+
               EG2*pG.BIOLOGÍA+
               EG3*pG.BIOQUÍMICA)*pz1+
           (min_max.gpa$min.gpa[2]*(pS.1+pS.4+0+pS.7)+
              min_max.gpa$min.gpa[1]*(pS.2+pS.6)+
              min_max.gpa$min.gpa[3]*pS.3)*(1-pz1),
         B3=(EG1*pG.BIOLOGÍA.MARINA+
                   EG2*pG.BIOLOGÍA+
                   EG3*pG.BIOQUÍMICA)*pz1+
           (EG1*(pS.1+pS.4+0+pS.7)+
              EG2*(pS.2+pS.6)+
              EG3*pS.3)*(1-pz1),
         B4=(EG1*pG.BIOLOGÍA.MARINA+
                EG2*pG.BIOLOGÍA+
                EG3*pG.BIOQUÍMICA)*pz1+7*(1-pz1)
  )

### LB UB UBAPSA UBw
### B1 B2 B3 B4

#a=data.frame(x=data.bounds$matem,unc1=(data.bounds$UB1-data.bounds$LB1))
#b=data.frame(x=data.bounds$matem,unc1=(data.bounds$UB3-data.bounds$LB1))
#c=data.frame(x=data.bounds$matem,unc1=(data.bounds$UB3-data.bounds$LB2))
#install.packages("sampleSelection")


pcH=data.biol.mat$nom_carrera
pcH[which(pcH=="BIOLOGÍA")]="Biology"
pcH[which(pcH=="BIOLOGÍA MARINA")]="Marine Biology"
pcH[which(pcH=="BIOQUÍMICA")]="Biochemistry"

data.biol.mat=data.biol.mat %>% 
  mutate(Program=pcH)

## PSA
data.pinta=data.frame(x=data.bounds$matem,lower=data.bounds$B1,upper=data.bounds$B2)

ggplot()+
  geom_point(data=data.biol.mat,
             aes(x=matem,
                 y=PGA_1ERsemestre,shape=Program)
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.full.obs,
            aes(x=pje.mate,
                y=ltp,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B1,
                color="Identification bounds under PSA",
                linetype = "Identification bounds under PSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B4,
                color="Upper bound under WIA",
                linetype = "Upper bound under WIA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B2,
                color="Identification bounds under PSA",
                linetype = "Identification bounds under PSA"),
            size=1.1)+  
  geom_ribbon(data=data.bounds,
              aes(x=matem,ymin = B1, ymax = B2), fill = "#d95f02", alpha = 0.1)+
  scale_color_manual(name='',
                     breaks=c('Upper bound under WIA', 
                              'Identification bounds under PSA',
                              'Predicted GPAs in selected applicants', 
                              'Identification bounds under PSA'),
                     values=c('Identification bounds under PSA'='#d95f02',
                              'Upper bound under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594', 
                              'Identification bounds under PSA'='#d95f02'))+
  scale_linetype_manual(name='',
                        breaks=c('Upper bound under WIA', 
                                 'Identification bounds under PSA',
                                 'Predicted GPAs in selected applicants', 
                                 'Identification bounds under PSA'),
                        values=c('Upper bound under WIA'='solid',
                                 'Identification bounds under PSA'='solid',
                                 'Predicted GPAs in selected applicants'='solid', 
                                 'Identification bounds under PSA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.78, 0.25),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))

ggsave("PSA.pdf",width=20,height = 15,units="cm")

##### FSA
ggplot()+
  geom_point(data=data.biol.mat,
             aes(x=matem,
                 y=PGA_1ERsemestre,shape=Program)
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.full.obs,
            aes(x=pje.mate,
                y=ltp,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B1,
                color="Identification bounds under WIA",
                linetype = "Identification bounds under WIA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B4,
                color="Identification bounds under WIA",
                linetype = "Identification bounds under WIA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B3,
                color="Identification bounds under FSA",
                linetype = "Identification bounds under FSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B2,
                color="Identification bounds under FSA",
                linetype = "Identification bounds under FSA"),
            size=1.1)+
  geom_ribbon(data=data.bounds,
              aes(x=matem,ymin = B2, ymax = B3), fill = "#d95f02", alpha = 0.1)+
  scale_color_manual(name='',
                     breaks=c('Identification bounds under WIA', 
                              'Identification bounds under FSA',
                              'Predicted GPAs in selected applicants', 
                              "Identification bounds under FSA",
                              'Identification bounds under WIA'),
                     values=c('Identification bounds under FSA'='#d95f02',
                              'Identification bounds under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594',
                              "Identification bounds under FSA"="#d95f02",
                              'Identification bounds under WIA'='#2ca25f'))+
  scale_linetype_manual(name='',
                        breaks=c('Identification bounds under WIA', 
                                 'Identification bounds under FSA',
                                 'Predicted GPAs in selected applicants', 
                                 "Identification bounds under FSA",
                                 'Identification bounds under WIA'),
                        values=c('Identification bounds under WIA'='solid',
                                 'Identification bounds under FSA'='solid',
                                 'Predicted GPAs in selected applicants'='solid',
                                 "Identification bounds under FSA"="solid",
                                 'Identification bounds under WIA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.78, 0.25),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))


########### WSA
ggplot()+
  geom_point(data=data.biol.mat,
             aes(x=matem,
                 y=PGA_1ERsemestre,shape=Program)
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.full.obs,
            aes(x=pje.mate,
                y=ltp,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B3,
                color="Identification bounds under WSA",
                linetype = "Identification bounds under WSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B4,
                color="Identification bounds under WSA",
                linetype = "Identification bounds under WSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B1,
                color="Lower bound under WIA",
                linetype = "Lower bound under WIA"),
            size=1.1)+ 
  geom_ribbon(data=data.bounds,
              aes(x=matem,ymin = B3, ymax = B4), fill = "#d95f02", alpha = 0.1)+
  scale_color_manual(name='',
                     breaks=c('Lower bound under WIA', 
                              'Identification bounds under WSA',
                              'Predicted GPAs in selected applicants', 
                              'Identification bounds under WSA'),
                     values=c('Identification bounds under WSA'='#d95f02',
                              'Lower bound under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594', 
                              'Identification bounds under WSA'='#d95f02'))+
  scale_linetype_manual(name='',
                        breaks=c('Lower bound under WIA', 
                                 'Identification bounds under WSA',
                                 'Predicted GPAs in selected applicants', 
                                 'Identification bounds under WSA'),
                        values=c('Lower bound under WIA'='solid',
                                 'Identification bounds under WSA'='solid',
                                 'Predicted GPAs in selected applicants'='solid', 
                                 'Identification bounds under WSA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.78, 0.25),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))


########### With X1 and X2
data.Ready=data.Ready %>% mutate(estado=case_when(
  `BIOLOGÍA MARINA`>0 & `BIOLOGÍA`==0 & `BIOQUÍMICA`==0 ~ "BIOLOGÍA MARINA",
  `BIOLOGÍA`>0 & `BIOLOGÍA MARINA`==0 & `BIOQUÍMICA`==0 ~ "BIOLOGÍA",
  `BIOQUÍMICA`>0 & `BIOLOGÍA MARINA`==0 & `BIOLOGÍA`==0 ~ "BIOQUÍMICA",
  TRUE~"No seleccionado"
))


### probabilidad de estar en una carrera
multinom.carreras=multinom(as.factor(nom_carrera)~matem,
                           data=data.biol.mat) #P(G=g|X,Z=1)
pz1=multinom(as.factor(Zsum)~matem+`leng y Com`,
             data=data.Ready)

### lm por carrera
biologia=data.Ready %>% filter(estado=="BIOLOGÍA")
biologia.M=data.Ready %>% filter(estado=="BIOLOGÍA MARINA")
bioquimica=data.Ready %>% filter(estado=="BIOQUÍMICA")


lm.biologia.M=lm(`BIOLOGÍA MARINA`~matem+`leng y Com`,
                 data=biologia.M) 
lm.biologia=lm(`BIOLOGÍA`~matem+`leng y Com`,
               data=biologia)
lm.bioquimica=lm(`BIOQUÍMICA`~matem+`leng y Com`,
                 data=bioquimica) 

matem <- seq(min(dataObs$matem), max(dataObs$matem), length.out = grid.lines)
`leng y Com` <- seq(min(dataObs$`leng y Com`), max(dataObs$`leng y Com`), length.out = grid.lines)
xy <- expand.grid( x = matem, y = `leng y Com`)
xy=as.data.frame(xy)
colnames(xy)=c("matem","leng y Com")

pred.biologia.M=predict(lm.biologia.M, newdata = xy) 
pred.biologia=predict(lm.biologia, newdata = xy) 
pred.bioquimica=predict(lm.bioquimica, newdata = xy)


fitted.carreras=predict(multinom.carreras, newdata = xy, type="probs")

data.Z0=data.Ready %>% filter(Z==0)
pS=multinom(as.factor(S)~matem+`leng y Com`,
            data=data.Z0)
pS.pred=predict(pS,newdata=xy,probs,type="probs")

##############################
m=820
l=800
data.predict=data.frame(a=m,b=l)
colnames(data.predict)=c("matem","leng y Com")
pS.pred.1=predict(pS,newdata=data.predict,probs,type="probs")
pred.biologia.M.1=predict(lm.biologia.M, newdata = list(matem=m,`leng y Com`=l))
pred.biologia.1=predict(lm.biologia, newdata = list(matem=m,`leng y Com`=l)) #E(Y|X,G=2)
pred.bioquimica.1=predict(lm.bioquimica, newdata = list(matem=m,`leng y Com`=l)) #E(Y|X,G=3)
fitted.carreras.1=predict(multinom.carreras, newdata = data.predict, type="probs")
pred.pz1=predict(pz1, newdata = data.predict, type="probs")

widthBound1=predict(fit,newdata=list(matem=m,`leng y Com`=l))*predict(prob, newdata = data.predict, type="response")+
  7*(1-predict(prob, newdata = data.predict, type="response"))-
  (predict(fit,newdata=list(matem=m,`leng y Com`=l))*predict(prob, newdata = data.predict, type="response")+
     1*(1-predict(prob, newdata =data.predict, type="response"))) 

lb1=(1-pred.pz1)+
  (pred.biologia.M.1*fitted.carreras.1["BIOLOGÍA MARINA"]+
     pred.biologia.1*fitted.carreras.1["BIOLOGÍA"]+
     pred.bioquimica.1*fitted.carreras.1["BIOQUÍMICA"])*pred.pz1

lb_min=LBminNM=(as.numeric(min_max.gpa$min.gpa[2])*(pS.pred.1[1]+pS.pred.1[4]+pS.pred.1[6])+
                  as.numeric(min_max.gpa$min.gpa[1])*(pS.pred.1[2]+pS.pred.1[5])+
                  as.numeric(min_max.gpa$min.gpa[3])*pS.pred.1[3])*(1-pred.pz1)+
  (pred.biologia.M.1*fitted.carreras.1["BIOLOGÍA MARINA"]+
     pred.biologia.1*fitted.carreras.1["BIOLOGÍA"]+
     pred.bioquimica.1*fitted.carreras.1["BIOQUÍMICA"])*pred.pz1

ub_mon=(pred.biologia.M.1*(fitted.carreras.1["BIOLOGÍA MARINA"]*pred.pz1+
                             (pS.pred.1[1]+pS.pred.1[4]+pS.pred.1[6])*(1-pred.pz1))+
          pred.biologia.1*(fitted.carreras.1["BIOLOGÍA"]*pred.pz1+
                             (pS.pred.1[2]+pS.pred.1[5])*(1-pred.pz1))+
          pred.bioquimica.1*(fitted.carreras.1["BIOQUÍMICA"]*pred.pz1+
                               pS.pred.1[3]*(1-pred.pz1)))

ub_mon2=(pred.biologia.M.1*fitted.carreras.1["BIOLOGÍA MARINA"]+
           pred.biologia.1*fitted.carreras.1["BIOLOGÍA"]+
           pred.bioquimica.1*fitted.carreras.1["BIOQUÍMICA"])*pred.pz1+
  7*(1-pred.pz1)

data.frame(m,l,lb1,lb_min,ub_mon,ub_mon2) %>% 
  mutate(wider=ub_mon2-lb1,
         mon_Y0=lb_min-lb1,
         MON2=ub_mon-lb_min)

#############################  
pred.pz=predict(pz1, newdata = xy, type="probs")  

LBY0=(1-pred.pz)+
  (pred.biologia.M*fitted.carreras[,"BIOLOGÍA MARINA"]+
     pred.biologia*fitted.carreras[,"BIOLOGÍA"]+
     pred.bioquimica*fitted.carreras[,"BIOQUÍMICA"])*pred.pz


UBmonNM=(pred.biologia.M*fitted.carreras[,"BIOLOGÍA MARINA"]+
           pred.biologia*fitted.carreras[,"BIOLOGÍA"]+
           pred.bioquimica*fitted.carreras[,"BIOQUÍMICA"])*pred.pz+
  (pred.biologia.M*(pS.pred[,1]+pS.pred[,4]+pS.pred[,6])+
     pred.biologia*(pS.pred[,2]+pS.pred[,5])+
     pred.bioquimica*pS.pred[,3])*(1-pred.pz)


UBmonNM2=(pred.biologia.M*fitted.carreras[,"BIOLOGÍA MARINA"]+
            pred.biologia*fitted.carreras[,"BIOLOGÍA"]+
            pred.bioquimica*fitted.carreras[,"BIOQUÍMICA"])*pred.pz+
  7*(1-pred.pz)



LBmon=matrix(LBY0, 
             nrow = grid.lines, ncol = grid.lines) 
UBmon=matrix(UBmonNM, 
             nrow = grid.lines, ncol = grid.lines) 

UBmon2=matrix(UBmonNM2, 
              nrow = grid.lines, ncol = grid.lines) 

min_max.gpa=data.biol.mat %>% group_by(nom_carrera) %>% 
  summarise(min.gpa=min(PGA_1ERsemestre),
            max.gpa=max(PGA_1ERsemestre))

#################################
lm.biologia.M1=lm(GPA~matem+`leng y Com`,
                  data=biologia.M) ## #E(Y|X,G=1)
lm.biologia1=lm(GPA~matem+`leng y Com`,
                data=biologia) ## #E(Y|X,G=2)
lm.bioquimica1=lm(GPA~matem+`leng y Com`,
                  data=bioquimica) ## #E(Y|X,G=3)


pred.biologia.M=predict(lm.biologia.M,newdata =xy) #E(Y|X,G=1)
pred.biologia=predict(lm.biologia,newdata = xy) #E(Y|X,G=2)
pred.bioquimica=predict(lm.bioquimica,newdata = xy) #E(Y|X,G=3)

ignorabilidad=pred.biologia*fitted.carreras[,1]+
  pred.biologia.M*fitted.carreras[,2]+
  pred.bioquimica*fitted.carreras[,3]

igno.mat=matrix(ignorabilidad, 
                nrow = grid.lines, ncol = grid.lines)


LBminNM=(as.numeric(min_max.gpa$min.gpa[2])*(pS.pred[,1]+pS.pred[,4]+pS.pred[,6])+
           as.numeric(min_max.gpa$min.gpa[1])*(pS.pred[,2]+pS.pred[,5])+
           as.numeric(min_max.gpa$min.gpa[3])*pS.pred[,3])*(1-pred.pz)+
  pred.biologia.M*fitted.carreras[,"BIOLOGÍA MARINA"]*pred.pz+
  pred.biologia*fitted.carreras[,"BIOLOGÍA"]*pred.pz+
  pred.bioquimica*fitted.carreras[,"BIOQUÍMICA"]*pred.pz

LBmin=matrix(LBminNM, 
             nrow = grid.lines, ncol = grid.lines)

####### PSA
library(rgl)
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$`leng y Com`, dataObs$GPA,
          pch = 19, cex = 0.5,colvar = NULL, col="black",
          theta =20, phi = 20, bty="b", zlim=c(1,7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = `leng y Com`, z = igno.mat,  
                      facets = TRUE,
                      col=ramp.col(col = c("#084594","#084594"), alpha=0.7)), main = "Predicted GPA")

persp3D(x = matem, y = `leng y Com`, z = UBmon2, add = TRUE,
        colvar = NULL, alpha = 0.6,col="#2ca25f")

persp3D(x = matem, y = `leng y Com`, z = LBmin,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = `leng y Com`, z = LBmon,add=TRUE,
        colvar = NULL,col="#d95f02",alpha=0.4)

legend("bottomleft", 
       legend=c("Upper bound under WIA",
                "Identification bounds under PSA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#d95f02","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")


####### FSA
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$`leng y Com`, dataObs$GPA,
          pch = 19, cex = 0.5,colvar = NULL, col="black",
          theta =20, phi = 20, bty="b", zlim=c(1,7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = `leng y Com`, z = igno.mat,  
                      facets = TRUE,
                      col=ramp.col(col = c("#084594","#084594"), alpha=0.7)), main = "Predicted GPA")

persp3D(x = matem, y = `leng y Com`, z = LBmin,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = `leng y Com`, z = UBmon,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = `leng y Com`, z = LBmon,add=TRUE,
        colvar = NULL,col="#2ca25f",alpha=0.4)

persp3D(x = matem, y = `leng y Com`, z = UBmon2,add=TRUE,
        colvar = NULL,col="#2ca25f",alpha=0.6)



legend("bottomleft", 
       legend=c("Identification bounds under WIA",
                "Identification bounds under FSA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#d95f02","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")

####### WSA
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$`leng y Com`, dataObs$GPA,
          pch = 19, cex = 0.5,colvar = NULL, col="black",
          theta = 20, phi = 18, bty="b", zlim=c(1,7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = `leng y Com`, z = igno.mat,  
                      facets = TRUE,
                      col=ramp.col(col = c("#084594","#084594"), alpha=0.7)), main = "Predicted GPA")


persp3D(x = matem, y = `leng y Com`, z = UBmon,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = `leng y Com`, z = UBmon2,add=TRUE,
        colvar = NULL,col="#d95f02",alpha=0.6)

persp3D(x = matem, y = `leng y Com`, z = LBmon,add=TRUE,
        colvar = NULL,col="#2ca25f",alpha=0.4)

legend("bottomleft", 
       legend=c("Lower bound under WIA",
                "Identification bounds under WSA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#d95f02","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")


### HECKMAN
d2=data.Ready %>% mutate(status=
                           case_when(Z.biol==1 ~ "Biology",
                                     Z.biolM==1 ~ "Marin Biology",
                                     Z.bioQ==1 ~ "Biochemistry",
                                     TRUE ~ "Non-selected"))
d2$status=as.factor(d2$status)
d2$Z=as.factor(d2$Z)

heckman.model=heckit(Z ~ matem+ciencias+
                       `leng y Com`+`ptje nem`+ranking,
                     GPA~ matem,
                     data=d2)

data.heck=data.Ready %>% select(matem,Z) %>% 
  mutate(h.model=predict(heckman.model,
                         data.Ready)) %>% arrange(matem)

data.heck=data.heck %>% filter(Z==1)
ggplot() +
  geom_point(data = data.biol.mat,
             aes(x = matem, y = PGA_1ERsemestre, shape = Program),
             show.legend = FALSE,
             col="darkgrey") +
  ylim(1, 7) +
  ylab("GPA") + xlab("Mathematics test score") +
  geom_line(data = data.heck,
            aes(x = matem, y = h.model, color = "Heckman's Solution", 
                linetype = "Heckman's Solution"),
            size = 1.1) +
  geom_line(data = data.full.obs,
            aes(x = pje.mate, y = ltp, color = "Ignorability Solution", 
                linetype = "Ignorability Solution"),
            size = 1.1) +
  geom_ribbon(data = data.bounds,
              aes(x = matem, ymin = B3, ymax = B4, 
                  fill = "Worst Selection Assumption"),
              alpha = 0.15) +
  geom_ribbon(data = data.bounds,
              aes(x = matem, ymin = B2, ymax = B3, fill = 
                    "Fallible Selection Assumption"),
              alpha = 0.15) +
  geom_ribbon(data = data.bounds,
              aes(x = matem, ymin = B1, ymax = B2, 
                  fill = "Perfect Selection Assumption"),
              alpha = 0.15)+
  scale_color_manual(name = 'Current solution', 
                     values = c("Heckman's Solution" = 'black', 
                                "Ignorability Solution" = 'black')) +
  scale_linetype_manual(name = 'Current solution', values = 
                          c("Heckman's Solution" = 'dotted', 
                            "Ignorability Solution" = 'solid')) +
  scale_fill_manual(name = 'Identification regions', 
                    breaks=c("Worst Selection Assumption",
                             "Fallible Selection Assumption",
                             "Perfect Selection Assumption"),
                    values = c("Worst Selection Assumption" = 'red', 
                               "Fallible Selection Assumption" = 'blue',
                               "Perfect Selection Assumption"="green")) +
  theme_bw() +
  theme(
    legend.direction = "vertical",
    legend.position = "right",
    legend.box = "vertical",
    legend.key = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"))
