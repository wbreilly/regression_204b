d = read.csv("~/drive/ASBI.davis/WBR.ANALOGY,MMCB, RAVENS, REMOTE, VOCAB, AMBIG -5.27.csv")

plot(d$AR.score.rt..Amb.corr.rt...Fill.corr.rt.,d$MMCB..Mean.Quiz1Master.ACC.)
m1 = lm(d$MMCB..Mean.Quiz1Master.ACC. ~ d$AR.score.rt..Amb.corr.rt...Fill.corr.rt.)
summary(m1)
abline(lm(formula = d$MMCB..Mean.Quiz1Master.ACC. ~ d$AR.score.rt..Amb.corr.rt...Fill.corr.rt.), col = "red")
