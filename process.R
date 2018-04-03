dat<-read.csv('DemDxData.csv',stringsAsFactor=F)

diff.nb.yes<-length(grep('YES',dat$diff_discharge_agreement))
diff.nb.no<-length(grep('NO',dat$diff_discharge_agreement))
diff.nb.diags<- diff.nb.yes+diff.nb.no
diff.percent.yes<-paste0(round(diff.nb.yes/diff.nb.diags *100,2),'%') 
message('Initial dx percent agreement with discharge dx: ',diff.percent.yes)

app.nb.yes<-length(grep('YES',dat$AppDischargeAgreement))
app.nb.no<-length(grep('NO',dat$AppDischargeAgreement))
app.nb.diags<- app.nb.yes+app.nb.no
app.percent.yes<-paste0(round(app.nb.yes/app.nb.diags *100,2),'%') 
message('App dx percent agreement with discharge dx: ',app.percent.yes)


comp<-read.csv('DemDxData_reliability.csv',stringsAsFactor=F)
comp.short<-data.frame(Study.Number=comp$Study.Number,SecondReviewer=comp$Did.App.and.Discharge.Agree.)
comp.short$SecondReviewer<-as.character(comp.short$SecondReviewer)
comp.short$SecondReviewer[grep('YES',comp.short$SecondReviewer,ignore.case=TRUE)]<-'YES'
comp.short$SecondReviewer[grep('NO',comp.short$SecondReviewer,ignore.case=TRUE)]<-'NO'
merged<-merge(dat,comp.short,by='Study.Number')
comp.yes<-length(which(merged$SecondReviewer=='YES'))
comp.no<-length(which(merged$SecondReviewer=='NO'))
full.yes<-length(which(merged$AppDischargeAgreement=='YES'))
full.no<-length(which(merged$AppDischargeAgreement=='NO'))

merged.sm<-data.frame(Study.Number=merged$Study.Number,AppDischargeAgreement=merged$AppDischargeAgreement,SecondReviewer=merged$SecondReviewer)
merged.yes<-merged.sm[merged.sm$AppDischargeAgreement=='YES',]
merged.no<-merged.sm[merged.sm$AppDischargeAgreement=='NO',]
merged.yes.comp<-merged.sm[merged.sm$SecondReviewer=='YES',]
merged.no.comp<-merged.sm[merged.sm$SecondReviewer=='NO',]
matches<-merged$AppDischargeAgreement==merged$SecondReviewer

matched.but.app.failed<-table(merged$AppDischargeAgreement[matches])[[1]]
matched.but.app.worked<-table(merged$AppDischargeAgreement[matches])[[2]]

b<-length(which(merged.no$SecondReviewer=='YES'))
c<-length(which(merged.no.comp$AppDischargeAgreement=='YES'))

counts.dat<-data.frame(matrix(c(matched.but.app.failed,b,c,matched.but.app.worked),nrow=2))
kappa<- (counts.dat[1,1]+counts.dat[2,2])/sum(counts.dat)*100
message(paste('comparing 2 researchers, kappa statistic:',round(kappa,2),'%'))


diffs<-data.frame(jd=dat$diff_discharge_agreement, app=dat$AppDischargeAgreement)
diffs[,1]<-as.character(diffs[,1])
diffs[,2]<-as.character(diffs[,2])
diffs2<-diffs[nchar(diffs[,1])>0,]
diffs2[diffs2=='YES']<-1
diffs2[diffs2=='NO']<-0
diffs2[,1]<-as.numeric(diffs2[,1])
diffs2[,2]<-as.numeric(diffs2[,2])
t.test.app.jd<- t.test(diffs2$jd,diffs2$app,paired=T)
message(paste('paired t test for difference between app and junior doctor:',round( t.test.app.jd$p.value,2)))

chisq.test(diffs2$jd,diffs2$app)

