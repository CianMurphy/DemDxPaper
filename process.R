dat<-read.csv('DemDxData.csv',stringsAsFactor=F)

topD1_agreement<- length(grep('YES',dat$Top_D1_agreement_w_D2))
topD1_disagreement<- length(grep('NO',dat$Top_D1_agreement_w_D2))
topD1.nb.diags<- topD1_agreement+topD1_disagreement
topD1.percent.yes<-paste0(round(topD1_agreement/topD1.nb.diags *100,2),'%') 

top_3_D1_agreement<- length(grep('YES',dat$Top_3_D1_agreement_w_D2))
top_3_D1_disagreement<- length(grep('NO',dat$Top_3_D1_agreement_w_D2))
top_3_D1.nb.diags<- top_3_D1_agreement+top_3_D1_disagreement
top_3_D1.percent.yes<-paste0(round(top_3_D1_agreement/top_3_D1.nb.diags *100,2),'%') 

app.nb.yes<-length(grep('YES',dat$AppDischargeAgreement))
app.nb.no<-length(grep('NO',dat$AppDischargeAgreement))
app.nb.diags<- app.nb.yes+app.nb.no
app.percent.yes<-paste0(round(app.nb.yes/app.nb.diags *100,2),'%') 
message('App dx percent agreement with discharge dx: ',app.percent.yes)

# compare app to topd1
topd1_app_diffs<-data.frame(jd=dat$Top_D1_agreement_w_D2, app=dat$AppDischargeAgreement)
topd1_app_diffs[,1]<-as.character(topd1_app_diffs[,1])
topd1_app_diffs[,2]<-as.character(topd1_app_diffs[,2])
topd1_app_diffs_2<-topd1_app_diffs[nchar(topd1_app_diffs[,1])>0,]

topd1_table<-data.frame(matrix(nrow=2,ncol=2))
colnames(topd1_table)<-c('topD1','DemDx')
rownames(topd1_table)<-c('Match','No Match')
topd1_table$topD1[1]<- length(grep('YES',topd1_app_diffs_2$jd))
topd1_table$topD1[2]<-length(grep('NO',topd1_app_diffs_2$jd))
topd1_table$DemDx[1]<-length(grep('YES',topd1_app_diffs_2$app))
topd1_table$DemDx[2]<-length(grep('NO',topd1_app_diffs_2$app))
  
top_d1_app_chisq<- chisq.test(topd1_table)


# compare app to top3_d1
top_3_d1_app_diffs<-data.frame(jd=dat$Top_3_D1_agreement_w_D2, app=dat$AppDischargeAgreement)
top_3_d1_app_diffs[,1]<-as.character(top_3_d1_app_diffs[,1])
top_3_d1_app_diffs[,2]<-as.character(top_3_d1_app_diffs[,2])
top_3_d1_app_diffs_2<-top_3_d1_app_diffs[nchar(top_3_d1_app_diffs[,1])>0,]

top_3_d1_table<-data.frame(matrix(nrow=2,ncol=2))
colnames(top_3_d1_table)<-c('top_3_D1','DemDx')
rownames(top_3_d1_table)<-c('Match','No Match')
top_3_d1_table$top_3_D1[1]<- length(grep('YES',top_3_d1_app_diffs_2$jd))
top_3_d1_table$top_3_D1[2]<-length(grep('NO',top_3_d1_app_diffs_2$jd))
top_3_d1_table$DemDx[1]<-length(grep('YES',top_3_d1_app_diffs_2$app))
top_3_d1_table$DemDx[2]<-length(grep('NO',top_3_d1_app_diffs_2$app))

top_3_d1_app_chisq<- chisq.test(top_3_d1_table)

# merge
merged<- data.frame(cbind(topD1=topd1_table$topD1,top_3_D1=top_3_d1_table$top_3_D1,DemDx=topd1_table$DemDx)) 
rownames(merged)<-c('Match','No Match')
