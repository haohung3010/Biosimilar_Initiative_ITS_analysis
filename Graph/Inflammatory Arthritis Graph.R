ia_its_data<-cbind(total_IJD,fitted(model_total_IJD))
ia_its_eq<-model_total_IJD$coef

write.csv(ia_its_data,file="R:/working/hdang_AnalysisFiles/R_Output/ia_its_data.csv")
write.csv(ia_its_eq,file="R:/working/hdang_AnalysisFiles/R_Output/ia_its_eq.csv")
write.csv(IJD_total_cost_breakdown,file="R:/working/hdang_AnalysisFiles/R_Output/ia_cost_breakdown.csv")

write.csv(msp_rate_ETAINF_IJD_SP1,file="R:/working/hdang_AnalysisFiles/R_Output/msp_rate_ETAINF_sp.csv")
write.csv(msp_rate_ETAINF_IJD_Oth1,file="R:/working/hdang_AnalysisFiles/R_Output/msp_rate_ETAINF_oth.csv")
write.csv(nacrs_visit_ETAINF_IJD_related1,file="R:/working/hdang_AnalysisFiles/R_Output/nacrs_ETAINF_rel.csv")
write.csv(nacrs_visit_ETAINF_IJD_unrelated1,file="R:/working/hdang_AnalysisFiles/R_Output/nacrs_ETAINF_unrel.csv")
write.csv(dad_days_ETAINF_IJD_related1,file="R:/working/hdang_AnalysisFiles/R_Output/dad_ETAINF_rel.csv")
write.csv(dad_days_ETAINF_IJD_unrelated1,file="R:/working/hdang_AnalysisFiles/R_Output/dad_ETAINF_unrel.csv")
write.csv(pn_count_IJD_ETAINF,file="R:/working/hdang_AnalysisFiles/R_Output/pn_ETAINF.csv")

write.csv(msp_rate_ADA_IJD_SP,file="R:/working/hdang_AnalysisFiles/R_Output/msp_rate_ADA_sp.csv")
write.csv(msp_rate_ADA_IJD_Oth,file="R:/working/hdang_AnalysisFiles/R_Output/msp_rate_ADA_oth.csv")
write.csv(nacrs_visit_ADA_IJD_related,file="R:/working/hdang_AnalysisFiles/R_Output/nacrs_ADA_rel.csv")
write.csv(nacrs_visit_ADA_IJD_unrelated,file="R:/working/hdang_AnalysisFiles/R_Output/nacrs_ADA_unrel.csv")
write.csv(dad_days_ADA_IJD_related,file="R:/working/hdang_AnalysisFiles/R_Output/dad_ADA_rel.csv")
write.csv(dad_days_ADA_IJD_unrelated,file="R:/working/hdang_AnalysisFiles/R_Output/dad_ADA_unrel.csv")
write.csv(pn_count_IJD_ADA,file="R:/working/hdang_AnalysisFiles/R_Output/pn_ADA.csv")

write.csv(drug_bydate_uptake_IJD_sum,file="R:/working/hdang_AnalysisFiles/R_Output/drug_uptake.csv")

drug_bydate_uptake_IJD_sum

tiff(filename="R:/working/hdang_AnalysisFiles/R_Output/IA_ITS.tiff",res=300,width=6.5,height=3,units="in")

par(mfrow=c(1,1),las=0,xaxs="i",oma=c(1,0,0,0),mar=c(2,4,1,1),cex=0.8)
plot(total_IJD$date, total_IJD$total_cost,
     xlim = c(as.Date("2015-01-01"),as.Date("2023-01-01")),
     ylim = c(0,500),
     ylab = "",
     xlab = "",
     pch = 20, col = "lightblue",
     bty="n",axes=F)

axis(1,at=c(as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01"),
            as.Date("2019-01-01"),
            as.Date("2020-01-01"),
            as.Date("2021-01-01"),
            as.Date("2022-01-01"),
            as.Date("2023-01-01")), tck=-0.03,labels=F,line=0)
axis(1,at=c(as.Date("2015-07-01"),
            as.Date("2016-07-01"),
            as.Date("2017-07-01"),
            as.Date("2018-07-01"),
            as.Date("2019-07-01"),
            as.Date("2020-07-01"),
            as.Date("2021-07-01"),
            as.Date("2022-07-01")),
            tick=F,labels=seq(2015,2022),line=-0.3)
mtext(side=2,text="Monthly Total Cost",line=2.5,outer=F)
axis(2,at=seq(0,500,100), tck=-0.02,labels=F,line=0.2)
par(las=1)
axis(2,at=seq(0,500,100),tick=F,labels=seq(0,500,100),line=-0.3)

rect(as.Date("2021-04-07"), 0, as.Date("2021-10-06"), 1000000, col = "#00000011", border = NA)
rect(as.Date("2019-05-27"), 0, as.Date("2019-11-25"), 1000000, col = "#00000011", border = NA)
lines(total_IJD$date, total_IJD$total_cost, col = "lightblue")
abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=2)
lines(total_IJD$date[1:53], fitted(model_total_IJD)[1:53], col = "blue", lwd = 2)
lines(total_IJD$date[60:75], fitted(model_total_IJD)[60:75], col = "blue", lwd = 2)
lines(total_IJD$date[82:96], fitted(model_total_IJD)[82:96], col = "blue", lwd = 2)
segments(as.Date("2019-12-01"), model_total_IJD$coef[1] + model_total_IJD$coef[2]*60,
         as.Date("2021-03-01"), model_total_IJD$coef[1] + model_total_IJD$coef[2]*75,
         lty = 1, col = "red", lwd = "2")
segments(as.Date("2021-10-01"), model_total_IJD$coef[1] + model_total_IJD$coef[2]*82 + model_total_IJD$coef[3]*29 + model_total_IJD$coef[4],
         as.Date("2022-12-01"), model_total_IJD$coef[1] + model_total_IJD$coef[2]*96 + model_total_IJD$coef[3]*43 + model_total_IJD$coef[4],
         lty = 1, col = "red", lwd = "2")
par(cex=1.0)
dev.off()






tiff(filename="R:/working/hdang_AnalysisFiles/R_Output/IA_CostBreakdown.tiff",res=300,width=6.5,height=3,units="in")
ggplot(IJD_total_cost_breakdown, aes(x=as.Date(paste(year,month, "01", sep = "-")), y = cost_value, fill = cost_type)) +
  geom_area() +
  labs(x = "", y = "Monthly Total Cost") +
  geom_vline(xintercept = as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06"))) +
  scale_x_date(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,500),expand=c(0,0)) +
  scale_fill_discrete(name=" ", 
                      breaks=c("avg_cost_dad", "avg_cost_ed", "avg_cost_msp", "avg_cost_pn"),
                      labels=c("Hospital Visits","Emergency Visits", "Physician Visits","Concomitant Medications")) +
  theme_classic()
dev.off()




tiff(filename="R:/working/hdang_AnalysisFiles/R_Output/IA_monthlyrates.tiff",res=300,width=6.5,height=9,units="in")
par(mfrow=c(3,1),las=0,xaxs="i",oma=c(1,0,0,0),mar=c(2,4,1,4),cex=0.7)

plot(x=as.Date(paste(pn_count_IJD_ETAINF$year,pn_count_IJD_ETAINF$month, "01", sep = "-")),
     y= pn_count_IJD_ETAINF$count_per_patient,
     xlim = c(as.Date("2015-01-01"),as.Date("2023-01-01")),
     ylim = c(0,27),
     ylab = "",
     xlab = "",
     pch = 20, col = "white",
     bty="n",axes=F)

lines(x=as.Date(paste(msp_rate_ETAINF_IJD_SP1$year,msp_rate_ETAINF_IJD_SP1$month, "01", sep = "-")),
      y=msp_rate_ETAINF_IJD_SP1$msp_rate*2,col="blue",lty=1,lwd=2)
lines(x=as.Date(paste(msp_rate_ETAINF_IJD_Oth1$year,msp_rate_ETAINF_IJD_Oth1$month, "01", sep = "-")),
      y= msp_rate_ETAINF_IJD_Oth1$msp_rate*2,col="blue",lty=2,lwd=2)

lines(x=as.Date(paste(nacrs_visit_ETAINF_IJD_related1$year,nacrs_visit_ETAINF_IJD_related1$month, "01", sep = "-")),
      y= nacrs_visit_ETAINF_IJD_related1$nacrs_visit_per_100*2,col="green",lty=1,lwd=2)
lines(x=as.Date(paste(nacrs_visit_ETAINF_IJD_unrelated1$year,nacrs_visit_ETAINF_IJD_unrelated1$month, "01", sep = "-")),
      y= nacrs_visit_ETAINF_IJD_unrelated1$nacrs_visit_per_100*2,col="green",lty=2,lwd=2)

lines(x=as.Date(paste(pn_count_IJD_ETAINF$year,pn_count_IJD_ETAINF$month, "01", sep = "-")),
      y= pn_count_IJD_ETAINF$count_per_patient+7,col="black",lty=1,lwd=2)
lines(x=as.Date(paste(dad_days_ETAINF_IJD_related1$year,dad_days_ETAINF_IJD_related1$month, "01", sep = "-")),
      y= dad_days_ETAINF_IJD_related1$dad_days_per_100+7,col="red",lty=1,lwd=2)
lines(x=as.Date(paste(dad_days_ETAINF_IJD_unrelated1$year,dad_days_ETAINF_IJD_unrelated1$month, "01", sep = "-")),
      y= dad_days_ETAINF_IJD_unrelated1$dad_days_per_100+7,col="red",lty=2,lwd=2)

axis(1,at=c(as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01"),
            as.Date("2019-01-01"),
            as.Date("2020-01-01"),
            as.Date("2021-01-01"),
            as.Date("2022-01-01"),
            as.Date("2023-01-01")), tck=-0.03,labels=F,line=0)
axis(1,at=c(as.Date("2015-07-01"),
            as.Date("2016-07-01"),
            as.Date("2017-07-01"),
            as.Date("2018-07-01"),
            as.Date("2019-07-01"),
            as.Date("2020-07-01"),
            as.Date("2021-07-01"),
            as.Date("2022-07-01")),
     tick=F,labels=seq(2015,2022),line=-0.3)

mtext(side=3,text="A",outer=F,line=1,cex=0.8,adj=0)
mtext(side=2,text="Average Rate Per Month",line=2.5,outer=F,cex=0.8)
axis(2,at=seq(0,5,1), tck=-0.02,labels=F,line=0.2)
axis(2,at=seq(7,27,5), tck=-0.02,labels=F,line=0.2)

axis(2,at=0,tick=F,labels=0,line=-0.3)
axis(2,at=5,tick=F,labels=2.5,line=-0.3)
axis(2,at=seq(7,27,5),tick=F,labels=seq(0,20,5),line=-0.3)

abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=1)

plot(x=as.Date(paste(pn_count_IJD_ADA$year,pn_count_IJD_ADA$month, "01", sep = "-")),
     y= pn_count_IJD_ADA$count_per_patient,
     xlim = c(as.Date("2015-01-01"),as.Date("2023-01-01")),
     ylim = c(0,27),
     ylab = "",
     xlab = "",
     pch = 20, col = "white",
     bty="n",axes=F)
lines(x=as.Date(paste(msp_rate_ADA_IJD_SP$year,msp_rate_ADA_IJD_SP$month, "01", sep = "-")),
      y=msp_rate_ADA_IJD_SP$msp_rate*2,col="blue",lty=1,lwd=2)
lines(x=as.Date(paste(msp_rate_ADA_IJD_Oth$year,msp_rate_ADA_IJD_Oth$month, "01", sep = "-")),
      y= msp_rate_ADA_IJD_Oth$msp_rate*2,col="blue",lty=2,lwd=2)

lines(x=as.Date(paste(nacrs_visit_ADA_IJD_related$year,nacrs_visit_ADA_IJD_related$month, "01", sep = "-")),
      y= nacrs_visit_ADA_IJD_related$nacrs_visit_per_100*2,col="green",lty=1,lwd=2)
lines(x=as.Date(paste(nacrs_visit_ADA_IJD_unrelated$year,nacrs_visit_ADA_IJD_unrelated$month, "01", sep = "-")),
      y= nacrs_visit_ADA_IJD_unrelated$nacrs_visit_per_100*2,col="green",lty=2,lwd=2)

lines(x=as.Date(paste(pn_count_IJD_ADA$year,pn_count_IJD_ADA$month, "01", sep = "-")),
      y= pn_count_IJD_ADA$count_per_patient+7,col="black",lty=1,lwd=2)
lines(x=as.Date(paste(dad_days_ADA_IJD_related$year,dad_days_ADA_IJD_related$month, "01", sep = "-")),
      y= dad_days_ADA_IJD_related$dad_days_per_100+7,col="red",lty=1,lwd=2)
lines(x=as.Date(paste(dad_days_ADA_IJD_unrelated$year,dad_days_ADA_IJD_unrelated$month, "01", sep = "-")),
      y= dad_days_ADA_IJD_unrelated$dad_days_per_100+7,col="red",lty=2,lwd=2)

axis(1,at=c(as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01"),
            as.Date("2019-01-01"),
            as.Date("2020-01-01"),
            as.Date("2021-01-01"),
            as.Date("2022-01-01"),
            as.Date("2023-01-01")), tck=-0.03,labels=F,line=0)
axis(1,at=c(as.Date("2015-07-01"),
            as.Date("2016-07-01"),
            as.Date("2017-07-01"),
            as.Date("2018-07-01"),
            as.Date("2019-07-01"),
            as.Date("2020-07-01"),
            as.Date("2021-07-01"),
            as.Date("2022-07-01")),
     tick=F,labels=seq(2015,2022),line=-0.3)

mtext(side=3,text="B",outer=F,line=1,cex=0.8,adj=0)
mtext(side=2,text="Average Rate Per Month",line=2.5,outer=F,cex=0.8)
axis(2,at=seq(0,5,1), tck=-0.02,labels=F,line=0.2)
axis(2,at=seq(7,27,5), tck=-0.02,labels=F,line=0.2)

axis(2,at=0,tick=F,labels=0,line=-0.3)
axis(2,at=5,tick=F,labels=2.5,line=-0.3)
axis(2,at=seq(7,27,5),tick=F,labels=seq(0,20,5),line=-0.3)

abline(v=as.Date(c("2019-05-27", "2019-11-25", "2021-04-07", "2021-10-06")), lty=1)



plot(x=c(0,15),y=c(0,15),
     ylab = "",
     xlab = "",
     pch = 20, col = "white",
     bty="n",axes=F)

par(adj=0)
lines(x=c(0,1),y=rep(15,2),col="black",lty=1,lwd=2)
text(x=1.5,y=15,"Concomitant Medications")

lines(x=c(0,1),y=rep(12,2),col="red",lty=1,lwd=2)
text(x=1.5,y=12,"Hospital Days per 100 people (related)")
lines(x=c(0,1),y=rep(11,2),col="red",lty=2,lwd=2)
text(x=1.5,y=11,"Hospital Days per 100 people (unrelated)")

lines(x=c(0,1),y=rep(14,2),col="blue",lty=1,lwd=2)
text(x=1.5,y=14,"Physician Visits (Specialist)")
lines(x=c(0,1),y=rep(13,2),col="blue",lty=2,lwd=2)
text(x=1.5,y=13,"Physician Visits (Other)")

lines(x=c(0,1),y=rep(10,2),col="green",lty=1,lwd=2)
text(x=1.5,y=10,"Emergency Visits per 100 people (related)")
lines(x=c(0,1),y=rep(9,2),col="green",lty=2,lwd=2)
text(x=1.5,y=9,"Emergency Visits per 100 people (unrelated)")

par(adj=0.5)

par(cex=1.0)

dev.off()



ggplot(drug_bydate_uptake_IJD_sum, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = cohort_size, fill = drug)) +
  geom_area() + 
  scale_fill_manual(values = c("ADA" = "red", "ADAB" = "pink",
                               "ETA" = "darkblue", "ETAB" = "lightblue",
                               "INF" = "cornsilk4", "INFB" = "bisque")) +
  labs(x = "Year", y = "Yearly cohort size") +
  ggtitle("Biosimilar uptake of IJD cohort") +
  geom_vline(xintercept = as.Date(c("2019-09-05", "2020-03-05", "2021-04-07", "2021-10-06"))) +
  ylim(0,4500) +
  theme_minimal()


