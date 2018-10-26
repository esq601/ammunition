library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(scales)
library(stringr)
library(lubridate)
library(ggalt)
library(stringr)
library(janitor)
library(plotly)
library(ggthemes)
library(tidyverse)


df1 <- SummaryReport2018
str(df1)
df1$DODIC <- as.factor(df1$DODIC)
df1$`Hierarchy Level` <- as.factor(df1$`Hierarchy Level`)
df1$`DODIC Description` <- as.factor(df1$`DODIC Description`)

df2 <- df1 %>%
  filter(DODIC %in% c("AB77","AB57","A080","A064","A075","A131","A111","A557","A598","BA30","B519")) %>%
  filter(`Hierarchy Level` %in% c("1 BCT (I), 10 MTN (FDNY)","2 BCT (I), 10 MTN (FDNY)","3 BCT (I), 10 MTN (FPLA)","CAB, 10 MTN (FDNY)"))  %>%
  gather("UtilizedCategory","Count",c(9,11,12)) %>%
  group_by(`Hierarchy Level`,DODIC) %>%
  mutate(Percent= percent(Count / sum(Count)))



p1 <- ggplot(df2,aes(`Hierarchy Level`,Count))
p1 + geom_col(aes(fill=UtilizedCategory)) + facet_wrap(~DODIC) + geom_point(aes(x=`Hierarchy Level`,y=`Current Auth: Total`)) 

df3 <- df2 %>%
  filter(DODIC %in% c("A557","A598","B519","BA30"))AT

p2 <- ggplot(df3,aes(DODIC,Count))
p2 + geom_col(aes(fill=UtilizedCategory)) + facet_wrap(~`Hierarchy Level`) + 
  geom_point(aes(x=DODIC,y=`Current Auth: Total`)) + geom_label(aes(label=Percent,color=UtilizedCategory),position=position_stack(vjust=.5))


df4 <- df2 %>%
  spread(UtilizedCategory,Percent) %>%
  group_by(DODIC)

df4$`Available: Total` <- percent(as.numeric(df4$`Available: Total`))
str(df4)
p4 <- ggplot(df4,aes(x=`Expenditures: Total`,xend=`Available: Total`,y=DODIC,group=DODIC))
p4 + geom_dumbbell()

fxex1 <- Forecast_and_Expenditures_Report2018

fxex1 <- read_excel("/data/rsworkspace/Eskew/Ammo/FY18 Forecast Report Vs Expenditures.xlsx")


fxex2 <- fxex1 %>%
  slice(-1:-5)

colnames(fxex2) = fxex2[1, ] # the first row will be the header
fxex2 = fxex2[-1, ]

fxex2[, 4:39] <- sapply(fxex2[, 4:39], as.numeric)


fxex3 <- fxex2 %>%
  gather("Category","Count",4:39) %>%
  filter(DODIC %in% c("A557","A598","B519","BA30")) %>%
  group_by(DODIC,Category) %>%
  mutate(Count = replace(Count,is.na(Count),0)) %>%
  #mutate(Category=str_sub(Category,4)) %>%
  summarise(Total = sum(Count)) %>%
  mutate(Month = match(str_sub(Category,1,3),month.abb)) %>%
  ungroup(Category) %>%
  mutate(Category=str_sub(Category,5)) %>%
  arrange(Month) %>%
  filter(Category %in% c("Exp","Fore"))


p3 <- ggplot(fxex3,aes(Month,Total))
p3 + geom_path(aes(color=Category)) + geom_point(aes(color=Category)) + facet_wrap(~DODIC)



fxex4 <- fxex2 %>%
  gather("Category","Count",4:39)
  
fxex4$Category <- str_sub(fxex4$Category, 4)  

fxex5<- fxex4 %>%
  filter(str_detect(Category,"%")==FALSE) %>%
  filter(!is.na(Count)) %>%
  group_by(DODIC,Category) %>%
  summarise(Total=sum(Count)) %>%
  spread(Category,Total) %>%
  clean_names()%>%
  mutate(Percent=exp/fore)

p4 <- ggplot(fxex5,aes(fore,exp))
ggplotly(p4 + geom_point(aes(color=(Percent))) +geom_abline(slope=1) + geom_text(vjust=-.5,aes(label=round(Percent,digits=2))))

?geom_text


fxex5 <- as.data.frame(fxex5)
fxex5$dodic <- as.factor(fxex5$dodic)
fxex6 <- fxex5 %>%
  filter(fore<10000)

p5 <- ggplot(fxex6,aes(fore,exp))
p5 + geom_point(aes(color=Percent)) +geom_abline(slope=1) + geom_text(vjust=-.5,aes(label=round(Percent,digits=2)))


fxex7 <- fxex5 %>%
  filter(fore>=10000)

ggplot(fxex7, aes(x=dodic, y=Percent)) + 
  geom_point(col="tomato2", aes(size=fore)) +   # Draw points
  geom_segment(aes(x=dodic, 
                   xend=dodic, 
                   y=0, 
                   yend=1), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Expenditure/Forecast %", 
       subtitle="DODICs Greater than 10000 annually") +
  geom_hline(yintercept=.75,color="darkgreen",size=1.3,alpha=.5,linetype="dashed") +
  coord_flip() + ggthemes::theme_fivethirtyeight() + theme(axis.text.y = element_text(size=8))

ggplot(fxex6, aes(x=dodic, y=Percent)) + 
  geom_point(col="tomato2", aes(size=fore)) +   # Draw points
  geom_segment(aes(x=dodic, 
                   xend=dodic, 
                   y=0, 
                   yend=1), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Expenditure/Forecast %", 
       subtitle="DODICs Less than 10000 annually") +
  geom_hline(yintercept=.75,color="darkgreen",size=1.3,alpha=.5,linetype="dashed") +
  coord_flip() + ggthemes::theme_fivethirtyeight() + theme(axis.text.y = element_text(size=8))



fxex8 <- fxex5 %>%
  summarise(Forecast=sum(na.omit(fore)),Expend=sum(na.omit(exp))) %>%
  mutate(Percent=Expend/Forecast)


#### Request Expenditure Data ####

# ad1 <- read_excel("/data/rsworkspace/Eskew/Ammo/ammodata.xlsx",
#                        col_types = c("text", "numeric", "text",
#                                      "date", "text", "numeric", "numeric",
#                                      "numeric", "text", "text"))
ex1 <- read_excel("/data/rsworkspace/Eskew/Ammo/ExpenditureReport.xlsx", 
                                skip = 6)

ex2016 <- read_excel("/data/rsworkspace/Eskew/Ammo/ExpenditureReport2016.xlsx", 
                     skip = 6)

ex2017 <- read_excel("/data/rsworkspace/Eskew/Ammo/ExpenditureReport2017.xlsx", 
                     skip = 6)


UICList <- read_excel("/data/rsworkspace/Eskew/Ammo/UICList.xlsx")

UICList1 <- UICList %>%
  distinct(Sub2UIC,.keep_all=TRUE) %>%
  dplyr::select(SubName,Sub2UIC)



ex2 <- ex1 %>%
  clean_names(case="snake")

ex2016a <- ex2016 %>%
  clean_names(case="snake")

ex2017a <- ex2017 %>%
  clean_names(case="snake")

colnames(ex2017a)[4] <- c("uic")
colnames(ex2)[4] <- c("uic")
colnames(ex2016a)[4] <- c("uic")

extotal <- ex2 %>%
  bind_rows(ex2016a) %>%
  bind_rows(ex2017a) %>%
  mutate(uiciso=str_sub(uic,1,6)) %>%
  left_join(UICList1,by = c("uiciso"="Sub2UIC")) %>%
  group_by(fiscal_year,asp,expenditure_type) %>%
  filter(asp %in% c("SDRM","SPOL"))
  

extotal$SubName <- factor(extotal$SubName, levels = c("10TH MTN DIV, 1 BCT","10TH MTN DIV, 2 BCT","10TH MTN DIV, 3 BCT",
                                                      "10TH MTN DIV, DIVARTY","10TH MTN DIV, 10 CAB","10TH MTN DIV, 10 SBDE",
                                                      "10TH MTN DIV, HHBN","10TH MTN DIV, LIGHT FIGHTER SCHOOL"))

extotal_issue <- extotal %>%
  filter(expenditure_type=="I") %>%
  ungroup() %>%
  group_by(fiscal_year,asp,uic,document_number,dpa) %>%
  summarise(IssueQty=sum(na.omit(quantity)),IssuePrice=sum(na.omit(total_price)))

extotal_ti <- extotal %>%
  ungroup() %>%
  filter(expenditure_type=="T") %>%
  group_by(fiscal_year,asp,uic,issue_document_number,dpa) %>%
  summarise(ReturnQty=abs(sum(na.omit(quantity))),ReturnPrice=abs(sum(na.omit(total_price))))
  
#?summarise

extotal_docnum <- extotal_issue %>%
  full_join(extotal_ti,by=c("document_number"="issue_document_number","dpa")) %>%
  distinct() %>%
  replace_na(list(ReturnQty=0,ReturnPrice=0)) %>%
  mutate(ReturnedPct = ReturnQty/IssueQty)

extotal_docnum$fiscal_year.x[is.na(extotal_docnum$fiscal_year.x)] <- as.character(extotal_docnum$fiscal_year.y[is.na(extotal_docnum$fiscal_year.x)])
extotal_docnum$asp.x[is.na(extotal_docnum$asp.x)] <- as.character(extotal_docnum$asp.y[is.na(extotal_docnum$asp.x)])


extotal_docnum_summary <- extotal_docnum %>%
  ungroup() %>%
  group_by(fiscal_year.x,asp.x) %>%
  summarise(IssueSum=abs(sum(na.omit(IssueQty))),ReturnSum=abs(sum(na.omit(ReturnQty)))) %>%
  mutate(Percent=ReturnSum/IssueSum)

plot1 <- ggplot(extotal_docnum,aes(x=IssueQty,y=ReturnedPct))
plot1 + geom_point(size=.1) + scale_x_log10(labels=comma) + geom_smooth()

plot2 <- ggplot(filter(extotal_docnum,ReturnedPct<=1),aes(x=ReturnedPct))
plot2 + geom_histogram(bins=10)
test1 <- extotal %>%
  filter(is.na(SubName))

extotalsummary <- extotal %>%
  group_by(fiscal_year,asp,expenditure_type) %>%
  summarise(Total=abs(sum(quantity)))

extotal_summary_cost <- extotal %>%
  ungroup() %>%
  group_by(expenditure_type) %>%
  summarise(Totale=abs(sum(total_price)))

extotalsummarybct <- extotal %>%
  group_by(fiscal_year,SubName,expenditure_type) %>%
  summarise(Total=abs(sum(quantity)))

extotal1 <- extotalsummary %>%
  ungroup() %>%
  group_by(fiscal_year,asp) %>%
  spread(expenditure_type,Total) %>%
  mutate(Percent=T/I)

extotal2 <- extotalsummarybct %>%
  ungroup() %>%
  group_by(fiscal_year,SubName) %>%
  spread(expenditure_type,Total) %>%
  filter(!is.na(SubName)) %>%
  mutate(Percent=T/I)

yearlyexp <- ggplot(extotalsummary,aes(x=fiscal_year,y=Total))
yearlyexp + geom_line(aes(color=expenditure_type)) + facet_grid(~asp)

yearlyexpbct <- ggplot(extotalsummarybct,aes(x=fiscal_year,y=Total,group=expenditure_type))
yearlyexpbct + geom_line(aes(color=expenditure_type)) +  facet_wrap(~SubName)

yearlyexp <- ggplot(extotalsummary,aes(x=fiscal_year,y=Total))
yearlyexp + geom_line(aes(color=expenditure_type)) + facet_grid(~asp)

yearlypercent <- ggplot(extotal1,aes(x=fiscal_year,y=Percent,fill=asp))
yearlypercent + geom_bar(stat="identity",position="dodge",aes(group=asp)) + scale_y_continuous(limits=c(0,1)) + ggtitle("% Ammo Returned to ASP") +
  geom_text(stat="identity",aes(group=asp,label=scales::percent(Percent)),position=position_dodge(width = 1),vjust=-.5) +
  scale_fill_tableau(palette="Tableau 10","asp") + theme_fivethirtyeight()

yearlypercentbct <- ggplot(extotal2,aes(x=fiscal_year,y=Percent,group=SubName))
yearlypercentbct + geom_bar(stat="identity",position="dodge",aes(fill=Percent)) + scale_y_continuous(limits=c(0,1)) + ggtitle("% Ammo Returned to ASP by BDE") +
  geom_text(stat="identity",aes(label=scales::percent(Percent)),position=position_dodge(width = 1),vjust=-.5) +
  theme_fivethirtyeight() + facet_wrap(~SubName) + scale_fill_gradient2(low="green",high="red",mid="red",midpoint=.5,limits=c(0,1))


extotal3 <- extotal2 %>%
  group_by(fiscal_year) %>%
  mutate(PercentTotal = I/sum(I))

yearlyissuebct <- ggplot(extotal3,aes(x=fiscal_year,y=SubName))
yearlyissuebct + geom_tile(aes(fill=PercentTotal)) + geom_text(aes(label=scales::percent(PercentTotal))) +
  scale_fill_gradient(low="brown",high="yellow")  + theme_fivethirtyeight() + ggtitle("% of DIV Issued")



ex2$dodic <- as.factor(ex2$dodic)
ex2$dpa <- as.factor(ex2$dpa)
ex2$uic <- as.factor(ex2$uic)
ex2$document_number <- as.factor(ex2$document_number)
ex2$expenditure_type <- as.factor(ex2$expenditure_type)
ex2$event_code <- as.factor(ex2$event_code)
ex2$asp <- as.factor(ex2$asp)
ex2$issue_document_number <- as.factor(ex2$issue_document_number)


ex3 <- ex2 %>%
  gather()

ex2a <- ex2 %>%
  filter(expenditure_type == "I") %>%
  mutate(original_doc = document_number) %>%
  mutate(doc_dpa = paste0(original_doc,dpa)) %>%
  group_by(dpa,original_doc)


ex2b <- ex2 %>%
  filter(expenditure_type == "T") %>%
  mutate(original_doc = issue_document_number) %>%
  mutate(doc_dpa = paste0(original_doc,dpa)) %>%
  group_by(dpa,original_doc) %>%
  dplyr::select(expenditure_type,doc_dpa,quantity,total_price)
  
ex3 <- ex2a %>%
  left_join(ex2b,by="doc_dpa") %>%
  gather(DocNum,Qty,quantity.x,quantity.y) %>%
  group_by(document_number,dpa.x) %>%
  arrange(document_number,dpa.x)

ex4 <- ex3 %>%
  ungroup() %>%
  group_by(dpa.x,DocNum) %>%
  summarise(total=sum(na.omit(Qty))) %>%
  spread(DocNum,total) %>%
  mutate(percent=-quantity.y/quantity.x) %>%
  mutate(cat = str_sub(dpa.x,1,1))

ex5 <- ex3 %>%
  ungroup() %>%
  group_by(uic,dpa.x,DocNum) %>%
  summarise(total=sum(na.omit(Qty))) %>%
  spread(DocNum,total) %>%
  mutate(percent=-quantity.y/quantity.x) %>%
  mutate(cat = str_sub(dpa.x,1,1))

highavg <- ex4 %>%
  ungroup() %>%
  filter(quantity.x > 1000000) %>%
  summarise(AVG = sum(-quantity.y)/sum(quantity.x))


simrds <- ex4 %>%
  ungroup() %>%
  filter(dpa.x %in% c("AB17","AB10","AB66","AB16","AB09","AA68")) %>%
  summarise(x=sum(quantity.x),y=sum(quantity.y)) %>%
  mutate(percent=-y/x)

totals <- ex4 %>%
  ungroup() %>%
  mutate(highlow=between(quantity.x,0,1000000)) %>%
  group_by(highlow) %>%
  summarise(sum(quantity.x))
  
totals$`sum(quantity.x)`[1]/sum(totals$`sum(quantity.x)`)

totalcost <- ex1 %>%
  ungroup() %>%
  group_by(`Expenditure Type`) %>%
  summarise(sum(`Total Price`))

#?between
catavg <- ex4 %>%
  ungroup() %>%
  group_by(cat) %>%
  summarise(AVG = sum(-quantity.y)/sum(quantity.x))


#write.csv(x=as.data.frame(catavg), file="catavg.csv")
#?write.csv
p3 <- ggplot(ex4, aes(x=quantity.x,y=percent))
p3 + geom_point(aes(color=cat),size=2.5) + scale_x_log10() + geom_text(aes(label=dpa.x),size=3,alpha=.75,vjust=1.5)# +
  #geom_text(aes(label=quantity.x),size=2.5, alpha = .5,vjust=-2.3) + geom_text(aes(label=-quantity.y),size=2.5, alpha = .5,vjust=-1) 

p3 + geom_point(aes(color=cat),size=2.5) + scale_x_log10() + geom_text(aes(label=dpa.x),size=3,alpha=.5,vjust=1.5) 

p3 + geom_point(aes(color=cat),size=2.5) + scale_x_log10() + geom_text(aes(label=dpa.x),size=3,alpha=.5,vjust=1.5) +
  facet_wrap(~uic) +scale_y_continuous(limits=0:1)



for (var in unique(ex5$uic)) {

  ggsave(ggplot(ex5[ex5$uic==var,], aes(x=quantity.x,y=percent)) + geom_point(aes(color=cat),size=2.5) + scale_x_log10() + geom_text(aes(label=dpa.x),size=3,alpha=.5,vjust=1.5) +
           scale_y_continuous(limits=0:1) + geom_text(aes(label=quantity.x),size=2.5, alpha = .5,vjust=-2.3) + 
        geom_text(aes(label=-quantity.y),size=2.5, alpha = .5,vjust=-1) + labs(title=var),filename=paste0("./BNImages/",str_sub(var,start=1,end=5),".jpeg"), height=4,width = 6)
}
#?ggsave

#?print
# ad2 <- ad1 %>%
#   fill(`Document Number`,Date,Unit) %>%
#   group_by(DODIC,DODAAC) %>%
#   mutate(PercentLTI =`Live Turn In` / `Original Amount`) %>%
#   mutate(cat = str_sub(DODIC,1,1))
# 
# ad2$PercentLTI[is.na(ad2$PercentLTI)] <- 0
# 
# ad3 <- ad2 %>%
#   ungroup() %>%
#   group_by(DODIC) %>%
#   summarise(LTITotal=sum(na.omit(`Live Turn In`)),OrigTotal=sum(na.omit(`Original Amount`))) %>%
#   mutate(PercentLTI = LTITotal/OrigTotal) %>%
#   mutate(cat = str_sub(DODIC,1,1))
# 
# ad3$LTITotal[is.na(ad3$LTITotal)] <- 0
# 
# 
# #?mutate
# p1 <- ggplot(ad2,aes(x=`Original Amount`,y=PercentLTI))
# p1 + geom_point(aes(color=DODIC)) + scale_x_log10()
# 
# p2 <- ggplot(ad3, aes(x=OrigTotal,y=PercentLTI))
# p2 + geom_point(aes(color=cat)) + scale_x_log10() + geom_text(aes(label=DODIC),size=3,alpha=.5,vjust=1.5)# + geom_encircle(aes(group=cat,color=cat))
# 
# ?geom_encircle()
# 
# sum(na.omit(ad1$`Live Turn In`))/sum(ad1$`Original Amount`)

###### SubAuthorizations ######

DIVSubauth <- read_excel("/data/rsworkspace/Eskew/Ammo/DIVSubauth.xlsx", 
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text", "text", "date"), 
                         skip = 6)

BCT1Subauth <- read_excel("/data/rsworkspace/Eskew/Ammo/1BCTSubauth.xlsx", 
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text", "text", "date"), 
                         skip = 6)

BCT2Subauth <- read_excel("/data/rsworkspace/Eskew/Ammo/2BCTSubauth.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "text", "date"), 
                          skip = 6)

BCT3Subauth <- read_excel("/data/rsworkspace/Eskew/Ammo/3BCTSubauth.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "text", "date"), 
                          skip = 6)

SBDESubauth <- read_excel("/data/rsworkspace/Eskew/Ammo/SBDESubauth.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "text", "date"), 
                          skip = 6)

CABSubauth <- read_excel("/data/rsworkspace/Eskew/Ammo/CABSubauth.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "text", "date"), 
                          skip = 6)

DIVAuths <- read_excel("/data/rsworkspace/Eskew/Ammo/DIVAuths.xlsx", 
                       skip = 6)

SummaryForecast <- read_excel("/data/rsworkspace/Eskew/Ammo/SummaryForecast.xlsx", 
                              skip = 6)

ForecastUIC <- read_excel("/data/rsworkspace/Eskew/Ammo/ForecastUIC.xlsx", 
                          skip = 6)

Requirements <- read_excel("/data/rsworkspace/Eskew/Ammo/Requirements.xlsx", 
                           skip = 6)

Hierarchy <- read_excel("/data/rsworkspace/Eskew/Ammo/Hierarchy.xlsx", 
                           skip = 6)


sum(na.omit(SummaryForecast$`Forecasts: Total`))
sum(na.omit(SumRpt$rem_fore_total))

DIVAuths1 <- DIVAuths %>%
  clean_names() %>%
  select(1,5) %>%
  mutate(cat= str_sub(dodic,1,1)) %>%
  group_by(cat) %>%
  summarise(auths = sum(current_authorizations))

SumRpt <- SummaryForecast %>%
  clean_names() %>%
  select(dodic,requirements_total,expenditures_total,e581_total,rem_fore_total,unforecasted) %>%
  mutate(cat=str_sub(dodic,1,1)) %>%
  group_by(cat) %>%
  summarise_at(c("requirements_total","expenditures_total","e581_total","rem_fore_total","unforecasted"),sum,na.rm=TRUE)


req_clean <- Requirements %>%
  clean_names() %>%
  group_by(hierarchy_level,dodic) %>%
  summarise(req_qty=sum(na.omit(req_qty)))

Req_dodic <- req_clean %>%
  group_by(dodic) %>%
  summarise(TotalReq=sum(req_qty))

Req_unit <- req_clean %>%
  mutate(uic= str_sub(hierarchy_level,1,4)) %>%
  group_by(hierarchy_level) %>%
  summarise(TotalReq=sum(req_qty))

hierarchy_list <- Hierarchy %>%
  clean_names() %>%
  separate(location,c("HQDA","FORSCOM","Corps","DIV","BDE","BN","CO"),sep="\\\\") %>%
  select(hierarchy_name,hierarchy_level,BDE,BN,CO)
  filter(!is.na(CO))





Forecast1 <- ForecastUIC %>%
  clean_names() %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(.[6:17])) %>%
  #select(macom,dodic,sum) %>%
  mutate(cat=str_sub(dodic,1,1))

Forecast2 <- Forecast1 %>%
  group_by(macom,cat) %>%
  summarise(forecast= sum(sum))

DIVSubauth1 <- DIVSubauth %>%
  dplyr::select(1,3,7:8) %>%
  distinct() %>%
  clean_names() %>%
  summarise(total=sum(current_auth))


TotalSub <- DIVSubauth %>%
  bind_rows(BCT1Subauth,BCT2Subauth,BCT3Subauth,SBDESubauth,CABSubauth) %>%
  clean_names(case="snake")

TotalSub1 <- TotalSub %>%
  dplyr::select(1,3,7:8) %>%
  distinct() %>%
  mutate(cat=str_sub(dodic,1,1)) %>%
  group_by(from,to,cat) %>%
  summarise(total_auth=sum(current_auth))

TotalSub2 <- TotalSub1 %>%
  dplyr::filter(from %in% "10 MTN DIV (FDNY)")

TotalSub3 <- TotalSub1 %>%
  filter(from != "10 MTN DIV (FDNY)") %>%
  group_by(from,cat) %>%
  summarise(total_auth=sum(total_auth))

TotalSub3a <- TotalSub1 %>%
  filter(from != "10 MTN DIV (FDNY)") %>%
  group_by(from) %>%
  summarise(total_auth=sum(total_auth))

TotalSub4 <- TotalSub2 %>%
  full_join(TotalSub3,by = c("to"="from","cat"="cat"))

TotalSub4$total_auth.y[is.na(TotalSub4$total_auth.y)] <- TotalSub4$total_auth.x[is.na(TotalSub4$total_auth.y)]

TotalSub5 <- TotalSub4 %>%
  #filter(!is.na(total.y)) %>%
  group_by(cat) %>%
  summarise(DIV_sub = sum(na.omit(total_auth.x)),BDE_sub=sum(na.omit(total_auth.y))) %>%
  left_join(DIVAuths1, by="cat") %>%
  mutate(DIV_percent = scales::percent(DIV_sub/auths)) %>%
  mutate(BDE_percent = scales::percent(BDE_sub/DIV_sub)) %>%
  left_join(SumRpt, by = "cat")

TotalSub5a <- TotalSub4 %>%
  group_by(to) %>%
  summarise(DIV_sub = sum(na.omit(total_auth.x)),BDE_sub=sum(na.omit(total_auth.y)))

TotalSub6 <- TotalSub5 %>%
  select(cat,expenditures_total,e581_total,rem_fore_total,unforecasted) %>%
  gather("metric","number",-1) %>%
  group_by(metric) %>%
  summarise(total= sum(number)) %>%
  mutate(frac = total/sum(total)) %>%
  mutate(percent = scales::percent(total/sum(total)))


fcst_dodic <- distinct(as.data.frame(Forecast1$dodic))
auth_dodic <- distinct(as.data.frame(DIVAuths$DODIC))

colnames(auth_dodic) <- "dodic"
colnames(fcst_dodic) <- "dodic"

dodics <- req_clean %>%
  ungroup() %>%
  dplyr::select(dodic) %>%
  dplyr::union(auth_dodic) %>%
  dplyr::union(fcst_dodic) %>%
  distinct()


#?union
dodics1 <- as.array(dodics$dodic)



str(dodics1)
unis <- hierarchy_list %>%
  select(hierarchy_level) %>%
  distinct()
  
# unit_dodic <- unis %>%
#   group_by_all() %>%
#   nest() %>%
#   mutate(dodic=dodics1)

unit_dodic <- as.data.frame(do.call(rbind, Map(cbind,  unis, dodics1)))
colnames(unit_dodic) <- c("unit","dodic")

div_auths_clean <- DIVAuths %>%
  clean_names() %>%
  select(hierarchy_level,dodic,current_authorizations)

div_subauth_clean <- DIVSubauth %>%
  clean_names() %>%
  select(dodic,current_auth,to) %>%
  distinct()

bde_subauth_clean <- TotalSub %>%
  clean_names() %>%
  filter(from != "10 MTN DIV (FDNY)") %>%
  select(from,to,dodic,current_auth) %>%
  distinct()

main_df <- hierarchy_list %>%
  full_join(unit_dodic,by=c("hierarchy_level"="unit")) %>%
  full_join(req_clean,by=c("hierarchy_level","dodic")) %>%
  full_join(Forecast1, by=c("hierarchy_level"="macom","dodic")) %>%
  full_join(div_auths_clean,by=c("hierarchy_level","dodic")) %>%
  full_join(div_subauth_clean,by=c("hierarchy_level"="to","dodic")) %>%
  full_join(bde_subauth_clean,by=c("hierarchy_level"="to","dodic")) %>%
  distinct()

div_df <- main_df %>%
  summarise(Req_total=sum(na.omit(req_qty)),Auth_total=sum(na.omit(current_authorizations)),DIVSubauth_total=sum(na.omit(current_auth.x)),
            BDESubauth_total=sum(na.omit(current_auth.y)),Fcst_total=sum(na.omit(sum))) %>%
  gather()

bde_df <- main_df %>%
  group_by(BDE) %>%
  summarise(Req_total=sum(na.omit(req_qty)),Auth_total=sum(na.omit(current_authorizations)),DIVSubauth_total=sum(na.omit(current_auth.x)),
            BDESubauth_total=sum(na.omit(current_auth.y)),Fcst_total=sum(na.omit(sum)))


div_df$key <- as.factor(div_df$key)
div_df$key <- factor(div_df$key,c("Req_total","Auth_total","DIVSubauth_total","BDESubauth_total","Fcst_total"))

div_plot <- ggplot(div_df,aes(x=key,y=value))
div_plot + geom_bar(stat="identity",aes(fill=key)) + scale_y_continuous(name="Quantity", labels = comma) + 
  theme_fivethirtyeight() + geom_label(aes(label=percent(value/lag(value,default = value[1]))),vjust=2) + geom_text(aes(label=comma(value)),size=3,vjust=-.2) +
  labs(title="Division Ammunition Snapshot") + guides(fill=FALSE)+ theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_discrete(label=c("Required","DIV Auth.","BDE Sub-Auth","BN Sub-Auth","Forecast"))





totalplot <- ggplot(TotalSub6,aes(x="DIV",y=frac,label=percent))
totalplot + geom_bar(aes(fill=metric),stat="identity") + geom_text(aes(label=percent,group=metric),position="stack",vjust=1)

unit_name <- TotalSub %>%
  select(to,from) %>%
  #filter(from != "10 MTN DIV (FDNY)") %>%
  distinct() 
  mutate(unit=str_sub(to,1,4))

unit_req <- Req_unit %>%
  left_join(hierarchy_list,by=c("hierarchy_level")) %>%
  group_by(BDE) %>%
  summarise(TotalReq=sum(na.omit(TotalReq)))

unit_forecast <- TotalSub1 %>%
  left_join(Forecast2,by = c("to"="macom","cat")) %>%
  #join(hierarchy_list, by=c("to"="BN")) %>%
  filter(from == "10 MTN DIV (FDNY)") %>%
  replace(is.na(.), 0) %>%
  mutate(percent = forecast/total_auth)

unit_forecast_bde <- unit_forecast %>%
  ungroup() %>%
  group_by(to) %>%
  summarise(forecast=sum(na.omit(forecast)),total_auth=sum(total_auth)) %>%
  left_join(TotalSub5a,by=c("to"="to")) %>%
  mutate(percent = forecast/DIV_sub)

forecast_plot <- ggplot(unit_forecast,aes(x=cat,y=to,group=from))
forecast_plot + geom_tile(aes(fill=percent)) + geom_text(aes(group=from,label=scales::percent(percent))) + 
  scale_fill_gradient(low="#f7fcb9",high="#31a354")

forecast_plot_bde <- ggplot(unit_forecast_bde,aes(x="Units",y=from,group=from))
forecast_plot_bde + geom_tile(aes(fill=percent)) + geom_text(aes(label=scales::percent(percent))) + 
  scale_fill_gradient(low="#f7fcb9",high="#31a354",limit=c(0,1))+ theme_fivethirtyeight() + labs(title="% of Auths Forecasted")

for (var in unique(unit_forecast$from)) {
  num1 <- max(na.omit(unit_forecast$percent))
  
  base::print(ggplot(unit_forecast[unit_forecast$from==var,],aes(x=cat,y=to,group=from)) +  geom_tile(aes(fill=percent)) + geom_text(aes(label=scales::percent(percent))) + 
    scale_fill_gradient(low="#f7fcb9",high="#31a354",limit=c(0,num1)) + labs(title = var)) 
}

unit_forecast_dist <- unit_forecast %>%
  group_by(from,cat) %>%
  mutate(percent_dist = total_auth/sum(total_auth))

for (var in unique(unit_forecast_dist$from)) {

  base::print(ggplot(unit_forecast_dist[unit_forecast_dist$from==var,],aes(x=cat,y=to,group=from)) +  geom_tile(aes(fill=percent_dist)) + geom_text(aes(label=scales::percent(percent_dist))) + 
                scale_fill_gradient2(low="white",mid="#a6bddb",high="#2b8cbe",midpoint = .5, limit=c(0,num1)) + labs(title = var))
}

unit_forecast_div <- TotalSub5 %>%
  #group_by(cat) %>%
  ungroup() %>%
  mutate(allocated=e581_total+rem_fore_total) %>%
  summarise(allocated_total=sum(allocated),expended_total=sum(expenditures_total),unforecasted_total=sum(unforecasted)) %>%
  gather()
 

#### Forecasts by time ####

month_forecast <- ForecastUIC %>%
  clean_names() %>%
  select(october:september) %>%
  summarise_all(funs(sum)) %>%
  gather("month","total") %>%
  mutate(order= c(1:12))

month_forecast$month <- factor(month_forecast$month, levels=c("october","november","december","january","february","march","april","may","june","july","august","september"))


total_auths <- sum(na.omit(main_df$current_authorizations))
total_req <- sum(na.omit(main_df$req_qty))
  
month_forecast_plot <- ggplot(month_forecast,aes(x=month,y=total))
month_forecast_plot + geom_point(size=3) +  geom_hline(aes(yintercept=total_req/12,linetype="Required Aveage",color="salmon"),color="salmon") +
  geom_hline(aes(yintercept=total_auths/12,linetype="Authorized Average",color="yellow4"),color="yellow4") + 
  labs(title="Forecasts by Month") + scale_linetype_manual(name = "Average", values = c(2, 2), 
                                                           guide = guide_legend(override.aes = list(color = c("yellow4", "salmon")))) +
  theme_fivethirtyeight() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + scale_y_continuous(name="Quantity", labels = comma)

#### e581 to Forecast Report ####
colors()
req_fcst <- read_excel("/data/rsworkspace/Eskew/Ammo/e581Forecast.xlsx", 
                       skip = 6)

req_fcst_16 <- read_excel("/data/rsworkspace/Eskew/Ammo/e581Forecast2016.xlsx", 
                       skip = 6)

req_fcst_17 <- read_excel("/data/rsworkspace/Eskew/Ammo/e581Forecast2017.xlsx", 
                       skip = 6)

req_fcst <- req_fcst %>%
  clean_names() %>%
  mutate(fiscal_year=2018) %>%
  arrange(dodic,month)

req_fcst_16 <- req_fcst_16 %>%
  clean_names() %>%
  mutate(fiscal_year=2016) %>%
  arrange(dodic,month)

req_fcst_17 <- req_fcst_17 %>%
  clean_names() %>%
  mutate(fiscal_year=2017) %>%
  arrange(dodic,month)

req_fcst_combine <- req_fcst %>%
  bind_rows(req_fcst_17) %>%
  bind_rows(req_fcst_16) %>%
  filter(asp %in% "SPOL"|asp %in% "SDRM")
  distinct()

req_fcst_combine$fiscal_year <- factor(req_fcst_combine$fiscal_year)

req_fcst_count <- req_fcst_combine %>%
  group_by(status) %>%
  summarise(Count=n()) %>%
  mutate(Percent=percent(Count/sum(Count)))


req_fcst_comp <- req_fcst_combine %>%
  group_by(fiscal_year) %>%
  summarise(Total_Req=sum(e581_quantity),Total_Fcst=sum(forecasts_quantity)) %>%
  mutate(Percent=percent(Total_Req/Total_Fcst)) 
  #gather("Type","Value",c(Total_Req,Total_Fcst))

#req_fcst_comp$month <- factor(req_fcst_comp$month, levels=c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))

plot_req_fcst <- ggplot(req_fcst_comp,aes(y=fiscal_year))
plot_req_fcst + geom_dumbbell(aes(x=Total_Req,xend=Total_Fcst),colour_x="orange", 
                              colour_xend="#0e668b",size_x = 3,size_xend = 3) + coord_flip() + geom_text(aes(label=Percent,x=(Total_Fcst-.5*(Total_Fcst-Total_Req)))) +
  scale_x_continuous(name="Quantity", labels = comma,limits = c(0,NA)) + scale_colour_manual(name="",labels=c("Forecasted","Requested"),values=c("red","orange")) +
  labs(title="Forecasts vs. Requests") +  theme_fivethirtyeight()


req_fcst_docnum <- req_fcst_combine %>%
  group_by(fiscal_year,document_number,dodic) %>%
  summarise(req_qty=sum(e581_quantity),fcst_qty=sum(forecasts_quantity))

req_fcst_issue_ti <- req_fcst_docnum %>%
  full_join(extotal_docnum,by=c("fiscal_year"="fiscal_year.x","document_number","dodic"="dpa")) %>%
  filter(!is.na(asp.x))

req_fcst_comp <- req_fcst_issue_ti %>%
  group_by(fiscal_year) %>%
  summarise(Total_Fcst=sum(na.omit(fcst_qty)),Total_Req=sum(na.omit(req_qty)),Total_Issue=sum(na.omit(IssueQty)),Total_TI=sum(na.omit(ReturnQty))) %>%
  gather("Category","Value",Total_Fcst:Total_TI) %>%
  group_by(fiscal_year) %>%
  #mutate(Category = factor(Category, levels = c("Total_Fcst","Total_Req","Total_Issue","Total_TI"))) %>%
  mutate(Percent=percent(Value/lag(Value)))



plot_total <- ggplot(req_fcst_comp,aes(x=fiscal_year,y=Value,label=Percent))
plot_total + geom_point(aes(color=Category),size=3) + scale_y_continuous(name="Quantity", labels = comma,limits = c(0,NA)) +  
  geom_path(aes(group=Category,color=Category))  + geom_text(data=subset(req_fcst_comp, Category %in% "Total_Req" |
                                                                                                    Category %in% "Total_TI"),
                                                                                                  aes(label=Percent),vjust=-1) +
  geom_text(data=subset(req_fcst_comp,Category %in% "Total_Issue"),
            aes(label=Percent),vjust=1.5) + theme_fivethirtyeight() + labs(title="Forecast/Req/Issue/TI")
