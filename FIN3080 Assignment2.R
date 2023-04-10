library(readxl)
balance_sheet <- read_excel("~/Downloads/ass2 fin3080/Balance%20Sheet154113390/FS_Combas.xlsx")
ROE <- read_excel("~/Downloads/ass2 fin3080/Earning%20Capacity160112555/FI_T5.xlsx")
return <- read_excel("~/Downloads/ass2 fin3080/Monthly%20Stock%20Price%20%20Returns161237827/TRD_Mnth.xlsx")
vol <- read_excel("~/Downloads/ass2 fin3080/Risk%20Factor%20Indices%20of%20Individual%20Stocks%20(250-%20Trading%20Day%20Rolling%20Samples)145922846/STK_MKT_Stkbtal.xlsx")
library(fpp3)
#更改列名
colnames(balance_sheet) <- c('code','date','type','total_asset','total_lia','name')
colnames(ROE) <- c('code','date','type','ROE','name')
colnames(return) <- c('code','date','total_market_value','return_with','return_without')
colnames(vol)<-c('code','date','volatility')
colnames(vol2)<-c('code','date','volatility')
colnames(vol3)<-c('code','date','volatility')
#筛选主板股票
balance_sheet <- balance_sheet %>% filter(startsWith(code,'000') == TRUE | startsWith(code,'600') == TRUE | startsWith(code,'601') == TRUE | startsWith(code,'603') == TRUE | startsWith(code,'605') == TRUE)
return <- return %>% filter(startsWith(code,'000') == TRUE | startsWith(code,'600') == TRUE | startsWith(code,'601') == TRUE | startsWith(code,'603') == TRUE | startsWith(code,'605') == TRUE)
ROE <- ROE %>% filter(startsWith(code,'000') == TRUE | startsWith(code,'600') == TRUE | startsWith(code,'601') == TRUE | startsWith(code,'603') == TRUE | startsWith(code,'605') == TRUE)
vol1 <- vol1 %>% filter(startsWith(code,'000') == TRUE | startsWith(code,'600') == TRUE | startsWith(code,'601') == TRUE | startsWith(code,'603') == TRUE | startsWith(code,'605') == TRUE)
vol2 <- vol2 %>% filter(startsWith(code,'000') == TRUE | startsWith(code,'600') == TRUE | startsWith(code,'601') == TRUE | startsWith(code,'603') == TRUE | startsWith(code,'605') == TRUE)
vol3 <- vol3 %>% filter(startsWith(code,'000') == TRUE | startsWith(code,'600') == TRUE | startsWith(code,'601') == TRUE | startsWith(code,'603') == TRUE | startsWith(code,'605') == TRUE)
balance_sheet <- balance_sheet %>% filter(date(date)>= date('2010-01-01'))
#筛选合并报表
balance_sheet <- balance_sheet %>% filter(type=='A')
ROE <- ROE %>% filter(type=='A')
#删除部份报表）
balance <- balance_sheet %>% filter(endsWith(date,'01-01')==FALSE)
colnames(roe) <- c('code','date','type','returnA','returnB','returnC','returnTTM')
roe <- roe %>%filter(type=='A')
#讲时间改为季度
balance <- balance %>% mutate(time=yearquarter(date)) %>% select(-date,-type)
#处理季度总市值
marketvalue <- return[c('code','date','total_market_value')]
marketvalue <- marketvalue %>% mutate(time=yearquarter(date)) %>% select(-date) %>% group_by(code,time) %>% summarise(TMV=1000*mean(total_market_value))
balance <- balance %>% group_by(code,time) %>% summarise(total_asset=mean(total_asset),total_lia = mean(total_lia))
#合并balance sheet和TMV
total <- left_join(balance,marketvalue)
#计算PB
PB <- total%>% mutate(PB=TMV/(total_asset-total_lia))
Q1_PB <- PB %>% filter(time == yearquarter('2010Q4')) %>% filter(is.na(PB)==FALSE)
#计算Q1
roe <- roe %>% mutate(time=yearquarter(date)) %>% select(-date) %>% filter(is.na(returnA)==FALSE)
Q1_ROE <- ROE %>% mutate(time=yearquarter(date)) %>% filter(time == yearquarter('2010Q4')) %>% select(-date,-name) %>% filter(is.na(ROE)==FALSE)
vol <- full_join(vol1,vol2)
Q1_vol <- vol %>% mutate(time= yearquarter(date)) %>% select(-date)
Q1 <- left_join(left_join(Q1_vol,Q1_PB),roe)
Q1 <- Q1 %>% filter(is.na(PB)==FALSE & is.na(returnA)==FALSE & is.na(volatility)==FALSE) %>% select(-type)
Q1 <- Q1 %>% filter(is.na(PB)==FALSE) %>% filter(
  PB > quantile(PB, 0.25) - 3*IQR(PB) &
    PB < quantile(PB, 0.75) + 3*IQR(PB))
mod <- lm(PB ~ returnTTM+volatility,data=Q1)
summary(mod)
#生成月度PB
marketvalue <- return[c('code','date','total_market_value')]  
marketvalue <- marketvalue %>% mutate(time = yearmonth(date)) %>% select(-date)
pb1 <- left_join(marketvalue,balance)
i=3
while (i<254321){
  if (is.na(pb1[i,"total_asset"])==FALSE){
    j=pb1[i,"total_asset"]
    k=pb1[i,"total_lia"]
  }
  else{
    pb1[i,"total_asset"]=j
    pb1[i,"total_lia"]=k
  }
  i=i+1
}
pb1 <- pb1 %>% mutate(PB=total_market_value*1000/(total_asset-total_lia)) %>% filter(is.na(PB)==FALSE)
pb1 %>% group_by(time) %>% summarise(PB = median(PB)) %>% as_tsibble() %>% autoplot()
monthly_pb <- pb1 %>% select(-total_market_value,-total_asset,-total_lia)
return <- return %>% mutate(time=yearmonth(date)) %>% select(-date)
monthly_pb['time'] <- monthly_pb['time'] + 1
monthly_pb <- monthly_pb %>% filter(PB != 'Inf')
monthly_po <- left_join(monthly_pb,return) %>% select(-total_market_value)
#按照上个月的PB分组
a=yearmonth('2010 Feb')
b <- monthly_po %>% filter(time==a)
b <-b %>% mutate(tercile = ntile(PB, 10))
b <- b %>% filter(is.na(return_without)==FALSE) %>% group_by(tercile,time) %>% summarise(return=mean(return_without))
a=a+1
while (a <=yearmonth('2022 Dec')){
  c <- monthly_po %>% filter(time==a)
  c <-c %>% mutate(tercile = ntile(PB, 10))
  c <- c %>% filter(is.na(return_without)==FALSE) %>% group_by(tercile,time) %>% summarise(return=mean(return_without))
  b <- full_join(b,c)
  a=a+1
}
#画图
b_1 <- b %>% filter(tercile == 1)
b_2 <- b %>% filter(tercile == 2)
b_3 <- b %>% filter(tercile == 3)
b_4 <- b %>% filter(tercile == 4)
b_5 <- b %>% filter(tercile == 5)
b_6 <- b %>% filter(tercile == 6)
b_7 <- b %>% filter(tercile == 7)
b_8 <- b %>% filter(tercile == 8)
b_9 <- b %>% filter(tercile == 9)
b_10 <- b %>% filter(tercile == 10)
ggplot()+geom_line(data=b_1,aes(x=time,y=return),color="#FF80FF",alpha=1)+
  geom_line(data=b_2,aes(x=time,y=return),color="#FF99FF",alpha=1)+
  geom_line(data=b_3,aes(x=time,y=return),color="#FFB3FF",alpha=1)+
  geom_line(data=b_4,aes(x=time,y=return),color="#FFCCFF",alpha=1)+
  geom_line(data=b_5,aes(x=time,y=return),color="#FFE6FF",alpha=1)+
  geom_line(data=b_6,aes(x=time,y=return),color="#E6FFFF",alpha=1)+
  geom_line(data=b_7,aes(x=time,y=return),color="#CCFFFF" ,alpha=1)+
  geom_line(data=b_8,aes(x=time,y=return),color="#B3FFFF",alpha=1)+
  geom_line(data=b_9,aes(x=time,y=return),color="#99FFFF",alpha=1)+
  geom_line(data=b_10,aes(x=time,y=return),color="#80FFFF",alpha=1)+theme_bw()+labs(title='Returns of Different Groups Sorted by P/B',y='Return',x='Time')
#累积回报率绘图
e=b
i=1
while (i<1541){
  l=e[i,'return']
  e[i+10,'return'] = e[i+10,'return']+l
  i=i+1
}
ggplot(data=e,aes(x=time,y=return,color=tercile))+geom_line()

e_1 <- e %>% filter(tercile == 1)
e_2 <- e %>% filter(tercile == 2)
e_3 <- e %>% filter(tercile == 3)
e_4 <- e %>% filter(tercile == 4)
e_5 <- e %>% filter(tercile == 5)
e_6 <- e %>% filter(tercile == 6)
e_7 <- e %>% filter(tercile == 7)
e_8 <- e %>% filter(tercile == 8)
e_9 <- e %>% filter(tercile == 9)
e_10 <- e %>% filter(tercile == 10)
ggplot()+geom_line(data=e_1,aes(x=time,y=return),color="#FF80FF",alpha=1)+
  geom_line(data=e_2,aes(x=time,y=return),color="#FF99FF",alpha=1)+
  geom_line(data=e_3,aes(x=time,y=return),color="#FFB3FF",alpha=1)+
  geom_line(data=e_4,aes(x=time,y=return),color="#FFCCFF",alpha=1)+
  geom_line(data=e_5,aes(x=time,y=return),color="#FFE6FF",alpha=1)+
  geom_line(data=e_6,aes(x=time,y=return),color="#E6FFFF",alpha=1)+
  geom_line(data=e_7,aes(x=time,y=return),color="#CCFFFF" ,alpha=1)+
  geom_line(data=e_8,aes(x=time,y=return),color="#B3FFFF",alpha=1)+
  geom_line(data=e_9,aes(x=time,y=return),color="#99FFFF",alpha=1)+
  geom_line(data=e_10,aes(x=time,y=return),color="#80FFFF",alpha=1)+
  theme_bw()+labs(title='Cumulative Returns of Different Groups Sorted by P/B',y='Return',x='Time')
  
  
