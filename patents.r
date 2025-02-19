setwd("C:/Users/Khushi/OneDrive/Desktop/Project1Patents-main")
patents=read.csv("patents_1.csv",na.strings = "", header = TRUE)
head(patents)
View(patents)

# number of rows
nrow(patents)

# Summary Statistics 
summary(patents)

# Number of Total Patents by Country
PatentsByTotalNumber = patents %>% count(ee_country)

Top10ByTotal =top_n(PatentsByTotalNumber, 10)

#Kinds of patents by country
PatentsByCountry = patents %>% count(ee_country,ptype)

#Top 10 countries by type of patent 
Top10byKind = top_n(PatentsByCountry, 10)
Top10byKind

# Which category has seen the highest number of patents being filed? 
# patents %>% count(ptype)
table(patents$ptype)
plot(table(patents$ptype))
# Utility Category 


#Country having the highest number of patent applications?
barplot(table(patents$ee_country))
# The US


# Who has filed highest number of patents? 
table(patents$ee_role_desc)
# Foreign companies and corporations have filed the highest number of patents.
# U.S. state government has applied for least number of patents.


# Patent with most forward citations as of Dec 31st, 2019.
patents[which.max(patents$forward_cites),c(1,7)]
patents$forward_cites[which.max(patents$forward_cites)]
# Patent with highest forward citations is patent with patnum=12353.
#It has been citied in 893 other patents.


# Country with the highest and lowest grant duration?
patents$approval_time= patents$grantyear - patents$applyear
A=aggregate(approval_time ~ ee_country,data = patents,FUN = mean)

A$ee_country[which.min(A$approval_time)]
min(A$approval_time)

A$ee_country[which.max(A$approval_time)]
max(A$approval_time)
# United Arab Emirates (AE) has the fastest approval time.
# Netherlands Antilles (AN) has the slowest approval time.

# Organisation with highest number of patents
tail(sort(table(patents$ee_name)))
#IBM has filled for the highest number of patents followed by Samsung

# Roles vs Number of Patents
hist(table(patents$ee_role))

# Plot of patent type
pie(table(patents$ptype),radius = 1.5, cex = 1)

#number of patents granted over the years 2011-2019 based on patent type
#loop for grantyear
Year=(min(patents$grantyear):max(patents$grantyear))
Years=0
u=1
for(h in 1:9){
  for(j in 1:4){
    Years[u]=Year[h]
    u=u+1
  }
  
}
#loop for ptype
Patents_granted=0
types=c("design","plant","reissue","utility")
PatentType=0
u=1
for(h in 1:9){
  for(j in 1:4){
    PatentType[u]=types[j]
    u=u+1
  }
  
}
#loop to get number of different patent types granted over the years
k=1
for(x in 2011:2019){
  for(y in 1:4){
    c=0
    for(z in 1:nrow(patents)){
      if((x==patents$grantyear[z]) & (patents$ptype[z]==types[y]) )
        c=c+1
    }
    Patents_granted[k]=c
    k=k+1
  }
}

df=data.frame(Years,PatentType,Patents_granted)
View(df)
sum(Patents_granted)

#plot a line chart
# install.packages("packagename")
library(ggplot2)
library(hrbrthemes)
library(viridis)

df %>% ggplot(aes(x=Years, y=Patents_granted, group=PatentType, color=PatentType)) +
  geom_line() + scale_color_viridis(discrete = TRUE) +ggtitle("Different patent types granted over years") +
  theme_ipsum() + xlab("Years") + ylab("Number of Patents granted")


# Following queries are specific to US.
library(dplyr)
pat_us=filter(patents,ee_country=="US")
# pat_us = subset(patents, ee_country == "US")
nrow(pat_us)
View(pat_us)

#No. of Non-US patents
nrow(patents)-nrow(pat_us)

# Which state has filed for the highest number of patents?
table(pat_us$ee_state)
# CA, California has filed for the highest number of patents.
# HI, Hawaii has filed the lowest number of patents. 


# State with extreme approval times.
X=aggregate(approval_time ~ ee_state,data = pat_us,FUN = mean)
X$ee_state[which.min(X$approval_time)]
min(X$approval_time)
# On an average Hawaii (HI) grants a patent within a year of applying - lowest approval time.

X$ee_state[which.max(X$approval_time)]
max(X$approval_time)
# On an average Delaware (DE) grants a patent in about 3.3 years of applying - highest approval time.

# City with highest number of patents?
tail(sort(table(pat_us$ee_city)))
# Armonk city has the highest number of patents filed at 750.

# no. of patents granted over years 2011 to 2019 in the US
Year=(min(pat_us$grantyear):max(pat_us$grantyear))
Patents_granted=0
a=1
for(i in 2011:2019){
  c=0
  for(j in 1:nrow(pat_us)){
    if(i==pat_us$grantyear[j])
      c=c+1
  }
  Patents_granted[a]=c
  a=a+1
}

df=data.frame(Year,Patents_granted)
View(df)

plot(df$Year,df$Patents_granted,xlab="Year",ylab="No. of patents granted")
# there is steady increase in the number of patents being granted from the year 2011 to 2019


#number of patents granted over the years 2011-2019 based on role description
#loop for grantyear
Year=(min(pat_us$grantyear):max(pat_us$grantyear))
Years=0
u=1
for(h in 1:9){
  for(j in 1:7){
    Years[u]=Year[h]
    u=u+1
  }
  
}


#loop for role description
PatentsGranted=0
roles=c("Foreign company or corporation","Foreign government","Foreign individual","U.S. Federal government","U.S. state government","United States company or corporation","United States individual")
RoleDescription=0
u=1
for(h in 1:9){
  for(j in 1:7){
    RoleDescription[u]=roles[j]
    u=u+1
  }
  
}


#loop to get number of patents granted per year keeping the 7 role descriptions in mind in mind
k=1
for(x in 2011:2019){
  for(y in 1:7){
    c=0
    for(z in 1:nrow(pat_us)){
      if((x==pat_us$grantyear[z]) & (pat_us$ee_role_desc[z]==roles[y]) )
        c=c+1
    }
    PatentsGranted[k]=c
    k=k+1
  }
}


df2=data.frame(Years,RoleDescription,PatentsGranted)
View(df2)
sum(PatentsGranted)


#plot a line chart
df2 %>% ggplot(aes(x=Years, y=PatentsGranted, group=RoleDescription, color=RoleDescription)) +
  geom_line() + scale_color_viridis(discrete = TRUE) +ggtitle("Patents granted over years by Organisations") +
  theme_ipsum() + xlab("Years") + ylab("Number of Patents granted")
