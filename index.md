---
title: "Supplementary R script for Successful mobilisation strategies: Increasing low-cost participation to engender civil society in hybrid regimes"
author: "Elizaveta Kopacheva"
date: "5/3/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Merging the datasets


```{r eval=F}
#Loading the dataset with the information extracted 
#for the ROI petition-signing
roi<-read.csv("dataset_roi.csv")
roi<-roi[,c("ID",
            "votes",
            "main_topic")]
#Dataset containing the information about 
#the Change.org petition-signing
ch<-read.csv("dataset_ch.csv")
ch<-ch[,c("ID",
          "Number.of.signatures",
          "main_topic")]

#Datasets in regard to mobilisation on Twitter
#ROI petition-mentioning
tw_roi<-read.csv("twitter_roi.csv")
#subsetting the dataset
tw_roi<-tw_roi[,c("ID",
                  "Retweet",
                  "Reply",
                  "Likes",
                  "Quote",
                  "Followers",
                  "Friends")]
#Change.org petition-mentioning
tw_ch<-read.csv("twitter_ch.csv")
tw_ch<-tw_ch[,c("ID",
                "Retweet",
                "Reply",
                "Likes",
                "Quote",
                "Followers",
                "Friends")]

#Datasets in regard to mobilisation on VK
#ROI petition-mentioning
vk_roi<-read.csv("vk_roi.csv")
vk_roi<-vk_roi[,c("ID",
                  "Comment",
                  "Repost",
                  "View",
                  "Like",
                  "Followers",
                  "Friends",
                  "Groups")]
#Change.org petition-mentioning
vk_ch<-read.csv("vk_ch.csv")
vk_ch<-vk_ch[,c("ID",
                "Comment",
                "Repost",
                "View",
                "Like",
                "Followers",
                "Friends",
                "Groups")]
```


```{r eval=F}
#Aggregating the data in the mobilisation datasets
#Summing up the number of mentions, views, etc.
#install.packages("dplyr",dependencies=T)
library(dplyr)

tw_roi<-tw_roi %>% 
  group_by(ID)%>%
  summarize(tw_retw=sum(Retweet,na.rm = T),
            tw_rep=sum(Reply,na.rm = T),
            tw_like=sum(Likes,na.rm = T),
            tw_q=sum(Quote,na.rm = T),
            tw_fol=sum(Followers,na.rm = T),
            tw_fr=sum(Friends,na.rm = T))
tw_ch<-tw_ch %>% 
  group_by(ID)%>%
  summarize(tw_retw=sum(Retweet,na.rm = T),
            tw_rep=sum(Reply,na.rm = T),
            tw_like=sum(Likes,na.rm = T),
            tw_q=sum(Quote,na.rm = T),
            tw_fol=sum(Followers,na.rm = T),
            tw_fr=sum(Friends,na.rm = T))
vk_roi<-vk_roi %>% 
  group_by(ID)%>%
  summarize(vk_com=sum(Comment,na.rm = T),
            vk_rep=sum(Repost,na.rm = T),
            vk_v=sum(View,na.rm = T),
            vk_like=sum(Like,na.rm = T),
            vk_fol=sum(Followers,na.rm = T),
            vk_fr=sum(Friends,na.rm = T),
            vk_gr=sum(Groups,na.rm = T))
vk_ch<-vk_ch %>% 
  group_by(ID)%>%
  summarize(vk_com=sum(Comment,na.rm = T),
            vk_rep=sum(Repost,na.rm = T),
            vk_v=sum(View,na.rm = T),
            vk_like=sum(Like,na.rm = T),
            vk_fol=sum(Followers,na.rm = T),
            vk_fr=sum(Friends,na.rm = T),
            vk_gr=sum(Groups,na.rm = T))
```

```{r eval=F}
#Merging the datasets
roi<-left_join(roi,tw_roi)
roi<-left_join(roi,vk_roi)

ch<-left_join(ch,tw_ch)
ch<-left_join(ch,vk_ch)

#If the petition was not mentiones on SNSs, then the number of 
#mentions, views, etc. is 0
for (i in 4:length(names(roi))){
  roi[,names(roi)[i]]<-ifelse(is.na(roi[,names(roi)[i]]),0,
                              roi[,names(roi)[i]])}

for (i in 4:length(names(ch))){
  ch[,names(ch)[i]]<-ifelse(is.na(ch[,names(ch)[i]]),0,
                              ch[,names(ch)[i]])}
```

# Dimensionality reduction

```{r eval=F}
#Examining if some of the variables correlate
#Checking the Twitter variables
#In the ROI dataset
cor(roi[,c("tw_retw",
                "tw_rep",
                "tw_like",
                "tw_q",
                "tw_fol",
                "tw_fr")])
```

```{r echo=F}
library(knitr)  
knitr::include_graphics("cor_roi_tw.tiff")
```

```{r eval=F}
#In the Change.org dataset
cor(ch[,c("tw_retw",
                "tw_rep",
                "tw_like",
                "tw_q",
                "tw_fol",
                "tw_fr")])
```

```{r echo=F}
knitr::include_graphics("cor_ch_twitter.tiff")
```

All of the variables are highly correlated. In this analysis, however, the distinction between the variables should be drawn. Thus, quotes and replies are reduced to one variable, i.e., petition-mentioning, while the number of friends and followers comprise the reach of the Twitter recruiters.

```{r eval=F}
# Confirmatory factor analysis
library(psych)
ch_tw_rep_q<-fa(as.matrix(ch[,c("tw_q","tw_rep")]),
           nfactors =1, 
           residuals = TRUE )

ch_tw_reach<-fa(as.matrix(ch[,c("tw_fr","tw_fol")]),
              nfactors =1, 
              residuals = TRUE )

roi_tw_rep_q<-fa(as.matrix(roi[,c("tw_q","tw_rep")]),
                nfactors =1, 
                residuals = TRUE )
roi_tw_reach<-fa(as.matrix(roi[,c("tw_fr","tw_fol")]),
                nfactors =1, 
                residuals = TRUE )

#loadings can be checked 
#loadings(roi_tw_reach)
#summary(roi_tw_reach$scores)
```

```{r eval=F}
#Checking the VK variables
#In the Change.org dataset
cor(roi[,c("vk_com",
           "vk_rep",
           "vk_v",
           "vk_like",
           "vk_fol",
           "vk_fr",
           "vk_gr")])
```

```{r echo=F}
knitr::include_graphics("cor_roi_vk.tiff")
```

```{r eval=F}
#In the Change.org dataset
cor(ch[,c("vk_com",
           "vk_rep",
           "vk_v",
           "vk_like",
           "vk_fol",
           "vk_fr",
           "vk_gr")])
```

```{r echo=F}
knitr::include_graphics("cor_ch_vk.tiff")
```
Once again, many variables are correlated. The dimensionality is reduced as follows. Comments and replies are reduced to the petition-mentioning, while number of friends, followers, groups and, in regard to the ROI petition-mentioning, views comprise the reach of the VK recruiters.

```{r eval=F}
roi_vk_com_rep<-fa(as.matrix(roi[,c("vk_com","vk_rep")]),
                 nfactors =1, 
                 residuals = TRUE )
ch_vk_com_rep<-fa(as.matrix(ch[,c("vk_com","vk_rep")]),
                   nfactors =1, 
                   residuals = TRUE )

roi_vk_reach<-fa(as.matrix(roi[,c("vk_v",
                                "vk_fol",
                                "vk_fr",
                                "vk_gr")]),
                   nfactors =1, 
                   residuals = TRUE )
ch_vk_reach<-fa(as.matrix(ch[,c("vk_v",
                                "vk_fr",
                                "vk_gr")]),
                 nfactors =1, 
                 residuals = TRUE )
```
```{r eval=F}
#Adding new variables to the df
ch$tw_rep_q<-ch_tw_rep_q$scores
ch$tw_reach<-ch_tw_reach$scores
ch$vk_com_rep<-ch_vk_com_rep$scores
ch$vk_reach<-ch_vk_reach$scores

roi$tw_rep_q<-roi_tw_rep_q$scores
roi$tw_reach<-roi_tw_reach$scores
roi$vk_com_rep<-roi_vk_com_rep$scores
roi$vk_reach<-roi_vk_reach$scores
```

# Regression analysis

```{r eval=F}
#There are outliers in the data
#Thus, subsetting the dataset to 96% of the variation
names_var<-c("tw_rep_q",
             "tw_like",
             "tw_retw",
             "tw_reach",
             "vk_com_rep",
             "vk_like",
             "vk_reach")
for (i in names_var){
  print(summary(lm(Number.of.signatures~
              main_topic*scale(get(i)),
              ch[ch$Number.of.signatures<
                   quantile(ch$Number.of.signatures,0.96),])))
}
```

```{r echo=F}
load("reg_res.RData")
names_var<-c("tw_rep_q",
             "tw_like",
             "tw_retw",
             "tw_reach",
             "vk_com_rep",
             "vk_like",
             "vk_reach")
for (i in names_var){
  print(summary(get(paste0("reg_ch_",i))))
}

```

```{r eval=F}
for (i in names_var){
  print(summary(lm(votes~
              main_topic*scale(get(i)),
              roi[roi$votes<
                   quantile(roi$votes,0.96),])))
}
```

```{r echo=F}
for (i in names_var){
  print(summary(get(paste0("reg_roi_",i))))
}

```

```{r eval=F}
#Checking the influence of the topic alone
summary(lm(Number.of.signatures~
              main_topic,
              ch[ch$Number.of.signatures<
                   quantile(ch$Number.of.signatures,0.96),]))

summary(lm(votes~
              main_topic,
              roi[roi$votes<
                   quantile(roi$votes,0.96),]))
```

```{r echo=F}
summary(reg_ch_topic)
summary(reg_roi_topic)

```

```{r eval=F}
#Comparing the influence of the number of Twitter friends
#on the Change.org petition signing

summary(lm(Number.of.signatures~
              main_topic*tw_fr,
              ch[ch$Number.of.signatures<
                   quantile(ch$Number.of.signatures,0.96),]))

#and the number of VK friends
#on the Change.org petition signing
summary(lm(Number.of.signatures~
              main_topic*vk_fr,
              ch[ch$Number.of.signatures<
                   quantile(ch$Number.of.signatures,0.96),]))
```

```{r echo=F}
summary(reg_ch_tw_fr)
summary(reg_ch_vk_fr)

```

# Bayesian network analysis
## Structure learning
Bayesian network structure learning includes 2 steps. The first step is to infer the structure using score-based and hybrid algorithms of structure learning. Here, 2 algorithms are used. Those are HC and H2PC. To learn the structure correctly all interactions between variables associates with the Twitter and VK mobilisation were excluded. Thus, interactions of the type tw_like -> vk_like or vk_reach -> tw_retw, etc. are excluded.

```{r eval=F}
#Reading the blacklist
blacklist<-read.csv("blacklist.csv",header = F)

#Subsetting the datasets
ch_bn<-ch[,c("Number.of.signatures",
             "main_topic",
             "tw_retw",
             "tw_rep_q",
             "tw_like",
             "tw_reach", 
             "vk_com_rep", 
             "vk_like",
             "vk_reach")]
roi_bn<-roi[,c("votes",
               "main_topic",
               "tw_retw",
               "tw_rep_q",
               "tw_like",
               "tw_reach", 
               "vk_com_rep", 
               "vk_like",
               "vk_reach")]

#Applying BN structure learning
#All the learned structures are averaged
#install.packages("bnlearn",dependencies=T)
library(bnlearn)
str_ch1 = boot.strength (ch_bn, R = 5000 , 
                         algorithm = "hc",
                         algorithm.args = list(blacklist = blacklist))
avg_ch1 = averaged.network (str_ch1)

str_ch2 = boot.strength (ch_bn, R = 5000 , 
                         algorithm = "h2pc",
                         algorithm.args = list(blacklist = blacklist))
avg_ch2 = averaged.network (str_ch2)


str_roi1 = boot.strength(roi_bn, R = 5000 , 
                        algorithm = "hc",
                        algorithm.args = list(blacklist = blacklist))
avg_roi1 = averaged.network(str_roi1)

str_roi2 = boot.strength(roi_bn, R = 5000 , 
                        algorithm = "h2pc",
                        algorithm.args = list(blacklist = blacklist))
avg_roi2 = averaged.network(str_roi2)
```

The second step is to use structural equation modeling to check for the significance of the paths that are different in the structures learned by the score-based and hybrid algorithms. Thus, as a base structure (or the 0 model), we can use those links that were learned by both algorithms.

```{r eval=F}
#SEM significance-of-the-paths testing in regard to participation
#in the CHange.org petition-signing

#subsetting the dataset
ch_sem<-ch[,1:16]
#renaming the "Number.of.signatures" variable into "votes"
names(ch_sem)[2]<-"votes"

#scaling all of the continuous variables
for (i in c(2,4:16)){
  ch_sem[,i]<-scale(ch_sem[,i])
}

#install.packages("lavaan",dependencies=T)
library(lavaan)
#defining the 0 model
mod_ch0 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach"
m_ch0 <- sem(mod_ch0, data = ch_sem )
```
After the 0 model was defined, we enrich the model by adding paths learned by one algorithm but not the other one by one. After each addition, we can test the significance of the path and choose the best-fitting model.
```{r eval=F}
mod_ch1.1 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach
tw_like~main_topic+tw_reach+tw_rep_q

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch1.1 <- sem(mod_ch1.1, data = ch_sem )
mod_ch1.2 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch1.2 <- sem(mod_ch1.2, data = ch_sem )

# Comparing the fits
anova(m_ch0,m_ch1.1)
anova(m_ch0,m_ch1.2)
anova(m_ch1.1,m_ch1.2)
#m_ch1.1 has the best fit
```

```{r echo=F}
library(lavaan)
load("ch_sem.RData")
anova(m_ch0,m_ch1.1)
anova(m_ch0,m_ch1.2)
anova(m_ch1.1,m_ch1.2)
#m_ch1.1 has the best fit
```

```{r eval=F}
# adding links to the model with the best fit, i.e., m_ch1.1
mod_ch2.1 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach
tw_like~main_topic+tw_reach+tw_rep_q

vk_like~main_topic+vk_reach+vk_com_rep
vk_com_rep~main_topic+vk_reach
"
m_ch2.1 <- sem(mod_ch2.1, data = ch_sem )
mod_ch2.2 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach
tw_like~main_topic+tw_reach+tw_rep_q

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach+vk_like
"
m_ch2.2 <- sem(mod_ch2.2, data = ch_sem )

#Comparing the fits
anova(m_ch1.2,m_ch2.1)
anova(m_ch1.2,m_ch2.2)
anova(m_ch2.1,m_ch2.2)
#model m_ch1.2 has the best fit
```

```{r echo=F}
anova(m_ch1.2,m_ch2.1)
anova(m_ch1.2,m_ch2.2)
anova(m_ch2.1,m_ch2.2)
#model m_ch1.2 has the best fit
```

```{r eval=F}
#Adding the links to the model with the best fit, i.e., m_ch1.2
mod_ch3 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach+tw_rep_q
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch3 <- sem(mod_ch3, data = ch_sem )

anova(m_ch1.2,m_ch3)
#model m_ch1.2 has the best fit
```

```{r echo=F}
anova(m_ch1.2,m_ch3)
```

```{r eval=F}
mod_ch4 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach+tw_rep_q

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch4 <- sem(mod_ch4, data = ch_sem )
anova(m_ch1.2,m_ch4)
#m_ch1.2 has the best fit
```

```{r echo=F}
anova(m_ch1.2,m_ch4)
#m_ch1.2 has the best fit
```

```{r eval=F}
mod_ch5 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach

tw_retw~main_topic+tw_reach+tw_like
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch5 <- sem(mod_ch5, data = ch_sem )
anova(m_ch1.2,m_ch5)
#m_ch1.2 has the best fit

```

```{r echo=F}
anova(m_ch1.2,m_ch5)
#m_ch1.2 has the best fit
```

```{r eval=F}
mod_ch6 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch6 <- sem(mod_ch6, data = ch_sem )
anova(m_ch1.2,m_ch6)
#m6 has a slightly better fit

```

```{r echo=F}
anova(m_ch1.2,m_ch6)
#m6 has a slightly better fit
```

```{r eval=F}
mod_ch7 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach+votes
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch7 <- sem(mod_ch7, data = ch_sem )
anova(m_ch6,m_ch7)
#m_ch6 has the best fit
```

```{r echo=F}
anova(m_ch6,m_ch7)
#m_ch6 has the best fit

```

```{r eval=F}
mod_ch8 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like+votes
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch8 <- sem(mod_ch8, data = ch_sem )

anova(m_ch6,m_ch8)
#m_ch6 has the best fit
```

```{r echo=F}
anova(m_ch6,m_ch8)
#m_ch6 has the best fit
```

```{r eval=F}
mod_ch9 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach+votes

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach
"
m_ch9 <- sem(mod_ch9, data = ch_sem )

anova(m_ch6,m_ch9)
#m_ch6 has the best fit
```

```{r echo=F}
anova(m_ch6,m_ch9)
#m_ch6 has the best fit
```

```{r eval=F}
mod_ch10 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach+votes
"
m_ch10 <- sem(mod_ch10, data = ch_sem )

anova(m_ch6,m_ch10)
#m_ch6 has the best fit
```

```{r echo=F}
anova(m_ch6,m_ch10)
#m_ch6 has the best fit
```

```{r eval=F}
mod_ch11 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach
vk_com_rep~main_topic+vk_reach+vk_like
"
m_ch11 <- sem(mod_ch11, data = ch_sem )
mod_ch12 <- "
vk_reach =~ vk_v+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_reach

tw_retw~main_topic+tw_reach
tw_rep_q~main_topic+tw_reach+tw_like
tw_like~main_topic+tw_reach

vk_like~main_topic+vk_reach+vk_com_rep
vk_com_rep~main_topic+vk_reach
"
m_ch12 <- sem(mod_ch12, data = ch_sem )
anova(m_ch6,m_ch11)
anova(m_ch6,m_ch12)
#m_ch6 has the best fit
```

```{r echo=F}
anova(m_ch6,m_ch11)
anova(m_ch6,m_ch12)
#m_ch6 has the best fit
```
At the end, the structure with the best fit is learned. In this case, it is model 6. It can be visualised as follows. 
```{r}
#install.packages("lavaanPlot",dependencies = T)
library(lavaanPlot)
lavaanPlot(model =m_ch6 , coefs =T, 
           edge_options =list(fontsize =14,color ="grey"),
           node_options = list ( fontsize =16) , stars =" regress ")
```
The same procedure is repeated for the ROI petition signing.
```{r eval=F}
#SEM significance-of-the-paths testing in regard to participation
#in the ROI petition signing

#subsetting the dataset
roi_sem<-roi[,1:16]

#scaling all of the continuous variables
for (i in c(2,4:16)){
  roi_sem[,i]<-scale(roi_sem[,i])
}

mod_roi0 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach
tw_rep_q~tw_like+tw_retw
tw_like~tw_retw

vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"
m_roi0 <- sem(mod_roi0, data = roi_sem )
mod_roi1 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach
tw_rep_q~tw_like+tw_retw
tw_like~tw_retw
tw_retw~tw_reach

vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"
m_roi1 <- sem(mod_roi1, data = roi_sem )

anova(m_roi0,m_roi1)
#model 0 has a better fit
```


```{r echo=F}
load("roi_sem.RData")
anova(m_roi0,m_roi1)
#model 0 has a better fit

```

```{r eval=F}
mod_roi2 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach
tw_rep_q~tw_like+tw_retw
tw_like~tw_retw+tw_reach


vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"

m_roi2 <- sem(mod_roi2, data = roi_sem )
anova(m_roi0,m_roi2)
#m_roi2 has a better fit
```

```{r echo=F}
anova(m_roi0,m_roi2)
#m_roi2 has a better fit
```
```{r eval=F}
mod_roi3 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach
tw_rep_q~tw_like+tw_retw+tw_reach
tw_like~tw_retw+tw_reach


vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"

m_roi3 <- sem(mod_roi3, data = roi_sem )
anova(m_roi2,m_roi3)
#m_roi3 has a better fit
```
```{r echo=F}
anova(m_roi2,m_roi3)
#m_roi3 has a better fit
```

```{r eval=F}
mod_roi4 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach
tw_rep_q~tw_like+tw_retw+tw_reach
tw_like~tw_retw+tw_reach
tw_retw~votes

vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"

m_roi4 <- sem(mod_roi4, data = roi_sem )
anova(m_roi3,m_roi4)
#m_roi3 has a better fit
```

```{r echo=F}
anova(m_roi3,m_roi4)
#m_roi3 has a better fit
```
```{r eval=F}
mod_roi5 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_com_rep
tw_rep_q~tw_like+tw_retw+tw_reach
tw_like~tw_retw+tw_reach


vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"

m_roi5 <- sem(mod_roi5, data = roi_sem )
anova(m_roi3,m_roi5)
#m_roi5 has a better fit

```
```{r echo=F}
anova(m_roi3,m_roi5)
#m_roi5 has a better fit
```

```{r eval=F}
mod_roi6 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_like
tw_rep_q~tw_like+tw_retw+tw_reach
tw_like~tw_retw+tw_reach


vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"

m_roi6 <- sem(mod_roi6, data = roi_sem )
anova(m_roi5,m_roi6)
#m_roi5 has a better fit
```

```{r echo=F}
anova(m_roi5,m_roi6)
#m_roi5 has a better fit
```
```{r eval=F}
mod_roi7 <- "
vk_reach =~ vk_v+vk_fol+vk_fr+vk_gr
tw_reach =~ tw_fr+tw_fol
vk_com_rep =~ vk_com+vk_rep
tw_rep_q=~ tw_q+tw_rep

votes ~ main_topic + tw_reach+vk_com_rep+vk_like
tw_rep_q~tw_like+tw_retw+tw_reach
tw_like~tw_retw+tw_reach


vk_like~vk_com_rep+vk_reach
vk_com_rep~vk_reach
"


m_roi7 <- sem(mod_roi7, data = roi_sem )
anova(m_roi5,m_roi7)
#m_roi5 has the best fit
```
```{r echo=F}
anova(m_roi5,m_roi7)
#m_roi5 has the best fit
```

```{r}
lavaanPlot(model =m_roi5 , coefs =T, 
           edge_options =list(fontsize =14,color ="grey"),
           node_options = list ( fontsize =16) , stars =" regress ")
```
## Fitting the parameters
Once the structures are learned, we can fit BN parameters.
First of all, we need to create empty graphs for both of the networks.
```{r eval=F}
newdag_ch <- empty.graph(nodes =c("vk_reach",
                                  "tw_reach",
                                  "vk_com_rep",
                                  "tw_rep_q",
                                  "votes",
                                  "main_topic",
                                  "tw_retw",
                                  "vk_like",
                                  "tw_like")) # creating an empty DAG
newdag_roi <- empty.graph(nodes =c("vk_reach",
                                   "tw_reach",
                                   "vk_com_rep",
                                   "tw_rep_q",
                                   "votes",
                                   "main_topic",
                                   "tw_retw",
                                   "vk_like",
                                   "tw_like")) # creating an empty DAG

```
Then, we will set up all of the learned arcs.
```{r eval=F}
newdag_ch<- set.arc(newdag_ch, from ="main_topic", to ="votes")
newdag_ch<- set.arc(newdag_ch, from ="tw_reach", to ="votes")
newdag_ch<- set.arc(newdag_ch, from ="vk_reach", to ="votes")
newdag_ch<- set.arc(newdag_ch, from ="main_topic", to ="tw_retw")
newdag_ch<- set.arc(newdag_ch, from ="tw_reach", to ="tw_retw")
newdag_ch<- set.arc(newdag_ch, from ="main_topic", to ="tw_like")
newdag_ch<- set.arc(newdag_ch, from ="tw_reach", to ="tw_like")
newdag_ch<- set.arc(newdag_ch, from ="main_topic", to ="tw_rep_q")
newdag_ch<- set.arc(newdag_ch, from ="tw_reach", to ="tw_rep_q")
newdag_ch<- set.arc(newdag_ch, from ="tw_like", to ="tw_rep_q")
newdag_ch<- set.arc(newdag_ch, from ="main_topic", to ="vk_like")
newdag_ch<- set.arc(newdag_ch, from ="vk_reach", to ="vk_like")
newdag_ch<- set.arc(newdag_ch, from ="main_topic", to ="vk_com_rep")
newdag_ch<- set.arc(newdag_ch, from ="vk_reach", to ="vk_com_rep")

newdag_roi<- set.arc(newdag_roi, from ="main_topic", to ="votes")
newdag_roi<- set.arc(newdag_roi, from ="tw_reach", to ="votes")
newdag_roi<- set.arc(newdag_roi, from ="vk_reach", to ="votes")
newdag_roi<- set.arc(newdag_roi, from ="tw_retw", to ="tw_rep_q")
newdag_roi<- set.arc(newdag_roi, from ="tw_reach", to ="tw_rep_q")
newdag_roi<- set.arc(newdag_roi, from ="tw_like", to ="tw_rep_q")
newdag_roi<- set.arc(newdag_roi, from ="tw_retw", to ="tw_like")
newdag_roi<- set.arc(newdag_roi, from ="tw_reach", to ="tw_like")
newdag_roi<- set.arc(newdag_roi, from ="vk_com_rep", to ="vk_like")
newdag_roi<- set.arc(newdag_roi, from ="vk_reach", to ="vk_like")
newdag_roi<- set.arc(newdag_roi, from ="vk_reach", to ="vk_com_rep")
```
```{r eval=F}
# Subsetting the dfs
ch_fit_dat<-ch[,c("vk_reach","tw_reach",
                  "vk_com_rep","tw_rep_q",
                  "Number.of.signatures","main_topic",
                  "tw_retw",
                  "vk_like","tw_like")]
names(ch_fit_dat)[5]<-"votes"




roi_fit_dat<-roi[,c("vk_reach","tw_reach",
                    "vk_com_rep","tw_rep_q",
                    "votes","main_topic",
                    "tw_retw",
                    "vk_like","tw_like")]
```
After that, we can fit the parameters.
```{r eval=F}
fit_ch<- bn.fit (newdag_ch , data = ch_fit_dat)
fit_roi<- bn.fit (newdag_roi , data = roi_fit_dat)
```
The results are represented by Fig 3, Table 4 and Table 5 of the main manuscript.
