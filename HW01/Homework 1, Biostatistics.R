library('tidyverse')
binom.test(12,85,p=0.5,alt='two.sided')
prop.test(12,85,p=0.5,alt='two.sided',correct=T)

# greater
k=0
for (i in c(4:6)){
  k = k + choose(6,i)*choose(28,19-i)/choose(34,19)
}

# less
k=0
for (i in c(4:0)){
  k = k + choose(6,i)*choose(28,19-i)/choose(34,19)
}

# two-sided
k=0
for (i in c(0:6)){
  r= choose(6,i)*choose(28,19-i)/choose(34,19)
  if (r<=choose(6,4)*choose(28,19-4)/choose(34,19)){
    k = k + r
  }
}
m=matrix(c(4,2,15,13),c(2,2),dimnames = list(c('control','streptokinase'),c('died','survival')))
kable(data.frame(m))
fisher.test(m,alt='greater')
fisher.test(m,alt='less')
fisher.test(m)

ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

n=function(p2){
  ans<-19.6**2*(0.16+p2*(1-p2))
  ans = ceiling_dec(ans,0)
  return (ans)
}
p2=seq(0,1,0.01)
n(p2)
plot(p2,n(p2))


n=function(p2){
  ans<-23.3**2*(0.16+p2*(1-p2))
  ans = ceiling_dec(ans,0)
  return (ans)
}
p2=seq(0,1,0.01)
n(p2)
plot(p2,n(p2))

Variable = c('ID number','Age','FEV','Height','Sex','Smoking status')
data=read.table("C:\\Users\\xx958\\OneDrive\\桌面\\nycu 碩一_上\\生物統計\\HW01\\FEV.TXT",col.names=Variable)
data['perfect_pulmonary'] = as.factor(data['FEV']>3)
data['Age_group'] = as.factor(data['Age']>6)
group_data <- data%>%
  group_by(perfect_pulmonary,Sex)%>%
  summarise(n=n())

binom.test(sum(data['FEV']>3),length(data[[1]]),p=0.25,alt='two.sided')


data[data['FEV']>3,][data[data['FEV']>3,]['Sex']==1,]

data['perfect_pulmonary'] = as.factor(data['FEV']>3)

table_Sex_and_perfect_pulmonary <- data%>%
  group_by(Sex,perfect_pulmonary)%>%
  summarise(n=n())

m = matrix(table_Sex_and_perfect_pulmonary$n,c(2,2),dimnames = list(c('Female','male'),c('no','yes')),byrow = T)

data['age_class']=1
data['age_class'][4<data['Age'] & data['Age']<10] = 1
data['age_class'][9<data['Age'] & data['Age']<15] = 2
data['age_class'][14<data['Age'] & data['Age']<20] = 3

table_age_class_and_perfect_pulmonary <- data%>%
  group_by(age_class,perfect_pulmonary)%>%
  summarise(n=n())

m = matrix(table_age_class_and_perfect_pulmonary$n,c(3,2),dimnames = list(c('5~9','10~14','15~20'),c('no','yes')),byrow = T)

m=matrix(c(243,16,83,36,60,32,31,31,39,12),c(2,5),dimnames = list(c('Number_children_with_caries','Number_children_with_caries_free_teeth'),c('Essex','Slough','Harwick','Burnham','Meresa')))
