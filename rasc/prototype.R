rm(list=ls(all=TRUE))
setwd('U:\\independent studies\\student_tool')
dat=read.csv('canvas_grades.csv',as.is=T)

#subset the columns that I need
ind=grep('Quiz',colnames(dat))
ind1=which(colnames(dat)%in%c('SIS.User.ID','SIS.Login.ID'))
dat1=dat[,c(ind,ind1)]

remove1=which(colnames(dat1)%in%c('Graded.Quizzes.Current.Score','Graded.Quizzes.Final.Score'))
dat2=dat1[,-remove1] 

#standardize everything according to max points
ind=grep('Quiz',colnames(dat2))
for (i in 1:length(ind)) dat2[,i]=dat2[,i]/dat2[1,i]
dat2=dat2[-1,] #remove first row because it just shows total of points

#organize data
ind1=which(colnames(dat2)%in%c('SIS.User.ID','SIS.Login.ID'))
id.info=dat2[,ind1]
grades=dat2[,-ind1]

#fix names
z=strsplit(colnames(grades),split='\\.')
nomes=rep(NA,length(z))
for (i in 1:length(z)){
  if (z[[i]][3]=='Conceptual') z[[i]][3]='Con.'
  tmp=paste(z[[i]][3],z[[i]][2])
  nomes[i]=tmp
}
colnames(grades)=nomes

#create base figure
par(mar=c(5,3,1,1))
boxplot(grades,las=2)

#show results for a specific student
cond=!is.na(id.info$SIS.User.ID) & id.info$SIS.User.ID==10000015 & 
     !is.na(id.info$SIS.Login.ID) & id.info$SIS.Login.ID=='username15'
ind=which(cond)
points(1:ncol(grades),grades[ind,],col='red',pch=19)
