library(shiny)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  # setwd('U:\\natercia\\student_pred_tool')
  dat=read.csv('naive bayes results.csv',as.is=T)
  dat=dat[order(dat$week),]
  
  ltt=c('A','B','C','D...E')
  ltt1=c('A','B','C','D & E')
  nltt=length(ltt)
  cores=c('chartreuse4','gold','red','purple')
  # input=list()
  # input$ufid='1783-1385'
  # input$email='nandreacchio@ufl.edu'
  
  output$plot1 <- renderPlot({
    #show results for a specific student
    cond=!is.na(dat$SIS.User.ID) & dat$SIS.User.ID==gsub('-','',input$ufid) & 
         !is.na(dat$SIS.Login.ID) & dat$SIS.Login.ID==input$email & 
         !is.na(dat$passwd) & dat$passwd==input$passwd &
         !is.na(dat$trat) & dat$trat==3 #third treatment (students selected for predictive analytics)
    tmp=dat[cond,]
    
    #if student not found
    if (sum(cond)==0) {
      plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
      text(0.5,0.5,'User not found',col='red')
    }
    
    #if student found
    if (sum(cond)>0){
      plot(NA,xlim=c(0,13),ylim=c(0,1),xlab='Week',ylab='Probability',main='Prediction history')
      for (i in 1:nltt){
        lines(tmp$week,tmp[,ltt[i]],col=cores[i],lwd=2)
      }
      legend(x=0,y=1,col=cores,lty=1,legend=ltt1)
    }
  })
  
  output$plot2 <- renderPlot({
    #show results for a specific student
    cond=!is.na(dat$SIS.User.ID) & dat$SIS.User.ID==gsub('-','',input$ufid) & 
      !is.na(dat$SIS.Login.ID) & dat$SIS.Login.ID==input$email & 
      !is.na(dat$passwd) & dat$passwd==input$passwd &
      !is.na(dat$trat) & dat$trat==3 #third treatment (students selected for predictive analytics)
    tmp=dat[cond,]
    
    #if student not found
    if (sum(cond)==0) {
      plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
      text(0.5,0.5,'User not found',col='red')
    }
    
    #if student found
    if (sum(cond)>0){
      max.week=max(tmp$week)
      cond=tmp$week==max.week
      tmp1=tmp[cond,]
      plot(1:4,tmp1[,ltt],type='h',ylab='Probability',xaxt='n',xlab='Predicted grade',col=cores,lwd=2,
           main=paste('Prediction for week ',max.week))
      axis(side=1,at=1:4,ltt1)
    }    
  })
})
