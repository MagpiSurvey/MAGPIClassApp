library(shiny)
library(shinyjs)
library(Rfits)
library(magicaxis)
library(jpeg)

#Reading CMasher colour scales
cmscales=readRDS('www/cmr_cmaps.RDS')

################################################################################
#
#Selecting galaxies to classify
#
################################################################################
gals2class=read.csv(file='www/magpiparent_withEL.csv', stringsAsFactors=F)

#ElGISTcat=read.table(file=('www/MAGPI_master_emission_lines.tbl'), header=T)
#gals2class=merge(gals2class,ElGISTcat[,c(1,5,6,18,23)], by='MAGPIID',all.x=T)
#write.csv(gals2class,file='www/magpiparent_withEL.csv', row.names=F)

#Only loading galaxies for which either the gas or stellar kinematic maps are useful.
gals2class=gals2class[(gals2class$R50_it > 0.75*gals2class$fwhm_i) & (gals2class$re > 0.75*gals2class$fwhm_r) & (gals2class$mag_it < 26),]
gals2class=gals2class[!is.na(gals2class$MAGPIID),]
notclass=gals2class$MAGPIID

#To check specific IDs:
#gals2class=gals2class[gals2class$MAGPIID=='2304279199' | gals2class$MAGPIID=='1503273048' | gals2class$MAGPIID=='1523197197',]

################################################################################
#
#Server.R
#
################################################################################

shinyServer(
  function(input, output) {
    values <- reactiveValues(MAGPIte='anonymous', galnum = 1, EdgeCase=FALSE, Morph='0', BarFlag=FALSE, VisFeatFlag=FALSE,
                             StellRR='0',StellFeat='0',StellKinFlag=FALSE,GasRR='0',GasFeat='0',comment='',
                             GasKinFlag=FALSE,classtable=data.frame(cbind(MAGPIID=sample(gals2class$MAGPIID),
                                                         GalNum=c(1:length(gals2class$MAGPIID)),EdgeCase=FALSE,
                                                         Morph='0',BarFlag=FALSE,VisFeatFlag=FALSE,StellRR='0',StellFeat='0',
                                                         StellKinFlag=FALSE,GasRR='0',GasFeat='0',GasKinFlag=FALSE,
                                                         comment=''),stringsAsFactors = FALSE))
    VelRanges <- reactiveValues(StelVelRange=c(-500,500),StelSigRange=c(0,500),GasVelRange=c(-500,500),GasSigRange=c(0,500))
    
    observeEvent(input$MAGPIteDone, {
      values$MAGPIte <- input$MAGPIte     # if the set MAGPIte button is clicked, save name of person doing the classifying
      if (file.exists(paste0('outdir/MAGPI_class_', values$MAGPIte, '.csv'))){
        values$classtable=as.data.frame(read.csv(paste0('outdir/MAGPI_class_', values$MAGPIte, '.csv'), header = TRUE,
                                                 colClasses = c('character','integer','logical','character','logical','logical','character','character','logical','character','character','logical','character')))
        #Find galaxies that have not yet been classified:
        notclass<<-values$classtable[!values$classtable$EdgeCase & values$classtable$Morph=='0' & !values$classtable$BarFlag & !values$classtable$VisFeatFlag & values$classtable$StellRR=='0' & values$classtable$StellFeat=='0' &
                            !values$classtable$StellKinFlag & values$classtable$GasRR=='0' & values$classtable$GasFeat=='0' & !values$classtable$GasKinFlag & values$classtable$comment=='','MAGPIID']
        values$galnum=which(values$classtable$MAGPIID %in% notclass)[1]
        gals2class<<-gals2class[match(values$classtable[,'MAGPIID'],gals2class[,'MAGPIID']),]
      }
    })

    observeEvent(input$infile, {
      values$classtable=as.data.frame(read.csv(input$infile$datapath, header = TRUE))
      #Find galaxies that have not yet been classified:
      notclass=values$classtable[!values$classtable$EdgeCase & values$classtable$Morph=='0' & !values$classtable$BarFlag & !values$classtable$VisFeatFlag & values$classtable$StellRR=='0' & values$classtable$StellFeat=='0' &
                                   !values$classtable$StellKinFlag & values$classtable$GasRR=='0' & values$classtable$GasFeat=='0' & !values$classtable$GasKinFlag,'MAGPIID']
      values$galnum=which(values$classtable$MAGPIID %in% notclass)[1]
      values$classtable[is.na(values$classtable$comment),'comment']=''
      values$classtable[,'MAGPIID']=substr(as.character(as.numeric(values$classtable$MAGPIID)),start=1,stop=10)
      values$classtable[,'GalNum']=as.integer(values$classtable$GalNum)
      values$classtable[,'EdgeCase']=as.logical(values$classtable$EdgeCase)
      values$classtable[,'BarFlag']=as.logical(values$classtable$BarFlag)
      values$classtable[,'VisFeatFlag']=as.logical(values$classtable$VisFeatFlag)
      values$classtable[,'StellKinFlag']=as.logical(values$classtable$StellKinFlag)
      values$classtable[,'GasKinFlag']=as.logical(values$classtable$GasKinFlag)
    })

    observeEvent(input$plusone, {
      values$classtable[values$galnum,
        c('EdgeCase','Morph','BarFlag','VisFeatFlag','StellRR','StellFeat','StellKinFlag','GasRR','GasFeat',
          'GasKinFlag','comment')] = 
        c(values$EdgeCase,values$Morph,values$BarFlag,values$VisFeatFlag,values$StellRR,values$StellFeat,
        values$StellKinFlag,values$GasRR,values$GasFeat,values$GasKinFlag,values$comment)
      write.csv(values$classtable, file=paste('outdir/MAGPI_class_', values$MAGPIte, '.csv', sep=''), 
                quote = FALSE, row.names = FALSE)
      #if reached the end of the list, celebrate with a figure!
      if (values$galnum == length(values$classtable[,1])) {
        values$MAGPIID='Done'
        output$magpiid=renderText('All done! Congratulations!')
        output$galimage <- renderPlot(expr={
          jj <- readJPEG("www/brainsizeofaplanet.jpeg")
          plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE, asp=1)
          rasterImage(jj,0,0,1,1)
        })
        output$plotstelmaps <- renderPlot(expr={
          plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE, asp=1)
        })
        output$plotgasmaps <- renderPlot(expr={
          plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE, asp=1)
        })
      } else{
        values$galnum <- values$galnum + 1     # if the next button is clicked, increment the value by 1 and update it
        while(!(values$classtable$MAGPIID[values$galnum] %in% notclass)){
          values$galnum <- values$galnum + 1
        }
        
        reset(id='')
        values$comment=''
      }
    })

    observeEvent(input$minusone, {
      values$galnum <- values$galnum - 1     # if the previous button is clicked, increment the value by 1 and update it
      reset(id='')
    })


    output$MAGPIte=renderText(values$MAGPIte)

    output$saveClass <- downloadHandler(
      filename = function()  {
        paste('outdir/MAGPI_class_', values$MAGPIte, '_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(values$classtable, con, quote = FALSE, row.names = FALSE)
      }
    )

    output$magpiid=renderText(paste('MAGPIID = ',values$classtable$MAGPIID[values$galnum]))

    output$galimage <- renderPlot(expr={
      plotgalimage(MAGPIID=values$classtable$MAGPIID[values$galnum])
    })
    
    output$plotstelmaps <- renderPlot(expr={
      plotstelmaps(MAGPIID=values$classtable$MAGPIID[values$galnum],velrange=VelRanges$StelVelRange,sigrange=VelRanges$StelSigRange)
    })
    
    observeEvent(input$StelVelRange,{
      VelRanges$StelVelRange=input$StelVelRange
    })
    
    observeEvent(input$StelSigRange,{
      VelRanges$StelSigRange=input$StelSigRange
    })
    
    observeEvent(input$resetstelscales, {
      reset(id='StelVelRange')
      reset(id='StelSigRange')
    })
    
    output$plotgasmaps <- renderPlot(expr={
      plotgasmaps(MAGPIID=values$classtable$MAGPIID[values$galnum],velrange=VelRanges$GasVelRange,sigrange=VelRanges$GasSigRange)
    })
    
    observeEvent(input$GasVelRange,{
      VelRanges$GasVelRange=input$GasVelRange
    })
    
    observeEvent(input$GasSigRange,{
      VelRanges$GasSigRange=input$GasSigRange
    })
    
    observeEvent(input$resetgasscales, {
      reset(id='GasVelRange')
      reset(id='GasSigRange')
    })
    
    observeEvent(input$EdgeCase, {
      values$EdgeCase <- input$EdgeCase
    })
    observeEvent(input$Morph, {
      values$Morph <- substr(input$Morph,start=2,stop=2)
    })
    observeEvent(input$BarFlag, {
      values$BarFlag <- input$BarFlag
    })
    observeEvent(input$VisFeatFlag, {
      values$VisFeatFlag <- input$VisFeatFlag
    })
    observeEvent(input$StellRR, {
      values$StellRR <- substr(input$StellRR,start=2,stop=2)
    })
    observeEvent(input$StellFeat, {
      values$StellFeat <- substr(input$StellFeat,start=2,stop=2)
    })
    observeEvent(input$StellKinFlag, {
      values$StellKinFlag <- input$StellKinFlag
    })
    observeEvent(input$GasRR, {
      values$GasRR <- substr(input$GasRR,start=2,stop=2)
    })
    observeEvent(input$GasFeat, {
      values$GasFeat <- substr(input$GasFeat,start=2,stop=2)
    })
    observeEvent(input$GasKinFlag, {
      values$GasKinFlag <- input$GasKinFlag
    })
    
    observeEvent(input$addcomment, {
      values$comment <- input$comment
    })
    
    output$AnswersTable=renderTable(values$classtable)

    output$Summary=renderUI(
      tableOutput("AnswersTable")
    )
    
  }
)

################################################################################
#
#Useful functions
#
################################################################################

getmode <<- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

plotgalimage <<- function(MAGPIID, miniimpath='mini-images'){
  miniimfile=paste0(miniimpath,'/MAGPI',MAGPIID,'_mini-images.fits')
  miniimdata=Rfits_read_all(miniimfile)
  
  mask=miniimdata$MASKUNDILATED$imDat
  mask[mask==1]=NaN
  mask[mask==0]=1
  
  re=gals2class[gals2class$MAGPIID==MAGPIID,'re']
  pa=gals2class[gals2class$MAGPIID==MAGPIID,'pa']
  q=gals2class[gals2class$MAGPIID==MAGPIID,'q']
  eps=1-q
  fwhm=gals2class[gals2class$MAGPIID==MAGPIID,'fwhm_r']
  pixscale=miniimdata$RDATA$keyvalues$CD2_2*3600
  #Central spaxel
  xcen=dim(mask)[1]/2+.5
  ycen=dim(mask)[2]/2+.5
  #Generating plots on the fly.
  par(oma=c(1,1,0,0), mar=c(1,1,0,0), mfrow=c(1,1))
  
  #synthetic colour image
  magimageRGB(R=miniimdata$IDATA$imDat,G=miniimdata$RDATA$imDat,B=miniimdata$GDATA$imDat,axes=F,margins=F)
  #magimageWCSRGB(R=miniimdata$IDATA,G=miniimdata$RDATA,B=miniimdata$GDATA,coord.axis='auto',margins=F)
  
  plotrix::draw.circle(x=dim(mask)[1]-.8*max(dim(mask)),y=dim(mask)[2]-.8*max(dim(mask)),radius=0.5*fwhm/pixscale, border='white', col=NaN, lwd=3, lty=1)
  plotrix::draw.ellipse(x=xcen,y=ycen,a=re/sqrt(1-eps)/pixscale,b=re*sqrt(1-eps)/pixscale,angle=pa+90, deg=T, border='red', col=NaN, lwd=2)
  
}

plotstelmaps <<- function(MAGPIID, stelkinpath='StellKinMaps', 
                           miniimpath='mini-images', velrange=NULL,sigrange=NULL){
  stelfile=paste0(stelkinpath,'/',MAGPIID,'_kinematics_ppxf-maps.fits')
  miniimfile=paste0(miniimpath,'/MAGPI',MAGPIID,'_mini-images.fits')
  if (file.exists(stelfile)){
    steldata=Rfits_read_all(stelfile)
    steldata$V$imDat[steldata$SIGMA$imDat<10]=NaN
    steldata$V$imDat[steldata$SIGMA$imDat>3000]=NaN
    steldata$V$imDat[steldata$SNR$imDat>1e+21]=NaN
    steldata$SIGMA$imDat[steldata$SIGMA$imDat<10]=NaN
    steldata$SIGMA$imDat[steldata$SIGMA$imDat>3000]=NaN
    steldata$SIGMA$imDat[steldata$SNR$imDat>1e+21]=NaN
    #computing default ranges:
    if ((velrange[1] == -500) & velrange[2]==500){
      velrange=as.numeric(quantile(steldata$V$imDat,probs=c(0.05,0.95),na.rm=T))
    }
    if ((sigrange[1] == 0) & (sigrange[2]==500)){
      sigrange=as.numeric(quantile(steldata$SIGMA$imDat,probs=c(0.05,0.95),na.rm=T))
    }
    steldata$V$imDat[steldata$V$imDat<velrange[1]]=velrange[1]
    steldata$V$imDat[steldata$V$imDat>velrange[2]]=velrange[2]
    steldata$SIGMA$imDat[steldata$SIGMA$imDat<sigrange[1]]=sigrange[1]
    steldata$SIGMA$imDat[steldata$SIGMA$imDat>sigrange[2]]=sigrange[2]
  }
  
  miniimdata=Rfits_read_all(miniimfile)
  
  mask=miniimdata$MASKUNDILATED$imDat
  mask[mask==1]=NaN
  mask[mask==0]=1
  
  re=gals2class[gals2class$MAGPIID==MAGPIID,'re']
  pa=gals2class[gals2class$MAGPIID==MAGPIID,'pa']
  q=gals2class[gals2class$MAGPIID==MAGPIID,'q']
  eps=1-q
  fwhm=gals2class[gals2class$MAGPIID==MAGPIID,'fwhm_r']
  pixscale=miniimdata$RDATA$keyvalues$CD2_2*3600
  #Central spaxel
  xcen=dim(mask)[1]/2+.5
  ycen=dim(mask)[2]/2+.5
  
  #Generating plots on the fly.
  par(oma=c(1,1,0,0), mar=c(1,1,0,0), bg='grey')
  layout(mat=matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(5.5,0.9,5.5,0.9))
  #Stellar velocity
  if (file.exists(stelfile)){
    #Stellar velocity
    image(y=col(steldata$V$imDat)[1,],x=row(steldata$V$imDat)[,1],steldata$V$imDat*mask, 
          axes=F, asp=1, zlim=velrange, col=rev(cmscales$fusion))
    title(main=expression(paste('V'[stars],'(km/s)')), cex.main=2, line=-3)
    plotrix::draw.circle(x=dim(mask)[1]-.8*max(dim(mask)),y=dim(mask)[2]-.8*max(dim(mask)),radius=0.5*fwhm/pixscale, border='black', col=NaN, lwd=3, lty=1)
    plotrix::draw.ellipse(x=xcen,y=ycen,a=re/sqrt(1-eps)/pixscale,b=re*sqrt(1-eps)/pixscale,angle=pa+90, deg=T, border='red', col=NaN, lwd=2)
    magplot(0,0, xlim=c(1,2),side=F)
    magbar(position='right',range=velrange, col=rev(cmscales$fusion), labN=5, scale = c(0.8, 1/5))
    #Stellar velocity dispersion
    image(y=col(steldata$SIGMA$imDat)[1,],x=row(steldata$SIGMA$imDat)[,1],
          steldata$SIGMA$imDat*mask, axes=F, asp=1, zlim=sigrange, 
          col=rev(cmscales$amber))
    title(main=expression(paste(sigma[stars],'(km/s)')), cex.main=2, line=-3)
    plotrix::draw.circle(x=dim(mask)[1]-.8*max(dim(mask)),y=dim(mask)[2]-.8*max(dim(mask)),radius=0.5*fwhm/pixscale, border='black', col=NaN, lwd=3, lty=1)
    plotrix::draw.ellipse(x=xcen,y=ycen,a=re/sqrt(1-eps)/pixscale,b=re*sqrt(1-eps)/pixscale,angle=pa+90, deg=T, border='red', col=NaN, lwd=2)
    magplot(0,0, xlim=c(1,2),side=F)
    magbar(position='right',range=sigrange, col=rev(cmscales$amber), labN=5, scale = c(0.8, 1/5))
  } else{
    image(mask*0,axes=F, asp=1)
    title(main=expression(paste('V'[stars],'(km/s)')), cex.main=2, line=-3)
    mtext('No Good Spaxel', line=-15)
    magplot(1,1,xlim=c(100,100),labels=F, side=F)
    image(mask*0,axes=F,asp=1)
    title(main=expression(paste(sigma[stars],'(km/s)')), cex.main=2, line=-3)
    mtext('No Good Spaxel', line=-15)
    magplot(1,1,xlim=c(100,100),labels=F, side=F)
  }
}

plotgasmaps <<- function(MAGPIID, gaskinpath='GasKinMaps',miniimpath='mini-images', velrange=NULL, sigrange=NULL,sncut=3){
  
  gasfile=paste0(gaskinpath,'/MAGPI',MAGPIID,'_GIST_EmissionLines.fits')
  miniimfile=paste0(miniimpath,'/MAGPI',MAGPIID,'_mini-images.fits')
  if (file.exists(gasfile)){
    gasdata=Rfits_read_all(gasfile)
    #From Andrew on lines to apply quality cut to: 
    #(1) Ha for 0<z<0.424
    #(2) [OIII] for 0.424<z<0.865
    #(3) [OII] for 0.865<z<1.507
    #From Emily: 
    #The best way to solve this issue is to use the table Andrew released here: 
    #MAGPI/valueadded/analysed_v2.2.1/MAGPI_master_emission_lines.tbl . You would 
    #only need to read in columns for the OII (#5+6), OIII(#18), and Ha (#23) and 
    #determine which is the largest. This would ensure the best quality map with 
    #the highest coverage for the classification.
    Ha=gals2class$Ha[which(gals2class$MAGPIID==MAGPIID)]
    OII3730=gals2class$OII_3730[which(gals2class$MAGPIID==MAGPIID)]
    OII3727=gals2class$OII_3727[which(gals2class$MAGPIID==MAGPIID)]
    OIII=gals2class$OIII_5008[which(gals2class$MAGPIID==MAGPIID)]
    maxel=which(c(Ha, OII3727, OII3730, OIII)==max(Ha, OII3727, OII3730, OIII,na.rm = T))
    if (length(maxel)==0) {maxel=0}
    if (length(maxel)>1) {maxel=0}
    
    #reds=gals2class[gals2class$MAGPIID==MAGPIID,'z.x']
    #if (reds<0.424){
    if(maxel==1){
      gasdata$V_GAS$imDat[(gasdata$Ha_F$imDat/gasdata$Ha_FERR$imDat)<sncut]=NaN
      gasdata$SIGMA_GAS$imDat[(gasdata$Ha_F$imDat/gasdata$Ha_FERR$imDat)<sncut]=NaN
    }
    #if ((reds>0.424) & (reds<0.865)){
    if (maxel==4) {
      gasdata$V_GAS$imDat[(gasdata$OIII_5008_F$imDat/gasdata$OIII_5008_FERR$imDat)<sncut]=NaN
      gasdata$SIGMA_GAS$imDat[(gasdata$OIII_5008_F$imDat/gasdata$OIII_5008_FERR$imDat)<sncut]=NaN
    }
    #if ((reds>0.865)){
    if ((maxel==2) | (maxel==3)) {
      gasdata$V_GAS$imDat[((gasdata$OII_3730_F$imDat+gasdata$OII_3727_F$imDat)/(gasdata$OII_3730_FERR$imDat+gasdata$OII_3727$imDat))<sncut]=NaN
      gasdata$SIGMA_GAS$imDat[((gasdata$OII_3730_F$imDat+gasdata$OII_3727_F$imDat)/(gasdata$OII_3730_FERR$imDat+gasdata$OII_3727$imDat))<sncut]=NaN
    }
    
    #computing default ranges:
    if ((velrange[1] == -500) & velrange[2]==500){
      velrange=as.numeric(quantile(gasdata$V_GAS$imDat,probs=c(0.05,0.95),na.rm=T))
    }
    if ((sigrange[1] == 0) & (sigrange[2]==500)){
      sigrange=as.numeric(quantile(gasdata$SIGMA_GAS$imDat,probs=c(0.05,0.95),na.rm=T))
    }
    gasdata$V_GAS$imDat[gasdata$V_GAS$imDat<velrange[1]]=velrange[1]
    gasdata$V_GAS$imDat[gasdata$V_GAS$imDat>velrange[2]]=velrange[2]
    gasdata$SIGMA_GAS$imDat[gasdata$SIGMA_GAS$imDat<sigrange[1]]=sigrange[1]
    gasdata$SIGMA_GAS$imDat[gasdata$SIGMA_GAS$imDat>sigrange[2]]=sigrange[2]
  }
  
  miniimdata=Rfits_read_all(miniimfile)
  
  mask=miniimdata$MASKUNDILATED$imDat
  mask[mask==1]=NaN
  mask[mask==0]=1
  
  re=gals2class[gals2class$MAGPIID==MAGPIID,'re']
  pa=gals2class[gals2class$MAGPIID==MAGPIID,'pa']
  q=gals2class[gals2class$MAGPIID==MAGPIID,'q']
  eps=1-q
  fwhm=gals2class[gals2class$MAGPIID==MAGPIID,'fwhm_r']
  pixscale=miniimdata$RDATA$keyvalues$CD2_2*3600
  #Central spaxel
  xcen=dim(mask)[1]/2+.5
  ycen=dim(mask)[2]/2+.5
  
  #Generating plots on the fly.
  par(oma=c(1,1,0,0), mar=c(1,1,0,0), bg='grey')
  layout(mat=matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(5.5,0.9,5.5,0.9))
  
  if (file.exists(gasfile)){
    #Gas velocity
    image(y=col(gasdata$V_GAS$imDat)[1,],x=row(gasdata$V_GAS$imDat)[,1],gasdata$V_GAS$imDat*mask, 
            axes=F, asp=1, zlim=velrange, col=rev(cmscales$fusion))
    title(main=expression(paste('V'[gas],'(km/s)')), cex.main=2, line=-3)
    plotrix::draw.circle(x=dim(mask)[1]-.8*max(dim(mask)),y=dim(mask)[2]-.8*max(dim(mask)),radius=0.5*fwhm/pixscale, border='black', col=NaN, lwd=3, lty=1)
    plotrix::draw.ellipse(x=xcen,y=ycen,a=re/sqrt(1-eps)/pixscale,b=re*sqrt(1-eps)/pixscale,angle=pa+90, deg=T, border='red', col=NaN, lwd=2)
    magplot(0,0, xlim=c(1,2),side=F)
    magbar(position='right',range=velrange, col=rev(cmscales$fusion), labN=5, scale = c(0.8, 1/5))
    
    #Gas velocity dispersion
    image(y=col(gasdata$SIGMA_GAS$imDat)[1,],x=row(gasdata$SIGMA_GAS$imDat)[,1],
          gasdata$SIGMA_GAS$imDat*mask, axes=F, asp=1, zlim=sigrange, 
          col=rev(cmscales$amber))
    plotrix::draw.circle(x=dim(mask)[1]-.8*max(dim(mask)),y=dim(mask)[2]-.8*max(dim(mask)),radius=0.5*fwhm/pixscale, border='black', col=NaN, lwd=3, lty=1)
    plotrix::draw.ellipse(x=xcen,y=ycen,a=re/sqrt(1-eps)/pixscale,b=re*sqrt(1-eps)/pixscale,angle=pa+90, deg=T, border='red', col=NaN, lwd=2)
    title(main=expression(paste(sigma[gas],'(km/s)')), cex.main=2, line=-3)
    magplot(0,0, xlim=c(1,2),side=F)
    magbar(position='right',range=sigrange, col=rev(cmscales$amber), labN=5, scale = c(0.8, 1/5))
  } else{
    image(mask*0,axes=F,asp=1)
    title(main=expression(paste('V'[gas],'(km/s)')), cex.main=2, line=-3)
    mtext('No Good Spaxel', line=-15)
    mtext(gasfile,line=-12)
    magplot(1,1,xlim=c(100,100),labels=F, side=F)
    image(mask*0,axes=F,asp=1)
    title(main=expression(paste(sigma[gas],'(km/s)')), cex.main=2, line=-3)
    mtext('No Good Spaxel', line=-15)
    magplot(1,1,xlim=c(100,100),labels=F, side=F)
  }
}
