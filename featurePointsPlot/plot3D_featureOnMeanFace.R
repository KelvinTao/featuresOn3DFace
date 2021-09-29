
get3Dindex<-function(imp,type){
  ##10_1 importance index back to origin 3D index; 
  if (type=='hybrid'){
      markCol=(0:6)*3226;
      subType=c('h','s','v','x','y','z');
    }else{
      markCol=(0:3)*3226;
      subType=c('hx','sy','vz');
    }
  index3D=list();
  for(i in 1:(length(markCol)-1)){
    each=imp[which(imp$colname>markCol[i] & imp$colname<=markCol[i+1]),];
    each[,2]=(each[,2]-markCol[i]-1)*10+1;
    each=round(each[,2:1]);names(each)=c('point','imp')
    index3D[[i]]=each;names(index3D)[i]=subType[i];
  }
  return(index3D)
}

###
addColor<-function(typeComp,impColor,vt,pngData){
    ##points used
    VTuse=vt[typeComp[,1],];##point NO. used
    pngSize=dim(pngData)
    pngPos=cbind(VTuse[,2]*pngSize[1],VTuse[,1]*pngSize[2])
    ##to plot all the points inside
    for (pI in 1:nrow(pngPos)){
        a=pngPos[pI,1];b=pngPos[pI,2]
        if(a<pngSize[1]/2){a=ceiling(a)}else{a=floor(a)}
        if(b<pngSize[2]/2){b=ceiling(b)}else{b=floor(b)}
        pngPos[pI,]=c(a,b)
    }
    ###color by importance score
    impUse=typeComp[,2];#importance score
    for(posI in 1:nrow(pngPos)){
        colorP=as.numeric(impColor[which(impColor$imp==impUse[posI]),-1])
        pngData[pngSize[1]+1-pngPos[posI,1],pngPos[posI,2],]=colorP
    }
    return(pngData)
}

##color prepare

######color value for importance score 0-100
n=101
colorGap=c('red','yellow','blue')
#colorGap=c('red','blue')
#colorGap=c('green','brown','blue')
#colorGap=c('blue','white')#for gray surface
#colorGap=c('green','white','blue')##for colorful
#colorGap=c('green','white')
#colorGap=c('blue','green','white')
#colorGap=c('brown','red','white')
#colors <- colorRampPalette(colorGap)(n)
#plot(1:n, bg = colors, cex = 2, pch = 22)
impColor=data.frame(imp=100:0,colorRamp(colorGap)(seq(0,1,len=n))/255)
##


###3D plot path
scriptPath='/Users/taoxianming/Documents/face_3D/script/predict2018/featurePointsPlot_ok2/r3Dplot.R';
source(scriptPath)
sex=c('male','female');
type=c('hsv','vtx_g','hybrid');
methods=c('glmnet','oscorespls','svmRadial');
##mean 3D face
meanPath='/Users/taoxianming/Documents/face_3D/RS/3D/MAP_mean_VNandPNGok2';
##prediction result path
predPath='/Users/taoxianming/Documents/face_3D/RS/result/age/tune'
impPath=paste0(predPath,'/model')
impFacePath=paste0(predPath,'/impFace')
dir.create(impFacePath)
#####
for (si in seq_along(sex)){
  for (ti in seq_along(type)){
      for (mi in seq_along(methods)){
#for (si in 1:1){
#     for (ti in 1:1){
#         for (mi in 1:1){
            ##get mean face
            objFile=paste0(meanPath,'/',sex[si],'.mean.obj');
            objData=readObj(objFile)
            vt=objData$'vt'
            imgFile=paste0(meanPath,'/',sex[si],'.mean.png');
            pngData=readPNG(imgFile)
            ##background
            #pngData[,,]=0.8#for gray surface
            pngData[,,]=pngData[,,]*1.1##for colorful face
            pngData[,,3]=pngData[,,2]=pngData[,,1]##gray face
            ####get predicted v index of feature position
            impFile=Sys.glob(paste0(impPath,'/',sex[si],'*',type[ti],'*',methods[mi],'*imp.RData'))
            load(impFile);print(impFile)
            index3D=get3Dindex(impRes$impRes,type[ti]);##3 part lists; hx,sy,vz:points,imp
            for(tcn in 1:length(index3D)){#3){
                pngUse=addColor(index3D[[tcn]],impColor,vt,pngData)
                imgPlotUse=paste0(impFacePath,'/',sex[si],'.',methods[mi],'.',type[ti],'.',names(index3D)[tcn],'.png');
                writePNG(pngUse,target=imgPlotUse)
                Sys.sleep(1)
                plot3D(pngFile=imgPlotUse,objFile=objFile)
                #break
                ##save
                #if(F){
                zoomValue=c(0.7,0.7,0.7);side=c('left','center','right');
                #############
                for (i in seq_along(side)){
                    jpgFile=gsub('png',paste0(side[i],'.',zoomValue[i],'.jpg'),imgPlotUse)
                    save2Dfrom3D(jpgFile,side[i],zoomValue[i])
                }
                close3D()
                #}
            }
        }
    }
}
