#######
library(rgl)
library(bmp) 
library(png)
plot3D<-function(pngFile=NULL,objData=NULL,objFile=NULL){
	### need at least one of objFile and objData
	##3D obj
	#objFile=paste0(path,imid,'.obj')
	##image format change
	#pngFile=paste0(path,imid,'.png')
	##3D data read
	#objFile='/Users/taoxianming/Documents/face_3D/RS/3D/LOC/131012090026.obj'
    if(!is.null(objFile))objData=readObj(objFile)
    objData=midProcess(objData);
    v=objData$'v';f=objData$'f';
    vn=objData$'vn';vt=objData$'vt';
	if(is.null(pngFile)){
		meshPlot=tmesh3d(vertices=v,indices=f,material=list(color=c('white')),normals =vn,texcoords =vt)
		par3d(windowRect = c(0,0,1920, 1080))##
		shade3d(meshPlot,lit=T)
	}else{
		#####mesh and plot
		meshPlot=tmesh3d(vertices=v,indices=f,material=list(color=c('white'),
		texture=pngFile),normals =vn,texcoords =vt)
		#return(meshPlot)
		###plot
		#par3d(windowRect = c(0,0,1680, 1050))##backgraound size
		#par3d(windowRect = c(0,0,1280, 800))
		par3d(windowRect = c(0,0,1920, 1080))#####par3d(viewport=c(0,0,1920,1080))
		#shade3d(meshPlot,lit=T)##light off , combinds with normals
		shade3d(meshPlot,lit=F)##lig
	}
}

bmp2png<-function(bmpFile,pngFile='no',pngDataReturn=F){
	#library(bmp) 
	#library(png)
	pngData=read.bmp(bmpFile)/255
	if(pngFile!='no')writePNG(pngData,target=pngFile)
	if(pngDataReturn)return(pngData)
	##rm png image file
	#file.remove(pngFile)
}

##mesh
getVector<-function(v,type){
	##change matrix to vector
	vrow=dim(v)[1];vcol=dim(v)[2]
    v=as.numeric(as.matrix(v))
    if(vcol==3){
    	if(type=='v'){v=rbind(v[1:vrow],v[(vrow+1):(vrow*2)],v[(vrow*2+1):(vrow*3)],1)}
    	#if(type=='vn'){v=rbind(v[1:vrow],v[(vrow+1):(vrow*2)],v[(vrow*2+1):(vrow*3)])}
    }
    #if(vcol==2){v=rbind(v[1:vrow],v[(vrow+1):(vrow*2)])}
    return(as.vector(v))
}

save2Dfrom3D<-function(jpgFile,side,zoomValue=5/15){
	####change pose, size and save
	###zoom smaller, image bigger
	if(side=='left')degree=90
	if(side=='center')degree=0
	if(side=='right')degree=-90
	rgl.viewpoint(zoom=zoomValue) #figure zoom; 
	#observer3d(0, 0, 440) # Viewed from very close up
	par3d(userMatrix = rotationMatrix(degree*pi/180, 0, 1, 0))
	rgl.snapshot(jpgFile)
}
close3D<-function()rgl.close()

readObj<-function(objFile){
	obj=read.table(objFile,stringsAsFactors=F,fill=T)
	v=apply(obj[which(obj[,1]=='v'),-1],2,as.numeric)
	vn=apply(obj[which(obj[,1]=='vn'),-1],2,as.numeric)
	vt=apply(obj[which(obj[,1]=='vt'),c(-1,-4)],2,as.numeric)
	f=obj[which(obj[,1]=='f'),-1]
	return(list(v=v,vn=vn,vt=vt,f=f))
}
midProcess<-function(objData){
    v=objData$'v';vn=objData$'vn';vt=objData$'vt';f=objData$'f';
	frow=dim(f)[1]
	ind1st=as.numeric(unlist(strsplit(f[,1],'/')))
	ind2nd=as.numeric(unlist(strsplit(f[,2],'/')))
	ind3rd=as.numeric(unlist(strsplit(f[,3],'/')))
	index=rbind(ind1st,ind2nd,ind3rd)
	####spread by face index
	fv=index[,seq(1,frow*3,3)]
	v2=v[as.vector(fv),]
	v2v=getVector(v2,'v')#get vector
	####
	fv2=1:dim(v2)[1]
	###
	fvt=index[,seq(2,frow*3,3)]#face texture index
	vt2=vt[as.vector(fvt),]
	###
	fvn=index[,seq(3,frow*3,3)]##face normal index
	vn2=vn[as.vector(fvn),]
    return(list(v=v2v,vn=vn2,vt=vt2,f=fv2))
}
mesh3D<-function(pngFile,objData=NULL,objFile=NULL){
	### need at least one of objFile and objData
	#objFile='/Users/taoxianming/Documents/face_3D/RS/3D/LOC/131012090026.obj'
    if(!is.null(objFile))objData=readObj(objFile)
    objData=midProcess(objData);
    v=objData$'v';f=objData$'f';
    vn=objData$'vn';vt=objData$'vt';
	#####mesh and plot
	meshPlot=tmesh3d(vertices=v,indices=f,material=list(color=c('white'),
		texture=pngFile),normals =vn,texcoords =vt)
    return(meshPlot)
}