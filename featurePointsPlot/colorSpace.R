
####
n=101
#colorGap=c("red",'white', "blue")
#colorGap=c("red", "yellow",'white')
#colorGap=c("purple", "yellow",'white')
#colorGap=c('brown','red','yellow','white')
#colorGap=c('brown','red','yellow','white')
colorGap=c('red','yellow','white')
#colorGap=c('red','yellow','blue')
#colorGap=c('red','yellow','gray')
###show
colors <- colorRampPalette(colorGap)(n)
plot(1:n, bg = colors, cex = 2, pch = 22)
##color value for importance score 0-100
colorRamp(colorGap)(seq(0,1,len=n))
##