
xcolors<-function(max_rank=-1) {
	if (max_rank>0 && max_rank<NROW(.color_data))
		.color_data$color_name[seq_len(max_rank)]
	else
		.color_data$color_name	
	}

name2color<-function(name,exact=TRUE,hex_only=TRUE,n=-1){
	if(exact){
		d<-.color_data[match(name,.color_data$color_name),]
	} else {
	   d<-.color_data[grep(name,.color_data$color_name),]
	   if (n>0 && nrow(d)>n) d<-d[seq_len(n),]
	}
	if (hex_only) {
		d$hex
	} else{
		d
	}
}

nearest_named<-function(color, hex_only=FALSE,max_rank=-1,Lab=TRUE){
	if (NCOL(color)==3){
		rgbcol<-color
	} else if (is.character(color)){
		rgbcol<-t(col2rgb(color))
	} else if (is.factor(color)){#sigh
		rgbcol<-t(col2rgb(as.character(color)))
	}
	if (Lab){
		labcol<-convertColor(rgbcol/255,from="sRGB",to="Lab")
		dists<-with(.color_data, (L-labcol[,1])^2+(a-labcol[,2])^2+(b-labcol[,3])^2)
	} else {
		dists<-with(.color_data, (red-rgbcol[,1])^2+(green-rgbcol[,2])^2+(blue-rgbcol[,3])^2)
	}
	if (max_rank>0) dists<-dists[seq_len(min(nrow(.color_data),max_rank))]
	closest<-which.min(dists)
	if (hex_only)
		.color_data[closest,"hex"]
	else
		.color_data[closest,]
}