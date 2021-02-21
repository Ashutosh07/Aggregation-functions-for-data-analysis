the.data <- as.matrix(read.table("Forest718.txt"))
the.data
my.data <- the.data[sample(1:517,200),c(1:13)]
my.data
head(my.data)
#scatterplot data
par(mfrow=c(2,2))
plot(my.data[,8],my.data[,13],main="ISI vs Area",xlab="area",ylab = "ISI")
plot(my.data[,9],my.data[,13],main="temp vs Area",xlab="area",ylab = "temp")
plot(my.data[,10],my.data[,13],main="RH vs Area",xlab="area",ylab = "RH")
plot(my.data[,11],my.data[,13],main="wind vs Area",xlab="area",ylab = "wind")

#histogramn data
par(mfrow=c(3,2))
hist(my.data[,8],main="ISI")
hist(my.data[,9],main="temp")
hist(my.data[,10],main="RH")
hist(my.data[,11],main="wind")
hist(my.data[,13],main="Y:area")


#scaling the data
transformed.data1=(my.data[,8]-min(my.data[,8]))/(max(my.data[,8])-min(my.data[,8]))
transformed.data2=(my.data[,9]-min(my.data[,9]))/(max(my.data[,9])-min(my.data[,9]))
transformed.data3=(my.data[,10]-min(my.data[,10]))/(max(my.data[,10])-min(my.data[,10]))
transformed.data4=(my.data[,11]-min(my.data[,11]))/(max(my.data[,11])-min(my.data[,11]))
transformed.data5=(my.data[,13]-min(my.data[,13]))/(max(my.data[,13])-min(my.data[,13]))
#viewing data for checking
par(mfrow=c(3,2))
hist(transformed.data1,main="Histogram of transformed.data1",xlab = "transformed.data1",ylab = "Frequency")
hist(transformed.data2,main="Histogram of transformed.data2",xlab = "transformed.data2",ylab = "Frequency")
hist(transformed.data3,main="Histogram of transformed.data3",xlab = "transformed.data3",ylab = "Frequency")
hist(transformed.data4,main="Histogram of transformed.data4",xlab = "transformed.data4",ylab = "Frequency")
hist(transformed.data5,main="Histogram of transformed.data5",xlab = "transformed.data5",ylab = "Frequency")


#transforming the data and merging all data to a single variable
tr_data = array(0,c(200,5))
tr_data[,1]=tranf1=tranformed.data1^0.37
tr_data[,2]=tranf2=tranformed.data2^1.1
tr_data[,3]=tranf3=transformed.data3^0.7
tr_data[,4]=tranf4=transformed.data4^0.75
tr_data[,5]=tranf5=transformed.data5^0.9
#viewing normalisation through histograms
par(mfrow=c(3,2))
hist(tranf1,main = "ISI",xlab = "tranf1",ylab = "Frequency")
hist(tranf2,main = "temp",xlab = "tranf2",ylab = "Frequency")
hist(tranf3,main = "RH",xlab = "tranf3",ylab = "Frequency")
hist(tranf4,main = "wind",xlab = "tranf4",ylab = "Frequency")
hist(tranf5,main = "Burned Area",xlab = "tranf5",ylab = "Frequency")

#creating a table file of all the transformed data
write.table(tr_data,"Ashutosh-transformed1.txt")


#Build models and investigate the importance of each variable
source ("AggWaFit718.R")

#calculating weighted arithmetic mean
W1=fit.QAM(tr_data,output.1="WAMoutput1.txt",stats.1="WAMstats1.txt",g=AM,g.inv=invAM)
#calculating weighted Power mean with p=0.5
W2= fit.QAM(tr_data,g=PM05,g.inv=invPM05,output.1="PM05output1.txt",stats.1="PM05stats1.txt")
#calculating weighted Power mean with p=2
W3= fit.QAM(tr_data,g=QM,g.inv=invQM,output.1="PM2output1.txt",stats.1="PM2stats1.txt")
#calculating Ordered weighted Averaging
W4=fit.OWA(tr_data,output.1="OWAoutput1.txt",stats.1="OWAstats1.txt")
#calculating Choquet Integral
W5=fit.choquet(tr_data,output.1="choquetoutput1.txt",stats.1="choquetstats1.txt")




#Using model for prediction
X8=7.6
X9=24.6
X10=44
X11=4

choquet_weights=c(0,0.564591520748351,0.611828272513259, 0,0,0.564591520748351,0.611828272513259,0,0,0.564591520748351,0.747777811857352,0,0.164819948552539,0.564591520748351,1)
X8=(X8-min(my.data[,8]))/(max(my.data[,8])-min(my.data[,8]))
X9=(X9-min(my.data[,9]))/(max(my.data[,9])-min(my.data[,9]))
X10=(X10-min(my.data[,10]))/(max(my.data[,10])-min(my.data[,10]))
X11=(X11-min(my.data[,11]))/(max(my.data[,11])-min(my.data[,11]))
choquet_inputs=c(X8,X9,X10,X11)
Final_Output=choquet(choquet_inputs,choquet_weights)
#De-transforming the output values
The_final_output=(Final_Output*(max(my.data[,13])-min(my.data[,13])))+min(my.data[,13])
The_final_output#This is the Output of the solution i.e. the burned area
