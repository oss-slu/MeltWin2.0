counter <- 1
start <- 1
molStateVal <- ""
helix <- c()

#Connector class that houses MeltR code
#constructObject() has to be called for each new method implemented. 
connector <- setRefClass(Class = "connector",
                         fields = list(df = "data.frame", 
                                       NucAcid = "character",
                                       blank = "double",
                                       Mmodel = "character",
                                       ranges = "list"
                         ),
                         methods = list(
                           #Creates MeltR object. 
                           constructObject = function(){
                             meltR.A(data_frame = df,
                                     blank = blank,
                                     NucAcid = helix,
                                     Mmodel = "Heteroduplex.2State",
                                     fitTs = ranges
                             )
                           },
                           #Constructs a plot containing the raw data
                           constructRawPlot = function(sampleNum){
                             data = df[df$Sample == sampleNum,]
                             ggplot(data, aes(x = Temperature, y = Absorbance)) +
                               geom_point() +
                               theme_classic()
                           },
                           constructFirstDerivative = function(sampleNum){
                             data = constructObject()
                             data = data$Derivatives.data[data$Derivatives.data == sampleNum,]
                             coeff = 4000
                             upper = max(data$dA.dT)/max(data$Ct)+coeff
                           },
                           #Constructs a plot of the first derivaitve and the raw data
                           constructFirstDerivative = function(sampleNum){
                             data = constructObject()
                             data = data$Derivatives.data[data$Derivatives.data == sampleNum,]
                             coeff = 4000 #Static number to shrink data to scale
                             upper = max(data$dA.dT)/max(data$Ct) + coeff
                             
                             ggplot(data,aes(x = Temperature)) +
                               geom_point(aes(y = Absorbance)) +
                               geom_point(aes(y = (dA.dT/(Pathlength*Ct))/upper+min(Absorbance)),color="blue") +
                               theme_classic()
                           }
                         )
)

