blank <- NULL #The blank for meltR input & background subtraction.
counter <- 1 #Same as the "start" variable, but only utilized in processing dataset loop
helix <- c() #The sequence information MeltR input.
molStateVal <- "" #Molecular state MeltR input.
myConnector = NULL #Variable in server that utilizes the "connecter" class.
start <- 1 #Number that indicates the beggining iterations when implementing multiple datasets.

#Connector class that houses MeltR code
#constructObject() has to be called for each new method implemented. 
connecter <- setRefClass(Class = "connecter",
                         fields = c("df",
                                    "NucAcid",
                                    "blank",
                                    "Mmodel",
                                    "object"
                         ),
                         methods = list(
                           #Creates MeltR object. 
                           constructObject = function(){
                             .self$object <- meltR.A(data_frame = df,
                                                     blank = blank,
                                                     NucAcid = NucAcid,
                                                     Mmodel = Mmodel)
                           },
                           #Constructs a plot containing the raw data
                           constructRawPlot = function(sampleNum){
                             data = df[df$Sample == sampleNum,]
                             ggplot(data, aes(x = Temperature, y = Absorbance)) +
                               geom_point() +
                               theme_classic()
                           },
                           #Constructs a plot of the first derivaitve and the raw data
                           constructFirstDerivative = function(sampleNum){
                             data = .self$object$Derivatives.data[.self$object$Derivatives.data == sampleNum,]
                             coeff = 4000 #Static number to shrink data to scale
                             upper = max(data$dA.dT)/max(data$Ct) + coeff
                             ggplot(data,aes(x = Temperature)) +
                               geom_point(aes(y = Absorbance)) +
                               geom_point(aes(y = (dA.dT/(Pathlength*Ct))/upper+min(Absorbance)),color="blue") +
                               theme_classic()
                           },
                           #Constructs a plot of the best fit and the raw data
                           constructBestFit = function(sampleNum){
                             data = .self$object$Method.1.data
                             data = data[data$Sample == sampleNum,]
                             ggplot(data,aes(x = Temperature)) +
                               geom_point(aes(y = Absorbance), color = "black") +
                               geom_line(aes(y = Model), color = "red") +
                               theme_classic()
                           },
                           #Constructs a plot of the best fit, first derivative, and the raw data
                           constructBoth = function(sampleNum){
                             data1 = .self$object$Derivatives.data[.self$object$Derivatives.data == 4,]
                             data2 = .self$object$Method.1.data[.self$object$Method.1.data$Sample == 4,]
                             coeff = 4000 #Static number to shrink data to scale
                             upper = max(data1$dA.dT)/max(data1$Ct) + coeff
                             ggplot() + 
                               geom_point(data2,mapping = aes(x = Temperature, y = Absorbance), color = "black") + #raw
                               geom_line(data2,mapping = aes(x = Temperature, y = Model), color = "red") + #best fit 
                               geom_point(data1, mapping = aes(x = Temperature, y = (dA.dT/(Pathlength*Ct))/upper+min(Absorbance)), color = "blue") + #first derivative
                               theme_classic()
                           }
                         )
)

