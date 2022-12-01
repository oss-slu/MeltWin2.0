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
                           }
                         )
)

