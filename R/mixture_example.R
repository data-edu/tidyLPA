library(OpenMx)
data(myFADataRaw)

# --------------------------------------------------
# Specifying model
# --------------------------------------------------

twoFactorRaw <- myFADataRaw[,c("x1","x2","x3","y1","y2","y3")]

dataRaw      <- mxData( observed=twoFactorRaw, type="raw" )
# residual variances
resVars      <- mxPath( from=c("x1", "x2", "x3", "y1", "y2", "y3"), arrows=2,
                        free=TRUE, values=c(1,1,1,1,1,1),
                        labels=c("e1","e2","e3","e4","e5","e6") )
# latent variances and covariance
latVars      <- mxPath( from=c("F1","F2"), arrows=2, connect="unique.pairs",
                        free=TRUE, values=c(1,.5,1), labels=c("varF1","cov","varF2") )
# factor loadings for x variables
facLoadsX    <- mxPath( from="F1", to=c("x1","x2","x3"), arrows=1,
                        free=c(F,T,T), values=c(1,1,1), labels=c("l1","l2","l3") )
# factor loadings for y variables
facLoadsY    <- mxPath( from="F2", to=c("y1","y2","y3"), arrows=1,
                        free=c(F,T,T), values=c(1,1,1), labels=c("l4","l5","l6") )
# means
means        <- mxPath( from="one", to=c("x1","x2","x3","y1","y2","y3","F1","F2"),
                        arrows=1,
                        free=c(T,T,T,T,T,T,F,F), values=c(1,1,1,1,1,1,0,0),
                        labels=c("meanx1","meanx2","meanx3",
                                 "meany1","meany2","meany3",NA,NA) )

class1 <- mxModel("Class1", type="RAM",
                  manifestVars=c("x1", "x2", "x3", "y1", "y2", "y3"),
                  latentVars=c("F1","F2"),
                  dataRaw, resVars, latVars, facLoadsX, facLoadsY, means)

class2       <- mxModel(class1, name="Class2", latVars, means)

# --------------------------------------------------
# Class probabilities
# --------------------------------------------------

classP       <- mxMatrix( type="Full", nrow=2, ncol=1,
                          free=c(TRUE, FALSE), values=1, lbound=0.001,
                          labels = c("p1","p2"), name="Props" )

classS       <- mxAlgebra( Props%x%(1/sum(Props)), name="classProbs" )

# --------------------------------------------------
# Fitting model
# --------------------------------------------------

algFit       <- mxAlgebra( -2*sum(log(classProbs[1,1]%x%Class1.fitfunction
                                      + classProbs[2,1]%x%Class2.fitfunction)),
                           name="mixtureObj")

fit          <- mxFitFunctionAlgebra("mixtureObj")
dataRaw      <- mxData( observed=twoFactorRaw, type="raw" )

lpa          <- mxModel("Latent Profile Model",
                        dataRaw, class1, class2, classP, classS, algFit, fit )

lpaFit       <- mxRun(lpa, suppressWarnings=TRUE)

summary(lpaFit)
