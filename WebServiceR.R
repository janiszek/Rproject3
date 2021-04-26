library(plyr)
library(randomForest)
library(AzureML)

ws <- workspace(id="19fa0face9694074a2dfbf665cc69942", 
                auth = "qrFxReOHoRhJOduqBbs/xQzaqneaiZt6ntFJjG2J37LeeXTe1isYvUv1S16FaXNR+Ef0WClTmcf9WFUnsIRv7g==")

#to dziala tylko dla wersji 3.5 R
irisinput<-iris[,-5]
#konwertowanie zmiennej tekstowej do factor
iris$V5 = factor(iris$V5)
model<-randomForest(V5 ~.,data = iris)

predykcja<-function(newdata)
  {require(randomForest)
   predict(model, newdata, type="response")}

predykcjaSerwis<-publishWebService(
  ws,
  fun = predykcja,
  name = "irispredykcja",
  inputSchema=irisinput
)