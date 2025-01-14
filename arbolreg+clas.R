# For decision tree model
library(rpart)
# For data visualization
library(rpart.plot)
# Contains the data
library(ISLR)
# Get the list of data sets contained in package
d <- data(package = "ISLR")
d$results[, "Item"]

data(Carseats)
# Get the variable names
names(Carseats)
dim(Carseats)

#Arbol de clasificacion
# Creates a new binary variable, High.
High = ifelse(Carseats$Sales <=8, "No", "Yes")
# Add High to the data set.
Carseats=data.frame(Carseats,High)
# Remove the Sales variable from the data.
Carseats.H <- Carseats[,-1]
# Code High as a factor variable
Carseats.H$High = as.factor(Carseats$High)
class(Carseats.H$High)

set.seed(134)
train = sample(1:nrow(Carseats.H), 200)
Carseats.train=Carseats.H[train,]
Carseats.test=Carseats.H[-train,]

High.test=High[-train]

# cp or complexity parameter determines how deep the tree will grow. Here it is assigned a small value which will allow a decesion on further pruning. That is, we want a cp value (with a more parsimonious tree) that minimizes the xerror (cross-validation error).

fit.tree = rpart(High ~ ., data=Carseats.train, method = "class", cp=0.008)
fit.tree

# Visualizar
rpart.plot(fit.tree)
# 
# El �rbol seleccionado contiene 4 variables con 5 divisiones. 
# Si observa el gr�fico y las descripciones de los nodos,
# notar� que se han producido divisiones en las variables ShelveLoc, Price, Advertising y Age. 
# Los nodos 2 y 3 se formaron dividiendo el nodo 1, el nodo ra�z, en la variable predictora ShelveLoc.
# El punto de divisi�n es ShelveLoc=Bad,Medium; es decir, el nodo 2 consta de todas las filas con el valor ShelveLoc=Bad,Medium 
#y el nodo 3 consta de todas las filas con ShelveLoc=Good. 
# La clase pronosticada para el nodo 2 es No, donde No indica ventas inferiores a $8k. 
#La p�rdida esperada es 52: este es el n�mero total de filas que se clasificar�n incorrectamente si 
# la clase predicha para el nodo se aplica a todas las filas. En concreto, del total de 154 casos, 
#52 (34%) estar�n mal clasificados y 102 (66%) correctamente.
# La clase pronosticada para el nodo 3 es S�, donde S� indica ventas superiores a $8k. La p�rdida esperada es 8, es decir, 
#8 filas se clasificar�n incorrectamente si la clase predicha para el nodo se aplica a todas las filas.
# Nuevamente, del total de 46 casos, 8 (17 %) se clasificar�n incorrectamente y 38 (83 %) se clasificar�n correctamente.


#Hacer predicciones con el modelo

# Luego, usamos el modelo de �rbol de decisiones que se cre� con datos de entrenamiento para predecir la variable de respuesta, High, 
# con el conjunto de datos de prueba. 
# Aqu� usamos la funci�n predict () para este prop�sito.
# Es importante tener en cuenta que, si bien este modelo puede producir 
# buenas predicciones en el conjunto de entrenamiento, es probable que
# sobreajuste los datos, lo que lleva a un rendimiento deficiente del 
# conjunto de prueba. Es decir, el �rbol resultante puede ser demasiado complejo.
# Sin embargo, un �rbol m�s peque�o con menos divisiones podr�a generar una 
# varianza m�s baja y una mejor interpretaci�n a costa de un peque�o sesgo.
# Como regla general, a medida que usamos m�todos m�s flexibles, 
# la varianza aumentar� y el sesgo disminuir�. 
# La poda reduce la flexibilidad del modelo. 
# M�s adelante consideraremos si la poda de este �rbol mejora el rendimiento medido por la tasa de error del conjunto de prueba. 
# Una tasa de error de test m�s baja significa un mejor rendimiento.

pred.tree = predict(fit.tree, Carseats.test, type = "class")
table(pred.tree,High.test)

# A continuaci�n, consideramos si podar el �rbol podr�a conducir a mejores resultados. 
# Es decir, queremos ver si la poda dar� como resultado un porcentaje m�s bajo de errores de clasificaci�n y, por lo tanto, un porcentaje m�s alto de predicciones correctas.
# La poda selecciona el valor cp (par�metro de complejidad) asociado con un �rbol m�s corto que minimiza la tasa de error de validaci�n cruzada (xerror). 
# Consulte la tabla a continuaci�n. Nos referimos a la tabla CP para seleccionar el valor cp que produce el error de validaci�n cruzada m�s bajo (xerror). 
#Con referencia a la tabla, podemos elegir o solicitar expl�citamente el valor de cp m�s bajo. El valor de cp m�s bajo es 0.0333333 y tiene un �rbol con 3 divisiones.

fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"] #nos da el valor mas bajo
bestcp <-fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(fit.tree, cp = bestcp)
rpart.plot(pruned.tree)


# Ahora aplicamos el �rbol podado a los datos de test.
# Ahora solo el 74 % de las observaciones de prueba se clasifican correctamente, 
# por lo que la tasa de errores de clasificaci�n es del 26 %. 
# Por el contrario, nuestro �rbol sin podar tiene una tasa de error de clasificaci�n err�nea del 24 %. 
# Por lo tanto, el modelo podado da como resultado un aumento en el sesgo medido por la tasa de error de prueba. 
# Como resultado, seleccionar�amos el �rbol m�s grande sin podar, ya que la tasa de error de prueba es m�s baja que la tasa de error de prueba del �rbol podado. 
# Recuerde que un buen clasificador es aquel en el que el error de prueba es m�nimo.
# Este ejemplo muestra que la poda no siempre es efectiva para reducir el sesgo.
# Es importante tener en cuenta que podr�amos elegir el �rbol podado si la interpretabilidad es m�s importante que un sesgo m�s bajo.

pred.prune = predict(pruned.tree, Carseats.test, type="class")
table(pred.prune, High.test)



#Arbol de regresion
# Remove the variable High variable from the data.
Carseats.S <- Carseats[,-12]

set.seed(134)
train = sample(1:nrow(Carseats.S), 200)
Carseats.train=Carseats.S[train,]
Carseats.test=Carseats.S[-train,]
# Build the regression tree on the training set
fit.tree = rpart(Sales ~ ., data=Carseats.train, method="anova", cp=0.008)
#summary(fit.tree)
fit.tree
rpart.plot(fit.tree)
#prediccion
pred.tree = predict(fit.tree, Carseats.test)
# Calcualte the mean square error
mse <- mean((pred.tree - Carseats.test$Sales)^2)
mse

bestcp <- fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
bestcp
# Prune the tree with the best cp  value (the lowest cross-validation error - xerror)
pruned.tree <- prune(fit.tree, cp = bestcp)
# Visualizing the pruned tree
rpart.plot(pruned.tree)

# Use the test data to evaluate performance of pruned regression tree
pred.prune = predict(pruned.tree, Carseats.test)

# Calcualte the MSE for the pruned tree
mse <- mean((pred.prune - Carseats.test$Sales)^2)
mse