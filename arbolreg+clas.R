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
# El árbol seleccionado contiene 4 variables con 5 divisiones. 
# Si observa el gráfico y las descripciones de los nodos,
# notará que se han producido divisiones en las variables ShelveLoc, Price, Advertising y Age. 
# Los nodos 2 y 3 se formaron dividiendo el nodo 1, el nodo raíz, en la variable predictora ShelveLoc.
# El punto de división es ShelveLoc=Bad,Medium; es decir, el nodo 2 consta de todas las filas con el valor ShelveLoc=Bad,Medium 
#y el nodo 3 consta de todas las filas con ShelveLoc=Good. 
# La clase pronosticada para el nodo 2 es No, donde No indica ventas inferiores a $8k. 
#La pérdida esperada es 52: este es el número total de filas que se clasificarán incorrectamente si 
# la clase predicha para el nodo se aplica a todas las filas. En concreto, del total de 154 casos, 
#52 (34%) estarán mal clasificados y 102 (66%) correctamente.
# La clase pronosticada para el nodo 3 es Sí, donde Sí indica ventas superiores a $8k. La pérdida esperada es 8, es decir, 
#8 filas se clasificarán incorrectamente si la clase predicha para el nodo se aplica a todas las filas.
# Nuevamente, del total de 46 casos, 8 (17 %) se clasificarán incorrectamente y 38 (83 %) se clasificarán correctamente.


#Hacer predicciones con el modelo

# Luego, usamos el modelo de árbol de decisiones que se creó con datos de entrenamiento para predecir la variable de respuesta, High, 
# con el conjunto de datos de prueba. 
# Aquí usamos la función predict () para este propósito.
# Es importante tener en cuenta que, si bien este modelo puede producir 
# buenas predicciones en el conjunto de entrenamiento, es probable que
# sobreajuste los datos, lo que lleva a un rendimiento deficiente del 
# conjunto de prueba. Es decir, el árbol resultante puede ser demasiado complejo.
# Sin embargo, un árbol más pequeño con menos divisiones podría generar una 
# varianza más baja y una mejor interpretación a costa de un pequeño sesgo.
# Como regla general, a medida que usamos métodos más flexibles, 
# la varianza aumentará y el sesgo disminuirá. 
# La poda reduce la flexibilidad del modelo. 
# Más adelante consideraremos si la poda de este árbol mejora el rendimiento medido por la tasa de error del conjunto de prueba. 
# Una tasa de error de test más baja significa un mejor rendimiento.

pred.tree = predict(fit.tree, Carseats.test, type = "class")
table(pred.tree,High.test)

# A continuación, consideramos si podar el árbol podría conducir a mejores resultados. 
# Es decir, queremos ver si la poda dará como resultado un porcentaje más bajo de errores de clasificación y, por lo tanto, un porcentaje más alto de predicciones correctas.
# La poda selecciona el valor cp (parámetro de complejidad) asociado con un árbol más corto que minimiza la tasa de error de validación cruzada (xerror). 
# Consulte la tabla a continuación. Nos referimos a la tabla CP para seleccionar el valor cp que produce el error de validación cruzada más bajo (xerror). 
#Con referencia a la tabla, podemos elegir o solicitar explícitamente el valor de cp más bajo. El valor de cp más bajo es 0.0333333 y tiene un árbol con 3 divisiones.

fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"] #nos da el valor mas bajo
bestcp <-fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(fit.tree, cp = bestcp)
rpart.plot(pruned.tree)


# Ahora aplicamos el árbol podado a los datos de test.
# Ahora solo el 74 % de las observaciones de prueba se clasifican correctamente, 
# por lo que la tasa de errores de clasificación es del 26 %. 
# Por el contrario, nuestro árbol sin podar tiene una tasa de error de clasificación errónea del 24 %. 
# Por lo tanto, el modelo podado da como resultado un aumento en el sesgo medido por la tasa de error de prueba. 
# Como resultado, seleccionaríamos el árbol más grande sin podar, ya que la tasa de error de prueba es más baja que la tasa de error de prueba del árbol podado. 
# Recuerde que un buen clasificador es aquel en el que el error de prueba es mínimo.
# Este ejemplo muestra que la poda no siempre es efectiva para reducir el sesgo.
# Es importante tener en cuenta que podríamos elegir el árbol podado si la interpretabilidad es más importante que un sesgo más bajo.

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