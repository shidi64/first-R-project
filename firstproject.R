
Perfect_wine <- data.frame(matrix(nrow = nrow(Result), ncol = 3))
row.names(Perfect_wine) <- row.names(Result)
colnames(Perfect_wine)<- c("Cantina Diomede","Maglari","Cuvee Jean-Louis")

for (var_name in row.names(Result)){
  print (var_name)
  Perfect_wine[var_name,1] <- sum(Result[var_name,11]*abs(Result[var_name,1]-Result[var_name,21]),
                                  Result[var_name,12]*abs(Result[var_name,2]-Result[var_name,22]),
                                  Result[var_name,13]*abs(Result[var_name,3]-Result[var_name,23]),
                                  Result[var_name,14]*abs(Result[var_name,4]-Result[var_name,24]),
                                  Result[var_name,15]*abs(Result[var_name,5]-Result[var_name,25]),
                                  Result[var_name,16]*abs(Result[var_name,6]-Result[var_name,26]),
                                  Result[var_name,17]*abs(Result[var_name,7]-Result[var_name,27]),
                                  Result[var_name,18]*abs(Result[var_name,8]-Result[var_name,28]),
                                  Result[var_name,19]*abs(Result[var_name,9]-Result[var_name,29]),
                                  Result[var_name,20]*abs(Result[var_name,10]-Result[var_name,30])
  )
  Perfect_wine[var_name,2] <- sum(Result[var_name,11]*abs(Result[var_name,1]-Result[var_name,31]),
                                  Result[var_name,12]*abs(Result[var_name,2]-Result[var_name,32]),
                                  Result[var_name,13]*abs(Result[var_name,3]-Result[var_name,33]),
                                  Result[var_name,14]*abs(Result[var_name,4]-Result[var_name,34]),
                                  Result[var_name,15]*abs(Result[var_name,5]-Result[var_name,35]),
                                  Result[var_name,16]*abs(Result[var_name,6]-Result[var_name,36]),
                                  Result[var_name,17]*abs(Result[var_name,7]-Result[var_name,37]),
                                  Result[var_name,18]*abs(Result[var_name,8]-Result[var_name,38]),
                                  Result[var_name,19]*abs(Result[var_name,9]-Result[var_name,39]),
                                  Result[var_name,20]*abs(Result[var_name,10]-Result[var_name,40])
  )
  Perfect_wine[var_name,3] <- sum(Result[var_name,11]*abs(Result[var_name,1]-Result[var_name,41]),
                                  Result[var_name,12]*abs(Result[var_name,2]-Result[var_name,42]),
                                  Result[var_name,13]*abs(Result[var_name,3]-Result[var_name,43]),
                                  Result[var_name,14]*abs(Result[var_name,4]-Result[var_name,44]),
                                  Result[var_name,15]*abs(Result[var_name,5]-Result[var_name,45]),
                                  Result[var_name,16]*abs(Result[var_name,6]-Result[var_name,46]),
                                  Result[var_name,17]*abs(Result[var_name,7]-Result[var_name,47]),
                                  Result[var_name,18]*abs(Result[var_name,8]-Result[var_name,48]),
                                  Result[var_name,19]*abs(Result[var_name,9]-Result[var_name,49]),
                                  Result[var_name,20]*abs(Result[var_name,10]-Result[var_name,50])
  )
}
  
vseznach<- data.frame(matrix(nrow = 14,ncol = 3))
colnames(vseznach)<- c("Cantina Diomede","Maglari","Cuvee Jean-Louis")
rownames(vseznach)<- c("Медиана","Среднее арифметическое","Стандартное отклонение",
                         "Коэффициент вариации","Максимум","Минимум","Размах","Коэффициент ассиметрии",
                         "Коэффициент эксцесса","Верхняя квартиль",
                       "Нижняя квартиль","Мода","Коэффициент дифференциации","Дисперсия")
find_mode<-function(x){
  freq <- table(x)
  max_freq <-max(freq)
  modes <-as.numeric(names(freq[freq == max_freq]))
  return(modes)}
for (var_name in colnames(Perfect_wine)){
  print (var_name)
  n <- length(row.names(Perfect_wine))
  k1 <- (n^2)-2*n+3
  k2<- (n-1)*(n-2)*(n-3)
  vseznach[2,var_name] <- mean(Perfect_wine[ ,var_name])
  vseznach[1,var_name] <- median(Perfect_wine[ ,var_name])
  vseznach[3,var_name]<- sd(Perfect_wine[ , var_name])
  vseznach[4,var_name]<- vseznach[3,var_name]/ abs(vseznach[2,var_name])*100
  vseznach[5,var_name]<- max(Perfect_wine[ ,var_name])
  vseznach[6,var_name]<- min(Perfect_wine[ ,var_name])
  vseznach[7,var_name]<- vseznach[5,var_name] - vseznach[6,var_name]
  vseznach[8,var_name]<-(n/((n-1)*(n-2)))*sum(((Perfect_wine[ , var_name]-vseznach[2,var_name])/vseznach[3,var_name])^3)
  vseznach[10:11, var_name]<- quantile(Perfect_wine[ , var_name], probs = c( 0.75, 0.25))
  vseznach[9,var_name]<-(k1*(sum((Perfect_wine[ ,var_name]-vseznach[2,var_name])^4))
                          +3*(2*n-3)*sum((Perfect_wine[ ,var_name]-vseznach[2,var_name])^2))/(k2*((vseznach[3,var_name])^4))-3
  vseznach[13,var_name]<-vseznach[10,var_name]/vseznach[11,var_name]
  vseznach[12,var_name]<- paste0(find_mode(Perfect_wine[ , var_name]),collapse = " ")
  vseznach[14,var_name]<-var(Perfect_wine[ , var_name])
}  

##6zadanie
ideal_first <- data.frame(matrix(nrow = nrow(Result), ncol = 10))
row.names(ideal_first)<-row.names(Result)
colnames(ideal_first)<-c("мероприятия","крепость","сладость","кислотность","танины",
                         "тельность","объем","сорт","цена","выдержка")
for (var_name in row.names(Result)){
  print (var_name)
  ideal_first[var_name,1]<-Result[var_name,1]
  ideal_first[var_name,2]<-Result[var_name,2]
  ideal_first[var_name,3]<-Result[var_name,3]
  ideal_first[var_name,4]<-Result[var_name,4]
  ideal_first[var_name,5]<-Result[var_name,5]
  ideal_first[var_name,6]<-Result[var_name,6]
  ideal_first[var_name,7]<-Result[var_name,7]
  ideal_first[var_name,8]<-Result[var_name,8]
  ideal_first[var_name,9]<-Result[var_name,9]
  ideal_first[var_name,10]<-Result[var_name,10]

}
ideal <- data.frame(matrix(nrow = 4, ncol = 10))
colnames(ideal) <- c("мероприятия","крепость","сладость","кислотность","танины",
                     "тельность","объем","сорт","цена","выдержка")
row.names(ideal)<- c("Мода","Медиана","Среднее арифметическое",
                     "Стандартное отклонение")
for (var_name in colnames(ideal_first)){
  print (var_name)
  ideal[2,var_name] <- median(ideal_first[ ,var_name])
  ideal[3,var_name] <- mean(ideal_first[ ,var_name])
  ideal[4,var_name]<- sd(ideal_first[ , var_name])
  ideal[1,var_name]<- which.max(tabulate(ideal_first[ , var_name]))
  
  
}
##7
table7 <- data.frame(matrix(nrow = 3, ncol = 10))
colnames(table7) <- c("мероприятия","крепость","сладость","кислотность","танины",
                     "тельность","объем","сорт","цена","выдержка")

row.names(table7)<- c("Мода","Медиана","Среднее арифметическое")
for (var_name in colnames(ideal_first)){
  print (var_name)
  table7[1,var_name]<-ideal[1,var_name]
  table7[2,var_name]<-ideal[2,var_name]
  table7[3,var_name]<-ideal[3,var_name]
  
  
}
table7.1 <- data.frame(matrix(nrow = 5, ncol = 10))
colnames(table7.1) <- c("мероприятия","крепость","сладость","кислотность","танины",
                      "тельность","объем","сорт","цена","выдержка")

row.names(table7.1)<- c("Максимум","Минимум","Мода","Медиана","Среднее арифметическое")
for (var_name in colnames(ideal_first)){
  print (var_name)
  table7.1[3,var_name]<-ideal[1,var_name]
  table7.1[4,var_name]<-ideal[2,var_name]
  table7.1[5,var_name]<-ideal[3,var_name]
  table7.1[1,var_name]<-max(ideal_first[ , var_name])
  table7.1[2,var_name]<-min(ideal_first[ , var_name])

  
}


#Коды для графиков
#столбчатая диаграмма
par(mar= c(10,4,4,2))
barplot(as.matrix(table7), beside = TRUE, col = c("tomato1", "thistle3", "royalblue1"),
        main = "Средние характеристики",
        ylab = "Значения",las =2
)
legend("bottomright", legend = c("Мода", "Медиана", "Среднее"), 
       col = c("tomato1", "thistle3", "royalblue1"), pch = 15, pt.cex = 1)

#Паутинчатая
library(fmsb)

radarchart(table7.1,
           axistype = 1,
           pcol = c("tomato1", "thistle3", "royalblue1"),
           plwd = 2,
           plty = 1,
           title = "Паутинчатая диаграмма")

legend("topright", legend = c("Мода", "Медиана", "Среднее арифметическое"), 
       col = c("tomato1", "thistle3", "royalblue1"), pch = 15, pt.cex = 1)


#линейная

inds <- order(Perfect_wine$`Cantina Diomede`, decreasing = TRUE)
ods <- order(Perfect_wine$Maglari, decreasing = TRUE)
rds <- order(Perfect_wine$`Cuvee Jean-Louis`, decreasing = TRUE)


plot(x = Perfect_wine$`Cantina Diomede`[inds],  
     main = "Сравнение вин", 
     ylab = "Значения",  
     xlab = "Респонденты", 
     col = "skyblue3",lwd = 2,type = "l",
     ylim = c(min(c(Perfect_wine$`Cantina Diomede`, Perfect_wine$Maglari, Perfect_wine$`Cuvee Jean-Louis`)) - 10, max(c(Perfect_wine$`Cantina Diomede`, Perfect_wine$Maglari, Perfect_wine$`Cuvee Jean-Louis`)) + 10))


lines(x = Perfect_wine$Maglari[ods], col ="tomato1", lwd = 2,type = "l")
lines(x = Perfect_wine$`Cuvee Jean-Louis`[rds], col ="olivedrab4", lwd = 2,type = "l")


legend(x = "topright", legend = c("Cantina Diomede", "Maglari", "Cuvee Jean-Louis"),  
       col = c("skyblue3", "tomato1", "olivedrab4"), 
       lty = c(1, 1, 1)) 


#коробка с усами
boxplot(Perfect_wine$`Cantina Diomede`,Perfect_wine$Maglari, Perfect_wine$`Cuvee Jean-Louis`,
        names = c("Cantina Diomede", "Maglari", "Cuvee Jean-Louis"),
        main = "Сравнение вин",
        ylab = "Оценки",
        col = c("skyblue3", "tan", "mistyrose3"),
        border = "olivedrab4")
