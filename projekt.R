#Temat projektu: 
#Przecietne miesieczne wynagrodzenia brutto w sektorze przedsiebiorstw

#Instalacja pakietow------------------------------------------------------------

install.packages("openxlsx") #pakiet, dzieki ktoremu otworzymy plik xlsx
install.packages("dplyr")
install.packages("latticeExtra")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("tidyverse")
install.packages("e1071")
install.packages("corrplot")


#Wczytywanie pakietow i bibliotek-----------------------------------------------

library("openxlsx")
library("dplyr")
library("latticeExtra")
library("lattice")
library("ggplot2")
library("plotrix")
library("tidyverse")
library("e1071") 
library("corrplot")

#Wczytanie danych z excela i wybranie sciezki dostepu---------------------------

getwd() #sprawdzanie aktualnego katalogu roboczego


dane <- read.xlsx("tabl19_przecietne_miesieczne_wynagrodzenia_brutto_w_sektorze_przedsiebiorstw.xlsx", 
                  sheet = 1, startRow=5, colNames = T)

#sheet = 1, czyli wybieramy arkusz 1 ze skoroszytu "dane.xlsx"


#Obrobka danych-----------------------------------------------------------------

#pozbywamy sie wartosci NA
dane1 <- na.omit(dane)

#nazwy wierszy z liczb od 1 do 127 zamieniamy na pierwsza kolumne,
#w ktorej znajdowaly sie lata i miesiace kazdego pomiaru

dane2 <- dane1[,-1]
rownames(dane2) <- dane1[,1]

#zamieniamy pierwsza kolumne na numeric, bo byla character
dane2[,1] <- as.numeric(dane2[,1])

#wybieramy interesujace nas kolumny
dane3 <- dane2[,c(1,10,44)]

#zmiana nazw wybranych kolumn
colnames(dane3)[1] <- "Ogó³em"
colnames(dane3)[2] <- "Produkcja odzie¿y"
colnames(dane3)[3] <- "Informacja i komunikacja"

#tworzenie wektora z miesiacami roku 2021 i 2010
miesiace21 <- c("2021 M01","2021 M02", "2021 M03", "2021 M04", "2021 M05", "2021 M06",
              "2021 M07", "2021 M08", "2021 M09", "2021 M10", "2021 M11", "2021 M12")

miesiace10 <- c("2010 M01","2010 M02", "2010 M03", "2010 M04", "2010 M05", "2010 M06",
                "2010 M07", "2010 M08", "2010 M09", "2010 M10", "2010 M11", "2010 M12")

#wydobywanie roku 2021 i 2010
rok2021 <- dane2[rownames(dane2) %in% miesiace21,]
rok2010 <- dane2[rownames(dane2) %in% miesiace10,]

#podzial danych na lata od 2010 do 2020
rok2011 <- dane2[c(13:24),]
rok2012 <- dane2[c(25:36),]
rok2013 <- dane2[c(37:48),]
rok2014 <- dane2[c(49:60),]
rok2015 <- dane2[c(61:72),]
rok2016 <- dane2[c(73:84),]
rok2017 <- dane2[c(85:96),]
rok2018 <- dane2[c(97:108),]
rok2019 <- dane2[c(109:120),]
rok2020 <- dane2[c(121:132),]

wszystko <- data.frame(mean(rok2010$Ogó³em), mean(rok2011$Ogó³em),mean(rok2012$Ogó³em),
                       mean(rok2013$Ogó³em),mean(rok2014$Ogó³em),mean(rok2015$Ogó³em),
                       mean(rok2016$Ogó³em),mean(rok2017$Ogó³em),mean(rok2018$Ogó³em),
                       mean(rok2019$Ogó³em),mean(rok2020$Ogó³em),mean(rok2021$Ogó³em))
wszystko <- data.frame(t(wszystko))
rownames(wszystko) <- c("2010","2011","2012","2013","2014","2015","2016","2017",
                        "2018","2019","2020","2021")
colnames(wszystko) <- "Œrednie zarobki"


#Obliczanie wybranych parametrow------------------------------------------------

#srednia arytmetyczna
mean(dane3$Ogó³em) #dla wszystkich lat dla Ogolnych
mean(dane3$`Produkcja odzie¿y`)
mean(dane3$`Informacja i komunikacja`)

#rozstêp
max(dane3$Ogó³em)-min(dane3$Ogó³em)
max(dane3$`Produkcja odzie¿y`)-min(dane3$`Produkcja odzie¿y`)
max(dane3$`Informacja i komunikacja`)-min(dane3$`Informacja i komunikacja`)

#Rozstêp æwiartkowy
IQR(dane3$Ogó³em)
IQR(dane3$`Produkcja odzie¿y`)
IQR(dane3$`Informacja i komunikacja`)

#mediana
median(dane3$Ogó³em)
median(dane3$`Produkcja odzie¿y`)
median(dane3$`Informacja i komunikacja`)

#kwartyle
quantile(dane3$Ogó³em,c(0.25,0.75))
quantile(dane3$`Produkcja odzie¿y`, c(0.25,0.75))
quantile(dane3$`Informacja i komunikacja`,c(0.25,0.75))

#odchylenie standardowe
sd(dane3$Ogó³em)
sd(dane3$`Produkcja odzie¿y`)
sd(dane3$`Informacja i komunikacja`)

#wariancja
var(dane3$Ogó³em)
var(dane3$`Produkcja odzie¿y`)
var(dane3$`Informacja i komunikacja`)

#skoœnoœæ
skewness(dane3$Ogó³em)
skewness(dane3$`Produkcja odzie¿y`)
skewness(dane3$`Informacja i komunikacja`)

#kurtoza
kurtosis(dane3$Ogó³em)
kurtosis(dane3$`Produkcja odzie¿y`)
kurtosis(dane3$`Informacja i komunikacja`)

#moment próby okreœlonego rzêdu
moment(dane3$Ogó³em, order=3)
moment(dane3$`Produkcja odzie¿y`, order=3)
moment(dane3$`Informacja i komunikacja`, order=3)

#b³¹d standardowy
std.error(dane3$Ogó³em)
std.error(dane3$`Produkcja odzie¿y`)
std.error(dane3$`Informacja i komunikacja`)


#Graficzna prezentacja----------------------------------------------------------

#histogram
histogram(dane3$Ogó³em, main="Histogram wynagrodzeñ Ogó³em", xlab="Wartoœci zarobków",
          ylab = "Liczebnoœæ", col="light pink", breaks=50, ylim=c(0,5), 
          xlim=c(min(dane3$Ogó³em)-100,max(dane3$Ogó³em)+100)) 

histogram(dane3$`Produkcja odzie¿y`, 
          main="Histogram wynagrodzeñ dla Produkcji odzie¿y",
          xlab="Wartoœci zarobków", ylab = "Liczebnoœæ", col="light blue", breaks=50,
          ylim=c(0,7),xlim=c(min(dane3$`Produkcja odzie¿y`)-100,
                             max(dane3$`Produkcja odzie¿y`)+100)) 

histogram(dane3$`Informacja i komunikacja`, 
          main="Histogram wynagrodzeñ dla Informacji i komunikacji",
          xlab="Wartoœci zarobków", ylab = "Liczebnoœæ", col="light green", breaks=50,
          ylim=c(0,6),xlim=c(min(dane3$`Informacja i komunikacja`)-100,
                             max(dane3$`Informacja i komunikacja`)+100)) 


ggplot(dane3, aes(x=Ogó³em)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=100)+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Wykres gêstoœci i histogram dla przeciêtnych wynagrodzeñ",
       x="Wartoœæ wynagrodzeñ", y="Liczebnoœæ") +
  geom_vline(aes(xintercept=mean(Ogó³em)), color="dark blue", linetype="dashed")


wykres <- barplot(wszystko$`Œrednie zarobki`,las= 3,
                  col = palette(rainbow(12, s=0.6 , v=0.75)),
                  main = " Przeciêtne wynagrodzenia brutto dla poszczególnych lat",
                  ylab = "Wartoœæ wynagrodzeñ", 
                  xlim = NULL, ylim = c(0,6000),
                  cex.axis = 0.8 , cex.names = 0.7,
                  font.axis= 4 , font.lab=2
)
text(wykres,par("usr")[1], labels=rownames(wszystko), srt=50,pos = 2, cex=1.5, xpd = TRUE)
title(xlab = "Lata", line=3, font.axis= 3, font.lab=2)
abline(h=mean(wszystko$`Œrednie zarobki`), col="black", lwd=6)



#dane procentowe dla poszczegolnych przedsiebiorstw
(produkcja_odziezy <- sum(dane3[,2]))
(informacja <- sum(dane3[,3]))


#obliczamy %
(dane1c <- c("Produkcja odzie¿y","Informacja i komunikacja"))
(calosc <- sum(produkcja_odziezy, informacja))
(danec <- c(produkcja_odziezy, informacja))
(pct <- round(danec/calosc*100))
(lbls <- paste(dane1c, pct)) #dodawanie procentow do napisow
(lbls <- paste(lbls,"%",sep="")) # dodawanie %

(wykres3d <- pie3D(danec, explode=0.1, cex.axis = 1,col= rainbow(length(lbls)),
                   main = "Przeciêtne wynagrodzenia dla wybranych przedsiêbiorstw"))
pie3D.labels(radialpos=wykres3d,radius=1.7,height=0.1,theta=pi/6,
             labels=lbls, labelcex=1.3,labelrad=0.8,minsep=0.3)


#porownanie danych Ogólnych,  Informacji i komunikacji i Produkcji odziezy
plot(dane3$`Produkcja odzie¿y`,
     main = "Wykres porównawczy dla Ogó³u przedsiebiorstw, Produkcji odziezy \n i Informacji i komunikacji", 
     ylab="Wartoœæ wynagrodzeñ", xlab="Iloœæ miesiêcy", cex.main=2, cex.lab=1.5, cex.axis=1.25,
     type = "l" ,col ="blue", lwd = 3, ylim=c(min(dane3$`Produkcja odzie¿y`),
                                              max(dane3$`Informacja i komunikacja`)),
     xlim=c(0,length(dane3$`Produkcja odzie¿y`)))
lines(dane3$`Informacja i komunikacja`, col="grey", lwd=3)
lines(dane3$Ogó³em, col="light green", lwd=3)


#wykresy pude³kowe dla kolumn iloœciowych
ggplot(stack(dane3), aes(x = ind, y = values)) + 
  geom_boxplot(fill=c("#FF00D8", "#00E8FF", "#3AFF00"),color="black", width = 0.2) +
  labs(title = "Podzia³ wynagrodzenia", y = "Wartoœæ", x="Przedsiêbiorstwa") +
  theme(
    plot.title=element_text(size=25, face="bold", color="dark blue", hjust=0.5),
    axis.title.x=element_text(size=20, vjust=0),
    axis.title.y=element_text(size=20, vjust=1),
    axis.text.x=element_text(size=15,vjust=1),
    axis.text.y=element_text(size=15,vjust=1))



#Rozklad gestosci dla przecietnych zarobkow ogolem
lines(
  plot(density(dane3$Ogó³em),
       lwd=3, main="Wykres gêstoœci przeciêtnych zarobków dla ogó³u",
       ylim=c(0, 0.00056), col="#0FA1C8", xlim=c(2000,7500),
       xlab="Wartoœæ zarobków", ylab="Gêstoœæ"),
  abline(v=mean(dane3$Ogó³em), col="#F046B2", lwd=3))



plot( density(dane3$`Informacja i komunikacja`),    # wykresy gêstoœci 3.4. / 3.5.
      lwd=5, col = "#EE069D", xlim=c(1000,12000), ylim=c(0,0.00081),
      main="Porównanie wartoœci przeciêtnych wynagrodzeñ dla \n Informacji i komunikacji oraz Produkcji odzie¿y",
      xlab="Wartoœæ zarobków", ylab="Gêstoœæ")
lines(density(dane3$`Produkcja odzie¿y`), lwd=5, col = "#B5CD08",)
abline(v=mean(dane3$`Informacja i komunikacja`), col='#EE069D', lwd=3.5)
abline(v=mean(dane3$`Produkcja odzie¿y`), col='#B5CD08', lwd=3.5)
legend("topright", "(x,y)", legend=c("Informacja i komunikacja", "Produkcja odzie¿y"),
       title="Rodzaje przedsiêbiorstw", col=c("#EE069D", "#B5CD08"), lty=c(1,1), cex=0.7,
       lwd=3, box.lwd=2)

#Wykresy dystrybuant

#Wykres dystrybuany empirycznej dla przeciêtnych zarobków Informacji i komunikacji
plot(ecdf(dane3$`Informacja i komunikacja`), lwd=3, 
     main="Wykres dystrybuany empirycznej dla przecietnych zarobkow \n Informacji i komunikacji",
     col="#06EE50", xlab="Wartoœæ zarobków", ylab="Wartoœæ dystrybuanty")

#Wykres dystrybuany empirycznej dla przeciêtnych wynagrodzeñ ogó³em
plot(ecdf(dane3$Ogó³em), lwd=3, 
     main="Wykres dystrybuany empirycznej dla przecietnych wynagrodzeñ ogó³em",
     col="#F3FF00", xlab="Wartoœæ zarobków", ylab="Wartoœæ dystrybuanty")

#Wykres dystrybuany empirycznej dla przeciêtnych wynagrodzeñ w sektorze Produkcji odzie¿y
plot(ecdf(dane3$`Produkcja odzie¿y`), lwd=3, 
     main="Wykres dystrybuany empirycznej dla przecietnych wynagrodzeñ w sektorze Produkcji odzie¿y",
     col="#FF6800", xlab="Wartoœæ zarobków", ylab="Wartoœæ dystrybuanty")




#Hipotezy statystyczne----------------------------------------------------------

#1. Porównanie œrednich wynagrodzeñ dla Produkcji odzie¿y i Informacji i komunikacji

t.test(dane3$`Produkcja odzie¿y`, dane3$`Informacja i komunikacja`, mu=50, alternative = "greater")

#2. Test zgodnoœci chi kwadrat dla wszystkich badanych przedsiêbiorstw

chisq.test(dane3)

#3. Test wariancji wynagrodzeñ Ogó³em a wynagrodzeñ w sektorze Produkcji odzie¿y

var.test(dane3$Ogó³em, dane3$`Produkcja odzie¿y`)

#Korelacja pomiêdzy wszystkimi przedsiêbiorstwami
korelacja <- cor(dane3)
corrplot(korelacja, method="number", diag=FALSE, type="upper")


