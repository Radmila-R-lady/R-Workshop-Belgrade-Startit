#######################Startit R radionica: Uvod u R programiranje###############
#########################Radmila VELICKOVIC#####################################
#########################19..2.2019, Beograd#########################
# Uopsteno -----------------------------------------------------------

#Koju R verziju koristite?
sessionInfo()
#gde se nalaze fajlovi koje cemo kortiti?
getwd()
setwd("C:/Users/Rada/POSM laptop/1. STATISTICS masters material/R_Treninzi")


# Strukture i tipovi podataka -----------------------------------------------


#iris set podataka
View(iris)
dim(iris)
names(iris)
str(iris)
#naslovna kolona
my.iris.header<-c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width")
print(my.iris.header)
#prvi red
my.iris.first.line<-c(5.1,3.5,1.4,0.2)
print(my.iris.first.line)
#kombinacija
my.iris.vectors<-rbind(my.iris.header,my.iris.first.line)
View(my.iris.vectors)
my.iris.vectors.transposed<-t(my.iris.vectors)
View(my.iris.vectors.transposed)



#liste

my.iris.vector2<-list(c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width","Type"),
                      c(5.1,3.5,1.4,0.2, "setosa"))

class(my.iris.vector2)
my.iris.vector2<-as.data.frame(my.iris.vector2)
View(my.iris.vector2)  


# matrices

matrix_3by2 = matrix( 
  c(2, 4, 3, 1, 5, 7), 
  nrow=3, 
  ncol=2) 
matrix_3by2

matrix_3by2

t(matrix_3by2)



# identity matrix

diag(4)
View(diag(4))

#mnozenje matrica
#
A=matrix( 
  c(2, 4, 3, 1, 5, 7), 
  nrow=3, 
  ncol=2) 

B= matrix( 
  c(2, 4, 3, 1, 5, 7), 
  nrow=2, 
  ncol=3) 
A
B
A%*%B
#idempotent matrix

C=matrix(c(4,-1,12,-3),
         nrow = 2,
         ncol=2, byrow=T)
C
C%*%C

#inverse matrix

C1=matrix(c(4,-1,12,-2),
          nrow = 2,
          ncol=2, byrow=T)

C1
solve(C1)

D<-matrix(c(-5,0,2,1,-2,3,6,-2,1),
          nrow=3,
          ncol=3, byrow=T)

D
solve(D)

#replicate first 4 numbers twice
rep(1:4,2 )
#replicate first 4 numbers twice using argument each
rep(1:4, each=2)
#replicate first 4 numbers twice using c()
rep(1:4, c(2,2,2,2))
#replicate first 4 numbers in such way that
#number 2 and 4 appear only once and 1,3 appear twice
rep(1:4, c(2,1,2,1))

#replicate first 4 numbers twice using argument each
#such that we get vector of length 4

rep(1:4, each=2, length.out=4)

#replicate first 4 numbers twice using argument each
#such that we get vector of length 10

rep(1:4,each=2, length=10)


# varijabla pol

gender <- c(rep("male",20), rep("female", 30)) 
View(gender)
is.character(gender)
table(gender)
is.factor(gender)
gender <- as.factor(gender) 
levels(gender)
table(gender)


# DATA MANIPULATION -------------------------------------------------------


library(foreign)
library(dplyr)
ESS<-as.data.frame(read.spss("C:/Users/Rada/Desktop/setovi_podataka/ESS8e02_1.sav", use.value.labels=T, max.value.labels=Inf, use.missings=F))
View(ESS)

#filtriranje podataka po jednom kriterujumu
ESS.DE<-filter(ESS, cntry== "Germany" )
View(ESS.DE)
#filitriranje podataka po dva kriterijuma
ESS.DE<-filter(ESS, cntry== "Germany", polintr == "Not at all interested")
View(ESS.DE)
#filtriranje podataka po tri kriterijuma
ESS.DE<-filter(ESS, cntry== "Germany", polintr == "Not at all interested", nwspol > 30 )

#filtriranje po jednom kriterijumu ali vise kategorija u okviru varijable
ESS.DE<-filter(ESS, cntry== "Germany", polintr == "Not at all interested" | polintr == "Hardly interested")
View(ESS.DE)


#izdvajanje kolona po zelji
#cntry-country/ pointr-political interest, 
#trstprl-trust in country´s parliament
#trstlgl-trust in legal system
#trstplc-trust in police
#trstplt-trust in politicians

ESSsubset<-select(ESS, cntry, polintr, 
                  trstprl, trstlgl,trstplc, trstplt )
View(ESSsubset)

#mutate ()
#sluzi za kreiranje nove kolone koja je transformisana

#groupby ()
#sluzi za grupisanje podataka na osnovu neke varijable
#(npr. grupisati prosecno vreme gledanja politickog sadrzaja prema zemlji) 

ESS.by.cntry<-select(ESS,cntry,nwspol)
View(ESS.by.cntry)

ESS.by.cntry$nwspol<-as.numeric(ESS.by.cntry$nwspol)

by.country <- ESS.by.cntry %>% 
  group_by(cntry) %>% 
  summarise(
    average_newspol = mean((nwspol), na.rm= T)
  )
View(by.country)

library(ggplot2)


ggplot(data = mpg) #nije od znacaja ali omogucava stavljanje slojeva (layers)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))   #napravili smo dijagram rasprsenosti

#kojoj klasi pripada koji automobil

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# dodavanje facet-a za razdvajanje grafikona
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3 )

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

#dijagram frekvencija

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))


#statistika u R-u

X <- c(275,292,281,284,285,283,290,294,300,286)
plot( ecdf(X) )
Y <- (X - mean(X)) / sd(X) 
hist(Y)
#da li podaci dolaze iz normalno rasporedjenog skupa
ks.test(Y,pnorm) # Ne odbacujemo hipotezu: podaci dolaze iz normalnog rasporedjenog skupa
#da li podaci dolazi iz eksponencijalno rasporedjenog skupa
ks.test(Y,pexp) #podaci ne dolaze iz eksponencijalno rasporedjenog skupa 

#deskriptivna statistika

mean(ESS$nwspol)
ESS$nwspol<-as.numeric(ESS$nwspol)

library(psych)
describeBy(ESS$nwspol, 
           group = ESS$cntry,
           digits= 4)

#prosta linearna regresija

getwd()
setwd("C:/Users/Rada/POSM laptop/UZH/unine/mati")
houses<-read.table("houses.txt",header=T)
View(houses)
attach(houses)

#simple linear regression						
#plotting the data and the regression line
plot(area,price)
cor(price,area, method = "pearson")
fit<-lm(price~area)
fit
abline(fit, col="blue")
cor^2

#fitted values
fitted(fit)

#residuals
residuals(fit)


par(mfrow=c(2,2))
plot(fit)



