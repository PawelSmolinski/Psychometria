# Instalacja pakietów
install.packages("dplyr")
install.packages("psych")
install.packages("lavaan")
install.packages("corrplot")
install.packages("GPArotation")
install.packages("ggplot2")
install.packages("semTools")


library('dplyr')
library('psych')
library('lavaan')
library('corrplot')
library('GPArotation')
library('ggplot2')
library('semTools')


dane <- read.csv('https://raw.githubusercontent.com/PawelSmolinski/Psychometria/main/Dane/Ciemna%20triada/dane50.csv')

str(dane)

View(dane)

dane <- rename(dane, kraj = country)

dane$kraj <- as.factor(dane$kraj)

macierz <- cor(dane[,1:18])
print(macierz)

corrplot(macierz, method = 'square')

describe(dane[,1:18])

dane$N2 <- 6 - dane$N2

dane$M3[dane$M3 == 0] <- NA #To odkrycie zmienia poprzednie statystyki jak t.test czy korelacje
dane$N6[dane$N6 == 0] <- NA 


#Troche bardziej zaawansowane, ale warto pamitać o takiej opcji:
dane <- dane %>%
  mutate(across(c(N6,N8), ~recode(.,`1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)))


dane$makiawelizm <- rowMeans(dane[,1:9], na.rm = TRUE)
dane$narcyzm <- rowMeans(dane[,10:18], na.rm = TRUE)

ggplot(dane, aes(x = makiawelizm)) +
  geom_histogram(fill = "orange",
                 color = "black", bins = 15) +
  labs(title = "Rozkład wyników dla Makiawelizmu",
       x = "Wynik w teście",
       y = 'Ilość osób z wynikiem') +
  theme_minimal()


t.test(makiawelizm ~ kraj, dane)


cor.test(dane$makiawelizm, dane$narcyzm)

macierz <- cor(dane[,1:18], use = 'pairwise.complete.obs')
print(macierz)

corrplot(macierz, method = 'square')

alfa <- psych::alpha(dane[,1:18])
summary(alfa)
print(alfa$alpha.drop)

alfa_makiawelizm <- psych::alpha(dane[,1:9])
summary(alfa_makiawelizm)

alfa_narcyzm <- psych::alpha(dane[,10:18])
summary(alfa_narcyzm)

efa <- fa(dane[,1:18], nfactors =  2)
summary(efa)
print(efa$loadings, cutoff = 0.5)

scree(dane[,1:18])

model <- 'Makiawelizm =~ M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9
          Narcyzm =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9'

cfa <- cfa(model, dane)
summary(cfa, fit.measures=TRUE)

semPaths(cfa, whatLabels="std", layout="tree")

omegaResults <- semTools::reliability(cfa)
print(omegaResults)
  
