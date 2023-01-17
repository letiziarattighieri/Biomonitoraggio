# First class with professor Roberto Cazzolla Gatti: we're going to use for the first time data concerning biodiversity

# 17/01 Biomonitoraggio con Cazzolla

# 17/01 Biomonitoraggio con Cazzolla

library(vegan)
library(tidyverse)
install.packages("")

# Scaricare i dati direttamente da drive
id <- "1qXI_SlH-myF8v4S0xtJ8M3ULuxQQDoPn"
a <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export_download", id), sep=";")
a
str(a)
summary(a)
# dataset di abbondanza di specie
# c'è una à in una variabile character (A) e la variabile C non va bene come character

sort(unique(a$A))
a$A <- gsub("2à", 2, a$A)
str(a)
# gsub trova il pattern, la variabile che mi serve è il 2, e poi mi chiede in che variabile voglio che venga sostituito.
?gsub
a$A <- as.double(a$A)
str(a)

sort(unique(a$C))
a$C <- gsub("assente", 0, a$C) # voglio che la parola "assente" diventi 0 o NA 
str(a)
a$C <- as.double(a$C)
# abbiamo fatto la stessa cosa con la colonna C che aveva una parola al posto di un numero e abbiamo fatto diventare numerico

sum(a$A) # ho NA quindi anche il risultato è NA
a[is.na(a)] <- 0 # sostituiamo tutti gli NA con gli 0 per poter fare operazioni tra le colonne

# somma delle righe delle specie trovate nei 4 plot (A-D). Abbondanza della specie nel dataset
a$ind <- rowSums(a [,c(3, 4, 5, 6)])

# tidyverse permette di raggruppare valori per variabile e ci sono funzioni di summary (operazioni sui gruppi)

b <- group_by(a, Site, species) # ho raggruppato i dati, non ho ancora fatto operazioni
# Usiamo la pipe, prende l'output della funzione precedente e lo elabora

b <- group_by(a, Site, species) %>% summarise(ind = sum(ind))
b

# calcoliamo gli indici di diversità
?diversity
data(BCI)
View(BCI)

# Creiamo il dataset invertendo righe e colonne perchè altrimenti diversity non funziona
c <- pivot_wider(b, values_from = ind, names_from = species)
# Rimuoviamo gli NA
c[is.na(c)] <- 0
View(c)
# per usare la funzione diversity dobbiamo invertire i nomi di righe e colonne

# simpson, shannon, evenness

sn <- specnumber(c[, -1]) # tolgo la prima colonna perchè è quella dei siti e non deve essere inclusa nel calcolo
ab <- rowSums(c) # calcola somma delle specie per sito, abbondanze
si <- diversity(c, index = "simpson")
sh <- diversity(c, index = "shannon")
ev <- sh/log(sn)
# evenness di pivot è di shannon

boxplot(sn)
boxplot(ab)
boxplot(si)
boxplot(sh)
boxplot(ev)

plot(si, ab)
plot(sh, ev)


hist(ab) #come si distribuiscono le abbondanze nei siti
hist(colSums(c[-1])) # riguardalo

# istogramma con abbondanze categorizzate basate su log
# case_when è una concatenazione di funzioni if-else ma più semplice da scrivere
ab_cat <- case_when(colSums(c[-1]) < 2 ~ 1,
                    colSums(c[-1]) < 4 ~ 2,
                    colSums(c[-1]) < 8 ~ 3,
                    colSums(c[-1]) < 16 ~ 4,
                    colSums(c[-1]) < 32 ~ 5,
                    colSums(c[-1]) < 64 ~ 6,
                    colSums(c[-1]) < 128 ~ 7)

hist(ab_cat)

# Whittaker grafico rango abbondanza
w_p <- data.frame(abb = colSums(c[-1]))
w_p <- w_p[order(w_p$abb),]
w_p$rank <- seq(1, 28, 1)
w_p

# jacopo si è bloccato, lo facciamo in modo più semplice: organizzo in ordine decrescente e poi metto il rango
w <- colSums(c[-1])
w <- sort(w, decreasing = T)
r <- seq(1:28)

w <- w/max(w)

plot(r, log(w), type = "o", col = "red", lwd = 2 )


# curve di accumulazione

sp <- specaccum(c[-1])
sp
plot(sp)

sp2 <- specaccum(c[-1], method = "collector")
lines(sp2, col = "red", lwd = 2) 
# la differenza tra le due linee: una è stata ricampionata molte volte, l'altra ha i valori precisi quindi è a zig-zag. 
# meglio fare cumulazione per vedere se sta arrivando a saturazione.



#### Curve ABC (abbondanza, biomassa, curva)

# Riguarda il codice perchè ha finito di spiegare all'1:30 

















































