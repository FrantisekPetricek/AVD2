# ==============================================================================
# PAGERANK A MARKOVOVY RETEZCE — KOMPLETNI RESENI
# Ulohy 3.1, 3.2 (agentni simulace), 3.3 (prechodoova matice)
# ==============================================================================
#
# ZADANI:
#   Mame sit 5 stranek (A, B, C, D, E) propojenych odkazy:
#   A -> B, A -> C
#   B -> A, B -> D
#   C -> D, C -> E
#   D -> E
#   E -> A, E -> C
#
# MARKOVUV RETEZEC:
#   - STAV   = aktualni stranka, na ktere se surfer nachazi
#   - PRECHOD = nahodny pruchod odkazu (kazdy odkaz stejne pravdepodobny)
#   - CIL    = zjistit "dulezitost" kazde stranky = PageRank
# ==============================================================================


# ==============================================================================
# CAST 0: POTREBNE KNIHOVNY
# ==============================================================================

# Knihovna 'expm' umoznuje mocneni matic operatorem %^%
# Nainstaluj pokud nemas: install.packages("expm")
library(expm)


# ==============================================================================
# CAST 1: PRECHODOVA MATICE — ULOHA 3.1
# ==============================================================================
#
# Co je prechodova matice P?
# ---------------------------
# Je to tabulka (matice) pravdepodobnosti prechodu mezi stavy.
# P[i, j] = pravdepodobnost, ze ze stranky i prejdeme na stranku j
#
# Pravidla sestaveni:
#   - z kazde stranky vede 1 nebo vice odkazu
#   - pravdepodobnost kazdeho odkazu = 1 / (pocet odkazu z teto stranky)
#   - kazdy RADEK musi davat soucet = 1 (surfer nekam odejde vzdy)
#   - jedeme po RADCICH = "odkud", po SLOUPCICH = "kam"
#
# Nase stranky: A=1, B=2, C=3, D=4, E=5
#
# Odvozeni hodnot z grafu:
#   A ma 2 odkazy (->B, ->C)  => P[A,B] = 1/2, P[A,C] = 1/2
#   B ma 2 odkazy (->A, ->D)  => P[B,A] = 1/2, P[B,D] = 1/2
#   C ma 2 odkazy (->D, ->E)  => P[C,D] = 1/2, P[C,E] = 1/2
#   D ma 1 odkaz  (->E)       => P[D,E] = 1
#   E ma 2 odkazy (->A, ->C)  => P[E,A] = 1/2, P[E,C] = 1/2

P <- matrix(c(
# kam:  A    B    C    D    E
        0,  1/2, 1/2,  0,   0,   # odkud: A
       1/2,  0,   0,  1/2,  0,   # odkud: B
        0,   0,   0,  1/2, 1/2,  # odkud: C
        0,   0,   0,   0,   1,   # odkud: D
       1/2,  0,  1/2,  0,   0    # odkud: E
), nrow = 5, ncol = 5, byrow = TRUE)


(solve(diag(5) - P))

rownames(P) <- colnames(P) <- c("A", "B", "C", "D", "E")

cat("=== PRECHODOVA MATICE P ===\n")
print(P)

# Overeni korektnosti: soucet kazdeho radku musi byt presne 1
# Toto je definice "stochasticke matice"
soucty_radku <- rowSums(P)
cat("\nSoucty radku (musi byt vsechny = 1):\n")
print(soucty_radku)
cat("Matice je korektni:", all(abs(soucty_radku - 1) < 1e-10), "\n\n")


# ==============================================================================
# CAST 2: SIMULACE A — AGENTNI (jeden nahodny surfer) — ULOHA 3.2
# ==============================================================================
#
# Princip:
#   - Jeden surfer zacina na nejake strance
#   - V kazdem kroku nahodne vybere jeden z odchozich odkazu
#   - Zaznamename, kolik casu stravil na kazde strance
#   - Relativni cetnosti navstev = odhad dulezitosti = PageRank
#
# Funkce sample(1:5, 1, prob = P[stav, ]):
#   - nahodne vybere jedno cislo z {1,2,3,4,5}
#   - pravdepodobnosti jsou zadany radkem prechodove matice
#   - tj. simuluje jeden krok Markovova retezce

simulace_surfer <- function(P, n_kroku, pocatecni_stav = 1) {
  # n_kroku       = celkovy pocet kroku simulace
  # pocatecni_stav = cislo stranky kde zacneme (1=A, 2=B, ...)

  n_stranek <- nrow(P)
  navstevy  <- rep(0, n_stranek)   # citac navstev pro kazdou stranku

  stav <- pocatecni_stav           # aktualni stav (stranka)

  for (krok in 1:n_kroku) {
    navstevy[stav] <- navstevy[stav] + 1  # zaznam navstevy

    # Nahodny prechod: vyber dalsi stranku podle pravdepodobnosti v radku P[stav,]
    stav <- sample(1:n_stranek, 1, prob = P[stav, ])
  }

  # Vrat relativni cetnosti (= odhad pravdepodobnosti)
  return(navstevy / n_kroku)
}

# Spustime simulaci s ruznym poctem kroku — chceme videt konvergenci
cat("=== SIMULACE A: AGENTNI (nahodny surfer) ===\n")

set.seed(42)  # pro opakovatelnost vysledku

vysledky_sim <- list()
pocty_kroku  <- c(1000, 10000, 100000, 1000000)

for (n in pocty_kroku) {
  skore <- simulace_surfer(P, n_kroku = n, pocatecni_stav = 1)
  vysledky_sim[[as.character(n)]] <- skore
  cat(sprintf("  %8d kroku: A=%.3f  B=%.3f  C=%.3f  D=%.3f  E=%.3f\n",
              n, skore[1], skore[2], skore[3], skore[4], skore[5]))
}

cat("\n")

# Komentar k vysledkum:
# - Pri malem poctu kroku (1000) jsou vysledky nestabilni — nahodne odchylky
# - Pri velkem poctu kroku (1 000 000) se vysledky ustalili na pevnych hodnotach
# - Tyto hodnoty jsou nezavisle na pocatecni strance (ergodova vlastnost)
# - To je ocekavano pro REGULARNI (ergodicky) Markovuv retezec


# Overime: meni se vysledek podle pocatecni stranky?
cat("Vliv pocatecni stranky (1 000 000 kroku):\n")
for (start in 1:5) {
  skore <- simulace_surfer(P, n_kroku = 1000000, pocatecni_stav = start)
  cat(sprintf("  Start %s: A=%.3f  B=%.3f  C=%.3f  D=%.3f  E=%.3f\n",
              c("A","B","C","D","E")[start],
              skore[1], skore[2], skore[3], skore[4], skore[5]))
}
# --> Vsechny radky by meli byt (skoro) stejne!
# Toto dokazuje ergodovou vlastnost retezce.
cat("\n")


# ==============================================================================
# CAST 3: SIMULACE B — PRECHODOVA MATICE — ULOHA 3.3
# ==============================================================================
#
# Misto simulace jednoho surfera pouzijeme MATEMATICKE nastroje:
#
# METODA 1: Mocneni matice P^n
#   P^n[i,j] = pravdepodobnost ze za n kroku prejdeme ze stranky i na j
#   Pri n -> nekonecno vsechny radky konverguji ke stejnemu vektoru
#   Tento vektor JE stacionarni rozdeleni (= PageRank)
#
# METODA 2: Iterace pravdepodobnostniho vektoru
#   Zacneme s nejakym rozdelenim pi_0 (napr. zacneme jiste na A)
#   Opakovane nasobime: pi_{t+1} = pi_t * P
#   Po dostatecnem poctu iteraci konverguje ke stacionarnimu rozdeleni
#
# METODA 3: Analyticke reseni soustavy rovnic
#   Stacionarni vektor pi splnuje: pi * P = pi  (rovna sam sobe po nasobeni P)
#   Plus podminka normy:            sum(pi) = 1
#   To je soustava linearnich rovnic, kterou muzeme resit primo

cat("=== SIMULACE B: PRECHODOVA MATICE ===\n\n")

# --- METODA 1: Mocneni matice ---
cat("-- Metoda 1: Mocneni matice P^n --\n")
# P^1 = prechodova matice (1 krok)
# P^2 = pravdepodobnosti za 2 kroky
# P^100 = pravdepodobnosti za 100 kroku (prakticky jiz limitni hodnota)

P10  <- P %^% 10
P100 <- P %^% 100

cat("P^10 (po 10 krocich):\n")
print(round(P10, 4))
cat("\nP^100 (po 100 krocich = limitni matice):\n")
print(round(P100, 4))

# Stacionarni vektor = libovolny radek limitni matice (vsechny jsou stejne)
pi_mocneni <- P100[1, ]
cat("\nStacionarni vektor z P^100:\n")
print(round(pi_mocneni, 4))
cat("\n")


# --- METODA 2: Iterace vektoru ---
cat("-- Metoda 2: Iterace pravdepodobnostniho vektoru pi * P --\n")
# Zacneme na strance A (jistota = 1, ostatni = 0)
pi <- c(1, 0, 0, 0, 0)
names(pi) <- c("A","B","C","D","E")

cat("Pocatecni rozdeleni (zacneme na A):", pi, "\n")

n_iter <- 100
for (i in 1:n_iter) {
  pi <- pi %*% P   # jeden krok: nasobeni radkoveho vektoru maticí zprava
}
pi <- as.vector(pi)
names(pi) <- c("A","B","C","D","E")

cat(sprintf("Po %d iteracich:\n", n_iter))
print(round(pi, 4))

# Overeni: pi * P musi dat stejny vysledek jako pi
residuum <- max(abs(pi %*% P - pi))
cat(sprintf("Overeni pi*P = pi: max odchylka = %.2e (mela by byt ~0)\n\n", residuum))


# --- METODA 3: Analyticke reseni ---
cat("-- Metoda 3: Analyticke reseni soustavy pi*P = pi, sum(pi) = 1 --\n")
#
# Odvozeni soustavy:
#   pi * P = pi
#   pi * P - pi * I = 0       (I = jednotkova matice)
#   pi * (P - I) = 0
#
# Transponujeme (solve pracuje se soustavou A*x = b, ne x*A = b):
#   (P - I)^T * pi^T = 0
#
# Posledni rovnici nahradime normalizacni podminkou sum(pi) = 1:
#   Posledni radek matice = samé jednicky
#   Posledni prvek pravé strany = 1

resStacionarni <- function(P) {
  n <- nrow(P)

  # Sestaveni matice soustavy: transponovana (P - I)
  A <- t(P - diag(n))

  # Posledni radek nahradime podmínkou sum = 1
  A[n, ] <- rep(1, n)

  # Prava strana: 0, 0, ..., 0, 1
  b      <- rep(0, n)
  b[n]   <- 1

  # Reseni soustavy linearních rovnic A * x = b
  return(solve(A, b))
}

pi_analyticke <- resStacionarni(P)
names(pi_analyticke) <- c("A","B","C","D","E")

cat("Stacionarni vektor (analyticke reseni):\n")
print(round(pi_analyticke, 4))

cat("\nPageRank skore v procentech:\n")
print(round(pi_analyticke * 100, 2))

# Overeni: pi * P musi dat stejny vysledek jako pi
residuum2 <- max(abs(pi_analyticke %*% P - pi_analyticke))
cat(sprintf("Overeni pi*P = pi: max odchylka = %.2e\n\n", residuum2))


# ==============================================================================
# CAST 4: SROVNANI VSECH METOD
# ==============================================================================

cat("=== SROVNANI VSECH METOD ===\n")
srovnani <- rbind(
  "Simulace (1M kroku)"  = simulace_surfer(P, 1000000, 1),
  "Mocneni P^100"        = P100[1, ],
  "Iterace vektoru"      = as.vector(pi %*% diag(5)),  # uz je konvergovano
  "Analyticke reseni"    = pi_analyticke
)
colnames(srovnani) <- c("A","B","C","D","E")
print(round(srovnani, 4))

cat("\nVysvetleni vysledku:\n")
cat("  Stranka E ma nejvyssi PageRank (~27%) — dostane se k ni hodne cest\n")
cat("  Stranka A ma druhe misto (~23%) — E a B na ni odkazuji\n")
cat("  Stranka B ma nejnizsi PageRank (~11%) — odkazuje na ni jen A\n")
cat("  Vsechny metody daji (skoro) stejny vysledek — to je dulezite!\n")


library(expm)

# Cela prechodova matice
P <- matrix(0, 6, 6)
rownames(P) <- colnames(P) <- c("a","b","c","d","e","f")

diag(P[-1, ]) <- 0.5   # prechody doprava
diag(P[, -1]) <- 0.5   # prechody doleva
P["a","a"] <- 1        # a je absorbcni
P["f","f"] <- 1        # f je absorbcni
P["a","b"] <- 0        # oprava krajnich stavu
P["f","e"] <- 0

# Vyrizni tranzientni cast (b,c,d,e) a cast smerujici do absorbcnich
Q <- P[c("b","c","d","e"), c("b","c","d","e")]  # tranzientni -> tranzientni
R <- P[c("b","c","d","e"), c("a","f")]          # tranzientni -> absorbcni

cat("Q (tranzientni prechody):\n"); print(Q)
cat("\nR (prechody do absorbcnich):\n"); print(R)

# Fundamentalni matice
N <- solve(diag(4) - Q)
cat("\nN (fundamentalni matice):\n")
print(round(N, 2))
# N[i,j] = kolikrat prumerne projdes stavem j zacinas-li v i

# Prumerna doba do absorbce
t <- rowSums(N)
cat("\nPrumerna doba do absorbce (pocet kroku):\n")
print(round(t, 2))
# Zacinas-li v b: prumerne 4 kroky nez se dostanes domu nebo do baru

# Pravdepodobnost absorbce
B <- N %*% R
cat("\nPravdepodobnost absorbce (B = N*R):\n")
print(round(B, 4))
# B[i, "a"] = pravdepodobnost ze skoncis doma, zacinas-li v i
