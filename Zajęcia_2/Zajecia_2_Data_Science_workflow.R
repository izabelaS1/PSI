kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")

# 1. Podgląd danych ----

# Pierwsze/ostatnie wiersze
head(kraje_1)      # pierwsze 6 wierszy (obserwacji)
head(kraje_2)      

head(kraje_1, 10)  # pierwsze 10 wierszy (obserwacji)
head(kraje_2, 10)

tail(kraje_1, 5)   # ostatnie 5 wierszy (obserwacji)
tail(kraje_2, 5)

# Podstawowe statystyki wszystkich kolumn (zmiennych)
summary(kraje_1)   # min, max, średnia, mediana, kwantyle
summary(kraje_2)

# Statystyki pojedynczej kolumny (zmiennej)
mean(kraje_1$Przyrost_populacji)       # średnia
median(kraje_1$Przyrost_populacji)     # mediana
min(kraje_1$Przyrost_populacji)        # minimum
max(kraje_1$Przyrost_populacji)        # maksimum


# 2. Porządkowanie nazw kolumn (zmiennych) ----

# Usuwanie zbędnej kolumny
kraje_1$X = NULL
kraje_2$X = NULL

# Zmiana nazw kolumn z angielskich na polskie
colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


# 3. Porządkowanie typów danych ----

# W ramce danych kraje_2 sprawdź typ zmiennej Region 
is.numeric(kraje_2$Region)      # czy zmienna jest liczbowa? Odp. Nie.
is.character(kraje_2$Region)    # czy zmienna jest tekstowa? Odp. Tak.

# Region to zmienna kategorialna, więc nadajemy jej typ factor:
kraje_2$Region = as.factor(kraje_2$Region)

# Sprawdzenie kategorii:
summary(kraje_2)
levels(kraje_2$Region)

# Teraz widać, że jest 7 kategorii regionów, na których operuje zmienna Region.


# 4. Porządkowanie braków danych ----

# Szybka kontrola braków danych we wszystkich kolumnach:
colSums(is.na(kraje_1))   # nie ma braków danych
colSums(is.na(kraje_2))   # są 4 braki danych w kolumnie (zmiennej) Internet_proc.

# Liczba braków w konkretnej kolumnie:
sum(is.na(kraje_2$Internet_proc.))     # 4 braki

# Zobaczmy te 4 wiersze, w których brakuje wartości:
kraje_2[is.na(kraje_2$Internet_proc.), ]

# Braki danych są częścią rzeczywistości ekonomisty, dlatego trzeba umieć je obsłużyć
# i podjąć decyzję analityczną:
# OPCJA 1 - Pozostawić (teraz tak postąpimy)
# OPCJA 2 - Usunąć obserwacje z brakami (czy usunięcie tych obserwacji zmieni analizę?)
# OPCJA 3 - Uzupełnić braki (np. imputacja medianą)


# 5. Czyszczenie danych ----

# W ramce danych kraje_2, w kolumnie Region są kategorie, w których nazwie jest znak &:
levels(kraje_2$Region)

# Znak & bywa problematyczny przy dalszym przetwarzaniu, dlatego zastąp go słownym spójnikiem "and".
# Funkcja gsub() działa jak "Znajdź i zamień" (Ctrl+H) w Excelu. 
# Zamienia wszystkie wystąpienia tekstu na inny tekst. Przykładowo: gsub("stary_tekst", "nowy_tekst", ramka$kolumna)

# W naszym przypadku wykonamy następujący kod:
kraje_2$Region <- gsub("&", "and", kraje_2$Region)

# Sprawdzenie (po zamianie ponownie ustawiamy typ factor):
kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)


# 6. Łączenie (scalanie) ramek danych w jedną ----

# Funkcja merge() łączy dwie ramki danych po wspólnej kolumnie (kluczu)
# Działa analogicznie jak WYSZUKAJ.PIONOWO w Excelu

# Przykładowo: merge(ramka1, ramka2, by.x="kolumna1", by.y="kolumna2")

# Łączenie (scalanie) ramek danych kraje_1 i kraje_2:
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")

# Usuwanie zbędnej kolumny po połączeniu:
kraje$Nazwa = NULL

# Zobacz ramkę danych po scaleniu:
summary(kraje)
str(kraje)


# 7. Podstawowa analiza danych ----

# Podstawowa analiza danych jest punktem wyjścia do dalszych metod
# Na tym etapie skupiamy się na poznaniu zbioru danych:
# Filtrowanie, sortowanie, podsumowania statystyczne, agregacja i wykrywanie błędów

library(dplyr)

# mutate() – tworzenie nowych zmiennych na bazie istniejących

# Tworzenie nowej zmiennej Populacja_w_mln:
kraje = kraje %>% mutate(Populacja_mln = Populacja / 1e6)

# Tworzenie nowej zmiennej PKB_per_capita:
kraje = kraje %>% mutate(PKB_per_capita = PKB / Populacja)

# filter() – wybieranie wierszy i select() – wybieranie kolumn

# Wyświetl kraje, w których poziom urbanizacji jest większy niż 50:
kraje %>% filter(Urbanizacja_proc. > 50)

# Wyświetl tylko dane pokazujące zmienne Panstwo, Region, PKB, Populacja_mln:
kraje %>% select(Panstwo, Region, PKB, Populacja_mln)

# arrange() – sortowanie

# Posortuj kraje według przyrostu populacji malejąco:
kraje %>% arrange(desc(Przyrost_populacji))

# Wybierz kraje z PKB > 1 bilion, posortuj rosnąco i wyświetl wybrane kolumny:
kraje %>% filter(PKB > 1e12) %>% arrange(PKB) %>% select(Panstwo, PKB, PKB_per_capita)

# group_by() – grupowanie i summarise() - obliczanie wartości zagregowanych 
# Policz, ile krajów jest w każdym regionie:
kraje %>% group_by(Region) %>% summarise(liczba_krajow = n())

# Statystyki regionów (internet, urbanizacja) posortowane malejąco wg internetu:
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# 8. Wizualizacja [* zaawansowane *] ----

library(ggplot2)

# Wykres punktowy: urbanizacja a PKB per capita
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Urbanizacja a PKB per capita", x = "Urbanizacja (%)", y = "PKB per capita (USD, skala log)") +
  theme_minimal()

# Wykres słupkowy: liczba krajów w regionach
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wykres pudełkowy (boxplot): dostęp do internetu według regionów
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")


# 9. Eksport ----

# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv")

# Zapisanie ramki danych do pliku Excel
# install.packages("writexl") # Zakomentowano po wykonaniu instalacji
library(writexl)
write_xlsx(kraje, "kraje_wynik.xlsx")
