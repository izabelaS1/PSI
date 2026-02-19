#Zadanie 4 
kalkulator = function(a, b, operacja) {
  
  if (operacja == "+") {
    wynik = a + b
  } else if (operacja == "-") {
    wynik = a - b
  } else if (operacja == "*") {
    wynik = a * b
  } else if (operacja == "/") {
    if (b == 0) {
      wynik = "Błąd: dzielenie przez zero!"
    } else {
      wynik = a / b
    }
  } else {
    wynik = "Nieznana operacja"
  }
  
  return(wynik)
}

print(kalkulator(20, 2, "+")) 
print(kalkulator(20, 2, "-")) 
print(kalkulator(20, 2, "*")) 
print(kalkulator(20, 2, "/")) 

print(kalkulator(15, 0, "+")) 
print(kalkulator(15, 0, "-")) 
print(kalkulator(15, 0, "*"))
print(kalkulator(15, 0, "/"))

print(kalkulator(10, 5, "xyz")) 