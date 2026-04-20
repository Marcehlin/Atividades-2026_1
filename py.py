import numpy as np
import time

def particionar(lista,inicio,fim):
  pivo = lista[fim]
  i = inicio -1
  for j in range (inicio,fim):
    if lista[j]<pivo:
      i+=1
      lista[i],lista[j] = lista[j],lista[i]
  lista[fim],lista[i+1] = lista[i+1],lista[fim]
  return i+1

def quick_sort(lista,inicio,fim):
  if inicio<fim:
    pivo = particionar(lista,inicio,fim)
    quick_sort(lista,inicio,pivo-1)
    quick_sort(lista,pivo+1,fim)
  return lista

vetor1 = np.random.randint(1000,9999999,10000000)
inicio = time.perf_counter()
quick_sort(vetor1,0,len(vetor1)-1)
fim = time.perf_counter()
print(f"Tempo decorrido{fim-inicio:.6f}")