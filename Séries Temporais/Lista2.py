import numpy as np

# Dados
Z = np.array([9, 13, 8, 11, 16, 12, 15])
N = len(Z)
s = sum(Z)
print(s)
# Média
media = np.mean(Z)
print(f"Média = {media:.4f}\n")

# Desvios
d = Z - media
print("Desvios:", d)

# Autocovariâncias e autocorrelações
c = []
r = []
for k in range(N):
    # soma dos produtos para defasagem k
    soma = 0
    for t in range(N - k):
        soma += d[t] * d[t + k]
    ck = soma / N
    c.append(ck)
    if k == 0:
        r.append(1.0)
    else:
        r.append(ck / c[0])

# Exibição formatada
print("\n k |     c_k (exato)      |     r_k")
print("---------------------------------------")
for k in range(N):
    # Fração exata para c_k (simplificada)
    numerador = round(c[k] * N)  # pois c_k * N é inteiro (soma)
    denominador = N
    frac_c = f"{numerador}/{denominador}" if numerador != 0 else "0"
    print(f"{k:2d} | {frac_c:>10} = {c[k]:8.6f} | {r[k]:8.6f}")