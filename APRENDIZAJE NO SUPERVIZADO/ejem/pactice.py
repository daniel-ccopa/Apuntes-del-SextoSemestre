#CALCULAR EL FACTORIAL DE UN NUMERO
n = int(input("Ingrese un numero: "))
fac = 1
for i in range(1,n+1):
    print(i)
    fac *= i

print(f'El factorial de {n} es {fac}')