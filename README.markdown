# Universidad Simón Bolívar
## Laboratorio de Lenguajes de Programación
### Proyecto 2: Aritmética de Precisión Arbitraria

**Desarrollado por:**

 - Daniel Cestari #04-36834

--------------------------------------------------

Implementación de una librería para el cálculo con
precisión arbitraria

Se define el tipo RealArbitrario como dos listas de
enteros (parte entera y parte decimal) y un entero
adicional para la base.

Cada elemento de la lista debe ser un entero menor
a la base (dígito). La parte entera se ordena de
menos significativo a más significativo y la parte
decimal en orden inverso.

**Ejemplo:** _(-> se lee "se representa como")_

    250,54 -> NoNeg [0,5,2] [5,4] 10
     -25    -> NoNeg [5,2] [] 10
      0     -> NoNeg [] [] 10
