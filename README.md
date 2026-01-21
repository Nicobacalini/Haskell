Algoritmos y Estructuras de Datos en Haskell

Estructura del Proyecto:

El código está organizado en 4 módulos principales según su propósito:

1.  Estructuras de Datos (/estructuras-datos)

    - Implementación de Tipos de Datos Abstractos (TDA) genéricos y reutilizables.

    - Pila.hs: Implementación de Stack (LIFO).

    - Cola.hs: Implementación de Queue (FIFO).

    - ArbolBinario.hs: Árbol Binario de Búsqueda (BST) completo, incluyendo algoritmos de búsqueda, cálculo de altura y recorridos (InOrder, PreOrder, PostOrder).

2.  Algoritmos y métodos de ordenamiento(/algoritmos)

    - QuickSort

    - MergeSort

    - SelectionSort

    - InsertionSort

    - BubbleSort

3.  Guía de Ejercicios (/guia-ejercicios)

    Resolución de guías prácticas de la cursada, cubriendo desde lo básico hasta recursión avanzada.

    - FuncionesIntro.hs: Sintaxis básica, guardas, composición y pattern matching.

    - ManejoDeListas.hs: Ejercicios de recursión sobre listas y listas por comprensión.

    - AritmeticaBinaria.hs: Simulación de hardware mediante software. Implementación de Sumadores y Multiplicadores.

    - EjerciciosVariados.hs: Ejercicios complementarios.

4.  Finales y Modelos de Examen (/finales-resueltos)

    - Recopilación y refactorización de ejercicios complejos tomados en exámenes finales.

Este portafolio demuestra el dominio de los pilares del paradigma funcional:

  - Tipos de Datos Algebraicos (ADTs) (data, newtype).

  - Funciones de Orden Superior (map, filter, fold, zipWith).

  - Recursividad (Simple, de cola y mutua).

  - Pattern Matching y Guardas.

  - Evaluación Perezosa (Lazy Evaluation).

  - Polimorfismo Paramétrico (Tipos genéricos a).

  - Cómo ejecutar el código

COMO EJECUTAR ESTE REPOSITORIO:

  1. Necesitarás tener instalado GHCi.
  2. Clona este repositorio:

         git clone [https://github.com/Nicobacalini/Haskell.git](https://github.com/Nicobacalini/Haskell.git)
          
  4. Abre una terminal en la carpeta raíz y escribe:

         ghci

  5. Carga el módulo que quieras probar. Por ejemplo, para probar los algoritmos de ordenamiento:

         :load algoritmos/Ordenamiento.hs

  6. Prueba las funciones:

    quickSort [5, 1, 9, 3, 7]
    -- Output: [1, 3, 5, 7, 9]
