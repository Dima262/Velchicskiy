# Метрические алгоритмы классификации
**Метрические методы обучения** -- методы, основанные на анализе сходства объектов.

Метрические алгоритмы классификации опираются на **_гипотезу компактности_**: схожим объектам соответствуют схожие ответы.

Метрические алгоритмы классификации с обучающей выборкой *Xl* относят объект *u* к тому классу *y*, для которого **суммарный вес ближайших обучающих объектов ![](https://latex.codecogs.com/gif.latex?W_y%28u%2C%20X%5El%29) максимален**:

![](https://latex.codecogs.com/gif.latex?W_y%28u%2C%20X%5El%29%20%3D%20%5Csum_%7Bi%20%3A%20y_%7Bu%7D%5E%7B%28i%29%7D%20%3D%20y%7D%20w%28i%2C%20u%29%20%5Crightarrow%20max)

, где весовая функция *w(i, u)* оценивает степень важности *i*-го соседа для классификации объекта *u*.

Функция ![](https://latex.codecogs.com/gif.latex?W_y%28u%2C%20X%5El%29) называется **_оценкой близости объекта u к классу y_**. Выбирая различную весовую функцию *w(i, u)* можно получать различные метрические классификаторы.

Для поиска оптимальных параметров для каждого из рассматриваемых ниже метрических алгоритмов используется **LOO -- leave-one-out** *(критерий скользящего контроля)*, который состоит в следующем: 

1. Исключать объекты *x(i)* из выборки *Xl* по одному, получится новая выборка без объекта *x(i)* (назовём её *Xl_1*).
2. Запускать алгоритм от объекта *u*, который нужно классифицировать, на выборке *Xl_1*.
3. Завести переменную *Q* (накопитель ошибки, изначально *Q = 0*) и, когда алгоритм ошибается, *Q = Q + 1*.
4. Когда все объекты *x(i)* будут перебраны, вычислить *LOO = Q / l* (*l* -- количество объектов выборки).

При минимальном значении LOO получим оптимальный параметр алгоритма.

## Алгоритм k ближайших соседей (kNN)
Алгоритм 1NN относит классифицируемый объект U к тому классу, которому принадлежит его ближайший сосед.
*w*(i,u)=[i=1];

Алгоритм kNN относит объект к тому классу, элементов которого больше среди k ближайших соседей x(i), i=1,..,k.

Для оценки близости классифицируемого объекта *u* к классу *y* **алгоритм kNN** использует следующую функцию:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D) , где *i* -- порядок соседа по расстоянию к классифицируемому объекту *u*, k-количество параметров.
**Реализация весовой функции производится следующим образом**:

``` R
distances <- matrix(NA, l, 2) # расстояния от классифицируемого объекта u до каждого i-го соседа 
for(i in 1:l) {
   distances[i, ] <- c(i, eDist(xl[i, 1:n], u))
}
orderedxl <- xl[order(distances[ , 2]), ] # сортировка расстояний
classes <- orderedxl[1:k, n + 1] # названия первых k классов (k ближайших соседей) в classes 
```
### Преимущества:
1. Простота реализации.
2. При *k*, подобранном около оптимального, алгоритм "неплохо" классифицирует.

### Недостатки:
1. Нужно хранить всю выборку.
2. При *k = 1* неустойчивость к погрешностям (*выбросам* -- объектам, которые окружены объектами чужого класса), вследствие чего этот выброс классифицировался неверно и окружающие его объекты, для которого он окажется ближайшим, тоже.
2. При *k = l* алгоритм наоборот чрезмерно устойчив и вырождается в константу.
3. Крайне бедный набор параметров.
4. Точки, расстояние между которыми одинаково, не все будут учитываться.

Прилагается **карта классификации всех объектов** *u* в диапазоне расположения объектов *x(i)* выборки *Xl*

![alt text](https://github.com/Dima262/Velchicskiy/blob/Knn/Rplot.png)
