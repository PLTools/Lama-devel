# compiler-workout

Supplementary repository for compiler course.

Prerequisites: ocaml [http://ocaml.org], opam [http://opam.ocaml.org].

Building:

* `opam pin add GT https://github.com/dboulytchev/GT.git`
* `opam pin add ostap https://github.com/dboulytchev/ostap.git`
* `opam install ostap`
* `opam install GT`
* To build the sources: `make` from the top project directory
* To test: `test.sh` from `regression` subfolder



### Про бенчмарки

Было рассмотрено три подхода к созданию синтаксического анализатора:

* LR анализ на основе Menhir
* Нисходящий с помощью библиотеки Opal
* Нисходящий с помощью Ostap

#### Сравнение производительности

Сравнивались три реализации:

* Старый[https://github.com/Kakadu/ostap/tree/master-very-old] Ostap с отключенной обработкой ошибок
* Opal[https://github.com/pyrocat101/opal]
* Menhir[http://gitlab.inria.fr/fpottier/menhir]

Opal не поддерживает человеческие сообщения об ошибках, поэтому в Ostap те были отключены. В Menhir сообщения об
ошибках так просто отключить нельзя.

Производительность показана в таблице. Парсеры запускали в течение 1 секунды и вычислялось сколько раз они успешно отработали. В таблице хранится следующая информация:

* Первый столбец -- название парсера
* Второй столбей -- абсолютная скорость, чем больше, тем лучше
* Третий -- таблица ускорения одного вида парсинга относительно другого. Для строки L и стобца R в ячейке `[L][R]` будет храниться ускроение, которое дает парсер L относительно R, которое вычисляется как (L-R)/R*100%. В зеркальных элементах таблицы при  таком подсчете будут всегда значения противоположного знака.

В итоге получилось, что LR анализатор (Menhir) работает существенно производительнее нисходящего анализа, но наша реализация нисходящего ничем не хуже других реализаций нисходящего анализа.

```
Benchmarking file `./regression/test014.expr`
           Rate    opal   Ostap  menhir
  opal    259/s      --    -30%   -100%
 Ostap    368/s     42%      --   -100%
menhir 264123/s 102048%  71662%      --
Benchmarking file `./regression/test031.expr`
           Rate  Ostap   opal menhir
 Ostap   1922/s     --   -11%   -99%
  opal   2150/s    12%     --   -99%
menhir 356878/s 18466% 16500%     --
Benchmarking file `./regression/test028.expr`
           Rate  Ostap   opal menhir
 Ostap   1989/s     --    -8%   -99%
  opal   2160/s     9%     --   -99%
menhir 326723/s 16323% 15023%     --
Benchmarking file `./regression/test018.expr`
           Rate   opal  Ostap menhir
  opal    796/s     --   -10%   -99%
 Ostap    882/s    11%     --   -99%
menhir 110767/s 13807% 12464%     --
Benchmarking file `./regression/test027.expr`
           Rate  Ostap   opal menhir
 Ostap   2623/s     --   -10%   -99%
  opal   2923/s    11%     --   -99%
menhir 220900/s  8322%  7456%     --
Benchmarking file `./regression/test033.expr`
           Rate  Ostap   opal menhir
 Ostap   2566/s     --   -20%   -99%
  opal   3224/s    26%     --   -99%
menhir 281372/s 10866%  8626%     --
Benchmarking file `./regression/test020.expr`
           Rate   opal  Ostap menhir
  opal   1561/s     --    -2%   -99%
 Ostap   1594/s     2%     --   -99%
menhir 222360/s 14148% 13849%     --
Benchmarking file `./regression/test012.expr`
           Rate  Ostap   opal menhir
 Ostap   1375/s     --    -7%   -99%
  opal   1473/s     7%     --   -99%
menhir 274087/s 19831% 18510%     --
Benchmarking file `./regression/test026.expr`
           Rate  Ostap   opal menhir
 Ostap   3064/s     --   -11%   -99%
  opal   3427/s    12%     --   -98%
menhir 221587/s  7133%  6365%     --
Benchmarking file `./regression/test001.expr`
            Rate  Ostap   opal menhir
 Ostap    7273/s     --   -42%   -99%
  opal   12574/s    73%     --   -99%
menhir 1045279/s 14273%  8213%     --
Benchmarking file `./regression/test029.expr`
           Rate  Ostap   opal menhir
 Ostap   1679/s     --    -6%   -99%
  opal   1788/s     6%     --   -99%
menhir 261000/s 15442% 14494%     --
Benchmarking file `./regression/test002.expr`
           Rate  Ostap   opal menhir
 Ostap   7518/s     --   -29%   -99%
  opal  10656/s    42%     --   -99%
menhir 935942/s 12349%  8684%     --
Benchmarking file `./regression/test017.expr`
           Rate  Ostap   opal menhir
 Ostap   4248/s     --   -18%   -99%
  opal   5166/s    22%     --   -99%
menhir 356754/s  8297%  6806%     --
Benchmarking file `./regression/test025.expr`
           Rate  Ostap   opal menhir
 Ostap   3019/s     --   -20%   -99%
  opal   3762/s    25%     --   -98%
menhir 240693/s  7874%  6297%     --
Benchmarking file `./regression/test035.expr`
           Rate  Ostap   opal menhir
 Ostap   1826/s     --   -13%   -99%
  opal   2102/s    15%     --   -99%
menhir 229154/s 12452% 10803%     --
Benchmarking file `./regression/test013.expr`
           Rate  Ostap   opal menhir
 Ostap   1708/s     --   -14%   -99%
  opal   1978/s    16%     --   -99%
menhir 261865/s 15230% 13138%     --
Benchmarking file `./regression/test032.expr`
           Rate   opal  Ostap menhir
  opal    886/s     --    -6%  -100%
 Ostap    945/s     7%     --  -100%
menhir 204868/s 23028% 21568%     --
Benchmarking file `./regression/test004.expr`
           Rate  Ostap   opal menhir
 Ostap   5301/s     --   -17%   -99%
  opal   6415/s    21%     --   -99%
menhir 562062/s 10502%  8661%     --
Benchmarking file `./regression/test003.expr`
           Rate  Ostap   opal menhir
 Ostap   7135/s     --   -29%   -99%
  opal  10086/s    41%     --   -99%
menhir 847906/s 11784%  8307%     --
Benchmarking file `./regression/test023.expr`
           Rate  Ostap   opal menhir
 Ostap   6238/s     --   -34%   -99%
  opal   9501/s    52%     --   -99%
menhir 775857/s 12337%  8066%     --
Benchmarking file `./regression/test024.expr`
           Rate  Ostap   opal menhir
 Ostap   5744/s     --   -25%   -99%
  opal   7657/s    33%     --   -99%
menhir 554833/s  9560%  7146%     --
Benchmarking file `./regression/test021.expr`
           Rate  Ostap   opal menhir
 Ostap   5316/s     --   -23%   -99%
  opal   6940/s    31%     --   -99%
menhir 677339/s 12642%  9661%     --
Benchmarking file `./regression/test016.expr`
           Rate  Ostap   opal menhir
 Ostap   5861/s     --   -29%   -99%
  opal   8221/s    40%     --   -99%
menhir 643746/s 10883%  7730%     --
Benchmarking file `./regression/test015.expr`
           Rate  Ostap   opal menhir
 Ostap   1694/s     --    -1%   -99%
  opal   1714/s     1%     --   -99%
menhir 214164/s 12543% 12397%     --
Benchmarking file `./regression/test007.expr`
            Rate  Ostap   opal menhir
 Ostap    8987/s     --   -42%   -99%
  opal   15592/s    73%     --   -99%
menhir 1082115/s 11941%  6840%     --
Benchmarking file `./regression/test036.expr`
           Rate   opal  Ostap menhir
  opal    913/s     --    -5%   -99%
 Ostap    966/s     6%     --   -99%
menhir 179548/s 19560% 18482%     --
Benchmarking file `./regression/test010.expr`
           Rate  Ostap   opal menhir
 Ostap   4207/s     --   -12%   -99%
  opal   4770/s    13%     --   -99%
menhir 357895/s  8407%  7403%     --
Benchmarking file `./regression/test006.expr`
           Rate  Ostap   opal menhir
 Ostap   3993/s     --    -9%   -99%
  opal   4369/s     9%     --   -99%
menhir 323620/s  8005%  7308%     --
Benchmarking file `./regression/test030.expr`
           Rate  Ostap   opal menhir
 Ostap   1742/s     --    -9%   -99%
  opal   1923/s    10%     --   -99%
menhir 337926/s 19303% 17472%     --
Benchmarking file `./regression/test011.expr`
            Rate  Ostap   opal menhir
 Ostap    5345/s     --   -38%  -100%
  opal    8680/s    62%     --   -99%
menhir 1221627/s 22754% 13974%     --
Benchmarking file `./regression/test005.expr`
           Rate  Ostap   opal menhir
 Ostap   4846/s     --   -23%   -99%
  opal   6289/s    30%     --   -99%
menhir 568211/s 11626%  8935%     --
Benchmarking file `./regression/test019.expr`
           Rate  Ostap   opal menhir
 Ostap   3280/s     --   -11%   -99%
  opal   3694/s    13%     --   -99%
menhir 339790/s 10259%  9099%     --
Benchmarking file `./regression/test022.expr`
           Rate  Ostap   opal menhir
 Ostap   3940/s     --   -17%   -99%
  opal   4725/s    20%     --   -99%
menhir 367938/s  9240%  7687%     --
Benchmarking file `./regression/test034.expr`
           Rate  Ostap   opal menhir
 Ostap   2059/s     --    -6%   -99%
  opal   2183/s     6%     --   -99%
menhir 282182/s 13607% 12824%     --
Benchmarking file `./regression/test008.expr`
           Rate  Ostap   opal menhir
 Ostap   8972/s     --   -42%   -99%
  opal  15532/s    73%     --   -98%
menhir 833777/s  9193%  5268%     --
Benchmarking file `./regression/test009.expr`
           Rate  Ostap   opal menhir
 Ostap   5508/s     --   -33%   -99%
  opal   8246/s    50%     --   -99%
menhir 577668/s 10387%  6905%     --
```

#### Сравнение размера кода

Реализации находятся в файлах

* LamaOpal.ml
* LamaOstapP5.ml
* LamaMenhir.mly и LamaLexer.mll

Ostap использует специальное синтаксическое расширение для написание парсера, поэтому естесственно, что размер реализации на Ostap меньше, чем на Opal. Но и там, и там, можно описывать специализированные парсеры (например, парсер арифметических выражений), использование которых может сделать код более похожим по размеру.

Menhir позволяет параметризовывать правила другими, в том числе "анонимными" правилами, поддерживает специальный синтаксии для операций EBNF. Поэтому размер непосредственно пасрера можно сопоставить по размеру с реализацией на Ostap, за несколькими исключениями.

* Ostap использует специальный парсер для парсинга арифметических выражений, поэтому эта часть на нём компактнее.
* Menhir использует lexer на основе OCamlLex, в Ostap это реализовано по-другому. Поэтому реализация лексической части на menhir выглядит сущетсвенно длиннее, чем на Ostap.

#### Разбираемый язык

Разновиность LaMa, где мы не используем определение кастомных инфиксных операторов. Такое ограничение выбрано потому, что в menhir будут сложности с их поддержкой, поэтому, для более 
честного сравнения был выбран сокрашщенный язык 
