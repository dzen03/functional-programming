# Задачи из проекта Эйлера

## Задача 5

2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible
(divisible with no remainder) by all of the numbers from 1 to 20?

## Задача 26

A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

* 1/2 = 0.5
* 1/3 =0.(3)
* 1/4 =0.25
* 1/5 = 0.2
* 1/6 = 0.1(6)
* 1/7 = 0.(142857)
* 1/8 = 0.125
* 1/9 = 0.(1)
* 1/10 = 0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle
in its decimal fraction part.

## Тесты

[![Lab1 tests](https://github.com/dzen03/functional-programming/actions/workflows/lab1test.yml/badge.svg)](https://github.com/dzen03/functional-programming/actions/workflows/lab1test.yml)

## Решения

### Задача №5

#### Рекурсия

```fsharp
module Task5 =
    let rec gcd (a: uint64) (b: uint64) : uint64 =
        match a, b with
        | (a, 0UL) -> a
        | (a, b) -> gcd b (a % b)

    let lcm (a: uint64) (b: uint64) : uint64 = (a * b) / gcd a b

    let smallestMultiple range = range |> List.reduce lcm
```

#### C# интеграция

```csharp
using System;

namespace Lab1.LibCSharp
{
    public static class MathUtils
    {
        public static ulong GCD(ulong a, ulong b)
        {
            while (b != 0)
            {
                ulong temp = b;
                b = a % b;
                a = temp;
            }
            return a;
        }

        public static ulong LCM(ulong a, ulong b)
        {
            if (a == 0 || b == 0)
                return 0;

            return (a / GCD(a, b)) * b;
        }
    }
}
```

```fsharp
module Task5CSharp =
    open Lab1.LibCSharp

    let smallestMultiple (range: uint64 list) : uint64 =
        range |> List.fold (fun acc number -> MathUtils.LCM(acc, number)) 1UL
```

#### Python (5)

```python
def smallest_multiple(arr: list):
  return reduce(math.lcm, arr)
```

### Задача №26

#### Циклы

```fsharp
module Task26 =
    let findCycleLength d =
        let mutable remainder = 1
        let remainders = System.Collections.Generic.Dictionary<int, int>()
        let mutable position = 0
        let mutable cycleLength = 0

        while remainder <> 0 && not (remainders.ContainsKey remainder) do
            remainders.[remainder] <- position
            remainder <- (remainder * 10) % d
            position <- position + 1

        if remainder <> 0 then
            cycleLength <- position - remainders.[remainder]

        cycleLength

    let findMaxCycleUnder limit =
        let mutable maxCycle = 0
        let mutable maxD = 0

        for d in 2 .. limit - 1 do
            let cycleLength = findCycleLength d

            if cycleLength > maxCycle then
                maxCycle <- cycleLength
                maxD <- d

        maxD
```

#### Бесконечная последовательность

```fsharp
module Task26InfSeq =
    let findMaxCycleUnder limit =
        let ds = Seq.initInfinite (fun i -> i + 2) |> Seq.take (limit - 2)
        let cycleLengths = ds |> Seq.map (fun d -> (d, Task26.findCycleLength d))

        cycleLengths |> Seq.maxBy snd |> fst
```

#### Модульный вариант

```fsharp
module Task26Modular =
    let generateSequence (limit: int) : seq<int> = seq { 2 .. limit - 1 }

    let mapCycleLengths (sequence: seq<int>) : seq<int * int> =
        sequence |> Seq.map (fun d -> (d, Task26.findCycleLength d))

    let filterCycles (mappedCycleLengths: seq<int * int>) : seq<int * int> =
        mappedCycleLengths |> Seq.filter (fun (_, cycleLength) -> cycleLength > 0)

    let findMaxCycle (filteredCycleLengths: seq<int * int>) : int =
        filteredCycleLengths |> Seq.maxBy snd |> fst

    let findMaxCycleUnderModular (limit: int) : int =
        generateSequence limit |> mapCycleLengths |> filterCycles |> findMaxCycle
```

#### Python (26)

```python
def find_cycle_length(d):
    remainders = {}
    remainder = 1
    position = 0

    while remainder != 0 and remainder not in remainders:
        remainders[remainder] = position
        remainder = (remainder * 10) % d
        position += 1

    if remainder != 0:
        return position - remainders[remainder]
    else:
        return 0

def find_max_cycle(limit):
    max_cycle = 0
    max_d = 0
    for d in range(2, limit):
        cycle_length = find_cycle_length(d)
        if cycle_length > max_cycle:
            max_cycle = cycle_length
            max_d = d
    return max_d
```
