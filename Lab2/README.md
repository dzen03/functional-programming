# Lab2

Дзензелюк Илья, 335140

## Задание

Bag(multiset) with Separate Chaining Hashmap

## Тесты

[![Lab2 tests](https://github.com/dzen03/functional-programming/actions/workflows/lab2test.yml/badge.svg)](https://github.com/dzen03/functional-programming/actions/workflows/lab2test.yml)

## Решения

<!-- markdownlint-configure-file
{
  "line_length": {
    "ignore_code_blocks": true,
    "line_length": 200
  }
}
-->

### Add

```fsharp
member this.Add(x: 'T) =
    let h = hash x
    let bucket = Map.tryFind h buckets |> Option.defaultValue []

    let updatedBucket =
        match List.tryFind (fun (k, _) -> k = x) bucket with
        | Some(k, count) -> (k, count + 1) :: (List.filter (fun (k', _) -> k' <> x) bucket)
        | None -> (x, 1) :: bucket

    Bag(buckets.Add(h, updatedBucket))
```

### Remove

```fsharp
member this.Remove(x: 'T) =
    let h = hash x

    match Map.tryFind h buckets with
    | Some bucket ->
        let updatedBucket =
            match List.tryFind (fun (k, _) -> k = x) bucket with
            | Some(k, count) when count > 1 -> (k, count - 1) :: (List.filter (fun (k', _) -> k' <> x) bucket)
            | Some _ -> List.filter (fun (k', _) -> k' <> x) bucket
            | None -> bucket

        let updatedBuckets =
            if List.isEmpty updatedBucket then
                buckets.Remove h
            else
                buckets.Add(h, updatedBucket)

        Bag(updatedBuckets)
    | None -> this
```

### Filter

```fsharp
member this.Filter(predicate: 'T -> bool) =
    let newBuckets =
        buckets
        |> Map.map (fun _ bucket -> bucket |> List.filter (fun (k, _) -> predicate k))
        |> Map.filter (fun _ bucket -> not (List.isEmpty bucket))

    Bag(newBuckets)
```

### Map

```fsharp
member this.Map<'U when 'U: equality>(mapping: 'T -> 'U) : Bag<'U> =
    let counts =
        buckets
        |> Seq.collect (fun kvp -> kvp.Value)
        |> Seq.map (fun (k, count) -> (mapping k, count))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, seq) -> (k, seq |> Seq.sumBy snd))
        |> Seq.toList

    let newBuckets =
        counts
        |> Seq.groupBy (fun (k, _) -> k.GetHashCode())
        |> Seq.map (fun (h, items) ->
            let bucket = Seq.toList items
            (h, bucket))
        |> Map.ofSeq

    Bag<'U>(newBuckets)
```

### Fold

```fsharp
member this.Fold(folder: 'State -> 'T -> int -> 'State, state: 'State) =
    buckets
    |> Map.toList
    |> List.collect snd
    |> List.fold (fun acc (k, count) -> folder acc k count) state
```

### + (monoid)

```fsharp
static member (+)(bag1: Bag<'T>, bag2: Bag<'T>) =
    let mergeBuckets b1 b2 =
        Map.fold
            (fun acc h bucket ->
                let existingBucket = Map.tryFind h acc |> Option.defaultValue []

                let mergedBucket =
                    (bucket @ existingBucket)
                    |> List.groupBy fst
                    |> List.map (fun (k, counts) -> (k, counts |> List.sumBy snd))

                acc.Add(h, mergedBucket))
            b1
            b2

    Bag(mergeBuckets bag1.Buckets bag2.Buckets)
```

## Выводы

В ходе выполнения лабораторной работы была успешно реализована полиморфная
неизменяемая структура данных Bag на языке F#. Данный проект позволил глубже
освоить ключевые концепции функционального программирования, такие как
создание пользовательских типов данных, полиморфизм, работа с неизменяемыми
структурами, а также применение рекурсивных алгоритмов.
