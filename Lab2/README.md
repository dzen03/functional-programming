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
    "line_length": 100
  }
}
-->

### Add

```fsharp
let add (key: 'T) (ms: HashMultiset<'T>) : HashMultiset<'T> = ms |> doAdd key |> ensureCapacity

let private doAdd (key: 'T) (ms: HashMultiset<'T>) : HashMultiset<'T> =
  let i = index ms.Capacity key
  let oldBucket = ms.Buckets.[i]

  let updatedBucket, delta =
    match tryFindCountInList key oldBucket with
      | Some oldCount ->
        let newBucket =
          oldBucket |> List.map (fun (k, c) -> if k = key then (k, c + 1) else (k, c))

        newBucket, 1
      | None -> (key, 1) :: oldBucket, 1

    let newBuckets = Array.copy ms.Buckets
    newBuckets.[i] <- updatedBucket

    { ms with
        Buckets = newBuckets
        Size = ms.Size + delta }
```

### Remove

```fsharp
  let remove (key: 'T) (ms: HashMultiset<'T>) : HashMultiset<'T> =
    let i = index ms.Capacity key
    let oldBucket = ms.Buckets.[i]

    match tryFindCountInList key oldBucket with
    | Some oldCount when oldCount > 1 ->
      let newBucket =
        oldBucket |> List.map (fun (k, c) -> if k = key then (k, c - 1) else (k, c))

      let newBuckets = Array.copy ms.Buckets
      newBuckets.[i] <- newBucket

      { ms with
          Buckets = newBuckets
          Size = ms.Size - 1 }
    | Some 1 ->
      let newBucket = oldBucket |> List.filter (fun (k, _) -> k <> key)
      let newBuckets = Array.copy ms.Buckets
      newBuckets.[i] <- newBucket

      { ms with
          Buckets = newBuckets
          Size = ms.Size - 1 }
    | _ -> ms
```

### Filter

```fsharp
  let filter (pred: 'T -> bool) (ms: HashMultiset<'T>) : HashMultiset<'T> =
    let newMs = empty<'T> ms.Capacity

    let rec addNCopies k n acc =
      if n <= 0 then acc else addNCopies k (n - 1) (doAdd k acc)

    let filtered =
      Array.fold
        (fun acc chain ->
          List.fold (fun a (k, cnt) -> if pred k then addNCopies k cnt a else a) acc chain)
        newMs
        ms.Buckets

    ensureCapacity filtered
```

### Map

```fsharp
  let map (f: 'T -> 'U) (ms: HashMultiset<'T>) : HashMultiset<'U> =
    let newMs = empty<'U> ms.Capacity

    let rec addNCopies k n acc =
      if n <= 0 then acc else addNCopies k (n - 1) (doAdd k acc)

    let mapped =
      Array.fold
        (fun acc chain -> List.fold (fun a (k, cnt) -> addNCopies (f k) cnt a) acc chain)
        newMs
        ms.Buckets

    ensureCapacity mapped
```

### Fold

```fsharp
  let private applyTimes (folder: 'State -> 'T -> 'State) (state: 'State) (x: 'T) (count: int) =
    let rec loop st n =
      if n <= 0 then st else loop (folder st x) (n - 1)

    loop state count

  let fold (folder: 'State -> 'T -> 'State) (state: 'State) (ms: HashMultiset<'T>) : 'State =
    Array.fold
      (fun acc chain -> List.fold (fun st (k, cnt) -> applyTimes folder st k cnt) acc chain)
      state
      ms.Buckets

```

## Выводы

В ходе выполнения лабораторной работы была успешно реализована полиморфная
неизменяемая структура данных Bag на языке F#. Данный проект позволил глубже
освоить ключевые концепции функционального программирования, такие как
создание пользовательских типов данных, полиморфизм, работа с неизменяемыми
структурами, а также применение рекурсивных алгоритмов.
