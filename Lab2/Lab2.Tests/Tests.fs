namespace Lab2.Tests

open Xunit
open Lab2.Lib.HashMultiset
open FsCheck
open FsCheck.Xunit

module HashMultisetTests =
  // Unit tests
  [<Fact>]
  let ``Add and countOf test`` () =
    let ms0 = empty<string> 16
    let ms1 = add "a" ms0
    let ms2 = add "a" ms1
    let ms3 = add "b" ms2

    Assert.Equal(2, countOf "a" ms3)
    Assert.Equal(1, countOf "b" ms3)
    Assert.Equal(0, countOf "c" ms3)

  [<Fact>]
  let ``Remove test`` () =
    let ms1 = empty<int> 16 |> add 42 |> add 42 |> add 42
    Assert.Equal(3, countOf 42 ms1)

    let ms2 = remove 42 ms1
    Assert.Equal(2, countOf 42 ms2)

    let ms3 = remove 42 ms2 |> remove 42
    Assert.Equal(0, countOf 42 ms3)

    let ms4 = remove 999 ms3
    Assert.True(equals ms3 ms4)

  [<Fact>]
  let ``Filter test`` () =
    let ms1 = empty<int> 16 |> add 1 |> add 2 |> add 2 |> add 3

    let msFiltered = filter (fun x -> x % 2 = 0) ms1
    Assert.Equal(2, countOf 2 msFiltered)
    Assert.Equal(0, countOf 1 msFiltered)
    Assert.Equal(0, countOf 3 msFiltered)

  [<Fact>]
  let ``Map test`` () =
    let ms1 = empty<int> 16 |> add 1 |> add 2 |> add 2

    let msMapped = map (fun x -> x * 10) ms1

    Assert.Equal(1, countOf 10 msMapped)
    Assert.Equal(2, countOf 20 msMapped)

  [<Fact>]
  let ``Fold test`` () =
    let ms1 = empty<int> 16 |> add 10 |> add 10 |> add 5

    let sum = fold (+) 0 ms1
    Assert.Equal(25, sum)

  [<Fact>]
  let ``toList test`` () =
    let ms0 = empty<string> 16
    let ms1 = ms0 |> add "x" |> add "y" |> add "y"
    let lst = toList ms1

    Assert.Equal(3, lst.Length)
    Assert.Equal(1, lst |> List.filter ((=) "x") |> List.length)
    Assert.Equal(2, lst |> List.filter ((=) "y") |> List.length)

  [<Fact>]
  let ``Merge test`` () =
    let ms0 = empty<int> 16
    let ms1 = ms0 |> add 1 |> add 2
    let ms2 = ms0 |> add 2 |> add 3

    let merged = merge ms1 ms2

    Assert.Equal(1, countOf 1 merged)
    Assert.Equal(2, countOf 2 merged)
    Assert.Equal(1, countOf 3 merged)

  [<Fact>]
  let ``IsEmpty and length test`` () =
    let ms0 = empty<char> 16
    Assert.True(isEmpty ms0)
    Assert.Equal(0, length ms0)

    let ms1 = add 'a' ms0
    Assert.False(isEmpty ms1)
    Assert.Equal(1, length ms1)

  // Property tests
  [<Property>]
  let ``Monoid left identity`` (xs: int list) =
    let cap = 16
    let initialMs = xs |> List.fold (fun ms x -> add x ms) (empty cap)
    let left = merge (empty cap) initialMs
    equals left initialMs

  [<Property>]
  let ``Monoid right identity`` (xs: int list) =
    let cap = 16
    let initialMs = xs |> List.fold (fun ms x -> add x ms) (empty cap)
    let right = merge initialMs (empty cap)
    equals right initialMs

  [<Property>]
  let ``Monoid associativity`` (xs: int list, ys: int list, zs: int list) =
    let cap = 16

    let fromList ls =
      ls |> List.fold (fun ms x -> add x ms) (empty cap)

    let msX = fromList xs
    let msY = fromList ys
    let msZ = fromList zs

    let left = merge msX (merge msY msZ)
    let right = merge (merge msX msY) msZ
    left =? right

  [<Property>]
  let ``Add makes countOf > 0`` (x: int) =
    let ms0 = empty<int> 16
    let ms1 = add x ms0
    countOf x ms1 > 0
