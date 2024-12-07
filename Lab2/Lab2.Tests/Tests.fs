// Тестовый проект
namespace Lab2.Tests

open Xunit
open Lab2.Lib.Bag
open FsCheck
open FsCheck.Xunit

module BagTests =

    type BagArbitrary() =
        static member Bag() : Arbitrary<Bag<int>> =
            Arb.generate<list<int>>
            |> Gen.map (fun xs -> xs |> List.fold (fun (bag: Bag<int>) x -> bag.Add(x)) Bag<int>.Empty)
            |> Arb.fromGen

    Arb.register<BagArbitrary> () |> ignore

    // Module tests

    [<Fact>]
    let ``Add elements to Bag`` () =
        let bag = Bag<int>.Empty.Add(1).Add(2).Add(1)
        let expected: int list = [ 1; 1; 2 ]
        let actual: int list = bag.ToList() |> List.sort
        Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``Remove elements from Bag`` () =
        let bag = Bag<int>.Empty.Add(1).Add(2).Add(1)
        let bagAfterRemove = bag.Remove(1)
        let expected = [ 1; 2 ]
        let actual = bagAfterRemove.ToList() |> List.sort
        Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``Filter Bag`` () =
        let bag = Bag<int>.Empty.Add(1).Add(2).Add(3).Add(2)
        let filteredBag = bag.Filter(fun x -> x > 1)
        let expected = [ 2; 2; 3 ]
        let actual = filteredBag.ToList() |> List.sort
        Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``Map Bag`` () =
        let bag = Bag<int>.Empty.Add(1).Add(2).Add(3)
        let mappedBag = bag.Map(fun x -> x * 2)
        let expected = [ 2; 4; 6 ]
        let actual = mappedBag.ToList() |> List.sort
        Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``Fold Bag`` () =
        let bag = Bag<int>.Empty.Add(1).Add(2).Add(3)
        let sum = bag.Fold((fun acc x count -> acc + x * count), 0)
        Assert.Equal<int>(6, sum)

    [<Fact>]
    let ``Monoid identity property`` () =
        let bag = Bag<int>.Empty.Add(1).Add(2).Add(1)
        let result = bag + Bag<int>.Empty
        Assert.Equal<Bag<int>>(bag, result)

    [<Fact>]
    let ``Monoid associativity property`` () =
        let bag1 = Bag<int>.Empty.Add(1)
        let bag2 = Bag<int>.Empty.Add(2)
        let bag3 = Bag<int>.Empty.Add(3)
        let left = bag1 + (bag2 + bag3)
        let right = (bag1 + bag2) + bag3
        Assert.Equal<Bag<int>>(left, right)

    [<Fact>]
    let ``Bag equality`` () =
        let bag1 = Bag<int>.Empty.Add(1).Add(2)
        let bag2 = Bag<int>.Empty.Add(2).Add(1)
        Assert.True(bag1.Equals(bag2))

    // Property-based tests

    [<Property(Arbitrary = [| typeof<BagArbitrary> |])>]
    let ``Monoid identity holds`` (bag: Bag<int>) =
        let leftIdentity = Bag<int>.Empty + bag
        let rightIdentity = bag + Bag<int>.Empty
        (leftIdentity = bag) && (rightIdentity = bag)

    [<Property(Arbitrary = [| typeof<BagArbitrary> |])>]
    let ``Monoid associativity holds`` (a: Bag<int>, b: Bag<int>, c: Bag<int>) =
        let left = a + (b + c)
        let right = (a + b) + c
        left = right

    [<Property(Arbitrary = [| typeof<BagArbitrary> |])>]
    let ``Adding and removing element`` (bag: Bag<int>, x: int) =
        let bagWithX = bag.Add(x)
        let bagAfterRemoval = bagWithX.Remove(x)
        bagAfterRemoval = bag || bagAfterRemoval = bag.Remove(x)
