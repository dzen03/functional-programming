module Tests

open System
open Xunit

module Task5Test =
    open Lab1.Lib.Task5

    [<Theory>]
    [<InlineData(48, 18, 6)>]
    [<InlineData(18, 48, 6)>]
    [<InlineData(10, 11, 1)>]
    let ``GCD test`` a b res =
        let result = gcd a b
        Assert.Equal(res, result)


    [<Theory>]
    [<InlineData(48, 18, 144)>]
    [<InlineData(18, 48, 144)>]
    [<InlineData(10, 11, 110)>]
    [<InlineData(20, 232792560, 232792560)>] // int32 overflow
    let ``LCM test`` a b res =
        let result = lcm a b
        Assert.Equal(res, result)

    [<Fact>]
    let ``Smallest multiple of numbers from 1 to 10 should be 2520`` () =
        let result = smallestMultiple [ 1UL .. 10UL ]
        Assert.Equal(2520UL, result)

    [<Fact>]
    let ``Smallest multiple of numbers from 1 to 20 should be 232792560`` () =
        let result = smallestMultiple [ 1UL .. 20UL ]
        Assert.Equal(232792560UL, result)

module Task5CSharpTest =
    open Lab1.Lib.Task5CSharp

    [<Fact>]
    let ``Smallest multiple of numbers from 1 to 10 should be 2520`` () =
        let result = smallestMultiple [ 1UL .. 10UL ]
        Assert.Equal(2520UL, result)

    [<Fact>]
    let ``Smallest multiple of numbers from 1 to 20 should be 232792560`` () =
        let result = smallestMultiple [ 1UL .. 20UL ]
        Assert.Equal(232792560UL, result)


module Task26Test =
    open Lab1.Lib.Task26

    [<Theory>]
    [<InlineData(2, 0)>]
    [<InlineData(3, 1)>]
    [<InlineData(4, 0)>]
    [<InlineData(7, 6)>]
    [<InlineData(9, 1)>]
    let ``findCycleLength test`` a res =
        let result = findCycleLength a
        Assert.Equal(res, result)

    [<Theory>]
    [<InlineData(10, 7)>]
    [<InlineData(100, 97)>]
    [<InlineData(1000, 983)>]
    let ``findMaxCycleUnder test`` a res =
        let result = findMaxCycleUnder a
        Assert.Equal(res, result)

module Task26InfSeqTest =
    open Lab1.Lib.Task26InfSeq

    [<Theory>]
    [<InlineData(10, 7)>]
    [<InlineData(100, 97)>]
    [<InlineData(1000, 983)>]
    let ``findMaxCycleUnderInf test`` a res =
        let result = findMaxCycleUnder a
        Assert.Equal(res, result)

module Task26ModularTest =
    open Lab1.Lib.Task26Modular

    [<Theory>]
    [<InlineData(10, 7)>]
    [<InlineData(100, 97)>]
    [<InlineData(1000, 983)>]
    let ``findMaxCycleUnderModular test`` a res =
        let result = findMaxCycleUnderModular a
        Assert.Equal(res, result)
