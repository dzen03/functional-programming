namespace Lab1.Lib

module Task5 =
    let rec gcd (a: uint64) (b: uint64) : uint64 =
        match a, b with
        | (a, 0UL) -> a
        | (a, b) -> gcd b (a % b)

    let lcm (a: uint64) (b: uint64) : uint64 = (a * b) / gcd a b

    let smallestMultiple range = range |> List.reduce lcm

module Task5CSharp =
    open Lab1.LibCSharp

    let smallestMultiple (range: uint64 list) : uint64 =
        range |> List.fold (fun acc number -> MathUtils.LCM(acc, number)) 1UL

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

module Task26InfSeq =
    let findMaxCycleUnder limit =
        let ds = Seq.initInfinite (fun i -> i + 2) |> Seq.take (limit - 2)
        let cycleLengths = ds |> Seq.map (fun d -> (d, Task26.findCycleLength d))

        cycleLengths |> Seq.maxBy snd |> fst

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
