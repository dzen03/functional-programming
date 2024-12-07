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
        let rec loop (remainder: int) (position: int) (remainders: Map<int, int>) =
            if remainder = 0 || remainders.ContainsKey remainder then
                if remainder = 0 then
                    0
                else
                    position - remainders.[remainder]
            else
                let remainders = remainders.Add(remainder, position)
                loop ((remainder * 10) % d) (position + 1) remainders

        loop 1 0 Map.empty

    let findMaxCycleUnder limit =
        [ 2 .. limit - 1 ]
        |> List.map (fun d -> (d, findCycleLength d))
        |> List.maxBy snd
        |> fst

module Task26InfSeq =
    let findMaxCycleUnder limit =
        let ds = Seq.initInfinite (fun i -> i + 2)
        let cycleLengths = ds |> Seq.map (fun d -> (d, Task26.findCycleLength d))

        cycleLengths |> Seq.take (limit - 2) |> Seq.maxBy snd |> fst

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
