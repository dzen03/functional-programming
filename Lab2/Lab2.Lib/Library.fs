namespace Lab2.Lib

module Bag =
    type Bag<'T when 'T: equality> private (buckets: Map<int, list<'T * int>>) =
        let hash x = x.GetHashCode()

        member private _.Buckets = buckets

        static member Empty = Bag<'T>(Map.empty)

        member this.Add(x: 'T) =
            let h = hash x
            let bucket = Map.tryFind h buckets |> Option.defaultValue []

            let updatedBucket =
                match List.tryFind (fun (k, _) -> k = x) bucket with
                | Some(k, count) -> (k, count + 1) :: (List.filter (fun (k', _) -> k' <> x) bucket)
                | None -> (x, 1) :: bucket

            Bag(buckets.Add(h, updatedBucket))

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

        member this.Filter(predicate: 'T -> bool) =
            let newBuckets =
                buckets
                |> Map.map (fun _ bucket -> bucket |> List.filter (fun (k, _) -> predicate k))
                |> Map.filter (fun _ bucket -> not (List.isEmpty bucket))

            Bag(newBuckets)

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

        member this.Fold(folder: 'State -> 'T -> int -> 'State, state: 'State) =
            buckets
            |> Map.toList
            |> List.collect snd
            |> List.fold (fun acc (k, count) -> folder acc k count) state

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

        override this.Equals(obj) =
            match obj with
            | :? Bag<'T> as other -> this.Buckets = other.Buckets
            | _ -> false

        override this.GetHashCode() = buckets.GetHashCode()

        member this.ToList() : 'T list =
            buckets
            |> Map.toList
            |> List.collect (fun (_, bucket) -> bucket |> List.collect (fun (k, count) -> List.init count (fun _ -> k)))
