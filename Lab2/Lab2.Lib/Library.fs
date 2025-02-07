namespace Lab2.Lib

module HashMultiset =

  type HashMultiset<'T when 'T: comparison> =
    { Buckets: ('T * int) list array
      Capacity: int
      Size: int }

  let empty<'T when 'T: comparison> capacity =
    { Buckets = Array.init<('T * int) list> capacity (fun _ -> [])
      Capacity = capacity
      Size = 0 }

  // make index >=0
  let private index (capacity: int) (x: 'T) = (hash x &&& 0x7FFFFFFF) % capacity

  let private tryFindCountInList (key: 'T) (lst: ('T * int) list) =
    lst |> List.tryFind (fun (k, _) -> k = key) |> Option.map snd

  // add without check of capacity
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

  let private applyTimes (folder: 'State -> 'T -> 'State) (state: 'State) (x: 'T) (count: int) =
    let rec loop st n =
      if n <= 0 then st else loop (folder st x) (n - 1)

    loop state count

  let fold (folder: 'State -> 'T -> 'State) (state: 'State) (ms: HashMultiset<'T>) : 'State =
    Array.fold
      (fun acc chain -> List.fold (fun st (k, cnt) -> applyTimes folder st k cnt) acc chain)
      state
      ms.Buckets

  let private rehash (newCapacity: int) (ms: HashMultiset<'T>) : HashMultiset<'T> =
    let newMs = empty<'T> newCapacity

    let rec addNCopies k n acc =
      if n <= 0 then acc else addNCopies k (n - 1) (doAdd k acc)

    fold (fun acc x -> doAdd x acc) newMs ms

  let private ensureCapacity (ms: HashMultiset<'T>) : HashMultiset<'T> =
    let loadFactor = float ms.Size / float ms.Capacity

    if loadFactor > 0.75 then
      rehash (ms.Capacity * 2) ms
    else
      ms

  let add (key: 'T) (ms: HashMultiset<'T>) : HashMultiset<'T> = ms |> doAdd key |> ensureCapacity

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

  let countOf (key: 'T) (ms: HashMultiset<'T>) =
    let i = index ms.Capacity key

    match tryFindCountInList key ms.Buckets.[i] with
    | Some c -> c
    | None -> 0

  let contains (key: 'T) (ms: HashMultiset<'T>) = countOf key ms > 0

  let merge (ms1: HashMultiset<'T>) (ms2: HashMultiset<'T>) : HashMultiset<'T> =
    let rec addNCopies k n acc =
      if n <= 0 then acc else addNCopies k (n - 1) (doAdd k acc)

    let merged =
      Array.fold
        (fun acc chain -> List.fold (fun a (k, cnt) -> addNCopies k cnt a) acc chain)
        ms1
        ms2.Buckets

    ensureCapacity merged

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


  let foldBack (folder: 'T -> 'State -> 'State) (ms: HashMultiset<'T>) (state: 'State) : 'State =
    let bucketsReversed = Array.rev ms.Buckets

    Array.fold
      (fun acc chain ->
        let chainRev = List.rev chain

        List.fold (fun st (k, cnt) -> applyTimes (fun s x -> folder x s) st k cnt) acc chainRev)
      state
      bucketsReversed


  let toList (ms: HashMultiset<'T>) : 'T list =
    fold (fun acc x -> x :: acc) [] ms |> List.rev

  let length (ms: HashMultiset<'T>) = ms.Size

  let isEmpty (ms: HashMultiset<'T>) = (ms.Size = 0)

  let equals (ms1: HashMultiset<'T>) (ms2: HashMultiset<'T>) =
    if ms1.Size <> ms2.Size then
      false
    else
      ms1.Buckets
      |> Array.forall (fun chain -> chain |> List.forall (fun (k, cnt) -> countOf k ms2 = cnt))

  let (=?) (ms1: HashMultiset<'T>) (ms2: HashMultiset<'T>) = equals ms1 ms2
