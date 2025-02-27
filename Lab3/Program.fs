open System
open Lab3.Lib

let parseArgs (args: string[]) : Options =
  let defaults =
    { UseLinear = false
      UseNewton = false
      Step = 1.0
      NewtonN = 4 }

  let rec parseRec idx opts =
    if idx >= args.Length then
      opts
    else
      match args.[idx].ToLower() with
      | "--linear" -> parseRec (idx + 1) { opts with UseLinear = true }
      | "--newton" -> parseRec (idx + 1) { opts with UseNewton = true }
      | "-n" ->
        if idx + 1 < args.Length then
          match System.Double.TryParse(args.[idx + 1]) with
          | (true, n) ->
            parseRec
              (idx + 2)
              { opts with
                  NewtonN = int n
                  UseNewton = true }
          | _ -> parseRec (idx + 2) opts
        else
          opts
      | "--step" ->
        if idx + 1 < args.Length then
          match System.Double.TryParse(args.[idx + 1]) with
          | (true, s) -> parseRec (idx + 2) { opts with Step = s }
          | _ -> parseRec (idx + 2) opts
        else
          opts
      | _ -> parseRec (idx + 1) opts

  parseRec 0 defaults

let printPoint (direction: string) (algo: string) (point: Point) =
  printfn "%s%s %g %g" direction algo point.X point.Y

let printLinear = printPoint "> " "linear"
let printNewton = printPoint "> " "newton"

let processLinear
  (opts: Options)
  (point: Point)
  (prevPoint: Point option)
  (prevLinearOut: Point option)
  : (Point option) =
  if opts.UseLinear then
    let res =
      match prevPoint with
      | Some prevPt ->
        let res = linearInterpolationSeq opts.Step ([ prevPt ] @ [ point ]) prevLinearOut

        res
        |> Seq.filter (fun pt -> pt.X > prevPt.X)
        |> Seq.iter (fun pt -> printLinear pt)

        if res |> Seq.length > 0 then
          Some(res |> Seq.last)
        else
          prevLinearOut

      | None ->
        printLinear point
        Some point

    res
  else
    None

let processNewton
  (opts: Options)
  (window: Point list)
  (prevNewtonOut: Point option)
  : (Point option) =
  if List.length window = opts.NewtonN && opts.UseNewton then
    let res = newtonInterpolationSeq opts.Step opts.NewtonN window prevNewtonOut

    match prevNewtonOut with
    | Some prevPt ->
      res
      |> Seq.filter (fun p -> p.X > prevPt.X)
      |> Seq.iter (fun pt -> printNewton pt)
    | None -> res |> Seq.iter (fun pt -> printNewton pt)

    if res |> Seq.length > 0 then
      Some(res |> Seq.last)
    else
      prevNewtonOut
  else
    prevNewtonOut


[<EntryPoint>]
let main argv =
  let opts = parseArgs argv

  if not opts.UseLinear && not opts.UseNewton then
    printfn "Usage: Lib3 [--linear] [--newton [-n N]] [--step S]"
    1
  else
    let rec processLine
      (window: Point list)
      (prevPoint: Point option)
      (prevNewtonOut: Point option)
      (prevLinearOut: Point option)
      =
      let line = Console.In.ReadLine()

      if line = null then
        printfn "< EOF"
        ()
      else
        match parsePoint line with
        | Some pt ->
          printPoint "<" "" pt

          let newWindow =
            if List.length window < opts.NewtonN then
              window @ [ pt ]
            else
              (List.tail window) @ [ pt ]

          let newLinearOut = processLinear opts pt prevPoint prevLinearOut
          let newNewtonOut = processNewton opts newWindow prevNewtonOut

          processLine newWindow (Some pt) newNewtonOut newLinearOut

        | None ->
          printfn "ERROR"
          processLine window prevPoint prevNewtonOut prevLinearOut

    processLine [] None None None
    0
