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

[<EntryPoint>]
let main argv =
  let opts = parseArgs argv

  if not opts.UseLinear && not opts.UseNewton then
    printfn "Usage: Lib3 [--linear] [--newton [-n N]] [--step S]"
    1
  else
    let inputLines =
      Seq.initInfinite (fun _ -> Console.In.ReadLine())
      |> Seq.takeWhile (fun line -> line <> null)
      |> Seq.cache

    inputLines |> Seq.iter (fun line -> printfn "< %s" line)

    let points = inputLines |> Seq.choose parsePoint

    if opts.UseLinear then
      let ptsList = Seq.toList points

      match ptsList with
      | [] -> ()
      | hd :: _ -> printfn "> linear: %g %g" hd.X hd.Y

      for pt in linearInterpolationSeq opts.Step points do
        if pt.X <> (List.head ptsList).X then
          printfn "> linear: %g %g" pt.X pt.Y

    if opts.UseNewton then
      for pt in newtonInterpolationSeq opts.Step opts.NewtonN points do
        printfn "> newton: %g %g" pt.X pt.Y

    0
