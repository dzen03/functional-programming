namespace Lab3

module Lib =

  type Point = { X: float; Y: float }

  type Options =
    { UseLinear: bool
      UseNewton: bool
      Step: float
      NewtonN: int }

  let parsePoint (line: string) : Point option =
    let parts =
      line.Split([| ' '; '\t'; ';'; ',' |], System.StringSplitOptions.RemoveEmptyEntries)

    if parts.Length >= 2 then
      match System.Double.TryParse(parts.[0]), System.Double.TryParse(parts.[1]) with
      | (true, x), (true, y) -> Some { X = x; Y = y }
      | _ -> None
    else
      None

  let rec findSegment (pts: list<Point>) (x: float) : (Point * Point) =
    match pts with
    | p0 :: p1 :: _ when x <= p1.X -> (p0, p1)
    | _ :: rest -> findSegment rest x
    | _ -> failwith "Значение x выходит за диапазон входных точек"

  let linearInterpolate (p0: Point) (p1: Point) (x: float) : float =
    if p1.X = p0.X then
      p0.Y
    else
      p0.Y + ((x - p0.X) / (p1.X - p0.X)) * (p1.Y - p0.Y)

  let linearInterpolationSeq
    (step: float)
    (points: Point list)
    (prevPoint: Point option)
    : seq<Point> =
    let start =
      match prevPoint with
      | Some pt -> pt.X + step
      | None -> (List.head points).X

    let last = (List.last points).X

    seq {
      let rec loop x =
        seq {
          if x <= last then
            let (p0, p1) = findSegment points x
            yield { X = x; Y = linearInterpolate p0 p1 x }
            yield! loop (x + step)
        }

      yield! loop start
    }


  let rec newtonCoeffAux (pts: list<Point>) (i: int) (j: int) : float =
    if i = j then
      (List.item i pts).Y
    else
      let f1 = newtonCoeffAux pts (i + 1) j
      let f0 = newtonCoeffAux pts i (j - 1)
      let xi = (List.item i pts).X
      let xj = (List.item j pts).X
      (f1 - f0) / (xj - xi)

  let newtonCoeffs (pts: list<Point>) : list<float> =
    let n = List.length pts
    [ for j in 0 .. n - 1 -> newtonCoeffAux pts 0 j ]

  let newtonEval (pts: list<Point>) (coeffs: list<float>) (x: float) : float =
    let n = List.length coeffs

    let rec loop j =
      if j = n - 1 then
        List.item (n - 1) coeffs
      else
        (List.item j coeffs) + (x - (List.item j pts).X) * loop (j + 1)

    loop 0


  let newtonInterpolationSeq (step: float) (windowSize: int) (points: Point list) (prevPoint: Point option) : seq<Point> =
    let firstInput = List.head points
    let lastInput = List.last points

    let rec processWindow (lastOut: float) (window: list<Point>) : seq<Point> =
      if List.length window < windowSize then
        Seq.empty
      else
        let coeffs = newtonCoeffs window
        let left = (List.head window).X
        let right = (List.last window).X
        let start = if lastOut + step < left then left else lastOut + step

        let rec yieldPoints x =
          seq {
            if x < right then
              yield
                { X = x
                  Y = newtonEval window coeffs x }

              yield! yieldPoints (x + step)
          }

        let newPoints = yieldPoints start |> Seq.toList

        let newLast =
          if List.isEmpty newPoints then
            lastOut
          else
            (List.last newPoints).X

        seq {
          yield! newPoints
          yield! processWindow newLast (List.tail window)
        }

    let lastOut =
      match prevPoint with
      | Some pt -> pt.X
      | None -> firstInput.X

    seq {
      if prevPoint.IsNone then
        yield firstInput
      yield! processWindow lastOut points
      if lastOut + step < lastInput.X then
        yield lastInput
    }
