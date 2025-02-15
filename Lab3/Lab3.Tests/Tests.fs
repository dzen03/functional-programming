namespace Lab3

open Xunit
open Lab3.Lib

module Tests =

  [<Fact>]
  let ``parsePoint returns Some for valid CSV input`` () =
    let line = "0,0"

    match parsePoint line with
    | Some p ->
      Assert.Equal(0.0, p.X, 6)
      Assert.Equal(0.0, p.Y, 6)
    | None -> Assert.True(false, "parsePoint returned None for valid input")

  [<Fact>]
  let ``parsePoint returns Some for semicolon-separated input`` () =
    let line = "1;2"

    match parsePoint line with
    | Some p ->
      Assert.Equal(1.0, p.X, 6)
      Assert.Equal(2.0, p.Y, 6)
    | None -> Assert.True(false, "parsePoint returned None for valid input")

  [<Fact>]
  let ``findSegment finds correct interval`` () =
    let pts =
      [ { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 2.0 }
        { X = 3.0; Y = 3.0 } ]

    let (p0, p1) = findSegment pts 0.7
    Assert.Equal(0.0, p0.X, 6)
    Assert.Equal(1.0, p1.X, 6)
    let (p0b, p1b) = findSegment pts 1.4
    Assert.Equal(1.0, p0b.X, 6)
    Assert.Equal(2.0, p1b.X, 6)

  [<Fact>]
  let ``linearInterpolate computes correct value`` () =
    let p0 = { X = 0.0; Y = 0.0 }
    let p1 = { X = 1.0; Y = 1.0 }
    let y = linearInterpolate p0 p1 0.7
    Assert.Equal(0.7, y, 6)

  [<Fact>]
  let ``linearInterpolationSeq yields expected points`` () =
    let points =
      [ { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 2.0 }
        { X = 3.0; Y = 3.0 } ]

    let interpolated = linearInterpolationSeq 0.7 points None |> Seq.toList

    let expected =
      [ { X = 0.0; Y = 0.0 }
        { X = 0.7; Y = 0.7 }
        { X = 1.4; Y = 1.4 }
        { X = 2.1; Y = 2.1 }
        { X = 2.8; Y = 2.8 } ]

    Assert.Equal(expected.Length, interpolated.Length)

    List.iter2
      (fun exp act ->
        Assert.Equal(exp.X, act.X, 6)
        Assert.Equal(exp.Y, act.Y, 6))
      expected
      interpolated

  [<Fact>]
  let ``newtonCoeffs for linear function yields expected coefficients`` () =
    let pts =
      [ { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 2.0 }
        { X = 3.0; Y = 3.0 } ]

    let coeffs = newtonCoeffs pts
    Assert.Equal(0.0, List.head coeffs, 6)
    Assert.Equal(1.0, coeffs.[1], 6)
    Assert.Equal(0.0, coeffs.[2], 6)
    Assert.Equal(0.0, coeffs.[3], 6)

  [<Fact>]
  let ``newtonEval evaluates linear function correctly`` () =
    let pts =
      [ { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 2.0 }
        { X = 3.0; Y = 3.0 } ]

    let coeffs = newtonCoeffs pts
    let testValues = [ 0.0; 0.5; 1.0; 1.5; 2.0; 2.5; 3.0 ]

    testValues
    |> List.iter (fun x ->
      let y = newtonEval pts coeffs x
      Assert.Equal(x, y, 6))

  [<Fact>]
  let ``newtonInterpolationSeq yields expected values for linear data`` () =
    let points =
      [ { X = 0.0; Y = 0.0 }
        { X = 1.0; Y = 1.0 }
        { X = 2.0; Y = 2.0 }
        { X = 3.0; Y = 3.0 }
        { X = 4.0; Y = 4.0 }
        { X = 5.0; Y = 5.0 }
        { X = 7.0; Y = 7.0 }
        { X = 8.0; Y = 8.0 } ]

    let interpolated =
      newtonInterpolationSeq 0.5 4 points
      |> Seq.filter (fun pt -> pt.X < 3.0)
      |> Seq.toList


    let expected =
      [ { X = 0.0; Y = 0.0 }
        { X = 0.5; Y = 0.5 }
        { X = 1.0; Y = 1.0 }
        { X = 1.5; Y = 1.5 }
        { X = 2.0; Y = 2.0 }
        { X = 2.5; Y = 2.5 } ]

    Assert.Equal(expected.Length, interpolated.Length)

    List.iter2
      (fun exp act ->
        Assert.Equal(exp.X, act.X, 6)
        Assert.Equal(exp.Y, act.Y, 6))
      expected
      interpolated
