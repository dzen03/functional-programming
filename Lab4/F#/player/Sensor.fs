namespace Tetris

open UnityEngine
open Unity.MLAgents.Sensors

type TetrisGridSensor(name: string, width: int, height: int) =
    let boardWidth = width
    let boardHeight = height
    let mutable board = Array2D.zeroCreate<int> boardWidth boardHeight
    let sensorName = name

    let scaleWidth = 2
    let scaleHeight = 2
    let scaledWidth = boardWidth * scaleWidth
    let scaledHeight = boardHeight * scaleHeight

    let finalWidth = if scaledWidth < 20 then 20 else scaledWidth
    let finalHeight = if scaledHeight < 20 then 20 else scaledHeight

    let observationSpec = ObservationSpec.Visual(1, scaledHeight, scaledWidth)

    member this.UpdateBoard(newBoard: int[,]) =
        if newBoard.GetLength(0) <> boardWidth || newBoard.GetLength(1) <> boardHeight then
            Debug.LogError("New board dimensions do not match the sensor configuration.")
        else
            board <- newBoard

    interface ISensor with
        member _.GetObservationSpec() = observationSpec

        member _.Write(writer: ObservationWriter) =
            for y in 0 .. finalHeight - 1 do
                for x in 0 .. finalWidth - 1 do
                    writer.[0, y, x] <- 0.0f

            let mutable sum = 0

            for by in 0 .. boardHeight - 1 do
                for bx in 0 .. boardWidth - 1 do
                    let value = board.[bx, by]
                    sum <- sum + value

                    for dy in 0 .. scaleHeight - 1 do
                        for dx in 0 .. scaleWidth - 1 do
                            let targetY = by * scaleHeight + dy
                            let targetX = bx * scaleWidth + dx

                            if targetY < finalHeight && targetX < finalWidth then
                                writer.[0, targetY, targetX] <- float32 value

            Debug.Log("total: " + sum.ToString())
            finalWidth * finalHeight

        member _.GetCompressedObservation() = null
        member _.Update() = ()
        member _.Reset() = ()
        member _.GetCompressionSpec() = CompressionSpec.Default()
        member _.GetName() = sensorName


open UnityEngine
open Unity.MLAgents.Sensors

[<AddComponentMenu("ML Agents/Tetris Grid Sensor Component")>]
type TetrisGridSensorComponent() =
    inherit SensorComponent()
    let mutable sensor: TetrisGridSensor option = None

    [<Header("Tetris Board Settings")>]



    [<SerializeField>]
    let mutable boardWidth = 10

    [<SerializeField>]
    let mutable boardHeight = 20

    [<SerializeField>]
    let mutable sensorName = "TetrisGridSensor"


    override this.CreateSensors() =
        sensor <- Some(new TetrisGridSensor(sensorName, boardWidth, boardHeight))
        [| sensor.Value :> ISensor |]

    member this.Update() =
        match sensor with
        | Some sens ->
            let mlScript = this.GetComponent<MLScript>()
            let grid = mlScript.GetGrid()
            let board = Array2D.zeroCreate<int> boardWidth boardHeight

            for i in 0 .. boardWidth - 1 do
                for j in 0 .. boardHeight - 1 do
                    board.[i, j] <- if grid.[i, j].IsNone then 0 else 1

            sens.UpdateBoard(board)
        | None -> ()
