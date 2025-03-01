namespace Tetris

open UnityEngine

open Logic

type Spawner() =
    inherit MonoBehaviour()

    [<SerializeField>]

    let mutable tetraminos: GameObject[] = [||]

    [<SerializeField>]
    let mutable scoreText: TMPro.TextMeshProUGUI = null

    member this.GetTetraminos = tetraminos

    member this.GetScoreText = scoreText

type Player() =
    inherit MonoBehaviour()

    let mutable gameState: GameState = 
        { startGame with
            Grid = startGame.Grid |> Array2D.copy }

    member this.Start() =
        Debug.Log "Start"
        let tetraminos = this.GetComponent<Spawner>().GetTetraminos

        gameState <-
            { gameState with
                Current = Some(spawn this.gameObject tetraminos) }

    member this.Update() =
        let tetraminos = this.GetComponent<Spawner>().GetTetraminos
        let scoreText = this.GetComponent<Spawner>().GetScoreText
        gameState <- gameLoop gameState scoreText tetraminos this.gameObject None

open Unity.MLAgents
open Unity.MLAgents.Actuators
open Unity.MLAgents.Sensors
open UnityEngine.SceneManagement

type MLScript() =
    inherit Agent()

    let mutable gameState: GameState =
        { startGame with
            Grid = startGame.Grid |> Array2D.copy }

    member this.updateCallback (isGood: bool) (grid: Transform option[,]) (score: float) : unit =
        Debug.Log "Callback"

        match isGood with
        | true ->
            this.AddReward(float32 score * 10.0f)

            let heights =
                [ 0 .. width - 1 ]
                |> List.map (fun col -> [ 0 .. height - 1 ] |> List.tryFindIndex (fun row -> grid.[col, row].IsSome))

            let avgHeight =
                heights
                |> List.filter (fun x -> x.IsSome)
                |> List.map (fun x -> x.Value |> float)
                |> List.average

            this.AddReward(float32 -avgHeight * 0.5f)

            let holes =
                [ 0 .. width - 1 ]
                |> List.sumBy (fun col ->
                    [ 0 .. height - 2 ]
                    |> List.sumBy (fun row ->
                        if grid.[col, row].IsNone && grid.[col, row + 1].IsSome then
                            1
                        else
                            0))

            this.AddReward(float32 -holes * 2.0f)

            ()
        | false ->
            this.AddReward -10.0f


            this.EndEpisode()
            ()

    member this.GetGrid() = gameState.Grid

    override this.Initialize() =
        Debug.Log "Initialize"
        let spawner = this.GetComponent<Spawner>()

        clearGrid gameState.Grid

        gameState <-
            { gameState with
                Current = Some(spawn spawner.gameObject spawner.GetTetraminos) }

        ()

    override this.OnEpisodeBegin() : unit =
        Debug.Log "OnEpisodeBegin"

        clearGrid gameState.Grid

    member this.RunGameLoop() =
        let spawner = this.GetComponent<Spawner>()
        let tetraminos = spawner.GetTetraminos
        let scoreText = spawner.GetScoreText

        gameState <- gameLoop gameState scoreText tetraminos spawner.gameObject (Some this.updateCallback)

    override this.CollectObservations(sensor: VectorSensor) : unit =
        Debug.Log "CollectObservations"


        match gameState.Current with
        | Some current ->
            if Object.ReferenceEquals(current, null) then
                ()
            else

                try
                    getAllChilds current.transform
                    |> Seq.iter (fun ch ->
                        let pos = GetGridPosition ch
                        sensor.AddObservation pos.x
                        sensor.AddObservation pos.y)
                with _ ->
                    Debug.LogWarning "Error accessing transform, object may have been destroyed"
        | None -> ()

    override this.OnActionReceived(actions: ActionBuffers) : unit =
        Debug.Log "OnActionReceived"


        match gameState.Current with
        | Some current ->
            let moveAction = actions.DiscreteActions.[0]
            let rotateAction = actions.DiscreteActions.[1]

            match moveAction with
            | 1 -> moveLeft current gameState.Grid
            | 2 -> moveRight current gameState.Grid
            | _ -> ()

            match rotateAction with
            | 1 -> rotate current gameState.Grid
            | _ -> ()

            ()
        | _ -> ()

        ()

    override this.Heuristic(actionsOut: inref<ActionBuffers>) =
        Debug.Log "Heuristic"
        let mutable discreteActions = actionsOut.DiscreteActions

        discreteActions.[0] <-
            if Input.GetKey(KeyCode.LeftArrow) then 1
            elif Input.GetKey(KeyCode.RightArrow) then 2
            else 3

        discreteActions.[1] <- if Input.GetKey(KeyCode.UpArrow) then 1 else 2

type MLHelper() =
    inherit MonoBehaviour()

    [<SerializeField>]
    let mutable needMLUpdates = false

    member this.FixedUpdate() =
        Debug.Log "Update"

        match needMLUpdates with
        | true ->
            let mlScript = this.GetComponent<MLScript>()
            mlScript.RunGameLoop()
            ()
        | false -> ()

        if Input.GetKeyDown KeyCode.Q then
            SceneManager.LoadScene "game"
        ()
