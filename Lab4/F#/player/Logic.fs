namespace Tetris

open UnityEngine

type Tetramino() =
  inherit MonoBehaviour()

  [<SerializeField>]
  let mutable rotationPoint_ = Vector3.zero

  let mutable markedForDeletion_ = false

  member this.rotationPoint = rotationPoint_

  member this.markedForDeletion
    with get () = markedForDeletion_
    and set (value) = markedForDeletion_ <- value

module Logic =
  open UnityEngine.SceneManagement

  type GameState =
    { Grid: Transform option[,]
      Current: GameObject option
      Time: float
      Score: int }

  type UpdateCallbackOptional = (bool -> Transform option[,] -> float -> unit) option

  let width = 10
  let height = 20
  let g = 7.0

  let initGrid () =
    Array2D.init width height (fun _ _ -> None: Transform option)

  let startGame =
    let initialGrid = initGrid ()

    let initialState =
      { Grid = initialGrid |> Array2D.copy
        Current = None
        Time = 0.0
        Score = 0 }

    initialState

  let getAllChilds (current: Transform) =
    seq {
      for i = 0 to current.childCount - 1 do
        yield current.GetChild i
    }


  let GetGridPositionMove (child: Transform) (move: Vector3) =
    let ref = child.parent.parent
    let localpos = ref.InverseTransformPoint child.position
    Vector2(Mathf.Round localpos.x + move.x, Mathf.Round localpos.y + move.y)


  let GetGridPosition (child: Transform) = GetGridPositionMove child Vector3.zero

  let rec checkValidityMove (grid: Transform option[,]) (current: GameObject) (move: Vector3) =
    getAllChilds current.transform
    |> Seq.forall (fun child ->

      let pos = GetGridPositionMove child move

      pos.x >= 0.0f
      && pos.x < float32 width
      && pos.y >= 0.0f
      && (pos.y >= float32 height
          || pos.y < float32 height && grid.[int pos.x, int pos.y] = None))

  let checkValidity (grid: Transform option[,]) (current: GameObject) =
    checkValidityMove grid current Vector3.zero

  let updateScore (score: int) (scoreText: TMPro.TextMeshProUGUI) =
    match scoreText with
    | null -> ()
    | _ -> scoreText.text <- "Score: " + score.ToString()

    ()

  let getScore linesCleared =
    match linesCleared with
    | 1 -> 1
    | 2 -> 3
    | 3 -> 5
    | 4 -> 8
    | _ -> 0

  let endGame (score: int) (scoreText: TMPro.TextMeshProUGUI) =
    Debug.Log("Game Over; Score: " + score.ToString())

    match scoreText with
    | null -> ()
    | _ -> scoreText.text <- "Game Over\nScore: " + score.ToString()

    ()

  let spawn (spawner: GameObject) (tetraminos: GameObject[]) : GameObject =
    downcast
      Object.Instantiate(
        tetraminos[Random.Range(0, tetraminos.Length)],
        spawner.transform.position,
        Quaternion.identity,
        spawner.transform.parent.transform
      )

  let clearGrid (grid: Transform option[,]) =
    for x in 0 .. width - 1 do
      for y in 0 .. height - 1 do
        match grid.[x, y] with
        | Some obj ->
          Object.Destroy obj.gameObject
          grid.[x, y] <- None
        | None -> ()

  let moveLeft (current: GameObject) grid =
    if checkValidityMove grid current (Vector3(-1.0f, 0.0f, 0.0f)) then
      current.transform.position <- current.transform.position + Vector3(-1.0f, 0.0f, 0.0f)

  let moveRight (current: GameObject) grid =
    if checkValidityMove grid current (Vector3(1.0f, 0.0f, 0.0f)) then
      current.transform.position <- current.transform.position + Vector3(1.0f, 0.0f, 0.0f)

  let rotate (current: GameObject) grid =
    current.transform.RotateAround(
      current.transform.TransformPoint(current.GetComponent<Tetramino>().rotationPoint),
      Vector3(0.0f, 0.0f, 1.0f),
      -90.0f
    )

    if not (checkValidity grid current) then
      current.transform.RotateAround(
        current.transform.TransformPoint(current.GetComponent<Tetramino>().rotationPoint),
        Vector3(0.0f, 0.0f, 1.0f),
        90.0f
      )

  let updateInput
    (current: GameObject option)
    grid
    scoreText
    (tetraminos: GameObject[])
    spawner
    score
    =
    let obj, score =
      match current with
      | Some c ->
        if Input.GetKeyDown KeyCode.LeftArrow then
          moveLeft c grid

        if Input.GetKeyDown KeyCode.RightArrow then
          moveRight c grid

        if Input.GetKeyDown(KeyCode.UpArrow) then
          rotate c grid

        current, score

      | None -> current, score

    if Input.GetKeyDown KeyCode.Q then
      clearGrid grid
      SceneManager.LoadScene "training"

    if Input.GetKeyDown KeyCode.R then
      match obj with
      | Some c -> Object.Destroy c.gameObject
      | _ -> ()

      updateScore 0 scoreText
      clearGrid grid
      Some(spawn spawner tetraminos), 0
    else
      obj, score

  let addToGrid (grid: Transform option[,]) (current: GameObject) =
    let isOk =
      getAllChilds current.transform
      |> Seq.forall (fun child ->
        let pos = GetGridPosition child

        pos.y < float32 height)

    if isOk then
      getAllChilds current.transform
      |> Seq.iter (fun child ->
        let pos = GetGridPosition child

        grid.[int pos.x, int pos.y] <- Some child)

    isOk

  let checkForLines (grid: Transform option[,]) =
    let linesCleared =
      [| for i in height - 1 .. -1 .. 0 do
           if List.forall (fun x -> grid.[x, i] <> None) [ 0 .. width - 1 ] then
             yield i |]

    linesCleared
    |> Seq.iter (fun i ->
      Debug.Log("Line cleared: " + i.ToString())

      List.iter
        (fun x ->
          if grid.[x, i].IsSome then
            Object.Destroy(grid.[x, i].Value.gameObject)

          grid.[x, i] <- None)
        [ 0 .. width - 1 ]

      for y in i .. height - 1 do
        for x in 0 .. width - 1 do
          match grid.[x, y] with
          | Some child ->
            grid.[x, y - 1] <- Some child
            grid.[x, y] <- None
            child.position <- child.position + Vector3(0.0f, -1.0f, 0.0f)
          | None -> ())

    linesCleared.Length

  let updatePlayer
    (current: GameObject option)
    grid
    tetraminos
    spawner
    score
    scoreText
    (callback: UpdateCallbackOptional)
    : GameObject option * int =
    match current with
    | Some c ->
      if not (checkValidityMove grid c (Vector3(0.0f, -1.0f, 0.0f))) then
        c.GetComponent<Tetramino>().enabled <- false

        if not (addToGrid grid c) then
          endGame score scoreText

          match callback with
          | Some cb -> cb false grid 0
          | None -> ()

          current, score
        else
          let linesCleared = checkForLines grid
          let addScore = getScore linesCleared
          updateScore (score + addScore) scoreText

          match callback with
          | Some cb -> cb true grid addScore
          | None -> ()

          Some(spawn spawner tetraminos), score + addScore
      else
        c.transform.position <- c.transform.position + Vector3(0.0f, -1.0f, 0.0f)
        current, score
    | _ -> current, score

  let gameLoop
    gameState
    scoreText
    (tetraminos: GameObject[])
    spawner
    (callback: UpdateCallbackOptional)
    =
    Debug.Log "Game Loop"
    let grid = gameState.Grid
    let time = gameState.Time + float Time.deltaTime * float Time.timeScale
    let current = gameState.Current
    let score = gameState.Score

    let timeDelta = if Input.GetKey(KeyCode.DownArrow) then g * 20.0 else g


    let updatePlayerIfNeeded =
      if float time > 1.0 / timeDelta then
        let updatedCurrent, newScore =
          updatePlayer current grid tetraminos spawner score scoreText callback

        Debug.Log("newScore: " + newScore.ToString())

        { gameState with
            Current = updatedCurrent
            Time = 0.0
            Score = newScore }
      else
        { gameState with Time = time }

    let updatedGameState = updatePlayerIfNeeded

    match callback with
    | None ->
      let finalCurrent, newScore =
        updateInput
          updatedGameState.Current
          grid
          scoreText
          tetraminos
          spawner
          updatedGameState.Score

      { updatedGameState with
          Current = finalCurrent
          Score = newScore }
    | Some _ -> updatedGameState
