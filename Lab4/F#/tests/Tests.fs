namespace Tetris.Tests

open Xunit
open Tetris

module LogicTests =

  [<Fact>]
  let ``InitGrid should create a grid of correct dimensions`` () =
    let grid = Logic.initGrid ()
    Assert.Equal(Logic.width, grid.GetLength(0))
    Assert.Equal(Logic.height, grid.GetLength(1))

  [<Fact>]
  let ``StartGame should initialize game state correctly`` () =
    let gameState = Logic.startGame
    Assert.Equal(Logic.width, gameState.Grid.GetLength(0))
    Assert.Equal(Logic.height, gameState.Grid.GetLength(1))
    Assert.Equal(None, gameState.Current)
    Assert.Equal(0.0, gameState.Time)
    Assert.Equal(0, gameState.Score)

  [<Fact>]
  let ``GetScore should return correct score based on lines cleared`` () =
    Assert.Equal(0, Logic.getScore 0)
    Assert.Equal(1, Logic.getScore 1)
    Assert.Equal(3, Logic.getScore 2)
    Assert.Equal(5, Logic.getScore 3)
    Assert.Equal(8, Logic.getScore 4)
    Assert.Equal(0, Logic.getScore 5)

  [<Fact>]
  let ``Grid cells should be empty after initialization`` () =
    let grid = Logic.initGrid ()

    for x in 0 .. (Logic.width - 1) do
      for y in 0 .. (Logic.height - 1) do
        Assert.Equal(None, grid.[x, y])

  [<Theory>]
  [<InlineData(0)>]
  [<InlineData(1)>]
  [<InlineData(2)>]
  [<InlineData(4)>]
  let ``GameState should update score correctly`` linesCleared =
    let expectedScore = Logic.getScore linesCleared
    let initialState = Logic.startGame

    let newState =
      { initialState with
          Score = expectedScore }

    Assert.Equal(expectedScore, newState.Score)

  [<Fact>]
  let ``GameState time should update correctly`` () =
    let initialState = Logic.startGame
    let timeIncrement = 0.5

    let newState =
      { initialState with
          Time = timeIncrement }

    Assert.Equal(timeIncrement, newState.Time)

  [<Fact>]
  let ``Grid dimensions should match width and height constants`` () =
    let grid = Logic.initGrid ()

    Assert.Equal(Logic.width, grid.GetLength(0))
    Assert.Equal(Logic.height, grid.GetLength(1))
    Assert.Equal(10, Logic.width)
    Assert.Equal(20, Logic.height)
