module TimeTravel exposing (addTimeTravel)

import Playground exposing (..)
import Set

addTimeTravel rawGame =
    { initialState = initialStateWithTimeTravel rawGame
    , updateState = updateWithTimeTravel rawGame
    , view = viewWithTimeTravel rawGame
    }

initialStateWithTimeTravel rawGame =
  {
    rawModel = rawGame.initialState
    , paused = False
    , history = []
    , historyPlaybackPosition = 0
  }

keyPressed keyName computer =
    [ String.toLower keyName
    , String.toUpper keyName
    ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)

mousePosToHistoryIndex computer =
  (computer.mouse.x - computer.screen.left) / computer.screen.width * maxVisibleHistory
    |> round


updateWithTimeTravel rawGame computer model =
    let
        replayHistory pastInputs =
            List.foldl rawGame.updateState rawGame.initialState pastInputs
    in
        if keyPressed "T" computer then
            { model | paused = True }
        else if keyPressed "R" computer then
            { model | paused = False
            , history = List.take model.historyPlaybackPosition model.history  
            }
        else if model.paused && computer.mouse.down then
            let
                newPlaybackPosition =
                    min (mousePosToHistoryIndex computer) (List.length model.history)
                pastInputs = List.take newPlaybackPosition model.history
            in
                { model | historyPlaybackPosition = newPlaybackPosition }
        else if not model.paused then
            { model
            | rawModel = rawGame.updateState computer model.rawModel
            , history = model.history ++ [computer]
            , historyPlaybackPosition = List.length model.history + 1
            }
        else
            model

maxVisibleHistory = 2000
controlBarHeight = 65

historyIndexToX computer index =
  (toFloat index) / maxVisibleHistory * computer.screen.width

historyBar color opacity index computer =
    let
        width = historyIndexToX computer index
    in
        rectangle color width controlBarHeight  
            |> move (computer.screen.left + width / 2)
                    (computer.screen.top - controlBarHeight / 2)
            |> fade opacity

viewWithTimeTravel rawGame computer model =
  let
    helpMessage =
        if model.paused then
          "Press R to resume or Drag Bar To Time Travel"
        else
          "Press T to time travel"
    blackBar = historyBar black 0.3 maxVisibleHistory computer
    colorBar = historyBar (rgb 0 0 255) 0.6 (List.length model.history) computer
    playbackBar = historyBar (rgb 255 0 0) 0.4 model.historyPlaybackPosition computer
  in
    (rawGame.view computer model.rawModel) ++
      [ blackBar, colorBar, playbackBar, words white helpMessage |> move 0 (computer.screen.top - controlBarHeight / 2) ]