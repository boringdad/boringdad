module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Process
import Random
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


highInt : Int
highInt =
    7


nbrExercises : Int
nbrExercises =
    5


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


initialModel : Model
initialModel =
    let
        stats =
            { multiplication = []
            , missingEntryInMultiplication = []
            , currentCount = 0
            }
    in
    { stats = stats
    , exercise = NoExerciseSelected
    , currentGuess = NoGuessYet
    , feedback = NoFeedback
    }


type alias Try =
    { rightAnswer : Bool
    }


type alias Stats =
    { multiplication : List Try
    , missingEntryInMultiplication : List Try
    , currentCount : Int
    }


type Exercises
    = NoExerciseSelected
    | CurrentMultiplication Int Int
    | MissingEntryInMultiplication Int Int


type Guessing
    = NoGuessYet
    | Guess String


type Feedback
    = NoFeedback
    | Hurray String
    | Error String


type alias Model =
    { stats : Stats
    , exercise : Exercises
    , currentGuess : Guessing
    , feedback : Feedback
    }


type Msg
    = AnotherMultiplication
    | AnotherMissingEntryInMultiplication
    | NewExercise Exercises
    | TypedInto String
    | KeyDown Int


nextMultiplication : Cmd Msg
nextMultiplication =
    let
        mkExer =
            \a b -> CurrentMultiplication a b
    in
    Random.generate NewExercise <| Random.map2 mkExer (Random.int 0 highInt) (Random.int 0 highInt)


nextMissingEntryInMultiplication : Cmd Msg
nextMissingEntryInMultiplication =
    let
        mkExer =
            \a b -> MissingEntryInMultiplication a (a * b)
    in
    Random.generate NewExercise <| Random.map2 mkExer (Random.int 0 highInt) (Random.int 0 highInt)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.exercise ) of
        ( AnotherMultiplication, _ ) ->
            ( model, nextMultiplication )

        ( AnotherMissingEntryInMultiplication, _ ) ->
            ( model, nextMissingEntryInMultiplication )

        ( NewExercise newExercise, _ ) ->
            let
                stats =
                    model.stats

                ( count, exer ) =
                    if stats.currentCount < nbrExercises then
                        ( stats.currentCount + 1, newExercise )

                    else
                        ( 0, NoExerciseSelected )

                s =
                    { stats | currentCount = count }
            in
            ( { model | exercise = exer, stats = s, feedback = NoFeedback, currentGuess = NoGuessYet }, Cmd.none )

        ( TypedInto txt, _ ) ->
            let
                m =
                    { model | currentGuess = Guess txt }
            in
            ( m, Cmd.none )

        ( KeyDown key, _ ) ->
            if key == 13 then
                executeCurrentGuess model

            else
                ( model, Cmd.none )


updateModelWithGuess : Bool -> (Stats -> List Try) -> (Stats -> List Try -> Stats) -> Model -> Model
updateModelWithGuess answerIsRight getTries setTries model =
    let
        tries =
            getTries model.stats

        try =
            { rightAnswer = answerIsRight }

        stats =
            setTries model.stats (try :: tries)

        feedback =
            if answerIsRight then
                Hurray "You are right!!! Great!!"

            else
                Error "No, sorry - you are wrong!"
    in
    { model | stats = stats, feedback = feedback }


executeCurrentGuess : Model -> ( Model, Cmd Msg )
executeCurrentGuess model =
    let
        g =
            case model.currentGuess of
                NoGuessYet ->
                    Nothing

                Guess n ->
                    String.toInt n
    in
    case g of
        Nothing ->
            let
                m =
                    { model | feedback = Error "Please make a proper guess!" }
            in
            ( m, Cmd.none )

        Just guess ->
            case model.exercise of
                NoExerciseSelected ->
                    ( model, Cmd.none )

                CurrentMultiplication a b ->
                    let
                        m =
                            updateModelWithGuess (guess == a * b) .multiplication (\s l -> { s | multiplication = l }) model

                        next =
                            delay 1000 AnotherMultiplication
                    in
                    ( m, next )

                MissingEntryInMultiplication a result ->
                    let
                        isRight =
                            a * guess == result

                        upd =
                            \s l -> { s | missingEntryInMultiplication = l }

                        m =
                            updateModelWithGuess isRight .missingEntryInMultiplication upd model

                        next =
                            delay 1000 AnotherMissingEntryInMultiplication
                    in
                    ( m, next )


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


renderExercise : Exercises -> Guessing -> Html Msg
renderExercise e g =
    case e of
        NoExerciseSelected ->
            div []
                [ p [] [ text "Vælg opgavetype : " ]
                , p [] [ button [ onClick AnotherMultiplication ] [ text "3 * 7 = ???" ] ]
                , p [] [ button [ onClick AnotherMissingEntryInMultiplication ] [ text "3 * ?? = 12" ] ]
                ]

        CurrentMultiplication a b ->
            let
                question =
                    String.fromInt a ++ " * " ++ String.fromInt b ++ " = "

                v =
                    case g of
                        NoGuessYet ->
                            ""

                        Guess s ->
                            s
            in
            div []
                [ span [] [ text question ]
                , input [ onKeyDown KeyDown, onInput TypedInto, value v ] []
                ]

        MissingEntryInMultiplication a result ->
            let
                question1 =
                    String.fromInt a ++ " * "

                question2 =
                    " = " ++ String.fromInt result

                v =
                    case g of
                        NoGuessYet ->
                            ""

                        Guess s ->
                            s
            in
            div []
                [ span []
                    [ text question1
                    , input [ onKeyDown KeyDown, onInput TypedInto, value v ] []
                    , text question2
                    ]
                ]


renderFeedback : Feedback -> Html Msg
renderFeedback feedback =
    case feedback of
        NoFeedback ->
            p [] []

        Hurray s ->
            p [] [ text s ]

        Error s ->
            p [] [ text s ]


renderStats : Stats -> Html Msg
renderStats s =
    let
        fetchRights =
            \l ->
                String.fromInt <|
                    List.foldl
                        (\{ rightAnswer } rights ->
                            rights
                                + (if rightAnswer then
                                    1

                                   else
                                    0
                                  )
                        )
                        0
                        l

        mul =
            fetchRights s.multiplication

        mul_all =
            s.multiplication |> List.length |> String.fromInt

        mis_mul =
            fetchRights s.missingEntryInMultiplication

        mis_mul_all =
            s.missingEntryInMultiplication |> List.length |> String.fromInt
    in
    p []
        [ p [] [ text <| "you have " ++ mul ++ " rights out of " ++ mul_all ++ " multiplications" ]
        , p [] [ text <| "you have " ++ mis_mul ++ " rights out of " ++ mis_mul_all ++ " missing in multiplications" ]
        ]


view : Model -> Html Msg
view model =
    let
        e =
            renderExercise model.exercise model.currentGuess

        s =
            renderStats model.stats

        f =
            renderFeedback model.feedback
    in
    div []
        [ e
        , s
        , div [] [ f ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
