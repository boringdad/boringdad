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
    7


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


initialModel : Model
initialModel =
    let
        stats =
            { multiplication = []
            , missingEntryInMultiplication = []
            , missingEntriesInTable = []
            , currentCount = 0
            }
    in
    { stats = stats
    , exercise = NoExerciseSelected
    , currentGuess = Guess []
    , feedback = NoFeedback
    }


type alias Try =
    { rightAnswer : Bool
    }


type alias Stats =
    { multiplication : List Try
    , missingEntryInMultiplication : List Try
    , missingEntriesInTable : List Try
    , currentCount : Int
    }


type Exercises
    = NoExerciseSelected
    | CurrentMultiplication Int Int
    | MissingEntryInMultiplication Int Int
    | MissingEntriesInTable Int Int Int


type Guessing
    = Guess (List String)


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
    | AnotherMissingEntriesInTable
    | NewExercise Guessing Exercises
    | TypedInto Int String
    | KeyDown Int


nextMultiplication : Cmd Msg
nextMultiplication =
    let
        mkExer =
            \a b -> CurrentMultiplication a b

        initialGuess =
            Guess [ "" ]
    in
    Random.generate (NewExercise initialGuess) <| Random.map2 mkExer (Random.int 0 highInt) (Random.int 0 highInt)


nextMissingEntryInMultiplication : Cmd Msg
nextMissingEntryInMultiplication =
    let
        mkExer =
            \a b -> MissingEntryInMultiplication a (a * b)

        initialGuess =
            Guess [ "" ]
    in
    Random.generate (NewExercise initialGuess) <| Random.map2 mkExer (Random.int 0 highInt) (Random.int 0 highInt)


nextMissingEntriesInTable : Cmd Msg
nextMissingEntriesInTable =
    let
        mkExer =
            \table index1 index2 ->
                if index1 == index2 then
                    MissingEntriesInTable table index1 (modBy (index2 + 1) 10)

                else
                    MissingEntriesInTable table index1 index2

        initialGuess =
            Guess [ "", "" ]
    in
    Random.generate (NewExercise initialGuess) <| Random.map3 mkExer (Random.int 0 highInt) (Random.int 0 10) (Random.int 0 10)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.exercise ) of
        ( AnotherMultiplication, _ ) ->
            ( model, nextMultiplication )

        ( AnotherMissingEntryInMultiplication, _ ) ->
            ( model, nextMissingEntryInMultiplication )

        ( AnotherMissingEntriesInTable, _ ) ->
            ( model, nextMissingEntriesInTable )

        ( NewExercise initialGuess newExercise, _ ) ->
            let
                stats =
                    model.stats

                ( count, exer, guess ) =
                    if stats.currentCount < nbrExercises then
                        ( stats.currentCount + 1, newExercise, initialGuess )

                    else
                        ( 0, NoExerciseSelected, Guess [] )

                s =
                    { stats | currentCount = count }
            in
            ( { model | exercise = exer, stats = s, feedback = NoFeedback, currentGuess = guess }, Cmd.none )

        ( TypedInto idx txt, _ ) ->
            let
                guessList =
                    case model.currentGuess of
                        Guess [] ->
                            []

                        Guess (_ :: []) ->
                            [ txt ]

                        Guess (a :: b :: _) ->
                            if idx == 0 then
                                [ txt, b ]

                            else
                                [ a, txt ]

                m =
                    { model | currentGuess = Guess guessList }
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
        guesses =
            case model.currentGuess of
                Guess l ->
                    List.map (\s -> String.toInt s) l
    in
    case model.exercise of
        NoExerciseSelected ->
            ( model, Cmd.none )

        CurrentMultiplication a b ->
            let
                m =
                    case guesses of
                        [] ->
                            model

                        (Just g1) :: _ ->
                            let
                                ok =
                                    g1 == a * b
                            in
                            updateModelWithGuess ok .multiplication (\s l -> { s | multiplication = l }) model

                        Nothing :: _ ->
                            { model | feedback = Error "Come on - please make a proper guess!" }

                next =
                    delay 1000 AnotherMultiplication
            in
            ( m, next )

        MissingEntryInMultiplication a result ->
            let
                m =
                    case guesses of
                        [] ->
                            model

                        (Just g1) :: _ ->
                            let
                                ok =
                                    a * g1 == result

                                upd =
                                    \s l -> { s | missingEntryInMultiplication = l }
                            in
                            updateModelWithGuess ok .missingEntryInMultiplication upd model

                        Nothing :: _ ->
                            { model | feedback = Error "Come on - please make a proper guess!" }

                next =
                    delay 1000 AnotherMissingEntryInMultiplication
            in
            ( m, next )

        MissingEntriesInTable table idx1 idx2 ->
            let
                m =
                    case guesses of
                        Nothing :: _ ->
                            { model | feedback = Error "Come on - please make a proper guess!" }

                        _ :: Nothing :: _ ->
                            { model | feedback = Error "Come on - please make a proper guess!" }

                        (Just guess1) :: (Just guess2) :: _ ->
                            let
                                ok1 =
                                    table * idx1 == guess1

                                ok2 =
                                    table * idx2 == guess2

                                upd =
                                    \s l -> { s | missingEntriesInTable = l }
                            in
                            updateModelWithGuess (ok1 && ok2) .missingEntriesInTable upd model

                        _ ->
                            model

                next =
                    delay 1000 AnotherMissingEntriesInTable
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
                , p [] [ button [ onClick AnotherMissingEntriesInTable ] [ text "2 ?? 6 8 ..." ] ]
                ]

        CurrentMultiplication a b ->
            let
                question =
                    String.fromInt a ++ " * " ++ String.fromInt b ++ " = "

                v =
                    case g of
                        Guess [] ->
                            ""

                        Guess (s :: _) ->
                            s
            in
            div []
                [ span [] [ text question ]
                , input [ onKeyDown KeyDown, onInput (TypedInto 0), value v ] []
                ]

        MissingEntryInMultiplication a result ->
            let
                question1 =
                    String.fromInt a ++ " * "

                question2 =
                    " = " ++ String.fromInt result

                v =
                    case g of
                        Guess [] ->
                            ""

                        Guess (s :: _) ->
                            s
            in
            div []
                [ span []
                    [ text question1
                    , input [ onKeyDown KeyDown, onInput (TypedInto 0), value v ] []
                    , text question2
                    ]
                ]

        MissingEntriesInTable table idx1 idx2 ->
            let
                ( guess1, guess2 ) =
                    case g of
                        Guess [] ->
                            ( "", "" )

                        Guess (s :: []) ->
                            ( s, "" )

                        Guess (s :: b :: _) ->
                            ( s, b )

                renderRange =
                    \i ->
                        if i == idx1 then
                            span [] [ input [ onKeyDown KeyDown, onInput (TypedInto 0), value guess1 ] [] ]

                        else if i == idx2 then
                            span [] [ input [ onKeyDown KeyDown, onInput (TypedInto 1), value guess2 ] [] ]

                        else
                            span [] [ text <| " " ++ String.fromInt (table * i) ++ " " ]
            in
            p [] <| List.map renderRange (List.range 0 10)


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

        table =
            fetchRights s.missingEntriesInTable

        table_all =
            s.missingEntriesInTable |> List.length |> String.fromInt
    in
    p []
        [ p [] [ text <| "you have " ++ mul ++ " rights out of " ++ mul_all ++ " multiplications" ]
        , p [] [ text <| "you have " ++ mis_mul ++ " rights out of " ++ mis_mul_all ++ " missing in multiplications" ]
        , p [] [ text <| "you have " ++ table ++ " table fixes of " ++ table_all ++ " table fixes trys" ]
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
        , div [] [ f ]
        , s
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
