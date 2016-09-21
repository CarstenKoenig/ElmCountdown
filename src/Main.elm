module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events as Events


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type Formula
    = Hole
    | Number Int
    | Plus Formula Formula
    | Minus Formula Formula
    | Mult Formula Formula
    | Div Formula Formula


eval : Formula -> Maybe Int
eval formula =
    case formula of
        Hole ->
            Nothing

        Number n ->
            Just n

        Plus a b ->
            Maybe.map2 (+) (eval a) (eval b)

        Minus a b ->
            Maybe.map2 (-) (eval a) (eval b)

        Mult a b ->
            Maybe.map2 (*) (eval a) (eval b)

        Div a b ->
            Maybe.map2 (//) (eval a) (eval b)


nrHoles : Formula -> Int
nrHoles formula =
    case formula of
        Hole ->
            1

        Number _ ->
            0

        Plus a b ->
            nrHoles a + nrHoles b

        Minus a b ->
            nrHoles a + nrHoles b

        Mult a b ->
            nrHoles a + nrHoles b

        Div a b ->
            nrHoles a + nrHoles b


filterPossibleNumbers : Formula -> List Int -> List Int
filterPossibleNumbers formula ns =
    let
        isSat n f =
            case f of
                Just f ->
                    isSatisfyable f (removeNumber n ns)

                Nothing ->
                    False
    in
        List.filter (\n -> insert (Number n) formula |> isSat n) ns


filterPossibleOperators : Formula -> List Int -> List Operator
filterPossibleOperators formula ns =
    let
        isSat f =
            case f of
                Just f ->
                    isSatisfyable f ns

                Nothing ->
                    False
    in
        List.filter (\op -> insert (op.action Hole Hole) formula |> isSat) allOperators


isSatisfyable : Formula -> List Int -> Bool
isSatisfyable formula ns =
    let
        holeCount =
            nrHoles formula
    in
        if holeCount == 0 then
            True
        else if List.isEmpty ns then
            False
        else
            let
                numberPicks =
                    List.filterMap (\n -> insert (Number n) formula |> Maybe.map (\formula' -> ( formula', removeNumber n ns ))) ns

                operatorPicks =
                    if holeCount < List.length ns then
                        List.filterMap (\op -> insert (op Hole Hole) formula) [ Plus, Minus, Mult, Div ]
                    else
                        []
            in
                List.any (\( formula', ns' ) -> isSatisfyable formula' ns') numberPicks || List.any (\formula' -> isSatisfyable formula' ns) operatorPicks


removeNumber : Int -> List Int -> List Int
removeNumber n ns =
    let
        remOne ns' =
            case ns' of
                [] ->
                    []

                n' :: ns' ->
                    if n' == n then
                        ns'
                    else
                        n' :: remOne ns'
    in
        remOne ns


isValid : Formula -> Bool
isValid formula =
    case formula of
        Hole ->
            True

        Number n ->
            n > 0

        Plus a b ->
            isValid a && isValid b

        Minus a b ->
            isValid a && isValid b && (Maybe.withDefault True (Maybe.map2 (>) (eval a) (eval b)))

        Mult a b ->
            isValid a && isValid b

        Div a b ->
            isValid a && isValid b && (Maybe.withDefault True (Maybe.map2 (\a b -> a `rem` b == 0) (eval a) (eval b)))


insert : Formula -> Formula -> Maybe Formula
insert value into =
    (insert' value into)
        `Maybe.andThen`
            (\formula ->
                if isValid formula then
                    Just formula
                else
                    Nothing
            )


insert' : Formula -> Formula -> Maybe Formula
insert' value into =
    case into of
        Hole ->
            Just value

        Number _ ->
            Nothing

        Plus a b ->
            case insert' value a of
                Just a' ->
                    Just (Plus a' b)

                Nothing ->
                    insert' value b |> Maybe.map (Plus a)

        Minus a b ->
            case insert' value a of
                Just a' ->
                    Just (Minus a' b)

                Nothing ->
                    insert' value b |> Maybe.map (Minus a)

        Mult a b ->
            case insert' value a of
                Just a' ->
                    Just (Mult a' b)

                Nothing ->
                    insert' value b |> Maybe.map (Mult a)

        Div a b ->
            case insert' value a of
                Just a' ->
                    Just (Div a' b)

                Nothing ->
                    insert' value b |> Maybe.map (Div a)


type alias Operator =
    { display : String
    , action : Formula -> Formula -> Formula
    }


allOperators : List Operator
allOperators =
    [ { display = "+", action = Plus }
    , { display = "-", action = Minus }
    , { display = "*", action = Mult }
    , { display = "/", action = Div }
    ]


type alias Model =
    { numbers : List Int
    , goal : Int
    , availableNumbers : List Int
    , formula : Formula
    }


clear : Model -> Model
clear model =
    { model
        | formula = Hole
        , availableNumbers = model.numbers
    }


type Message
    = NoOp
    | Clear
    | AddOperator (Formula -> Formula -> Formula)
    | ChooseNumber Int


initialModel : Model
initialModel =
    let
        numbers =
            [ 1, 2, 3, 3 ]
    in
        { numbers = numbers, availableNumbers = numbers, goal = 6, formula = Hole }


update : Message -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Clear ->
            clear model

        ChooseNumber n ->
            { model
                | formula = Maybe.withDefault model.formula (insert (Number n) model.formula)
                , availableNumbers = removeNumber n model.availableNumbers
            }

        AddOperator op ->
            { model | formula = Maybe.withDefault model.formula (insert (op Hole Hole) model.formula) }


view : Model -> Html Message
view model =
    Html.div
        []
        [ viewNumberButtons (filterPossibleNumbers model.formula model.availableNumbers)
        , viewOperatorButtons (filterPossibleOperators model.formula model.availableNumbers)
        , viewFormula model.formula
        , Html.div [ Attr.style [ ( "margin", "5px" ) ] ]
            [ Html.button [ Events.onClick Clear ] [ Html.text "C" ]
            , Html.h3 [] [ Html.text (Maybe.withDefault "---" (Maybe.map toString (eval model.formula))) ]
            ]
        ]


viewNumberButtons : List Int -> Html Message
viewNumberButtons ns =
    Html.div
        [ Attr.style
            [ ( "padding", "3px" )
            , ( "margin", "5px" )
            ]
        ]
        (List.map viewNumberButton ns)


viewNumberButton : Int -> Html Message
viewNumberButton n =
    Html.button
        [ Attr.style
            [ ( "display", "inline-block" )
            , ( "width", "24px" )
            , ( "height", "24px" )
            , ( "color", "blue" )
            , ( "margin", "3px" )
            , ( "text-align", "center" )
            , ( "vertical-align", "middle" )
            , ( "line-height", "20px" )
            , ( "font-weight", "bold" )
            ]
        , Events.onClick (ChooseNumber n)
        ]
        [ Html.text (toString n) ]


viewOperatorButtons : List Operator -> Html Message
viewOperatorButtons ops =
    Html.div
        [ Attr.style
            [ ( "padding", "3px" )
            , ( "margin", "5px" )
            ]
        ]
        (List.map viewOperatorButton ops)


viewOperatorButton : Operator -> Html Message
viewOperatorButton op =
    Html.button
        [ Attr.style
            [ ( "display", "inline-block" )
            , ( "width", "24px" )
            , ( "height", "24px" )
            , ( "color", "red" )
            , ( "margin", "3px" )
            , ( "text-align", "center" )
            , ( "vertical-align", "middle" )
            , ( "line-height", "20px" )
            , ( "font-weight", "bold" )
            ]
        , Events.onClick (AddOperator op.action)
        ]
        [ Html.text op.display ]


viewFormula : Formula -> Html Message
viewFormula formula =
    let
        ( _, sub ) =
            viewSubFormula formula True
    in
        Html.div
            [ Attr.style
                [ ( "display", "inline-block" )
                , ( "border-width", "2px" )
                , ( "padding", "2px" )
                , ( "margin", "5px" )
                ]
            ]
            [ sub ]


viewSubFormula : Formula -> Bool -> ( Bool, Html Message )
viewSubFormula formula isActive =
    case formula of
        Hole ->
            ( False, viewHole isActive )

        Number n ->
            ( isActive, viewNumber n )

        Plus a b ->
            viewOperator isActive "+" a b

        Minus a b ->
            viewOperator isActive "-" a b

        Mult a b ->
            viewOperator isActive "*" a b

        Div a b ->
            viewOperator isActive "/" a b


viewHole : Bool -> Html Message
viewHole isActive =
    let
        color =
            if isActive then
                "DarkKhaki"
            else
                "gray"
    in
        Html.div
            [ Attr.style
                [ ( "display", "inline-block" )
                , ( "width", "20px" )
                , ( "height", "20px" )
                , ( "border-radius", "5px" )
                , ( "background", color )
                , ( "color", color )
                , ( "margin", "3px" )
                , ( "text-align", "center" )
                , ( "vertical-align", "middle" )
                , ( "line-height", "20px" )
                , ( "font-weight", "bold" )
                ]
            ]
            [ Html.text "0" ]


viewOperator : Bool -> String -> Formula -> Formula -> ( Bool, Html Message )
viewOperator isActive op a b =
    let
        ( stillActive, subA ) =
            viewSubFormula a isActive

        ( stillActive', subB ) =
            viewSubFormula b stillActive
    in
        ( stillActive'
        , Html.div
            [ Attr.style
                [ ( "display", "inline-block" )
                , ( "border-radius", "5px" )
                , ( "border-width", "2px" )
                , ( "border-style", "solid" )
                , ( "border-color", "blue" )
                , ( "padding", "3px" )
                , ( "margin", "5px" )
                ]
            ]
            [ subA
            , viewOp op
            , subB
            ]
        )


viewNumber : Int -> Html Message
viewNumber n =
    Html.div
        [ Attr.style
            [ ( "display", "inline-block" )
            , ( "width", "20px" )
            , ( "height", "20px" )
            , ( "border-radius", "5px" )
            , ( "background", "LightBlue" )
            , ( "color", "DarkBlue" )
            , ( "margin", "3px" )
            , ( "text-align", "center" )
            , ( "vertical-align", "middle" )
            , ( "line-height", "20px" )
            , ( "font-weight", "bold" )
            ]
        ]
        [ Html.text (toString n) ]


viewOp : String -> Html Message
viewOp op =
    Html.div
        [ Attr.style
            [ ( "display", "inline-block" )
            , ( "width", "18px" )
            , ( "height", "18px" )
            , ( "border-radius", "5px" )
            , ( "background", "LightGray" )
            , ( "color", "blue" )
            , ( "margin", "5px" )
            , ( "font-weight", "bold" )
            , ( "text-align", "center" )
            , ( "vertical-align", "middle" )
            , ( "line-height", "18px" )
            , ( "font-weight", "bold" )
            ]
        ]
        [ Html.text op ]
