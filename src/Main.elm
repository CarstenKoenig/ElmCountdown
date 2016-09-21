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


insert : Formula -> Formula -> Maybe Formula
insert value into =
    case into of
        Hole ->
            Just value

        Number _ ->
            Nothing

        Plus a b ->
            case insert value a of
                Just a' ->
                    Just (Plus a' b)

                Nothing ->
                    insert value b |> Maybe.map (Plus a)

        Minus a b ->
            case insert value a of
                Just a' ->
                    Just (Minus a' b)

                Nothing ->
                    insert value b |> Maybe.map (Minus a)

        Mult a b ->
            case insert value a of
                Just a' ->
                    Just (Mult a' b)

                Nothing ->
                    insert value b |> Maybe.map (Mult a)

        Div a b ->
            case insert value a of
                Just a' ->
                    Just (Div a' b)

                Nothing ->
                    insert value b |> Maybe.map (Div a)


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
    , operators : List Operator
    , goal : Int
    , formula : Formula
    }


clear : Model -> Model
clear model =
    { model | formula = Hole }


type Message
    = NoOp
    | Clear
    | AddOperator (Formula -> Formula -> Formula)
    | AddNumber Int


initialModel : Model
initialModel =
    { numbers = [ 1, 2, 3 ], operators = allOperators, goal = 6, formula = Hole }


update : Message -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Clear ->
            clear model

        AddNumber n ->
            { model | formula = Maybe.withDefault model.formula (insert (Number n) model.formula) }

        AddOperator op ->
            { model | formula = Maybe.withDefault model.formula (insert (op Hole Hole) model.formula) }


view : Model -> Html Message
view model =
    Html.div
        []
        [ viewNumberButtons model.numbers
        , viewOperatorButtons model.operators
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
        , Events.onClick (AddNumber n)
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
