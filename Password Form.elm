-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--


module Main exposing (..)

import Browser
import Char exposing (fromCode, isDigit, isLower, isUpper)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (fromChar)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , ul [ style "list-style" "none" ]
            [ li []
                [ vDrawSymbol pwLength model
                , text " Password must be at least 8 characters long."
                ]
            , li []
                [ vDrawSymbol pwIsUpper model
                , text " Password must contain at least one uppercase character."
                ]
            , li []
                [ vDrawSymbol pwIsLower model
                , text " Password must contain at least one lowercase character."
                ]
            , li []
                [ vDrawSymbol pwIsDigit model
                , text " Password must contain at least one numeric character."
                ]
            , li []
                [ vDrawSymbol pwIsMatched model
                , text " Passwords must match."
                ]
            ]
        , vSubmitButton model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


vSubmitButton : Model -> Html msg
vSubmitButton model =
    if pwLength model && pwIsUpper model && pwIsLower model && pwIsDigit model && pwIsMatched model then
        button [] [ text "Submit" ]

    else
        button [ disabled True ] [ text "Submit" ]


vDrawSymbol : (Model -> Bool) -> Model -> Html Msg
vDrawSymbol pwAttribute model =
    if pwAttribute model then
        vDrawCheckMark

    else
        vDrawCircle


vDrawCircle : Html msg
vDrawCircle =
    span [] [ text (fromChar (fromCode 10061)) ]


vDrawCheckMark : Html msg
vDrawCheckMark =
    span [] [ text (fromChar (fromCode 9989)) ]


pwLength : Model -> Bool
pwLength model =
    String.length model.password >= 8 || String.length model.passwordAgain >= 8


pwIsUpper : Model -> Bool
pwIsUpper model =
    String.any isUpper model.password || String.any isUpper model.password


pwIsLower : Model -> Bool
pwIsLower model =
    String.any isLower model.password || String.any isLower model.passwordAgain


pwIsDigit : Model -> Bool
pwIsDigit model =
    String.any isDigit model.password || String.any isDigit model.passwordAgain


pwIsEmpty : Model -> Bool
pwIsEmpty model =
    String.length model.password == 0 || String.length model.passwordAgain == 0


pwIsMatched : Model -> Bool
pwIsMatched model =
    model.password == model.passwordAgain && not (pwIsEmpty model)
