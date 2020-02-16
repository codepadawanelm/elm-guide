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
                [ vPwLength model
                , text " Password must be at least 8 characters long."
                ]
            , li []
                [ vPwIsUpper model
                , text " Password must contain at least one uppercase character."
                ]
            , li []
                [ vPwIsLower model
                , text " Password must contain at least one lowercase character."
                ]
            , li []
                [ vPwIsDigit model
                , text " Password must contain at least one numeric character."
                ]
            , li []
                [ vPwIsMatched model
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
    if
        pwIsMatched model
            && pwLength model.password
            && pwIsUpper model.password
            && pwIsLower model.password
            && pwIsDigit model.password
    then
        button [ disabled False ] [ text "Submit" ]

    else
        button [ disabled True ] [ text "Submit" ]


vPwLength : Model -> Html msg
vPwLength model =
    if List.any pwLength [ model.password, model.passwordAgain ] then
        vDrawCheckMark

    else
        vDrawCircle


vPwIsUpper : Model -> Html msg
vPwIsUpper model =
    if List.any pwIsUpper [ model.password, model.passwordAgain ] then
        vDrawCheckMark

    else
        vDrawCircle


vPwIsLower : Model -> Html msg
vPwIsLower model =
    if List.any pwIsLower [ model.password, model.passwordAgain ] then
        vDrawCheckMark

    else
        vDrawCircle


vPwIsDigit : Model -> Html msg
vPwIsDigit model =
    if List.any pwIsDigit [ model.password, model.passwordAgain ] then
        vDrawCheckMark

    else
        vDrawCircle


vPwIsMatched : Model -> Html msg
vPwIsMatched model =
    if pwIsMatched model && not (pwIsEmpty model.password) == True then
        vDrawCheckMark

    else
        vDrawCircle


vDrawCircle : Html msg
vDrawCircle =
    span [] [ text (fromChar (fromCode 10061)) ]


vDrawCheckMark : Html msg
vDrawCheckMark =
    span [] [ text (fromChar (fromCode 9989)) ]


pwLength : String -> Bool
pwLength n =
    String.length n >= 8


pwIsUpper : String -> Bool
pwIsUpper n =
    String.any isUpper n


pwIsLower : String -> Bool
pwIsLower n =
    String.any isLower n


pwIsDigit : String -> Bool
pwIsDigit n =
    String.any isDigit n


pwIsEmpty : String -> Bool
pwIsEmpty n =
    String.length n == 0


pwIsMatched : Model -> Bool
pwIsMatched model =
    model.password == model.passwordAgain
