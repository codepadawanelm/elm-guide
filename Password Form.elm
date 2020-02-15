-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--


module Main exposing (Model, Msg(..), drawCheckMark, drawCircle, init, main, pwIsDigit, pwIsLower, pwIsUpper, pwLength, pwNotEmpty, update, vPwIsDigit, vPwIsLower, vPwIsMatched, vPwIsUpper, vPwLength, vSubmitButton, view, viewInput)

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
        model.password
            == model.passwordAgain
            && pwLength model.password
            == True
            && pwIsUpper model.password
            == True
            && pwIsLower model.password
            == True
            && pwIsDigit model.password
            == True
    then
        button [ disabled False ] [ text "Submit" ]

    else
        button [ disabled True ] [ text "Submit" ]


vPwLength : Model -> Html msg
vPwLength model =
    if pwLength model.password == True || pwLength model.passwordAgain == True then
        drawCheckMark

    else
        drawCircle


vPwIsUpper : Model -> Html msg
vPwIsUpper model =
    if pwIsUpper model.password == True || pwIsUpper model.passwordAgain == True then
        drawCheckMark

    else
        drawCircle


vPwIsLower : Model -> Html msg
vPwIsLower model =
    if pwIsLower model.password == True || pwIsLower model.passwordAgain == True then
        drawCheckMark

    else
        drawCircle


vPwIsDigit : Model -> Html msg
vPwIsDigit model =
    if pwIsDigit model.password == True || pwIsDigit model.passwordAgain == True then
        drawCheckMark

    else
        drawCircle


vPwIsMatched : Model -> Html msg
vPwIsMatched model =
    if model.password == model.passwordAgain && pwNotEmpty model.password == True then
        drawCheckMark

    else
        drawCircle


drawCircle : Html msg
drawCircle =
    span [] [ text (fromChar (fromCode 10061)) ]


drawCheckMark : Html msg
drawCheckMark =
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


pwNotEmpty : String -> Bool
pwNotEmpty n =
    String.length n > 0
