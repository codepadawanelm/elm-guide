-- Input a password and confirm it based on criteria.
--
-- Read more:
--   https://guide.elm-lang.org/architecture/forms.html
--


module Main exposing (..)

import Browser
import Char exposing (fromCode, isDigit, isLower, isUpper, toCode)
import Html exposing (Html, br, button, form, input, label, li, span, text, ul)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import String exposing (fromChar)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { password : String
    , confirmPassword : String
    }


init : Model
init =
    Model "" ""



-- UPDATE


type Msg
    = Password String
    | ConfirmPassword String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Password password ->
            { model | password = password }

        ConfirmPassword password ->
            { model | confirmPassword = password }



-- VIEW


view : Model -> Html Msg
view model =
    form []
        [ label [] [ text "Password" ]
        , br [] []
        , input [ type_ "password", placeholder "Password", value model.password, onInput Password ] []
        , br [] []
        , label [] [ text "Confirm Password" ]
        , br [] []
        , viewConfirmPassword model
        , ul [ style "list-style" "none" ]
            [ li []
                [ viewDrawSymbol pwLength model.password
                , text "8 characters or longer."
                ]
            , li []
                [ viewDrawSymbol pwIsUpper model.password
                , text "1 or more uppercase characters."
                ]
            , li []
                [ viewDrawSymbol pwIsLower model.password
                , text "1 or more lowercase characters."
                ]
            , li []
                [ viewDrawSymbol pwIsDigit model.password
                , text "1 or more numbers."
                ]
            , li []
                [ viewDrawSymbol pwIsSpecial model.password
                , text "1 or more special characters."
                ]
            , li []
                [ viewDrawSymbol pwIsMatched model
                , text "Passwords must match."
                ]
            ]
        , viewSubmitButton model
        ]


viewSubmitButton : Model -> Html msg
viewSubmitButton model =
    button [ disabled (not (pwIsMatched model)) ] [ text "Submit" ]


viewConfirmPassword : Model -> Html Msg
viewConfirmPassword model =
    let
        n =
            model.password

        passwordCriteriaMet =
            not (pwLength n && pwIsUpper n && pwIsLower n && pwIsDigit n && pwIsSpecial n)
    in
    input [ type_ "password", placeholder "Confirm Password", value model.confirmPassword, onInput ConfirmPassword, disabled passwordCriteriaMet ] []


viewDrawSymbol : (a -> Bool) -> a -> Html Msg
viewDrawSymbol pwAttribute a =
    if pwAttribute a then
        viewDrawCheckMark

    else
        viewDrawCircle


viewDrawCircle : Html msg
viewDrawCircle =
    span [] [ text (fromChar (fromCode 10061)) ]


viewDrawCheckMark : Html msg
viewDrawCheckMark =
    span [] [ text (fromChar (fromCode 10004)) ]


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


pwIsSpecial : String -> Bool
pwIsSpecial n =
    String.any isSpecial n


pwIsEmpty : String -> Bool
pwIsEmpty n =
    String.isEmpty n


pwIsMatched : Model -> Bool
pwIsMatched model =
    model.password == model.confirmPassword && not (pwIsEmpty model.password)


isSpecial : Char -> Bool
isSpecial char =
    let
        code =
            toCode char
    in
    (0x20 <= code && code <= 0x2E)
        || (0x3A <= code && code <= 0x40)
        || (0x5B <= code && code <= 0x60)
        || (0x7B <= code && code <= 0x7E)
