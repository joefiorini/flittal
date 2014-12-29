module User.SignUp where

import Html (input, form, label, text, button)
import Html.Attributes (for, type', name, id, method)
import Html.Events (on, targetValue, onClick)
import Http
import Json.Encode as Encode
import Debug
import LocalChannel as LC

type Update = UpdateField String String
            | SubmitForm
            | NoOp

type alias Model =
  { email    : String
  , password : String
  }

startingState : Model
startingState =
  { email = ""
  , password = ""
  }


buildUrl path =
  "http://localhost:3000" ++ path

encoder form =
  Encode.object
    [ ("signupEmail", Encode.string form.email)
    , ("signupPassword", Encode.string form.password)
    ]

toJson form =
  Encode.encode 0 <| encoder form

buildRequest form =
  Http.request
    "POST"
    (buildUrl "/users")
    (toJson form)
    [ ("Accept", "application/json")
    , ("Content-Type", "application/json") ]

step : Update -> Model -> Model
step update model =
  case update of
    UpdateField fieldName value ->
      case fieldName of
        "email" -> { model | email <- value }
        "password" -> { model | password <- value }
        _ -> model
    SubmitForm -> Debug.log "submitting" model
    NoOp -> model

view channel model =
  let updateField name value = LC.send channel (UpdateField name value)
  in
    form
      [ method "post"
      ]
      [ label
          [ for "email" ]
          [ text "Email" ]
      , input
          [ type' "email"
          , name "email"
          , id "email"
          , on "input" targetValue (updateField "email")
          ]
          []
      , input
          [ type' "password"
          , name "password"
          , id "password"
          , on "input" targetValue (updateField "password")
          ]
          []
      , button
          [ type' "submit"
          , name "submit"
          , onClick (LC.send channel SubmitForm)
          ]
          [ text "Sign Up" ]
        ]
