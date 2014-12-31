module Error where

type Type = DuplicateEmailAddressError
           | GenericError

toError : String -> Type
toError str =
  case str of
    "DuplicateEmailAddressError" ->
      DuplicateEmailAddressError
    _ -> GenericError
