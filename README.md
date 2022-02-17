# servant-elm-street
Generate elm requests, with types, and JSON encoders, and decoders derived by elm-street, given a servant API.

## Encoding headers
Header values are passed to the generated request functions as elm `String`s.
It is not always clear how to encode custom types automatically so we leave this to the user.
For example, `elm-street` does not generate string encoders when value constructors take parameters.