# servant-elm-street
Generate elm requests, with types, and JSON encoders, and decoders derived by elm-street, given a servant API.

## Encoding headers
Header values are passed to the generated request functions as elm `String`s.
It is not always clear how to encode custom types automatically so we leave this to the user.
For example, `elm-street` does not generate string encoders when value constructors take parameters.

## Header values
We assume that `Maybe` values will not be used as header types in the targetted API.

The `Maybe a` type would be suitable for header values when we need to see if the value was parsed correctly and handle the error on our own.
There is a Lenient option for this case though.
