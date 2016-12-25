module Decoder exposing (..)
import Json.Decode exposing (int, string, list, float, nullable, Decoder, field)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional, optionalAt, hardcoded)

type alias DecodedCard =
  { id : Int,
    card_image : Maybe String,
    card_idolized_image : Maybe String,
    pair_id : Maybe Int
  }


cardDecoder : Decoder (List DecodedCard)
cardDecoder =
  let d =
    decode DecodedCard
    |> required "id" int
    |> required "card_image" (nullable string)
    |> required "card_idolized_image" (nullable string)
    |> optionalAt ["ur_pair","card","id"] (nullable int) Nothing in
  field "results" (list d)
