module Card exposing (..)
import Decoder exposing (DecodedCard)

type alias Card = {
  id : Int,
  card_image : String,
  card_idolized_image : String,
  pair_id : Maybe Int
}

fromDecodedCards : List DecodedCard -> List Card
fromDecodedCards l =
  let aux c acc = case (c.card_image, c.card_idolized_image) of
    (Just i, Just ii)  -> {id = c.id, pair_id = c.pair_id, card_image = i, card_idolized_image = ii }::acc
    _ -> acc in
  List.foldl aux [] l
