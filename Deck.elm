module Deck exposing (..)
import Card exposing (Card)
import Dict

type alias Deck = Dict.Dict Int (List Card)

empty = Dict.empty

insert : Deck -> Card -> Deck
insert deck card =
  case card.pair_id of
    Just key_id ->
      let pair = Dict.get key_id deck in
      case pair of
        Nothing -> Dict.insert key_id [card] deck
        Just v -> Dict.insert key_id (card::v) deck
    Nothing ->
      let pair = Dict.get card.id deck in
      case pair of
        Just v -> Dict.insert card.id (card::v) deck
        Nothing -> Dict.insert card.id [card] deck

fromCards cards = List.foldl (\c deck -> insert deck c) empty cards |> Dict.filter (\_ v -> if (List.length v) /= 2 then False else True)
