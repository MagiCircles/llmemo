module Main exposing (..)
import Random

import Html.Events exposing (onClick)
import Html exposing (Html, text, div, img, button)
import Html.Attributes exposing (href, src, height)
import Http
import Json.Decode exposing (Decoder)
import Decoder exposing (cardDecoder, DecodedCard)
import Card exposing (Card, fromDecodedCards)
import Deck exposing (Deck, fromCards)
import Dict

type PickedState = Hidden | Picked | Matched

type alias PickedCard =
  {
    idolized : Bool,
    card : Card,
    state : PickedState
  }

toPickedCard : List PickedPair -> List PickedCard
toPickedCard pairlist =
  let map_card ido p = List.map (\c -> {idolized = ido, card = c, state = Hidden }) p in
  List.foldl (\p acc -> List.append (map_card p.idolized p.cards) acc) [] pairlist

shufflePickedCards pairs l =
  let cards = toPickedCard pairs in
  let shuffled =
    List.map2 (,) cards l
    |> List.sortBy Tuple.second
    |> List.unzip
    |> Tuple.first
  in ShuffledCards shuffled

type alias PickedPair =
  {
    idolized : Bool,
    cards : List Card
  }

toPickedPair (cards, num) =
  let is_idolized = if (num % 2) == 0 then True else False in
  ({ cards = cards, idolized = is_idolized }, num)

randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 0 100
        |> Random.list len
        |> Random.generate msg

shuffleDeck deck l =
  let values = Dict.values deck in
  let shuffled =
    List.map2 (,) values l
    |> List.sortBy Tuple.second
    |> List.map toPickedPair
    |> List.unzip
    |> Tuple.first in
  ShuffledList shuffled

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- TYPES

type State = Init | Picking | PickingSecond PickedCard | Won | WrongPick PickedCard PickedCard | EndGame

type alias Model =
    {
      debug : Maybe String,
      deck : Deck,
      state : State,
      counter : Int,
      picked : List PickedCard
    }

type Msg = Error | GotDeck Deck | ShuffledList (List PickedPair) | ShuffledCards (List PickedCard) | Pick PickedCard | Restart


-- MODEL

prepareDeck cards = List.filter (\c -> if c.pair_id == Nothing then False else True) cards

cardMap : Result Http.Error (List DecodedCard) -> Msg
cardMap c = case c of
  Ok c -> fromDecodedCards c |> fromCards |> GotDeck
  Err o -> Error

init : ( Model, Cmd Msg )
init =
    (
      {
        debug = Nothing,
        deck = Deck.empty,
        state = Init,
        counter = 0,
        picked = []
      }
    , fetch cardDecoder "http://schoolido.lu/api/cards/?is_promo=False&is_special=False&rarity=UR&page_size=150&expand_ur_pair=" cardMap)

-- UPDATE

updateList : a -> a -> List a -> List a
updateList v new_v l =
  List.map (\lv -> if lv == v then new_v else lv) l

checkMatch card1 card2 =
  if card2.idolized /= card1.idolized then
    False
  else
    case (card1.card.pair_id, card2.card.pair_id) of
    (Just id1, Just id2) -> id1 == id2
    (Just id1, Nothing) -> id1 == card2.card.id
    (Nothing, Just id2) -> id2 == card1.card.id
    _ -> False

isFinished model =
  if not (List.isEmpty model.picked) then
    not (List.any (\c -> c.state /= Matched) model.picked)
  else
    False

endGame model = ({model | state = EndGame }, Cmd.none)

checkWin model =
  if isFinished model then
    ({ model | state = EndGame }, Cmd.none )
  else
    (model, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      Error ->
        (model, Cmd.none)
      GotDeck deck ->
        ({model | deck = deck }, randomList (shuffleDeck deck) (Dict.size deck))
      ShuffledList shu ->
        let picked = List.take 8 shu in
        (model, randomList (shufflePickedCards picked) (List.length picked))
      ShuffledCards cards ->
        ({ model | state = Picking, picked = cards}, Cmd.none)
      Pick card ->
        case model.state of
          Picking ->
            if card.state == Matched || card.state == Picked then
              (model, Cmd.none)
            else
              let new_l = updateList card { card | state = Picked } model.picked in
              checkWin { model | state = PickingSecond { card | state = Picked}, picked = new_l, counter = model.counter + 1 }
          PickingSecond card_picked ->
            if card.state == Matched || card.state == Picked then
              (model, Cmd.none)
            else
              case checkMatch card card_picked of
              True ->
                let picked =
                  updateList card { card | state = Matched } model.picked
                  |> updateList card_picked { card_picked | state = Matched } in
                  checkWin { model | picked = picked, state = Picking, counter = model.counter + 1 }
              False ->
                let new_l = updateList card { card | state = Picked } model.picked in
                checkWin { model | picked = new_l, state = WrongPick { card | state = Picked} card_picked, counter = model.counter + 1 }
          WrongPick card1 card2 ->
            let picked_ = updateList card1 { card1 | state = Hidden } model.picked in
            let picked__ = updateList card2 { card2 | state = Hidden } picked_ in
            checkWin {model | state = Picking, picked = picked__ }
          _ ->
            (model, Cmd.none)
      Restart ->
        ({ model | state = Init, picked = [], counter = 0 }, randomList (shuffleDeck model.deck) (Dict.size model.deck))


-- VIEW

isWrong model =
  case model of
    WrongPick _ _ -> True
    _ -> False

cardGrid model =
  let singleCard card =
    let image = if card.state == Hidden then "back.png" else (if card.idolized then card.card.card_idolized_image else card.card.card_image) in
    div [] [img [src image, height 200, onClick (Pick card)] []] in
  div [] (List.map singleCard model.picked )

view : Model -> Html Msg
view model =
  case model.state of
    Picking ->
      div [] [cardGrid model]
    EndGame ->
      div [] [button [onClick Restart] [text "restart game", text "it took many actions ! ", text (toString model.counter)]]
    _ -> div [] [cardGrid model]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

fetch : Decoder a -> String -> (Result Http.Error a -> b) -> Cmd b
fetch decoder url action =
    Http.get url decoder |> Http.send action
