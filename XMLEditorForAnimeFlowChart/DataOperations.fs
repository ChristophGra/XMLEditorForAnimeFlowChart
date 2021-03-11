module DataOperations

open Helpers
open DataDefinition
open System.Data.Common
open Microsoft.VisualBasic.CompilerServices
open System


let chooseAnime result = 
  match result with
  | Anime a -> Some a
  | Decision d -> None

let chooseDecision result =
  match result with 
  | Anime a -> None
  | Decision d -> Some d

let filterList chooser results =
  List.choose chooser results

let filterAnime =
  filterList chooseAnime

let filterDecisions =
  filterList chooseDecision



let replaceAnimeHash oldAnime newAnime data=
  if data = oldAnime.animeHash then
    newAnime.animeHash 
  else 
    oldAnime.animeHash

let replaceAnime oldAnime newAnime data: animeHash=
  if data = oldAnime.animeHash then
     newAnime.animeHash
  else
     data
  
let rec extractAllAnime decision =
  match decision.results with
  | [] -> []
  | lst ->
    let anime = filterAnime lst
    let decisions = filterDecisions lst
    List.map extractAllAnime decisions
    |> List.fold (fun x y -> x @ y) anime 

let doEitherAnimeOrDecisionAction animeAction decisionAction res =
  match res with 
  | Anime a -> 
    result.Anime (animeAction a)
  | Decision d -> 
    result.Decision (decisionAction d)

let rec UpdateAnimeInDecisions oldAnime newAnime decision =
  let decisionAction = UpdateAnimeInDecisions oldAnime newAnime
  let animeAction = replaceAnime oldAnime newAnime
  let action = 
    doEitherAnimeOrDecisionAction animeAction decisionAction

  {decision with results = List.map action decision.results}

let UpdateAnimeInHashList oldAnime newAnime hashList =
  let action = replaceAnime oldAnime newAnime
  List.map action hashList