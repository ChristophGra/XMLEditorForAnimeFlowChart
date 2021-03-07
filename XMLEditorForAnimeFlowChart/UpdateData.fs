module UpdateData

open BasicData
open BasicData.decisionTree

let matcher oldHash = (fun x -> x = oldHash)
let updateIfMatch matcher newHash hash = 
  match matcher hash with
  | false -> hash
  | true -> newHash

let applyEitherOr fun1 fun2 apply1 apply2 value =
  match fun1 value with
  | Some v -> apply1 v
  | None -> 
    match fun2 value with
    | Some v -> apply2 v
    | None -> value

let updateAnimeHashInHashList oldHash newHash state =
  let curriedmatcher = matcher oldHash
  let curriedupdateIfMatch = updateIfMatch curriedmatcher newHash
  match state.animeHashList with 
  | [] -> state
  | head::tail -> 
    let res =
      state.animeHashList
      |> Seq.map (fun x -> curriedupdateIfMatch x)
      |> Seq.toList
    {treebase = state.treebase; animeHashList= res}


let updateAnimeHashInDecisionTree oldHash newHash state =
  let curriedmatcher = matcher oldHash
  let curriedupdateIfMatch = updateIfMatch curriedmatcher newHash
  let deciderAnime value =
    match value with
    | Anime a -> Some a
    | _ -> None
  let deciderDecision value =
    match value with 
    | Decision d -> Some d
    | _ -> None

  
  let rec updater updaterfunction (decision:decision) : decision=
    let applyEitherAnimeOrDecision = applyEitherOr deciderAnime deciderDecision updaterfunction (fun x -> createDecisionResultFromDecision(updater updaterfunction x))
    match decision.result with
    | [] -> decision
    | head::tail ->
      let resultL = List.map applyEitherAnimeOrDecision decision.result
      {decision with result = resultL}

  {state with treebase = updater (curriedupdateIfMatch >> createDecisionResultFromAnimeHash) state.treebase}

let updateAnimeHash oldHash newHash state =
  let curriedupdateAnimeHashInHashList = updateAnimeHashInHashList oldHash newHash


  state 
  |> curriedupdateAnimeHashInHashList




  //{text = head.text; result = (a |> updaterfunction) :: (List.map (fun x -> updater updaterfunction x) tail)}