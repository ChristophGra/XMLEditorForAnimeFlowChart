module UpdateData

open BasicData
open BasicData.decisionTree



let updateAnime state hash newEntry =
  let rec updateEntryOnly animelist hash newEntry =
    match animelist with 
    | [] -> []
    | head::tail ->
      if head.hash = hash then
        newEntry :: tail
      else
        head :: updateEntryOnly tail hash newEntry

  let rec updateTreeOnly (decisionTree:decision) hash newHash =
    match decisionTree.result with
    | [] -> {decisionTree with decision.result = []}
    | head::tail -> 
      {decisionTree with result = traverseResultList decisionTree.result hash newHash}

  and traverseResultList list hash newhash =
    match list with
    | [] -> []
    | head::tail -> 
      match head with 
      | decisionResult.Decision d -> List.concat [(updateTreeOnly d hash newhash).result; (traverseResultList tail hash newhash)]
      | decisionResult.Anime a -> 
        match a with 
        | hash ->  newhash :: traverseResultList tail hash newhash

  let updateTreeAndEntry state hash newEntry =
    let newList = updateEntryOnly state.animeHashList hash newEntry
    let newTree = updateTreeOnly state.treebase hash (createDecisionResultFromAnimeHash newEntry.hash)
    {state with animeHashList = newList; treebase = newTree}

  let mapVersion = Map.ofSeq (state.animeHashList |> Seq.map (fun f -> f.hash, f.anime))
  match newEntry.hash with 
  | _ when mapVersion.ContainsKey newEntry.hash -> {state with animeHashList = updateEntryOnly state.animeHashList hash newEntry}
  | _ when mapVersion.ContainsKey hash -> updateTreeAndEntry state hash newEntry
  | _ -> state