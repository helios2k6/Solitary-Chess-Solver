module internal InternalHelpers

type ContinuationStep<'a> =
   | Finished
   | Step of 'a * (unit -> ContinuationStep<'a>)

let internal generateContinuationStepsForList<'a> (list : 'a list) =
   let rec processList (internalList : 'a list) cont =
      match internalList with
      | [] -> cont()
      | h::t -> Step(h, fun () -> processList t cont)
   
   processList list (fun () -> Finished)