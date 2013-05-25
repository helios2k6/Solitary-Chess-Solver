module InternalHelpers

type internal ContinuationStep<'a> =
   | Finished
   | Step of 'a * (unit -> ContinuationStep<'a>)

let internal createContForList<'a> (list : 'a list) =
   let rec processList (internalList : 'a list) cont =
      match internalList with
      | [] -> cont()
      | h::t -> Step(h, fun () -> processList t cont)
   
   processList list (fun () -> Finished)

let internal mapCont<'a, 'b> (f : 'a -> 'b) (root : ContinuationStep<'a>) =
   let rec internalIterator f root accum =
      match root with
      | Step(a, cont) -> internalIterator f (cont()) ((f a)::accum)
      | Finished -> accum

   internalIterator f root []