module MaybeMonad

type MaybeMonad<'a>(predicate : ('a -> bool)) =
   member this.Bind(x, f) = 
      match x with 
      | Some(y) when predicate y -> f y
      | _ -> None
   member this.Delay(f) = f()
   member this.Return(x) = Some x