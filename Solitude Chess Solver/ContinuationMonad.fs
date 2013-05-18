module ContinuationMonad

type ContinuationMonad() =
   member this.Bind (m, f) = fun c -> m (fun a -> f a c)
   member this.Return x = fun k -> k x
   member this.ReturnFrom m = m