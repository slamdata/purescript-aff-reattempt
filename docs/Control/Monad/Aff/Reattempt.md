## Module Control.Monad.Aff.Reattempt

#### `reattempt`

``` purescript
reattempt :: forall e a. Int -> Aff (ref :: REF | e) a -> Aff (ref :: REF | e) a
```

Repeatedly runs the specified asynchronous computation until either it succeeds or
the specified timeout elapses. A running continuation won't be cancelled.


