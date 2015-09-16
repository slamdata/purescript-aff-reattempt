## Module Control.Monad.Aff.Reattempt

#### `reattempt`

``` purescript
reattempt :: forall e a. Int -> Aff (ref :: REF | e) a -> Aff (ref :: REF | e) a
```

`reattempt` repeatedly attempts to run the provided `Aff` until either an attempt
succeeds or the provided timeout elapses.

After the timeout elapses no more attempts will be made but the last attempt will
not be cancelled. Each attempt either fails or succeeds. The timeout has no effect on
the outcome of an attempt.

When an attempt to run the provided `Aff` succeeds the `Aff` returned by `reattempt`
will succeed. When no attempts succeed the `Aff` returned by `reattempt` will fail
with the `Error` raised by the last attempt.


