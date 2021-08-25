# blockfrost-api

Core types and Servant API definitions.

## Exploring data types

All data types have a `ToSample` instance
for [servant-docs](https://hackage.haskell.org/package/servant-docs)
which can be used to get a sample response in `cabal repl`:

``` haskell
λ: import Data.Proxy
λ: import Servant.Docs
λ: Just block = toSample (Proxy :: Proxy Block)
λ: _blockHash block
BlockHash "4ea1ba291e8eef538635a53e59fddba7810d1679631cc3aed7c8e6c4091a516a"
```

## Lenses

Instead of using long record names, it is recommended
to use provided lenses and a [lens](https://hackage.haskell.org/package/lens) or similar package.

``` haskell
λ: import Control.Lens (^.)
λ: import Blockfrost.Lens
λ: block ^. epoch
Just (Epoch 425)
```

## Monetary values

Ada values and values of assets are represented using [`Discrete`](https://hackage.haskell.org/package/safe-money/docs/Money.html#t:Discrete) type
from [safe-money](https://hackage.haskell.org/package/safe-money) library.

We use a type alias `type Lovelaces = Money.Discrete "ADA" "lovelace"`
for Ada values and [`SomeDiscrete`](https://hackage.haskell.org/package/safe-money/docs/Money.html#t:SomeDiscrete) for asset values. This should allow working
with monetary values safely, preventing summing different currencies.
See the [blog post](https://ren.zone/articles/safe-money) by the library author.

``` haskell
λ: block ^. fees
Just (Discrete "ADA" 1000000%1 592661)
```
