# blockfrost-pretty

Pretty printing for Ada/Lovelace values and
some Blockfrost types.

Example of pretty printing lovelace values.

```haskell
λ: :set -XNumericUnderscores
λ: import Blockfrost.Pretty
λ: import Data.Text.IO
λ: Data.Text.IO.putStrLn $ prettyLovelaces 123_456
123 456 lovelaces
λ: Data.Text.IO.putStrLn $ prettyLovelaces 42_123_456
42.123456 ₳
λ: Data.Text.IO.putStrLn $ prettyLovelaces' testnetConfig 42_123_456
42.123456 t₳
λ: Data.Text.IO.putStrLn $ prettyLovelaces' (testnetConfig { pcUnicode = False }) 42_123_456
42.123456 tADA
```

## Using `Prettyprinter`

To pretty print a `Block` to terminal with colors,
we use [prettyprinter](https://hackage.haskell.org/package/prettyprinter) package
with [prettyprinter-ansi-terminal](https://hackage.haskell.org/package/prettyprinter-ansi-terminal).

Example:

```haskell
λ: import Prettyprinter
λ: import Prettyprinter.Render.Terminal
λ: import Data.Text.IO

λ: Data.Text.IO.putStrLn $ renderStrict $ layoutPretty defaultLayoutOptions $ prettyBlock b
```
