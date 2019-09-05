# RgHs 

A minimal Reagent implementation for Haskell, to support the [Rahse (working name) linter](https://github.com/acrylic-origami/rahse).

## What the heck is this?

ReactiveX is a household name, but [Reagent](https://github.com/JakeWharton/Reagent) shows off that ReactiveX isn't great everywhere, by virtue of not being ReactiveX. Await-async languages, so it turns out, have [a lot to gain from rethinking the operator](https://lam.io/blog/HPx/).

Above providing definitions for rahse, this implementation is also an example of how even non-await languages that demand simplicity can also benefit from the thinking that went into Reagent. More is available at <https://lam.io/blog/Rg2/>.

## How is this used?

Haskell is especially nice to implement this Reagent variant, because it has really excellent function composition idioms. Even then, I expect the reactive pipelines of this framework to turn some heads sideways. In particular, operators are "backwards" in that they take a sink and return their own sink. The order in code is still from data source to sink, but the composition is opposite to all the other Rx-ish frameworks. Here is a window-like implementation, showing how to partition streams for distributed processing:

```haskell
main :: IO ()
main = do
  -- showing off a window of sorts:
  join $ (
    uncurry (>>) . ( -- create sources
        forkIO . Rg.interval 1000000
        *** Rg.from [0..20]
      ))
    <$> (Rg.zip
            $ Rg.filter (((==0) . (`rem` 3)) . snd) -- only every third
            $ ((True<$) . putStrLn . show . snd)
          )
    
  forever (threadDelay maxBound) -- spin (other `forever` lifetime is lost to `forkIO`)
```
