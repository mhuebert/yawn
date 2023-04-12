# Yawn

Another hiccup library.

This is a fork of Kevin Lynagh's [fork](https://github.com/lynaghk/hicada) of [Hicada](https://github.com/rauhs/hicada), which is a fork of [sablono](https://github.com/r0man/sablono), which is a fork of [hiccup](https://github.com/weavejester/hiccup).

Our approach:

- Primary target is React (maybe support emit-to-string on Clojure down the road)
- Do as much work as possible at compile-time, with graceful fallback to a fast interpreter (sometimes generating hiccup dynamically is worth it)
- Be extensible within reason (extension points need to work across compile + runtime)

Features supported out of the box:

- The `:class` key may be provided as a string, or vector of strings (joined at compile time)
- Keyword tags may contain IDs and classes like so: `:div#id.class-1.class-2`
- Symbol tags like `[my-fn {}]` are compiled to function calls, eg `(my-fn {})`. Children are untouched unless they are vectors that appear to be hiccup forms, in which case they are compiled.
- Special tags:
  - `:>` compiles to `createElement` instead of a function call, eg `[:> my-react-constructor <props> child-1 child-2]`. Props (if provided) are converted to javascript.
  - `:<>`, `:...` and `:Fragment` resolve to `react/Fragment`.
  - `:Suspense` resolves to a `react/Suspense`.
  - `:Portal` resolves to `react-dom/Portal`
- Props must be a literal map, or have ^:props metadata, eg. `[:div ^:props foo ...]`
