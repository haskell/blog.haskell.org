+++
title = "Case Study - Using foreign component library inside a Haskell application"
date = 2025-01-08
[taxonomies]
authors = ["Mateusz Goślinowski"]
categories = ["GHC"]
tags = ["javascript"]
+++

GHC since version 9.8 allows us to create callbacks from JS to Haskell code, which enables us to create full-fledged browser apps.
This article shows how to use the JS backend with foreign component libraries.

<!-- more -->

- repository: https://github.com/Swordlash/halogen-blog
- ghc used: javascript-unknown-ghcjs-ghc-9.12.1 (ghcup)

## Preface

Any useful browser single-page application needs to be able to react to user input to modify the webpage content on response. 
Since GHC version 9.8 release, we can instantiate JavaScript-level functions with Haskell closures, allowing us to pass Haskell-level actions to DOM event listeners.

```haskell
foreign import javascript unsafe 
  """ 
  ((f) => { 
    var node = document.createElement("button"); 
    node.textContent = "Click me!"; 
    node.onclick = () => { 
      node.textContent = `Clicked ${f()} times`;
    }; 
    document.body.appendChild(node);
  })
  """
  install_handler :: Callback (IO JSVal) -> IO ()

main :: IO ()
main = do
  ref <- newIORef 0
  let incRef = toJSInt <$> (modifyIORef' ref (+1) *> readIORef ref)
  syncCallback' incRef >>= install_handler
```

In the above snippet we're creating an `IORef` and pass a callback incrementing it to a foreign function that installs a button in body, counting a number of clicks
(note for brevity I used a multiline syntax that is not yet available for foreign calls). The callback closes over the `IORef` and correctly updates the number after each click.

https://github.com/user-attachments/assets/ceb86903-d136-4821-8e2d-de2e433889a5

Callbacks fully enable probably the most fascinating purpose of JavaScript backend, which is web programming. GHCJS has been around for quite some time now,
however it is both outdated (being a GHC fork requiring separate maintenance; currently stuck on 8.10) and cumbersome to use (often necessitating a separate setup, typically through Nix). In one of my previous companies, while evaluating potential options for rewriting the frontend, I decided to use PureScript. It was close enough to Haskell and very easy to set up - it can be installed directly through `npm`, has its own `stack`-like package manager `spago` with a suite of existing bundler plugins, and a blazing fast language server. 
During this journey I was using [purescript-halogen](https://github.com/purescript-halogen/purescript-halogen) library - a typesafe, declarative VDOM framework based on [The Elm Architecture](https://guide.elm-lang.org/architecture/).

However, with all the joy that the Haskell-like web programming brought me, I couldn't shake the feeling that there is a room for improvement. From the small things that are
always annoying in polyglot stacks (like FE-BE JSON encoding discrepancies or lack of code sharing), to sometimes very illegible `purs` compiler error messages and of course lack of a lot of language features. This is not my intention to criticize - for a language this small I was amazed by the number and quality of existing tools and frameworks and a plenty of documentation; the overall experience exceeded my expectations by far. I would still recommend PureScript to anyone that wants a lightweight, easy to setup web language with strong type system.

However the mentioned shortcomings were why I was closely following the development of the GHC JS backend, wishing to port the `purescript-halogen` library to GHC as soon as it's possible. A fruit of this labour was recently released as [haskell-halogen](https://github.com/Swordlash/haskell-halogen).

## Using JavaScript backend with existing JavaScript tools

Yet, even the most complex and technically beautiful undertaking is worthless if its fruits are not usable. 
One of the measures of such usability in this case is interoperability with the JavaScript ecosystem - existing libraries, bundlers, minifiers, as well of the performance (size and speed) of the generated code. 

### Optimizing and bundling

Currently, `google-closure-compiler` is supported for minification of the bundle (see the blog post on [JS code minification](https://blog.haskell.org/report-of-js-code-minification/)). Let's try it.

```sh
mateusz@m12844:~/personal/halogen-blog$ ./run_example 1
(...)
+ npx google-closure-compiler --language_in UNSTABLE --compilation_level ADVANCED_OPTIMIZATIONS --warning_level QUIET --isolation_mode IIFE --assume_function_wrapper --emit_use_strict --js /home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example1-0.1.0.0/x/example1/build/example1/example1.jsexe/all.js --js /home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example1-0.1.0.0/x/example1/build/example1/example1.jsexe/all.externs.js --js_output_file ../dev/index.js
Input size: 1.8M
Output size: 396K
```

Notice we have to call `google-closure-compiler` with two input files: first is the `all.js` package generated by the compiler after bundling our library with all dependencies,
GHC's RTS and emscripten RTS files; second, is the `all.externs.js` file that declares external variables for the minifier. Its purpose is twofold - It informs the compiler that those variables are declared elsewhere, as not to fail with an "undeclared variable" error, and it prevents the mangling of those identifiers during minification.
Note we had to add `--language_in UNSTABLE` when compiling for recent `emscripten` due to [this issue](https://github.com/emscripten-core/emscripten/issues/23179).

Nice! We reduced size of the js file from 1.8M to merely 396K. This is even smaller after compression:
```sh
mateusz@m12844:~/personal/halogen-blog$ brotli -Z -o dev/index.js.br dev/index.js
mateusz@m12844:~/personal/halogen-blog$ du -h dev/index.js.br
76K     dev/index.js.br
```

Now, in the real life we very rarely just serve one file; we usually pull some external dependencies, stylesheets, static files. Those then get processed
into a smaller number of optimized output files in a process called bundling. `parcel` is one of the simplest bundlers out there - it scans your html file
for dependencies, recursively checks for js files embedded there and their dependencies, and has sensible defaults for installing browser polyfills on 
things that are `require`d in code. Let's check it out.

```sh
mateusz@m12844:~/personal/halogen-blog$ npx parcel dev/index.html
(this is going to install some polyfill libraries like os-browserify)
Server running at http://localhost:1234
✨ Built in 1.55s
```

Great! Let's check out the page.

And behold!.. an empty page. What's happening? A quick look in dev console shows an issue:
```sh
Uncaught Error: process.binding is not supported
    at process.binding (browser.js:177:11)
    at Object.<anonymous> (index.js:144:368)
    at d9Zcj.6c0765b8bdc1a4da (index.js:898:391)
    at newRequire (index.f708bf3c.js:71:24)
    at index.f708bf3c.js:122:5
    at index.f708bf3c.js:145:3
```

Looking at the error, this comes from a parcel polyfill library for `process`. Why is `process` ever used? 
After some digging I found a related [issue comment](https://github.com/parcel-bundler/parcel/issues/4468#issuecomment-613598904) in `parcel` and
the affected `ghc-internal` [line](https://gitlab.haskell.org/ghc/ghc/-/blob/777f108f/libraries/ghc-internal/jsbits/platform.js#L49).
So it seems `parcel` is wrapping the processed module in a "header" that provides `require`, which confuses the `ghc-internal` over which environment
it's running in - it thinks that since `require` is available it is running in `nodejs`, and calls the `process.binding` which is not polyfilled.

Yuck!

Now, there is an option to compile without this code entirely and omit the environment checking at all, if you configure your GHC in the following way:
```sh
CONF_CC_OPTS_STAGE2="-sENVIRONMENT=web" emconfigure ./configure --target=javascript-unknown-ghcjs --with-js-cpp-flags="-DGHCJS_BROWSER -E -CC -Wno-unicode -nostdinc"
```
This way all environment checking code will be dropped, and `emcc` will compile-in only web-related part of its runtime.
However, it being ghc-compile-time option (not link time option unlike in GHCJS), it's cumbersome to use and maintain from GHC side (separate testing, CI etc.), and also half-baked and not supported in all boot libraries (see GHC [PR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13779) with sub-PRs fixing it in `unix` and `process`).

Therefore, we need to abandon hopes about `parcel` for now. How about `webpack`? 
```sh
mateusz@m12844:~/personal/halogen-blog$ cat config/webpack.config.js 
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './dev/index.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, '../dist'),
  },
  resolve: {
    fallback: {
      os: false,
      fs: false,
      child_process: false,
      path: false,
    }
  },
  plugins: 
    [ new HtmlWebpackPlugin({
        title: 'Blog'
    })]
};
mateusz@m12844:~/personal/halogen-blog$ npx webpack build --config config/webpack.config.js && npx http-server dist/
(...)
Starting up http-server, serving dist/
```

It works! The button appeared on screen and the console errors disappeared. 

### Bundling with external deps

Now, the above was only child's play since we didn't have any actual external dependencies to bundle, only our code.
Let's create something more involved. Let's have our button be styled and managed by Google Material Components library.

Doing it is very simple, as decribed in the [Material documentation](https://m2.material.io/components/buttons/web#using-buttons). We basically have to
- Create an HTML button component structure on page as described in docs:
```html
<div class="mdc-touch-target-wrapper">
  <button class="mdc-button mdc-button--outlined mdc-button--icon-leading"> <!-- this is {element} in below code -->
    <span class="mdc-button__ripple"></span>
    <span class="mdc-button__touch"></span>
    <i class="material-icons mdc-button__icon" aria-hidden="true">bookmark</i>
    <span class="mdc-button__label">My Accessible Button</span>
  </button>
</div>
```
- "Activate" the material library on it:
```javascript
import {MDCRipple} from '@material/ripple';

const buttonRipple = new MDCRipple(element);
```
- Import material styles and icons into your `.scss` bundle
- Destroy the element once it's removed from DOM: `buttonRipple.destroy()`.

With this, we can start implementation!
file: example2/src/Button.hs 

First, a few imports:
```haskell
import Halogen hiding (Initialize, Finalize, State)
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.HTML.Properties.ARIA qualified as HPA
import Halogen.HTML.Events qualified as HE
import Web.DOM.Internal.Types (HTMLElement)
import Control.Monad.State
import Data.Foreign
import Data.Foldable
import Data.Text qualified as T
```

We are going to use foreign import for initializing and destroying a button ripple effect. We will include this code in `js-sources` field of the package's cabal file:
```cabal
library
  (..)
  js-sources: src/Button.js
```

The js file itself is quite simple and doesn't do much - we import the needed module and expose two mentioned functions.
```javascript
import {MDCRipple} from '@material/ripple';

function init_ripple(element) {
  return new MDCRipple(element);
}

function destroy_ripple(ripple) {
  ripple.destroy();
}
```

Now, in the haskell file, using `Data.Foreign` module of `haskell-halogen` (which is just a type-tagged newtype over `GHC.JS.Prim.JSVal`), we define our imports:

```haskell
newtype MDCRipple = MDCRipple (Foreign MDCRipple)

foreign import javascript unsafe "init_ripple" initRipple :: HTMLElement -> IO MDCRipple
foreign import javascript unsafe "destroy_ripple" destroyRipple :: MDCRipple -> IO ()
```

Note, because `HTMLElement` and `MDCRipple` runtime representation is that of a `JSVal`, we can specify them directly in signatures without the need of unwrapping arguments into `JSVals` and wrapping `JSVal` results back.

The Halogen library uses the Elm Architecture, so we need to specify the state of our button component and its actions. Our state will simply be a pair of `(foreign MDCRipple, click counter)`. The actions will be `Initialize` and `Finalize`, called when the element is added and removed from DOM, and `Click`:

```haskell
data Action = Initialize | Finalize | Click
data State = State { ripple :: Maybe MDCRipple, counter :: Int }
```

Note `ripple` is `Nothing` at the beginning, when our button is in DOM but the `Initialize` hasn't been dispatched yet. Now it's time for the rest of the plumbing:

```haskell
button :: Component q i o IO
button = mkComponent $ ComponentSpec
  { initialState = const $ State Nothing 0
  , render
  , eval = mkEval $ defaultEval { handleAction, initialize = Just Initialize, finalize = Just Finalize }
  }

  where
    ref = RefLabel "mdc-button" -- reference that is added to .mdc-button element, to look it up in DOM and initialize with foreign code

    text 0 = "Click me!"
    text n = T.pack $ "Clicked " <> show n <> " times"

    render State{counter} = 
      HH.div [HP.class_ (HH.ClassName "mdc-touch-target-wrapper")]
        $ pure
        $ HH.button
          [ HP.classes 
            [ HH.ClassName "mdc-button", HH.ClassName "mdc-button--outlined", HH.ClassName "mdc-button--icon-leading" ] -- the element in question
            , HE.onClick (const Click) -- this action is returned when button is clicked
            , HP.ref ref -- we add reference here
          ]
          -- rest of the HTML as the documentation specifies
          [ HH.span [HP.class_ (HH.ClassName "mdc-button__ripple")] []
          , HH.span [HP.class_ (HH.ClassName "mdc-button__touch")] []
          , HH.i [HP.classes [HH.ClassName "material-icons", HH.ClassName "mdc-button__icon"], HPA.hidden "true"] [HH.text "add"]
          , HH.span [HP.class_ (HH.ClassName "mdc-button__label")] [HH.text $ text counter]
          ]
```

Pretty straightforward, now the action handling:
```haskell
    handleAction Click = modify' $ \s -> s { counter = counter s + 1 }
    handleAction Finalize = gets ripple >>= traverse_ (liftIO . destroyRipple)
    handleAction Initialize =
      getHTMLElementRef ref >>= \case
        Just el -> do
          r <- liftIO $ initRipple el 
          modify' (\s -> s { ripple = Just r })
        Nothing -> error "Could not find button element"
```

And that's it. What's left is attaching this component to the `<body>` element in `Main.hs`:
```haskell
import Button
import Halogen.VDom.Driver (runUI)
import Halogen.IO.Util
import Control.Monad (void)

main :: IO ()
main = awaitBody >>= void . runUI button ()
```

Let's run the example.
```sh
mateusz@m12844:~/personal/halogen-blog$ ./run_example 2
(...)
+ npx google-closure-compiler --language_in UNSTABLE --compilation_level ADVANCED_OPTIMIZATIONS --warning_level QUIET --isolation_mode IIFE --assume_function_wrapper --emit_use_strict --js /home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example2-0.1.0.0/x/example2/build/example2/example2.jsexe/all.js --js /home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example2-0.1.0.0/x/example2/build/example2/example2.jsexe/all.externs.js --js_output_file ./example2/assets/index.js
/home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example2-0.1.0.0/x/example2/build/example2/example2.jsexe/all.js:17760:0: ERROR - [JSC_INVALID_MODULE_PATH] Invalid module path "@material/ripple" for resolution mode "BROWSER"
  17760| import {MDCRipple} from '@material/ripple';
```

Oof. `google-closure-compiler` doesn't like our `import`. And it makes total sense, since it checks for undefined variables, so it doesn't know what are we importing.
Now, there isn't an easy way to fix this. We would essentially have to pass it all `node_modules` in question - unfortunately the webpack plugin is outdated to Webpack 4.

However, there is a workaround if you really like `google-closure-compiler`, and want to use it. What we can do is not bundle the `js-sources`, but instead create externs
file for the closure compiler, and bundle the actual implementation later with webpack. To prevent name mangling just in case we hook our functions into `window` variable, like
some of the ponyfills do.

src/Button.externs.js
```js
/** @externs */
/** @type {*} */ window.Halogen = {};
/** @return {*} */ window.Halogen.init_ripple = function (_el) {};
/** @return {*} */ window.Halogen.destroy_ripple = function (_ripple) {};
```
src/Button.hs
```haskell
foreign import javascript unsafe "window.Halogen.init_ripple" initRipple :: HTMLElement -> IO MDCRipple
foreign import javascript unsafe "window.Halogen.destroy_ripple" destroyRipple :: MDCRipple -> IO ()
```

Now we add it to our `google-closure-compiler` invokation:
```sh
mateusz@m12844:~/personal/halogen-blog$ ./run_example 2-workaround
(...)
+ npx google-closure-compiler --language_in UNSTABLE --compilation_level ADVANCED_OPTIMIZATIONS --warning_level QUIET --isolation_mode IIFE --assume_function_wrapper --emit_use_strict --js /home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example2-workaround-0.1.0.0/x/example2-workaround/build/example2-workaround/example2-workaround.jsexe/all.js --js /home/mateusz/personal/halogen-blog/dist-newstyle/build/javascript-ghcjs/ghc-9.12.1/example2-workaround-0.1.0.0/x/example2-workaround/build/example2-workaround/example2-workaround.jsexe/all.externs.js --js ./example2-workaround/src/Button.externs.js --js_output_file ./example2-workaround/assets/index.js
Input size: 3.4M
Output size: 756K
```

Great! It passed! Now we add the real `Button.js` to entry files in webpack:
```javascript
mateusz@m12844:~/personal/halogen-blog$ cat example2-workaround/assets/webpack.config.js 
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: ['./example2-workaround/assets/index.js', './example2-workaround/assets/style.scss', './example2-workaround/src/Button.js'],
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, '../../dist'),
  },
  resolve: {
    fallback: {
      os: false,
      fs: false,
      child_process: false,
      path: false,
    }
  },
  module: {
    rules: [
      {
        test: /\.s[ac]ss$/i,
        use: [ "style-loader", "css-loader", "sass-loader"],
      }
    ],
  },
  plugins: 
    [ new HtmlWebpackPlugin({
        title: 'Blog'
    })]
```

We also use scss to add styles and font needed:
```scss
mateusz@m12844:~/personal/halogen-blog$ cat example2-workaround/assets/style.scss 
@use "@material/button/styles";

@font-face {
  font-family: 'Material Icons';
  font-style: normal;
  font-weight: 400;
  src: url(https://fonts.gstatic.com/s/materialicons/v142/flUhRq6tzZclQEJ-Vdg-IuiaDsNcIhQ8tQ.woff2) format('woff2');
}

.material-icons {
  font-family: 'Material Icons';
  font-weight: normal;
  font-style: normal;
  font-size: 24px;
  line-height: 1;
  letter-spacing: normal;
  text-transform: none;
  display: inline-block;
  white-space: nowrap;
  word-wrap: normal;
  direction: ltr;
  -webkit-font-feature-settings: 'liga';
  font-feature-settings: 'liga';
  -webkit-font-smoothing: antialiased;
}
```

And we're ready:
```
mateusz@m12844:~/personal/halogen-blog$webpack build --config example2-workaround/assets/webpack.config.js && http-server dist/
(...)
Starting up http-server, serving dist/
```

Behold, a material button!

https://github.com/user-attachments/assets/4abe2e52-0bd2-4cb7-99c0-c0b0a1796528

### Bundling with `webpack` and `swc-loader`

However, while the above trick works for standalone executables, it isn't really useful for libraries that have to ship with full code - haskell and javascript.
Making downstream users to the to add separately-shipped externs and js files to their pipeline sounds like an unpleasant thing to do, and exposes
them to too much implementation detail. Can we do better?

The answer is - yes, if we ditch the closure compiler but use `swc` instead. [swc](https://swc.rs/) is a Rust-based platform used by a lot of tools like `Parser`, `Next.js` 
or `Vercel`, and provides its own minifier pipeline with webpack plugin.

What we have to do is, instead of installing our workaround and calling `google-closure-compiler`, add the following loader to our webpack file:
```javascript
// mateusz@m12844:~/personal/halogen-blog$ cat example2/assets/webpack.config.js

module.exports = {
  // no more Button here, `index.js` is just a copy of `all.js`
  entry: ['./example2/assets/index.js', './example2/assets/style.scss'],
  // (...)
  module: {
    rules: [
      {
        test: /\.s[ac]ss$/i,
        use: [ "style-loader", "css-loader", "sass-loader"],
      },
      {
        test: /\.m?js$/,
        exclude: /(node_modules)/,
        use: {
          loader: "swc-loader"
        }
      }
    ],
  },
  // (...)
};

// mateusz@m12844:~/personal/halogen-blog$ cat .swcrc
{
  "minify": true,
  "jsc": {
    "minify": {
      "compress": true,
      "mangle": true,
      "sourceMap": false
    }
  }
}
```

And voilà! It all works as before. The uncompressed bundle size is slightly bigger (844 KiB vs 803 KiB with `google-closure-compiler`) however we don't need any more workarounds,
and we can safely ship our foreign code with imports to our users (provided they do install our `npm` dependencies).

A library with richer functionality is available [here](https://github.com/Swordlash/haskell-halogen-material).
At the moment of writing, it contains Halogen components for customizable buttons, lists and tabbed panes.

## Conclusion

The above article shows how to use the JavaScript backend effectively and integrate it with foreign libraries, using `webpack` for bundling and `swc-loader` for minification/mangling.

What's next? There is still a lot to do in terms of code size & performance, as well as integration with other tools:

- This cabal [PR](https://github.com/haskell/cabal/pull/10722) adds a new field, `js-options`, that allows to pass custom flags to `js-sources` preprocessor.
  Notably that would enable i.e. conditional compilation of traces along the lines of `#ifdef XXX <put-trace>` in foreign library code and `if flag(trace-flag) js-options: -optJSP-DXXX` in cabal file.
- Low-hanging fruits like [adding multiline strings support to inline foreign imports](https://gitlab.haskell.org/ghc/ghc/-/issues/25633).
- Bigger [integration efforts](https://gitlab.haskell.org/ghc/ghc/-/issues/25469) with npm.

What is still a small unknown is deeper integration of the GHC build pipeline with `webpack` build pipeline, in the spirit of gathering `npm` libraries that need to be installed for each Haskell dependency, like `@material/button` in the above example. 
I believe there will have to be a way of declaring inside the cabal package an `npm` dependency (something like the existing `pkgconfig-depends`) and
a webpack plugin will be created for loading haskell package for bundling.

Personally, I'm going to continue maintaining and developing the `haskell-halogen` and `haskell-halogen-material` libraries, adding more component classes to the latter.

## Thanks!

Many thanks to [Serge S. Gulin](https://github.com/GulinSS) for his help and discussions on Matrix channel, and to [Hécate Kleidukos](https://gitlab.haskell.org/Kleidukos) for inviting me to write this blog post. I want also to thank [Sylvain Henry](https://gitlab.haskell.org/hsyl20) and [Luite Stegeman](https://gitlab.haskell.org/luite) for our mail and PR discussions, and whole IOG GHC Engineering team for the joint effort of releasing the JS backend. Awesome work!
