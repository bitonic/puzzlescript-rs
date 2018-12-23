WIP

[![CircleCI](https://circleci.com/gh/bitonic/puzzlescript-rs/tree/master.svg?style=svg)](https://circleci.com/gh/bitonic/puzzlescript-rs/tree/master)

# Running

Install rustup and rust nightly

```
$ curl https://sh.rustup.rs -sSf | sh
$ rustup install nightly
```

then

```
$ cargo +nightly run -- puzzlescripts/third_party/heroes_of_sokoban_1.pzl
```

The only external dependencies are FreeType and OpenGL.
