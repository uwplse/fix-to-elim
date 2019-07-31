This repository contains the `Preprocess` command by [Nate Yazdani](https://github.com/nateyazdani), which does simple match and fixpoint to eliminator (induction principle) translation for certain terms, as described in the [DEVOID paper](http://tlringer.github.io/pdf/ornpaper.pdf). This command is a part of the [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH) plugin suite. Here it exists as a standalone plugin so that others can build on it as desired.

This plugin depends on Coq 8.8 and our Coq plugin library.
The library is included automatically. (The library is currently in a preliminary state; it will be stable by the end of the summer, at which point we will remove this comment.)
To build the standalone plugin, run:

```
cd plugin
./build.sh
```

For examples of using this plugin within another plugin,
see [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH) and [DEVOID](https://github.com/uwplse/ornamental-search).
(Currently, these use cases are in the 0.1 branches.)
