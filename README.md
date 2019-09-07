This repository contains the `Preprocess` command, which does simple match and fixpoint to eliminator (induction principle) translation for certain terms, as described in the [DEVOID paper](http://tlringer.github.io/pdf/ornpaper.pdf). This command is a part of the [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH) plugin suite. Here it exists as a standalone plugin so that others can build on it as desired.

This plugin depends on Coq 8.8 and our [Coq plugin library](https://github.com/uwplse/coq-plugin-lib).
The library is included automatically.
To build the standalone plugin, run:

```
cd plugin
./build.sh
```

For examples of using this plugin within another plugin,
see [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH) and [DEVOID](https://github.com/uwplse/ornamental-search).
For examples of using this command directly, see the [coq](/plugin/coq) directory.

## Guide

* [LICENSE](/LICENSE): License
* [README.md](/README.md): You are here!
* [plugin](/plugin): Main plugin directory
  - [build.sh](/plugin/build.sh): Build script
  - [test.sh](/plugin/test.sh): Test script
  - [theories](/plugin/theories): Coq theories to load plugin
  - [src](/plugin/src): Main source directory
    - [fixtoelim.mlpack](/plugin/src/fixtoelim.mlpack)
    - [fixtranslation.ml4](/plugin/src/fixtranslation.ml4): **Preprocess** top-level
    - [automation](/plugin/src/automation): **Preprocess** implementation
    - [components](/plugin/src/components): Components in the style of [PUMPKIN PATCH](https://github.com/uwplse/PUMPKIN-PATCH)
    - [coq-plugin-lib](/plugin/src/coq-plugin-lib): [Coq plugin library](https://github.com/uwplse/coq-plugin-lib)

## Contributors

The vast majority of this code was written by [Nate Yazdani](https://github.com/nateyazdani). Talia Ringer mostly ported it to a plugin and wrote a lot of library functions. Github history is not accurate here.

## Licensing

We use the MIT license because we think Coq plugins have a right not to use GPL. If this is wrong, please let us know kindly so we can fix this.
