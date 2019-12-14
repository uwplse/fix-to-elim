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

## Troubleshooting

Error messaging for this plugin is really difficult. Here are some quick guidelines:

1. If you get an error message about not being able to find an induction principle `Foo_ind`, `Foo_rec`, or `Foo_rect`,
you need to tell Coq to define an induction principle for that type. First, run this command:

```
Check Foo.
```

Determine if your type is in `Prop` or not. If it is in `Prop`, then run these commands:

```
Scheme Minimality for Foo Sort Prop.
Scheme Induction for Foo Sort Set.
Scheme Induction for Foo Sort Type.
```

Otherwise, run these commands:

```
Scheme Induction for Foo Sort Prop.
Scheme Induction for Foo Sort Set.
Scheme Induction for Foo Sort Type.
```

This will tell Coq to generate induction principles. See [this command](https://coq.inria.fr/refman/user-extensions/proof-schemes.html) for more information.

2. If a module you are processing includes or depends on an unsupported term, like one that uses mutual recursion,
you can use the `opaque` option to tell `Preprocess Module` to ignore it. For example, if you want to ignore `Baz.bar` when preprocessing the module `Baz`, you can write this:

```
Preprocess Module Baz as Baz' { opaque Baz.bar }.
```

Do note, however, that you may run into issues with later definitions that depend on the definitions you ignore.
So you may have to list several definitions to treat as opaque.

3. If you encounter an issue you can't solve, or you don't know why something isn't working, then please look
at the output up to that error. It will print all of the definitions up until the one that it failed to process.
You can then try to set that as opaque and continue, if you do not need it to use eliminators later on.

4. If you can't set a term as opaque or don't know why it isn't working but would like for it to work, then 
when you report a bug, please include enough information to reproduce the bug, as well as the name of the
last definition `Preprocess Module` tries before failing. If you see a type error, please also include the
type error.

## Guide

* [LICENSE](/LICENSE): License
* [README.md](/README.md): You are here!
* [plugin](/plugin): Main plugin directory
  - [build.sh](/plugin/build.sh): Build script
  - [test.sh](/plugin/test.sh): Test script
  - [coq](/plugin/coq): Examples and tests using Preprocess
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
