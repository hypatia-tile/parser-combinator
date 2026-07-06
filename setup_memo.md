# setup memo

build each packages explicitly:

```sh
nix build .#megaparsec-example
nix build .#parser-demo
nix build .#megaparsec-tutorial
```

And enter the shared development shell:

```sh
nix develop
```


