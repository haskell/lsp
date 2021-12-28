# Contributing
 
## Versioning policy

`lsp` and `lsp-types` should typically have identical versions and be released in lockstep.
This is for two reasons:
1. They are morally one package, and may become one again once multiple public libraries are better supported.
2. `lsp` exports many types from `lsp-types`, so in practice will need a major verison bump when `lsp-types` does anyway.

`lsp-test` can evolve independently.

## Git tags

The tagging scheme is `$package-$version`, e.g. `lsp-types-1.0.0`.

## Making a release

1. Bump version numbers as needed (see 'Versioning policy')
2. Update changelogs (currently manual, sadly)
3. Once the above is in master, create git tags following 'Git tags'
4. Create package candidates for the packages, and check that they all look good.
5. Finalize the package candidates.
