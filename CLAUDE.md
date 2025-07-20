# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the `subcategories` Haskell package that provides variants of functor-like structures where domain types are constrained. The package allows treating structures like `Set` as functors with domains restricted to specific typeclass constraints (e.g., `Ord` instances).

## Build System & Commands

This project uses Cabal with Hpack for package management:

- **Build**: `cabal v2-build all`
- **Test**: `cabal v2-test` (uses tasty-discover for automatic test discovery)
- **Configure**: `cabal v2-configure --enable-tests --enable-benchmarks`
- **Clean**: `cabal v2-clean`
- **Update deps**: `cabal v2-update`
- **Build only dependencies**: `cabal v2-build all --only-dependencies`

The project uses `package.yaml` as the source of truth, which generates `subcategories.cabal` via Hpack.

## Testing Framework

- Uses **Tasty** testing framework with **tasty-discover** for automatic test discovery
- Test entry point: `test/spec.hs` (uses tasty-discover F-preprocessor)
- Tests are automatically discovered from `test/` directory following naming patterns
- Additional test dependencies: QuickCheck, inspection-testing, tasty-hunit, tasty-quickcheck, tasty-expected-failure

## Code Style & Formatting

- **Formatter**: Fourmolu (config in `fourmolu.yaml`)
- **Indentation**: 2 spaces
- **Comma style**: Leading commas
- **Record brace space**: Enabled
- **Import/export style**: Not diff-friendly (compact)

## Architecture

### Core Module Structure

The library is organized around constrained versions of standard Haskell typeclasses:

- `Control.Subcategory` - Main umbrella module re-exporting all subcategory classes
- `Control.Subcategory.Functor` - `CFunctor` typeclass for constrained functors
- `Control.Subcategory.Applicative` - `CApplicative` for constrained applicatives  
- `Control.Subcategory.Alternative` - `CAlternative` for constrained alternatives
- `Control.Subcategory.Foldable` - `CFoldable` for constrained foldables
- `Control.Subcategory.Bind` - `CBind` for constrained monadic bind
- `Control.Subcategory.Zip` - `CZip` for constrained zipping operations
- `Control.Subcategory.Pointed` - `CPointed` for constrained pointed functors
- `Control.Subcategory.Semialign` - `CSemialign` for constrained semialign
- `Control.Subcategory.RebindableSyntax` - Rebindable syntax support

### Key Design Patterns

- **Wrapper Types**: `WrapFunctor` and `WrapMono` provide zero-cost abstractions
- **Constraint Abstraction**: `Constrained` typeclass captures constraint requirements
- **Domain Types**: `Dom` type family specifies constraint domains for each type
- **Zero-cost Coercion**: Heavy use of `coerce` for runtime efficiency

### Internal Module

- `Control.Subcategory.Wrapper.Internal` - Contains `WrapFunctor` and `WrapMono` newtypes with role annotations and coercion utilities

## GHC Compatibility

Supports GHC versions: 9.0.2, 9.2.8, 9.4.8, 9.6.4, 9.8.2, 9.10.1

- Uses conditional compilation with CPP for GHC-specific features
- GHC >= 9.2.4 enables `DeepSubsumption` extension

## Default Extensions

The project uses many modern GHC extensions by default:
- `ConstraintKinds`, `DataKinds`, `TypeFamilies`, `UndecidableInstances`
- `GADTs`, `FlexibleContexts`, `FlexibleInstances`, `MultiParamTypeClasses`
- `ScopedTypeVariables`, `TypeApplications`, `PolyKinds`
- `GeneralizedNewtypeDeriving`, `DerivingStrategies`, `InstanceSigs`

## CI/CD

Uses GitHub Actions with dynamic matrix generation:
- Multiple GHC versions tested via `scripts/enumerate-ci.js`
- Separate build and test phases with artifact caching
- Custom artifact collection via `scripts/collect-artifacts.sh`

## Freeze File Management

- When Adding new freeze file to ci-configs, Prepend `import: cabal.project` at the top of freeze file.
- When downloading freeze file under ci-configs, use `wget https://www.stackage.org/{snapshot}/cabal.config -O ci-configs/ghc-{version}.project` command DO NOT read the file contents.