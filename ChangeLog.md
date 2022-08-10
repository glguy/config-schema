# Revision history for config-schema

## 1.3.0.0

* Generalize atomSpec to suport arbitrary exactSpec matches

## 1.2.2.0

* Re-export `Alt` from `Config.Schema.Spec`

## 1.2.1.0

* Expose `Config.Schema.Load.Error.simplifyValueSpecMismatch`
  for providing more focused error feedback.
* Added `instance ErrorAnnotation FilePosition`

## 1.2.0.0

* Update to build against `config-value-0.7.0.0`
* Added additional specs and instances to `Config.Schema.Spec`
* Primitive number spec now only matches `Number`, previous
  `IntegerSpec` is now derived in terms of `NumberSpec`

## 1.1.0.0

* Simplify field types of `ValueSpecMismatch`
* More aggressively eliminate `TypeMismatch` and `WrongAtom`
  when other, more specific errors, are available.

## 1.0.0.0

* Rename `ValueSpec` to `PrimValueSpec`
* Rename `ValueSpecs` to `ValueSpec`
* Rename `SectionSpec` to `PrimSectionSpec`
* Rename `SectionSpecs` to `SectionsSpec`
* Rename `Spec` class to `HasSpec`
* Rename `valuesSpec` to `anySpec`
* Custom specifications changed type to expose an error message.
* Move spec types to `Config.Schema.Types`. Now `Config.Schema.Spec`
  has only the exports needed for building specs and not defining
  new spec consumers.
* Improve schema mismatch type and errors in `Config.Schema.Load.Error`

## 0.5.0.1
* Support GHC 8.4.1

## 0.5.0.0

* Add Spec instances for Int and Word types. All instances
  including the previous Int instance now validate ranges.
* Changed argument order for `loadValueFromFile`

## 0.4.1.0

* Add `loadValueFromFile` and `SchemaError`. This is intended
  as a quick way to get a configuration file loaded with all
  errors being thrown as exceptions.

## 0.4.0.0

* Parameterize the Load module on a position type
* Allow Docs module to process recursively defined
  specifications as long as a named section breaks
  the loop.
* Add parentheses to docs when needed

## 0.3.1.1  -- 2017-05-17

* Add support for GHC 7.10.3

## 0.3.1.0  -- 2017-05-16

* Allow `generateDocs` to work on any ValueSpec, rather than
  top-level empty-named section specs.

## 0.3.0.0  -- 2017-05-09

* Added "association list" specifications
* Use `pretty` library for documentation generation
* Reorder parameters so that documentation comes last
* Hide implementations of `ValueSpecs` and `SectionSpecs`

## 0.2.0.0  -- 2017-05-07

* Expose `liftValueSpec` and `liftSectionSpec`
* Add `fractionalSpec`
* Add `nonemptySpec` and `oneOrNonemptySpec`
* `loadValue` returns a `NonEmpty LoadError`

## 0.1.0.0  -- 2017-05-06

* First version. Released on an unsuspecting world.
