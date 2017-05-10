# Revision history for config-schema

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
