{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support", "either", "tuples", "maybe", "strings", "arrays", "foldable-traversable", "unicode", "control", "unfoldable", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
