let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "scripts/**/*.purs" ]
        , dependencies = conf.dependencies # [ "quickcheck" ]
        }
