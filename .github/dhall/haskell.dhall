let lib = ./lib.dhall

let GHA = lib.GHA

let ghc = lib.ghc

let versions = [ ghc "8.8.4", ghc "8.10.7", ghc "9.0.2", lib.ghcHead "9.2.1" ]

let ghcHeaders = lib.makeGhcHeader versions

let test-bins-artifact =
      { name = "test-artifacts-\${{ matrix.ghc }}", path = "test-bins/" }

let test-list-file = "tests.txt"

let test-bins-dir = "tests"

let global-stack-cache
    : lib.CacheSetup
    = { base-key = "\${{runner.os}}-build-global-stack-\${{matrix.ghc}}"
      , key-files =
        [ [ "'**/package.yaml'", "'**/*.cabal'" ]
        , [ "format('{0}', env.STACK_YAML)" ]
        ]
      , path = "~/.stack"
      }

let local-stack-cache
    : lib.CacheSetup
    = { base-key = "\${{runner.os}}-build-local-stack-\${{matrix.ghc}}"
      , key-files =
        [ [ "'**/package.yaml'", "'**/*.cabal'" ]
        , [ "format('{0}', env.STACK_YAML)" ]
        , [ "'**/*.hs'", "'**/*.lhs'" ]
        ]
      , path = "**/.stack-work"
      }

in  { on =
      { pull_request.branches = [ "master" ]
      , push.branches = [ "master" ]
      , schedule = [ { cron = "00 13 * * *" } ]
      }
    , name = "Build"
    , jobs.build
      =
            ghcHeaders
        /\  { name = "Build \${{matrix.ghc}}"
            , steps =
              [ GHA.steps.actions/checkout
              , lib.action/cache global-stack-cache
              , lib.action/cache local-stack-cache
              , lib.action/run
                  { name = "Build", run = "stack build --test --no-run-tests" }
              , lib.action/run
                  { name = "Collect test exes"
                  , run =
                      let test-list =
                            "${test-bins-artifact.path}${test-list-file}"

                      in  ''
                          mkdir -p "${test-bins-artifact.path}${test-bins-dir}"
                          touch "${test-list}"
                          stack ide targets 2>&1 | grep :test: | while read i; do
                            PACK=$(echo "$i" | cut -d':' -f1);
                            EXE=$(echo "$i" | cut -d':' -f3);
                            echo "''${EXE}" >> "${test-list}"
                            cp "''${PACK}/$(stack path --dist-dir)/build/''${EXE}/''${EXE}" "${test-bins-artifact.path}${test-bins-dir}";
                            echo 
                          done
                          ''
                  }
              , lib.action/upload test-bins-artifact
              ]
            }
    , jobs.test
      =
            ghcHeaders
        /\  { needs = [ "build" ]
            , name = "Test \${{matrix.ghc}}"
            , steps =
              [ GHA.steps.actions/checkout
              , GHA.Step::{
                , uses = Some "actions/download-artifact@v2"
                , id = Some "test-bins"
                , `with` = Some (toMap test-bins-artifact)
                }
              , lib.action/run
                  { name = "Run all tests"
                  , run =
                      let dl-path =
                            "\${{steps.test-bins.outputs.download-path}}"

                      in  ''
                          ls "${dl-path}"
                          while read -r TEST_EXE; do
                            echo "Testing: ''${TEST_EXE}"
                            TEST_BIN="${dl-path}/${test-bins-dir}/''${TEST_EXE}"
                            chmod +x "''${TEST_BIN}"
                            "''${TEST_BIN}" +RTS -N
                          done < "${dl-path}/${test-list-file}"
                          ''
                  }
              ]
            }
    }
