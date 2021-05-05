let abs_path = (^) (try Sys.getenv "DAI_ROOT" with _ -> failwith "environment variable DAI_ROOT is unset; set manually or build with `make build`")

let test_case f = "test_cases/" ^ f ^ ".js"

let daig_output f = abs_path ("out/daig/" ^ f ^ ".dot")

let cfg_output f = abs_path ("out/cfg/" ^ f ^ ".dot")

let log_output f = abs_path ("out/log/" ^ f ^ ".log")

let exp_output f = abs_path ("out/experiments/" ^ f ^ ".log")
