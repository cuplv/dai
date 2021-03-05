let abs_path = (^) "/home/pldi/d1a_impl/"

let test_case f = "test_cases/" ^ f ^ ".js"

let output = ( ^ ) "out/"

let daig_output f = abs_path @@ output ("daig/" ^ f ^ ".dot")

let cfg_output f = abs_path @@ output ("cfg/" ^ f ^ ".dot")

let log_output f = abs_path @@ output ("log/" ^ f ^ ".log")

let exp_output f = abs_path @@ output ("experiments/" ^ f ^ ".log")
