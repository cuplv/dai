let test_case f = "/home/pldi/d1a_impl/test_cases/" ^ f ^ ".js"

let output = ( ^ ) "/out/"

let daig_output f = output ("daig/" ^ f ^ ".dot")

let cfg_output f = output ("cfg/" ^ f ^ ".dot")

let log_output f = output ("log/" ^ f ^ ".log")

let exp_output f = output ("experiments/" ^ f ^ ".log")
