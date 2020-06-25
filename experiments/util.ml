let test_case f = "/Users/benno/Documents/CU/code/d1a/test_cases/" ^ f ^ ".js"

let output = ( ^ ) "/Users/benno/Documents/CU/code/d1a/out/"

let daig_output f = output ("daig/" ^ f ^ ".dot")

let cfg_output f = output ("cfg/" ^ f ^ ".dot")

let log_output f = output ("log/" ^ f ^ ".log")
