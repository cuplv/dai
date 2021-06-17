let test_case f = "test_cases/" ^ f ^ ".js"

let daig_output f = Dai.Import.abs_of_rel_path ("out/daig/" ^ f ^ ".dot")

let cfg_output f = Dai.Import.abs_of_rel_path ("out/cfg/" ^ f ^ ".dot")

let log_output f = Dai.Import.abs_of_rel_path ("out/log/" ^ f ^ ".log")

let exp_output f = Dai.Import.abs_of_rel_path ("out/experiments/" ^ f ^ ".log")
