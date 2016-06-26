open Core.Std
open OUnit

let test_fixture = "Searchresult" >:::
[
  "singlelineresult" >:: (fun () ->
    let pattern = "Search" in
    let file = Searchfile.create "./xsearch/csharp/CsSearch/CsSearch/Searcher.cs" Filetypes.Text in
    let linenum = 10 in
    let matchstartindex = 15 in
    let matchendindex = 23 in
    let line = "\tpublic class Searcher\n" in
    let linesbefore = [] in
    let linesafter = [] in
    let result = Searchresult.create pattern linenum matchstartindex matchendindex line linesbefore linesafter in
    let result_with_file = { result with file=file } in
    let expectedoutput = sprintf "%s: %d: [%d:%d]: %s" (Searchfile.to_string file) linenum matchstartindex matchendindex (String.strip line) in
    let resultoutput = Searchresult.to_string result_with_file in
    printf "expectedoutput: \"%s\"\n" expectedoutput;
    printf "resultoutput: \"%s\"\n" resultoutput;
    assert_equal expectedoutput resultoutput
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
