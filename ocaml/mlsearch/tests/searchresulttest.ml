open Core.Std
open OUnit

let test_fixture = "Searchresult" >:::
[
  "singleline_result" >:: (fun () ->
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

  "multiline_result" >:: (fun () ->
    let pattern = "Search" in
    let file = Searchfile.create "./xsearch/csharp/CsSearch/CsSearch/Searcher.cs" Filetypes.Text in
    let linenum = 10 in
    let matchstartindex = 15 in
    let matchendindex = 23 in
    let line = "\tpublic class Searcher" in
    let linesbefore = ["namespace CsSearch"; "{"] in
    let linesafter = ["\t{"; "\t\tprivate readonly FileTypes _fileTypes;"] in
    let result = Searchresult.create pattern linenum matchstartindex matchendindex line linesbefore linesafter in
    let result_with_file = { result with file=file } in
    let expectedoutput = String.concat [
      "================================================================================\n";
      sprintf "%s: %d: [%d:%d]\n" (Searchfile.to_string file) linenum matchstartindex matchendindex;
      "--------------------------------------------------------------------------------\n";
      "   8 | namespace CsSearch\n";
      "   9 | {\n";
      "> 10 | \tpublic class Searcher\n";
      "  11 | \t{\n";
      "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    ] in
    let resultoutput = Searchresult.to_string result_with_file in
    printf "expectedoutput:\n\"%s\"\n" expectedoutput;
    printf "resultoutput:\n\"%s\"\n" resultoutput;
    assert_equal expectedoutput resultoutput
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
