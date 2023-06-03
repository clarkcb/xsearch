open Core.Std
open OUnit

let test_fixture = "Searchresult" >:::
[
  "singleline_result" >:: (fun () ->
    let pattern = "Search" in
    let file = Searchfile.create "./xsearch/csharp/CsSearch/CsSearch/Searcher.cs" Filetypes.Text in
    let line_num = 10 in
    let matchstartindex = 15 in
    let matchendindex = 23 in
    let line = "\tpublic class Searcher\n" in
    let lines_before = [] in
    let lines_after = [] in
    let result = Searchresult.create pattern line_num matchstartindex matchendindex line lines_before lines_after in
    let result_with_file = { result with file=file } in
    let expected_output = sprintf "%s: %d: [%d:%d]: %s" (Searchfile.to_string file) line_num matchstartindex matchendindex (String.strip line) in
    let resultoutput = Searchresult.to_string result_with_file in
    printf "expected_output: \"%s\"\n" expected_output;
    printf "resultoutput: \"%s\"\n" resultoutput;
    assert_equal expected_output resultoutput
  );

  "multiline_result" >:: (fun () ->
    let pattern = "Search" in
    let file = Searchfile.create "./xsearch/csharp/CsSearch/CsSearch/Searcher.cs" Filetypes.Text in
    let line_num = 10 in
    let matchstartindex = 15 in
    let matchendindex = 23 in
    let line = "\tpublic class Searcher" in
    let lines_before = ["namespace CsSearch"; "{"] in
    let lines_after = ["\t{"; "\t\tprivate readonly FileTypes _fileTypes;"] in
    let result = Searchresult.create pattern line_num matchstartindex matchendindex line lines_before lines_after in
    let result_with_file = { result with file=file } in
    let expected_output = String.concat [
      "================================================================================\n";
      sprintf "%s: %d: [%d:%d]\n" (Searchfile.to_string file) line_num matchstartindex matchendindex;
      "--------------------------------------------------------------------------------\n";
      "   8 | namespace CsSearch\n";
      "   9 | {\n";
      "> 10 | \tpublic class Searcher\n";
      "  11 | \t{\n";
      "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
    ] in
    let resultoutput = Searchresult.to_string result_with_file in
    printf "expected_output:\n\"%s\"\n" expected_output;
    printf "resultoutput:\n\"%s\"\n" resultoutput;
    assert_equal expected_output resultoutput
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
