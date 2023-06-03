open Core.Std
open OUnit

let settings = Searchsettings.default_settings

let test_fixture = "Searcher" >:::
[
  (*****************************************************************************
   * is_search_dir tests
   *****************************************************************************)
  "test_is_search_dir_no_patterns" >:: (fun () ->
    let dir = "mlsearch" in
    assert_equal (Searcher.is_search_dir settings dir) true;
  );

  "test_is_search_dir_matches_in_pattern" >:: (fun () ->
    let dir = "mlsearch" in
    let ss = { settings with in_dir_patterns=[Re2.Regex.create_exn dir] } in
    assert_equal (Searcher.is_search_dir ss dir) true;
  );

  "test_is_search_dir_no_match_in_pattern" >:: (fun () ->
    let dir = "mlsearch" in
    let ss = { settings with in_dir_patterns=[Re2.Regex.create_exn "nomatch"] } in
    assert_equal (Searcher.is_search_dir ss dir) false;
  );

  "test_is_search_dir_matches_out_pattern" >:: (fun () ->
    let dir = "mlsearch" in
    let ss = { settings with out_dir_patterns=[Re2.Regex.create_exn dir] } in
    assert_equal (Searcher.is_search_dir ss dir) false;
  );

  "test_is_search_dir_no_match_out_pattern" >:: (fun () ->
    let dir = "mlsearch" in
    let ss = { settings with out_dir_patterns=[Re2.Regex.create_exn "nomatch"] } in
    assert_equal (Searcher.is_search_dir ss dir) true;
  );

  "test_is_search_dir_single_dot" >:: (fun () ->
    let dir = "." in
    assert_equal (Searcher.is_search_dir settings dir) true;
  );

  "test_is_search_dir_double_dot" >:: (fun () ->
    let dir = ".." in
    assert_equal (Searcher.is_search_dir settings dir) true;
  );

  "test_is_search_dir_hidden_dir" >:: (fun () ->
    let dir = ".git" in
    assert_equal (Searcher.is_search_dir settings dir) false;
  );

  "test_is_search_dir_hidden_dir_include_hidden" >:: (fun () ->
    let dir = ".git" in
    let ss = { settings with exclude_hidden=false } in
    assert_equal (Searcher.is_search_dir ss dir) true;
  );

  (*****************************************************************************
   * is_search_file tests
   *****************************************************************************)
  "test_is_search_file_matches_by_default" >:: (fun () ->
    let file = "fileutil.ml" in
    assert_equal (Searcher.is_search_file settings file) true;
  );

  "test_is_search_file_matches_in_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["ml"] } in
    assert_equal (Searcher.is_search_file ss file) true;
  );

  "test_is_search_file_no_match_in_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["pl"] } in
    assert_equal (Searcher.is_search_file ss file) false;
  );

  "test_is_search_file_matches_out_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_extensions=["ml"] } in
    assert_equal (Searcher.is_search_file ss file) false;
  );

  "test_is_search_file_no_match_out_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_extensions=["pl"] } in
    assert_equal (Searcher.is_search_file ss file) true;
  );

  "test_is_search_file_matches_in_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_file_patterns=[Re2.Regex.create_exn "file"] } in
    assert_equal (Searcher.is_search_file ss file) true;
  );

  "test_is_search_file_no_match_in_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_file_patterns=[Re2.Regex.create_exn "search"] } in
    assert_equal (Searcher.is_search_file ss file) false;
  );

  "test_is_search_file_matches_out_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_file_patterns=[Re2.Regex.create_exn "file"] } in
    assert_equal (Searcher.is_search_file ss file) false;
  );

  "test_is_search_file_no_match_out_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_file_patterns=[Re2.Regex.create_exn "search"] } in
    assert_equal (Searcher.is_search_file ss file) true;
  );

  (*****************************************************************************
   * is__archive_search_file tests
   *****************************************************************************)
  "test_is_archive_search_file_matches_by_default" >:: (fun () ->
    let file = "archive.zip" in
    assert_equal (Searcher.is_archive_search_file settings file) true;
  );

  "test_is_archive_search_file_matches_in_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archive_extensions=["zip"] } in
    assert_equal (Searcher.is_archive_search_file ss file) true;
  );

  "test_is_archive_search_file_no_match_in_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archive_extensions=["gz"] } in
    assert_equal (Searcher.is_archive_search_file ss file) false;
  );

  "test_is_archive_search_file_matches_out_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archive_extensions=["zip"] } in
    assert_equal (Searcher.is_archive_search_file ss file) false;
  );

  "test_is_archive_search_file_no_match_out_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archive_extensions=["gz"] } in
    assert_equal (Searcher.is_archive_search_file ss file) true;
  );

  "test_is_archive_search_file_matches_in_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archive_file_patterns=[Re2.Regex.create_exn "arch"] } in
    assert_equal (Searcher.is_archive_search_file ss file) true;
  );

  "test_is_archive_search_file_no_match_in_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archive_file_patterns=[Re2.Regex.create_exn "archives"] } in
    assert_equal (Searcher.is_archive_search_file ss file) false;
  );

  "test_is_archive_search_file_matches_out_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archive_file_patterns=[Re2.Regex.create_exn "arch"] } in
    assert_equal (Searcher.is_archive_search_file ss file) false;
  );

  "test_is_archive_search_file_no_match_out_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archive_file_patterns=[Re2.Regex.create_exn "archives"] } in
    assert_equal (Searcher.is_archive_search_file ss file) true;
  );

  (*****************************************************************************
   * filter_file tests
   *****************************************************************************)
  "test_filter_file_matches_by_default" >:: (fun () ->
    let file = "fileutil.ml" in
    let sf = Searchfile.create file Filetypes.Text in
    assert_equal (Searcher.filter_file settings sf) true;
  );

  "test_filter_file_is_search_file" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["ml"] } in
    let sf = Searchfile.create file Filetypes.Text in
    assert_equal (Searcher.filter_file ss sf) true;
  );

  "test_filter_file_not_is_search_file" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["pl"] } in
    let sf = Searchfile.create file Filetypes.Text in
    assert_equal (Searcher.filter_file ss sf) false;
  );

  "test_filter_file_is_hidden_file" >:: (fun () ->
    let file = ".gitignore" in
    let sf = Searchfile.create file Filetypes.Unknown in
    assert_equal (Searcher.filter_file settings sf) false;
  );

  "test_filter_file_hidden_includehidden" >:: (fun () ->
    let file = ".gitignore" in
    let ss = { settings with exclude_hidden=false } in
    let sf = Searchfile.create file Filetypes.Unknown in
    assert_equal (Searcher.filter_file ss sf) true;
  );

  "test_filter_file_archive_no_search_archive" >:: (fun () ->
    let file = "archive.zip" in
    let sf = Searchfile.create file Filetypes.Archive in
    assert_equal (Searcher.filter_file settings sf) false;
  );

  "test_filter_file_archive_search_archive" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with search_archive=true } in
    let sf = Searchfile.create file Filetypes.Archive in
    assert_equal (Searcher.filter_file ss sf) true;
  );

  "test_filter_file_archive_archives_only" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with search_archive=true; archives_only=true } in
    let sf = Searchfile.create file Filetypes.Archive in
    assert_equal (Searcher.filter_file ss sf) true;
  );

  "test_filter_file_nonarchive_archives_only" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with search_archive=true; archives_only=true } in
    let sf = Searchfile.create file Filetypes.Text in
    assert_equal (Searcher.filter_file ss sf) false;
  );

  (*****************************************************************************
   * search_lines tests
   *****************************************************************************)
  "test_search_lines" >:: (fun () ->
    let testfile = (Config.xsearchpath ^ "/shared/testFiles/testFile2.txt") in
    let search_patterns = [Re2.Regex.create_exn "Searcher"] in
    let ss = { settings with startpath=testfile; search_patterns=search_patterns } in
    match Searcher.search ss with
    | Ok (results : Searchresult.t list) ->
        assert_equal (List.length results) 2;
        let res1 = List.hd_exn results in
        assert_equal res1.line_num 23;
        assert_equal res1.match_start_index 3;
        assert_equal res1.match_end_index 11;
        let res2 = List.hd_exn (List.tl_exn results) in
        assert_equal res2.line_num 29;
        assert_equal res2.match_start_index 24;
        assert_equal res2.match_end_index 32
    | Error msg ->
        printf "Error msg: %s\n" msg;
        assert_equal false true
  );

  (*****************************************************************************
   * search_contents tests
   *****************************************************************************)
  "test_search_contents" >:: (fun () ->
    let testfile = (Config.xsearchpath ^ "/shared/testFiles/testFile2.txt") in
    let search_patterns = [Re2.Regex.create_exn "Searcher"] in
    let ss = { settings with multi_line_search=true; startpath=testfile; search_patterns=search_patterns } in
    match Searcher.search ss with
    | Ok (results : Searchresult.t list) ->
        assert_equal (List.length results) 2;
        let res1 = List.hd_exn results in
        assert_equal res1.line_num 23;
        assert_equal res1.match_start_index 3;
        assert_equal res1.match_end_index 11;
        let res2 = List.hd_exn (List.tl_exn results) in
        assert_equal res2.line_num 29;
        assert_equal res2.match_start_index 24;
        assert_equal res2.match_end_index 32
    | Error msg ->
        printf "Error msg: %s\n" msg;
        assert_equal false true
  );

]

let _ = run_test_tt ~verbose:true test_fixture;;

