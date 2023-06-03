open Core.Std
open OUnit

let search_options = Searchoptions.get_search_options

let test_fixture = "Searchoptions" >:::
[
  "test_no_args" >:: (fun () ->
    match (Searchoptions.settings_from_args search_options []) with
    | Ok settings ->
      assert_equal settings.archives_only false;
      assert_equal settings.debug false;
      assert_equal settings.exclude_hidden true;
      assert_equal settings.first_match false;
      assert_equal settings.in_archive_extensions [];
      assert_equal settings.in_archive_file_patterns [];
      assert_equal settings.in_dir_patterns [];
      assert_equal settings.in_extensions [];
      assert_equal settings.in_file_patterns [];
      assert_equal settings.in_lines_after_patterns [];
      assert_equal settings.in_lines_before_patterns [];
      assert_equal settings.lines_after 0;
      assert_equal settings.lines_aftertopatterns [];
      assert_equal settings.lines_afteruntilpatterns [];
      assert_equal settings.lines_before 0;
      assert_equal settings.list_dirs false;
      assert_equal settings.list_files false;
      assert_equal settings.list_lines false;
      assert_equal settings.multi_line_search false;
      assert_equal settings.out_archive_extensions [];
      assert_equal settings.out_archive_file_patterns [];
      assert_equal settings.out_dir_patterns [];
      assert_equal settings.out_extensions [];
      assert_equal settings.out_file_patterns [];
      assert_equal settings.out_lines_after_patterns [];
      assert_equal settings.out_lines_before_patterns [];
      assert_equal settings.print_results true;
      assert_equal settings.print_usage false;
      assert_equal settings.print_version false;
      assert_equal settings.recursive true;
      assert_equal settings.search_archive false;
      assert_equal settings.search_patterns [];
      assert_equal settings.startpath "";
      assert_equal settings.unique_lines false;
      assert_equal settings.verbose false;
    | _ -> ()
  );

  "test_valid_args" >:: (fun () ->
    let args = ["-x"; "py,rb"; "-s"; "Search"; "."] in
    match (Searchoptions.settings_from_args search_options args) with
    | Ok settings ->
      assert_equal settings.in_extensions ["py"; "rb"];
      assert_equal settings.search_patterns [Re2.Regex.create_exn "Search"];
      assert_equal settings.startpath ".";
    | _ -> ()
  );

  "test_archives_only_arg" >:: (fun () ->
    let args = ["--archivesonly"] in
    match (Searchoptions.settings_from_args search_options args) with
    | Ok settings ->
      assert_equal settings.archives_only true;
      assert_equal settings.search_archive true;
    | _ -> ()
  );

  "test_debug_arg" >:: (fun () ->
    let args = ["--debug"] in
    match (Searchoptions.settings_from_args search_options args) with
    | Ok settings ->
      assert_equal settings.debug true;
      assert_equal settings.verbose true;
    | _ -> ()
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
