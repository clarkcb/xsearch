open Core.Std
open OUnit

let test_fixture = "Searchsettings" >:::
[
  "test_default_settings" >:: (fun () ->
    let settings = Searchsettings.default_settings in
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
  );

  "test_add_extensions" >:: (fun () ->
    assert_equal (Searchsettings.add_extensions "py" []) ["py"];
    assert_equal (Searchsettings.add_extensions "py,rb" []) ["py"; "rb"];
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
