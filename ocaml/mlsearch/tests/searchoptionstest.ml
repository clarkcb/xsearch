open Core.Std
open OUnit

let searchoptions = Searchoptions.get_searchoptions

let test_fixture = "Searchoptions" >:::
[
  "test_no_args" >:: (fun () ->
    match (Searchoptions.settings_from_args searchoptions []) with
    | Ok settings ->
      assert_equal settings.archivesonly false;
      assert_equal settings.debug false;
      assert_equal settings.excludehidden true;
      assert_equal settings.firstmatch false;
      assert_equal settings.in_archiveextensions [];
      assert_equal settings.in_archivefilepatterns [];
      assert_equal settings.in_dirpatterns [];
      assert_equal settings.in_extensions [];
      assert_equal settings.in_filepatterns [];
      assert_equal settings.in_linesafterpatterns [];
      assert_equal settings.in_linesbeforepatterns [];
      assert_equal settings.linesafter 0;
      assert_equal settings.linesaftertopatterns [];
      assert_equal settings.linesafteruntilpatterns [];
      assert_equal settings.linesbefore 0;
      assert_equal settings.listdirs false;
      assert_equal settings.listfiles false;
      assert_equal settings.listlines false;
      assert_equal settings.multilinesearch false;
      assert_equal settings.out_archiveextensions [];
      assert_equal settings.out_archivefilepatterns [];
      assert_equal settings.out_dirpatterns [];
      assert_equal settings.out_extensions [];
      assert_equal settings.out_filepatterns [];
      assert_equal settings.out_linesafterpatterns [];
      assert_equal settings.out_linesbeforepatterns [];
      assert_equal settings.printresults true;
      assert_equal settings.printusage false;
      assert_equal settings.printversion false;
      assert_equal settings.recursive true;
      assert_equal settings.searcharchives false;
      assert_equal settings.searchpatterns [];
      assert_equal settings.startpath "";
      assert_equal settings.uniquelines false;
      assert_equal settings.verbose false;
    | _ -> ()
  );

  "test_valid_args" >:: (fun () ->
    let args = ["-x"; "py,rb"; "-s"; "Search"; "."] in
    match (Searchoptions.settings_from_args searchoptions args) with
    | Ok settings ->
      assert_equal settings.in_extensions ["py"; "rb"];
      assert_equal settings.searchpatterns [Re2.Regex.create_exn "Search"];
      assert_equal settings.startpath ".";
    | _ -> ()
  );

  "test_archivesonly_arg" >:: (fun () ->
    let args = ["--archivesonly"] in
    match (Searchoptions.settings_from_args searchoptions args) with
    | Ok settings ->
      assert_equal settings.archivesonly true;
      assert_equal settings.searcharchives true;
    | _ -> ()
  );

  "test_debug_arg" >:: (fun () ->
    let args = ["--debug"] in
    match (Searchoptions.settings_from_args searchoptions args) with
    | Ok settings ->
      assert_equal settings.debug true;
      assert_equal settings.verbose true;
    | _ -> ()
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
