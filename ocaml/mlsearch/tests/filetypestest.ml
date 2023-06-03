open Core.Std
open OUnit

let file_types = Filetypes.get_file_types

let test_fixture = "Filetypes" >:::
[
  "get_file_type" >:: (fun () ->
    assert_equal Filetypes.Text (Filetypes.get_file_type file_types "text.txt");
    assert_equal Filetypes.Binary (Filetypes.get_file_type file_types "binary.exe");
    assert_equal Filetypes.Archive (Filetypes.get_file_type file_types "archive.zip")
  );
  "is_type" >:: (fun () ->
    assert_equal true (Filetypes.is_text file_types "text.txt");
    assert_equal true (Filetypes.is_binary file_types "binary.exe");
    assert_equal true (Filetypes.is_archive file_types "archive.zip")
  )
]

let _ = run_test_tt ~verbose:true test_fixture;;
