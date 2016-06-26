open Core.Std
open OUnit

let filetypes = Filetypes.get_filetypes

let test_fixture = "Filetypes" >:::
[
  "get_filetype" >:: (fun () ->
    assert_equal Filetypes.Text (Filetypes.get_filetype filetypes "text.txt");
    assert_equal Filetypes.Binary (Filetypes.get_filetype filetypes "binary.exe");
    assert_equal Filetypes.Archive (Filetypes.get_filetype filetypes "archive.zip")
  );
  "is_text" >:: (fun () ->
    assert_equal true (Filetypes.is_text filetypes "text.txt");
    assert_equal true (Filetypes.is_binary filetypes "binary.exe");
    assert_equal true (Filetypes.is_archive filetypes "archive.zip")
  )
]

let _ = run_test_tt ~verbose:true test_fixture;;
