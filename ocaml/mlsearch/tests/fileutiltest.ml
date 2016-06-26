open Core.Std
open OUnit

let test_fixture = "Fileutil" >:::
[
  "is_dotdir" >:: (fun () ->
    assert_equal true (Fileutil.is_dotdir ".");
    assert_equal true (Fileutil.is_dotdir "..");
    assert_equal false (Fileutil.is_dotdir "./subdir")
  );
  "is_hidden" >:: (fun () ->
    assert_equal true (Fileutil.is_hidden ".hidden.txt");
    assert_equal false (Fileutil.is_hidden "nothidden.txt")
  );
  "get_extension" >:: (fun () ->
    assert_equal "py" (Fileutil.get_extension "/Users/cary/src/xsearch/python/pysearch/pysearch.py");
    assert_equal "" (Fileutil.get_extension "filename");
    assert_equal "" (Fileutil.get_extension ".filename");
    assert_equal "" (Fileutil.get_extension "filename.")
  )
]

let _ = run_test_tt ~verbose:true test_fixture;;
